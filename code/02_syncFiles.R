# AGB -- Oct 2026
# Make data_files/ match the index: fetch new files, re-fetch changed ones,
# delete the ones NOAA no longer lists.
#
## AGB Oct 2026: replaces the download half of download_itrdb.R and all of
## check_stale_files.R. Those sent one request per file (~10,000) and judged a
## file stale by comparing the server's date against the local file's
## modification time. That time means nothing after a git clone or checkout,
## which stamps every file with the moment of the checkout.
##
## This script asks for the directory listing of each server folder instead:
## about ten requests, and each listing gives every file's date and exact size.
## It records what the server said when each file was fetched in
## Rdatafiles/sync_manifest.csv, which is in git. A file is changed when the
## server's date or size differs from the manifest. No local clock is involved.
##
## The first run has no manifest. For that run only, a file counts as current
## when its size matches the server and its local time is later than the
## server's. That is the old test, and it is right on a machine that downloaded
## the files itself. On a fresh clone, set refetch_unknown <- TRUE to fetch
## every file the manifest does not know (~10,000 requests, once).
##
## Deletes: a .rwl under data_files/treering/measurements/ that is not in the
## index gets deleted, so the clone holds what NOAA lists and nothing more.
## Andy's call, Oct 2026. Other files there (-rwl-noaa.txt, .crn) are not
## touched; they are counted at the end.
##
## Set dry_run <- TRUE to see what would happen without fetching or deleting.
## Writes reports/sync-log.csv: one row per file this run acted on.

rm(list=ls())
library(curl)

dry_run <- FALSE
refetch_unknown <- FALSE

load("Rdatafiles/itrdb_index.Rdata")
manifest_file <- "Rdatafiles/sync_manifest.csv"
data_root <- "data_files/treering/measurements"

## ---- 1. ask each server folder what it holds ------------------------------
## Apache listing rows look like
##   <td><a href="ak015.rwl">ak015.rwl</a></td>
##   <td align="right">2026-06-22 15:59</td>
##   <td align="right">43180</td>
## Times are US Eastern (checked against Last-Modified: 15:59 EDT is 19:59 GMT
## in June, 18:45 EST is 23:45 GMT in January).
read_listing <- function(dir_url) {
  html <- rawToChar(curl_fetch_memory(paste0(dir_url, "/"))$content)
  pat <- paste0('<a href="([^"/]+)">[^<]*</a></td>\\s*',
                '<td align="right">([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2})</td>\\s*',
                '<td align="right">([0-9]+)</td>')
  ## Split into table rows first. One gregexpr() over the whole page (1.3 MB
  ## for europe/) takes a minute; row by row takes a twentieth of a second.
  rows <- strsplit(html, "<tr>", fixed = TRUE)[[1]]
  rows <- rows[grepl(pat, rows, perl = TRUE)]
  if (length(rows) == 0) stop("no files found in the listing for ", dir_url,
                              ". Has the listing format changed?")
  parts <- utils::strcapture(pat, rows, perl = TRUE,
                             proto = data.frame(name = "", when = "", size = ""))
  when <- as.POSIXct(parts$when, format = "%Y-%m-%d %H:%M", tz = "America/New_York")
  data.frame(url = paste0(dir_url, "/", parts$name),
             serverModified = format(when, "%Y-%m-%d %H:%M", tz = "UTC"),
             serverSize = as.numeric(parts$size))
}

dirs <- unique(dirname(itrdb_files$url))
cat("reading", length(dirs), "server listings\n")
listing <- do.call(rbind, lapply(dirs, read_listing))

files <- merge(itrdb_files[, c("file", "code", "url", "localPath")], listing,
               by = "url", all.x = TRUE)

## ---- 2. decide what each file needs ---------------------------------------
if (file.exists(manifest_file)) {
  manifest <- read.csv(manifest_file, stringsAsFactors = FALSE)
} else {
  manifest <- data.frame(localPath = character(0), url = character(0),
                         serverModified = character(0), serverSize = numeric(0),
                         fetched = character(0))
}
known <- match(files$localPath, manifest$localPath)
on_disk <- file.exists(files$localPath)
local_size <- ifelse(on_disk, file.size(files$localPath), NA)
local_time <- file.mtime(files$localPath)

files$action <- "current"
files$detail <- ""

## In the API but not on the server. Nothing to fetch. A local copy, if any,
## stays until NOAA either lists the file again or drops it from the index.
gone <- is.na(files$serverSize)
files$action[gone] <- "missing on server"
files$detail[gone] <- ifelse(on_disk[gone], "local copy kept", "no local copy")

new <- !gone & !on_disk
files$action[new] <- "new"

## A size that differs from the server means the local copy is wrong, whatever
## the manifest says.
bad_size <- !gone & on_disk & local_size != files$serverSize
files$action[bad_size] <- "changed"
files$detail[bad_size] <- sprintf("local %d bytes, server %d",
                                  local_size[bad_size], files$serverSize[bad_size])

in_manifest <- !gone & on_disk & !bad_size & !is.na(known)
moved <- in_manifest & manifest$serverModified[known] != files$serverModified
files$action[moved] <- "changed"
files$detail[moved] <- sprintf("server date %s, was %s", files$serverModified[moved],
                               manifest$serverModified[known[moved]])

unknown <- !gone & on_disk & !bad_size & is.na(known)
if (refetch_unknown) {
  files$action[unknown] <- "changed"
  files$detail[unknown] <- "not in manifest, refetch_unknown is TRUE"
} else {
  server_time <- as.POSIXct(files$serverModified, format = "%Y-%m-%d %H:%M", tz = "UTC")
  newer <- unknown & server_time > local_time
  files$action[newer] <- "changed"
  files$detail[newer] <- sprintf("not in manifest; server date %s is after local %s",
                                 files$serverModified[newer],
                                 format(local_time[newer], "%Y-%m-%d %H:%M", tz = "UTC"))
}

## Local .rwl files the index does not list.
local_rwl <- list.files(data_root, pattern = "\\.rwl$", recursive = TRUE,
                        full.names = TRUE, ignore.case = TRUE)
delisted <- setdiff(local_rwl, files$localPath)

cat("\ncurrent:          ", sum(files$action == "current"), "\n")
cat("new:              ", sum(files$action == "new"), "\n")
cat("changed:          ", sum(files$action == "changed"), "\n")
cat("missing on server:", sum(files$action == "missing on server"), "\n")
cat("delisted (delete):", length(delisted), "\n")

## ---- 3. act -------------------------------------------------------------
to_get <- which(files$action %in% c("new", "changed"))
fetched_ok <- rep(NA, nrow(files))

if (dry_run) {
  cat("\ndry run: nothing fetched or deleted\n")
} else {
  if (length(to_get) > 0) {
    cat("\nfetching", length(to_get), "files\n")
    pb <- txtProgressBar(0, length(to_get), style = 3)
    for (j in seq_along(to_get)) {
      k <- to_get[j]
      dest <- files$localPath[k]
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      ## Fetch to a temporary name and swap it in only once it has arrived
      ## whole, so a failed fetch leaves the old copy in place.
      tmp <- paste0(dest, ".tmp")
      res <- try(curl_download(files$url[k], tmp, quiet = TRUE), silent = TRUE)
      ok <- !inherits(res, "try-error") && file.exists(tmp) &&
        file.size(tmp) == files$serverSize[k]
      if (ok) {
        file.rename(tmp, dest)
      } else {
        unlink(tmp)
        files$detail[k] <- paste0(files$detail[k], if (nzchar(files$detail[k])) "; ",
                                  "FETCH FAILED, ",
                                  if (inherits(res, "try-error")) conditionMessage(attr(res, "condition"))
                                  else "size does not match the listing")
      }
      fetched_ok[k] <- ok
      setTxtProgressBar(pb, j)
    }
    close(pb)
    files$action[which(fetched_ok %in% FALSE)] <- "failed"
  }
  if (length(delisted) > 0) {
    unlink(delisted)
    cat("deleted", length(delisted), "delisted files\n")
  }

  ## The manifest records what the server said for every file we now hold a
  ## good copy of. A failed fetch keeps its old entry, so it is tried again.
  now <- format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC")
  good <- files$action %in% c("current", "new", "changed")
  update <- data.frame(localPath = files$localPath[good], url = files$url[good],
                       serverModified = files$serverModified[good],
                       serverSize = files$serverSize[good],
                       fetched = ifelse(files$action[good] == "current",
                                        manifest$fetched[match(files$localPath[good], manifest$localPath)],
                                        now))
  ## Files that were current before the manifest existed have no fetch time.
  update$fetched[is.na(update$fetched)] <- "before manifest"
  keep_old <- manifest[manifest$localPath %in% files$localPath[!good], ]
  manifest <- rbind(update, keep_old)
  manifest <- manifest[order(manifest$localPath), ]
  write.csv(manifest, manifest_file, row.names = FALSE)
}

## ---- 4. report ----------------------------------------------------------
sync_log <- rbind(
  files[files$action != "current", c("localPath", "code", "action", "detail")],
  if (length(delisted)) data.frame(localPath = delisted, code = NA,
                                   action = if (dry_run) "delisted (would delete)" else "deleted",
                                   detail = "not in the API index"))
sync_log <- cbind(run = format(Sys.time(), "%Y-%m-%d %H:%M"), dry_run = dry_run, sync_log)
write.csv(sync_log, "reports/sync-log.csv", row.names = FALSE)
cat("\nwrote reports/sync-log.csv\n")
print(table(sync_log$action))

if (any(files$action == "failed")) {
  warning(sum(files$action == "failed"), " files failed to fetch. Old copies, if any, ",
          "were kept. Run this script again; see reports/sync-log.csv.")
}

## Files under data_root this script does not manage. Not an error, but they
## go stale with nothing to refresh them.
other <- list.files(data_root, recursive = TRUE)
other <- other[!grepl("\\.rwl$", other, ignore.case = TRUE)]
cat("\nnon-.rwl files under", data_root, "(not managed here):", length(other), "\n")
