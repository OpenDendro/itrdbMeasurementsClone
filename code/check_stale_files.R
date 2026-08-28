# AGB -- Aug 2026
# Find local data files that the ITRDB has changed since we downloaded them.
#
# Why this is needed: download_itrdb.R only fetches a file if it is absent
# locally, so once a file is on disk it is never refreshed. Studies added since
# the last run come down, but corrections to existing files never do. NC4 is the
# worked example -- NOAA fixed a 71-year dating error in nc4.rwl in Nov 2024 and
# our copy from May 2024 never changed.
#
# The server does return Last-Modified for files under /pub/data/paleo/, so we
# can ask cheaply. This sends one HEAD request per file, so run it sparingly.

library(curl)

## Which files to check. Defaults to .rwl only -- those are what 02_getRWLs.R
## reads. Set stale_pattern <- NULL before sourcing to check everything under
## data_files (~54k requests). Sourcing this script leaves `stale_files`, a
## character vector of local paths the server has changed since we fetched them.
if (!exists("stale_pattern")) {
  stale_pattern <- "\\.rwl$"
}
file_pattern <- stale_pattern

base_url <- "https://www.ncei.noaa.gov/pub/data/paleo/"
data_dir <- "data_files"

local_files <- list.files(data_dir, recursive = TRUE, full.names = TRUE)
if (!is.null(file_pattern)) {
  local_files <- grep(file_pattern, local_files, value = TRUE, ignore.case = TRUE)
}
n <- length(local_files)
cat("checking", n, "files\n")

## data_files/treering/... maps to <base_url>treering/...
urls <- paste0(base_url, sub(paste0("^", data_dir, "/"), "", local_files))

remote_time <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "GMT")
status <- integer(n)

h <- new_handle(nobody = TRUE, timeout = 30L, failonerror = FALSE)
pb <- txtProgressBar(0, n, style = 3)
for (k in seq_len(n)) {
  res <- try(curl_fetch_memory(urls[k], handle = h), silent = TRUE)
  if (!inherits(res, "try-error")) {
    status[k] <- res$status_code
    lm <- parse_headers_list(res$headers)[["last-modified"]]
    if (!is.null(lm)) {
      remote_time[k] <- as.POSIXct(lm, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
    }
  }
  setTxtProgressBar(pb, k)
}
close(pb)

local_time <- file.mtime(local_files)
stale <- !is.na(remote_time) & remote_time > local_time

cat("\n")
cat("stale (server newer than local):", sum(stale), "\n")
cat("no Last-Modified returned:      ", sum(is.na(remote_time)), "\n")
cat("non-200 responses:              ", sum(status != 200), "\n")

out <- data.frame(local_file = local_files,
                  url = urls,
                  status = status,
                  local_modified = local_time,
                  remote_modified = remote_time,
                  stale = stale)
write.csv(out[out$stale | out$status != 200, ],
          file = "QA_Stuff/staleFiles.csv", row.names = FALSE)
cat("wrote QA_Stuff/staleFiles.csv\n")

stale_files <- local_files[stale]

## AGB Aug 2026: fetch the new copies here rather than deleting and letting
## download_itrdb.R backfill. That older approach lost files: download_itrdb.R
## only fetches URLs it finds in the DIF metadata, so any file NOAA has since
## delisted got deleted and never came back. Nine went that way on the first
## run (ital017, neth031, cana168*).
##
## Each file goes to a temp path first and only replaces the local copy once it
## has arrived non-empty, so a failed download leaves the old file in place.
if (length(stale_files) > 0) {
  cat("refreshing", length(stale_files), "stale files\n")
  stale_idx <- which(stale)
  refresh_ok <- rep(FALSE, length(stale_idx))
  pb <- txtProgressBar(0, length(stale_idx), style = 3)
  for (j in seq_along(stale_idx)) {
    k <- stale_idx[j]
    tmp <- paste0(local_files[k], ".tmp")
    res <- try(curl_download(urls[k], tmp), silent = TRUE)
    if (!inherits(res, "try-error") && file.exists(tmp) && file.size(tmp) > 0) {
      file.rename(tmp, local_files[k])
      refresh_ok[j] <- TRUE
    } else {
      unlink(tmp)
    }
    setTxtProgressBar(pb, j)
  }
  close(pb)
  cat("\nrefreshed:", sum(refresh_ok), " failed:", sum(!refresh_ok), "\n")
  if (any(!refresh_ok)) {
    cat("left untouched (old copy kept):\n")
    print(local_files[stale_idx][!refresh_ok])
  }
}
