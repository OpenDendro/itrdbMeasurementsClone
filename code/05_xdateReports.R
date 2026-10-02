# AGB -- Oct 2026
# Run dplR::xdate.report() on every ring-width file in the clone.
#
## AGB Oct 2026: a demonstration that xdate.report() can be run across the
## whole ITRDB. Chris Guiterman and Ed Gille (NOAA) like its format, and NOAA
## plans to replace the COFECHA output in its correlation-stats/ folder after
## vetting, through its own process. This script shows that the function
## runs in bulk; it is not NOAA's pipeline.
##
## Writes, under reports/correlation-stats/:
##   txt/<continent>/<country>/<file>.txt   fixed-width, as in the ITRDB files
##   html/<continent>/<country>/<file>.html one self-contained page each
##   correlation-overview.txt and .html     a summary of the whole run
##   README.md                              the same summary, which GitHub
##                                          renders, linking every report
## and reports/xdate-summary.csv, one row per file.
##
## The folders mirror data_files/treering/measurements/, so a report sits at
## the same relative path as its .rwl. GitHub's web view lists at most 1,000
## files in a folder; northamerica/usa (2,555) and europe (2,013) go past
## that. README.md links every report, so nothing is out of reach.
##
## Only total ring width files. NOAA's correlation stats cover those, and
## crossdating statistics on density or blue intensity are another exercise.
##
## A report is rebuilt only when its .rwl changed (by MD5), the dplR version
## changed, or the settings below changed. Every report stamps the date it
## was made, so rebuilding all of them would rewrite every file in git for
## nothing. Set force <- TRUE to rebuild everything. Reports whose .rwl has
## left the index are deleted.
##
## About 0.4 s a file; a full run took 12.5 minutes on 10 cores (1 Oct 2026).

rm(list=ls())
library(dplR)
library(parallel)
library(digest)
stopifnot(packageVersion("dplR") >= "1.8.0")

force <- FALSE
n_cores <- max(1, detectCores() - 1)

## xdate.report() defaults, written out so the run records what it used.
## These match COFECHA's where Andy agrees with COFECHA: 50-year segments
## lagged 25, Pearson's r, p < 0.01, a 32-year spline. Where he does not --
## partial segments at the ends of a series, and 50-year segments on records
## too short for them -- xdate.report() does its own thing and says so in
## each report's notes.
xd_settings <- list(seg.length = 50, bin.floor = 100, nyrs = 32,
                    prewhiten = TRUE, ar.order.max = 3, pcrit = 0.01,
                    lag.max = 10, method = "pearson", biweight = TRUE)
settings_key <- paste(names(xd_settings), unlist(xd_settings), sep = "=", collapse = ";")

load("Rdatafiles/itrdb_meta.Rdata")

out_root <- "reports/correlation-stats"
summary_file <- "reports/xdate-summary.csv"
data_root <- "data_files/treering/measurements/"

## ---- 1. which files ------------------------------------------------------
rw <- itrdb_files[itrdb_files$variableShort %in% "total ring width", ]
rw <- rw[file.exists(rw$localPath), ]
rw$subdir <- sub(paste0("^", data_root), "", dirname(rw$localPath))
rw$txt <- file.path(out_root, "txt", rw$subdir, paste0(rw$file, ".txt"))
rw$html <- file.path(out_root, "html", rw$subdir, paste0(rw$file, ".html"))
rw$md5 <- vapply(rw$localPath, digest, "", file = TRUE, algo = "md5", USE.NAMES = FALSE)
st <- itrdb_studies[rw$code, ]
cat(nrow(rw), "ring-width files\n")

## ---- 2. which need a new report -----------------------------------------
dplr_version <- as.character(packageVersion("dplR"))
old <- if (file.exists(summary_file)) read.csv(summary_file, stringsAsFactors = FALSE) else NULL
## With no earlier summary every comparison below would be against NULL and
## come back zero-length, which which(!current) reads as "nothing to do".
current <- rep(FALSE, nrow(rw))
prev <- rep(NA_integer_, nrow(rw))
if (!is.null(old) && !force) {
  prev <- match(rw$file, old$file)
  current <- !is.na(prev) & old$md5[prev] == rw$md5 &
    old$dplR.version[prev] == dplr_version & old$settings[prev] == settings_key &
    file.exists(rw$txt) & file.exists(rw$html)
  current[is.na(current)] <- FALSE
}
todo <- which(!current)
cat("reports up to date:", sum(current), "  to build:", length(todo), "\n")

## ---- 3. build them ------------------------------------------------------
fmt_coord <- function(x, pos, neg) {
  ifelse(is.na(x), NA_character_,
         sprintf("%.4f %s", abs(x), ifelse(x >= 0, pos, neg)))
}

one_report <- function(i) {
  s <- st[i, ]
  meta <- list(site.name = s$siteName,
               investigators = s$investigators,
               species = trimws(paste(ifelse(is.na(s$speciesCode), "", s$speciesCode),
                                      ifelse(is.na(s$scientificName), "", s$scientificName))),
               latitude = fmt_coord(s$Lat, "N", "S"),
               longitude = fmt_coord(s$Long, "E", "W"),
               elevation = if (is.na(s$Altitude)) NA_character_ else
                 paste0(round(s$Altitude), " m",
                        if (!identical(s$AltitudeSource, "NOAA")) " (terrain model)" else ""))
  ## Blank fields would print as empty header lines; leave them out.
  meta <- meta[vapply(meta, function(v) length(v) == 1 && !is.na(v) && nzchar(v), TRUE)]

  ## Keep the warnings: count them and keep the distinct messages.
  w <- character(0)
  x <- withCallingHandlers(
    tryCatch(do.call(xdate.report, c(list(rw$localPath[i], meta = meta), xd_settings)),
             error = function(e) e),
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    })

  row <- data.frame(file = rw$file[i], code = rw$code[i], subdir = rw$subdir[i],
                    md5 = rw$md5[i], dplR.version = dplr_version, settings = settings_key,
                    status = "ok", error = NA_character_,
                    nSeries = NA_integer_, nCrossdated = NA_integer_,
                    firstYear = NA_real_, lastYear = NA_real_,
                    segUsed = NA_real_, binUsed = NA_real_,
                    nSegments = NA_integer_, nA = NA_integer_, nB = NA_integer_,
                    nBweak = NA_integer_, pctFlagged = NA_real_,
                    interseriesCor = NA_real_,
                    nCheckErrors = NA_integer_, nCheckWarnings = NA_integer_,
                    notes = "", nWarnings = length(w),
                    warnings = paste(unique(w), collapse = " | "),
                    stringsAsFactors = FALSE)
  if (inherits(x, "error")) {
    row$status <- "error"
    row$error <- conditionMessage(x)
    return(row)
  }

  dir.create(dirname(rw$txt[i]), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(rw$html[i]), recursive = TRUE, showWarnings = FALSE)
  write.xdate.report(x, rw$txt[i], type = "text")
  write.xdate.report(x, rw$html[i], type = "html")

  s2 <- x$stats
  wm <- function(v, wt) { ok <- !is.na(v); if (any(ok)) sum(v[ok] * wt[ok]) / sum(wt[ok]) else NA_real_ }
  row$nSeries <- nrow(s2)
  row$nCrossdated <- sum(s2$crossdated)
  row$firstYear <- min(s2$first)
  row$lastYear <- max(s2$last)
  row$segUsed <- x$settings$seg.used
  row$binUsed <- x$settings$bin.used
  if (!is.null(x$flags)) {
    row$nSegments <- sum(s2$n.seg, na.rm = TRUE)
    row$nA <- sum(x$flags == "A")
    row$nB <- sum(x$flags == "B")
    row$nBweak <- if (is.null(x$flagged)) 0L else sum(x$flagged$weak)
    row$pctFlagged <- 100 * (row$nA + row$nB) / max(row$nSegments, 1)
    row$interseriesCor <- wm(s2$corr, s2$n.years)
  }
  f <- x$check$findings
  if (!is.null(f)) {
    row$nCheckErrors <- sum(f$severity == "error")
    row$nCheckWarnings <- sum(f$severity == "warning")
  }
  row$notes <- paste(x$notes, collapse = " | ")
  row
}

if (length(todo) > 0) {
  t0 <- Sys.time()
  new_rows <- mclapply(todo, one_report, mc.cores = n_cores, mc.preschedule = TRUE)
  print(Sys.time() - t0)
  died <- vapply(new_rows, function(r) !is.data.frame(r), logical(1))
  if (any(died)) {
    stop(sum(died), " workers failed, e.g. on ", rw$file[todo[which(died)[1]]],
         ". Re-run, or set n_cores <- 1 to see the error.")
  }
  new_rows <- do.call(rbind, new_rows)
} else {
  new_rows <- NULL
}

## Keep the rows of reports that did not need rebuilding. Take only the
## columns this version writes, so a column dropped since the last run (as
## meanSens was) does not break the rbind().
summary_cols <- c("file", "code", "subdir", "md5", "dplR.version", "settings", "status",
                  "error", "nSeries", "nCrossdated", "firstYear", "lastYear", "segUsed",
                  "binUsed", "nSegments", "nA", "nB", "nBweak", "pctFlagged",
                  "interseriesCor", "nCheckErrors", "nCheckWarnings", "notes",
                  "nWarnings", "warnings")
summ <- rbind(if (any(current)) old[prev[current], summary_cols],
              if (!is.null(new_rows)) new_rows[, summary_cols])
summ <- summ[match(rw$file, summ$file), ]
rownames(summ) <- NULL
summ$txt <- rw$txt
summ$html <- rw$html
summ$siteName <- st$siteName
summ$species <- st$scientificName

## ---- 4. remove reports whose .rwl is gone -------------------------------
for (kind in c("txt", "html")) {
  have <- list.files(file.path(out_root, kind), recursive = TRUE, full.names = TRUE)
  have <- have[basename(have) != "README.md"]
  orphan <- setdiff(have, rw[[kind]])
  if (length(orphan) > 0) {
    unlink(orphan)
    cat("removed", length(orphan), kind, "reports whose .rwl left the index\n")
  }
}

## ---- 5. the summary -----------------------------------------------------
write.csv(summ[, setdiff(names(summ), c("txt", "html", "siteName", "species"))],
          summary_file, row.names = FALSE)

ok <- summ$status == "ok"
xd <- ok & !is.na(summ$nSegments) & summ$nSegments > 0
n_flag <- sum(summ$nA + summ$nB, na.rm = TRUE)
n_seg <- sum(summ$nSegments, na.rm = TRUE)
short_seg <- ok & !is.na(summ$segUsed) & summ$segUsed < xd_settings$seg.length
too_few <- ok & !xd
q <- function(v) formatC(quantile(v, c(0.1, 0.5, 0.9), na.rm = TRUE), format = "f", digits = 3)
## By folder (northamerica/usa, europe ...), the same split as the reports.
cont <- summ$subdir
by_cont <- do.call(rbind, lapply(split(seq_len(nrow(summ)), cont), function(j) {
  data.frame(region = cont[j[1]], files = length(j), crossdated = sum(xd[j]),
             segments = sum(summ$nSegments[j], na.rm = TRUE),
             flagged = sum(summ$nA[j] + summ$nB[j], na.rm = TRUE),
             medianCor = median(summ$interseriesCor[j], na.rm = TRUE))
}))
by_cont <- by_cont[order(-by_cont$files), ]

## Files ordered for reading: the most flagged first, then the ones that
## could not be crossdated, then the rest by code.
ord <- order(!xd, -ifelse(is.na(summ$pctFlagged), -1, summ$pctFlagged), summ$file)
tab <- summ[ord, ]

run_line <- sprintf("Run %s with dplR %s on %d ring-width files from the ITRDB clone.",
                    format(Sys.Date(), "%d %B %Y"), dplr_version, nrow(summ))
set_line <- sprintf(paste("Settings: %d-year segments, first segment at a multiple of %d years,",
                          "%d-year spline, prewhitened (AR order up to %d), Pearson's r,",
                          "p < %s, lags of up to %d years searched."),
                    xd_settings$seg.length, xd_settings$bin.floor, xd_settings$nyrs,
                    xd_settings$ar.order.max, format(xd_settings$pcrit), xd_settings$lag.max)
headline <- list(
  "Files reported" = sum(ok),
  "Files that failed" = sum(!ok),
  "Files crossdated" = sum(xd),
  "Files with too few series or years to crossdate" = sum(too_few),
  "Files on a shortened segment length" = sum(short_seg),
  "Series" = sum(summ$nSeries, na.rm = TRUE),
  "Segments tested" = n_seg,
  "Segments flagged (A or B)" = sprintf("%d (%.1f%%)", n_flag, 100 * n_flag / max(n_seg, 1)),
  "B flags weak at every lag" = sum(summ$nBweak, na.rm = TRUE),
  "Files with no flagged segment" = sum(xd & summ$nA + summ$nB == 0),
  "Files with 10% or more of segments flagged" = sum(xd & summ$pctFlagged >= 10),
  "Series intercorrelation, 10th / 50th / 90th percentile" = paste(q(summ$interseriesCor[xd]), collapse = " / "))
## AGB Oct 2026: no mean sensitivity, here or in the summary CSV. Andy's call:
## it says nothing useful about crossdating. The per-file reports still carry
## "Avg mean sensitivity", because xdate.report() prints it.
about <- c(
  "These reports were made by dplR's xdate.report(), a crossdating report in the layout of",
  "the COFECHA output in the ITRDB's correlation-stats files. They are a demonstration that the",
  "function can be run across the whole archive, not NOAA's published statistics.",
  "",
  "A segment is flagged B when some other position correlates better with the master than the",
  "dated one, and A when the dated position is the best tested but under the critical value.",
  "A B flag that is weak at every lag marks a poorly correlated segment, not a dating error.")

## -- text --
fw <- function(v, w) formatC(as.character(v), width = w)
fmt_num <- function(v, d) ifelse(is.na(v), "", formatC(v, format = "f", digits = d))
txt <- c("CROSSDATING OVERVIEW: ITRDB RING-WIDTH FILES", "", run_line, set_line, "",
         about, "", "SUMMARY", "",
         sprintf("  %-56s %s", paste0(names(headline), ":"), unlist(headline)), "",
         "BY REGION", "",
         sprintf("  %-15s %7s %11s %10s %9s %10s", "Region", "Files", "Crossdated",
                 "Segments", "Flagged", "Median r"),
         sprintf("  %-15s %7d %11d %10d %9d %10s", by_cont$region, by_cont$files,
                 by_cont$crossdated, by_cont$segments, by_cont$flagged,
                 fmt_num(by_cont$medianCor, 3)), "",
         "EVERY FILE (most flagged first; reports are in txt/ and html/ under the same path)", "",
         sprintf("  %-14s %-10s %-22s %6s %10s %5s %5s %7s %6s  %s", "File", "Study", "Folder",
                 "Series", "Years", "A", "B", "% flag", "r", "Notes"),
         sprintf("  %-14s %-10s %-22s %6s %10s %5s %5s %7s %6s  %s", tab$file, tab$code, tab$subdir,
                 fmt_num(tab$nSeries, 0),
                 ifelse(is.na(tab$firstYear), "", paste0(tab$firstYear, "-", tab$lastYear)),
                 fmt_num(tab$nA, 0), fmt_num(tab$nB, 0), fmt_num(tab$pctFlagged, 1),
                 fmt_num(tab$interseriesCor, 3),
                 ifelse(tab$status == "ok", tab$notes, paste("FAILED:", tab$error))))
writeLines(txt, file.path(out_root, "correlation-overview.txt"))

## -- html --
h <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
td <- function(x, num = FALSE) paste0(if (num) "<td class=\"n\">" else "<td>", h(x), "</td>")
row_cls <- ifelse(tab$status != "ok", " class=\"fail\"",
                  ifelse(!is.na(tab$pctFlagged) & tab$pctFlagged >= 10, " class=\"hi\"", ""))
html_rows <- paste0("<tr", row_cls, ">",
                    ifelse(tab$status == "ok",
                           paste0("<td><a href=\"", h(sub(paste0("^", out_root, "/"), "", tab$html)),
                                  "\">", h(tab$file), "</a></td>"),
                           paste0("<td>", h(tab$file), "</td>")), td(tab$code), td(tab$subdir),
                    td(ifelse(is.na(tab$species), "", tab$species)),
                    td(fmt_num(tab$nSeries, 0), TRUE),
                    td(ifelse(is.na(tab$firstYear), "", paste0(tab$firstYear, "\u2013", tab$lastYear)), TRUE),
                    td(fmt_num(tab$nA, 0), TRUE), td(fmt_num(tab$nB, 0), TRUE),
                    td(fmt_num(tab$pctFlagged, 1), TRUE), td(fmt_num(tab$interseriesCor, 3), TRUE),
                    td(ifelse(tab$status == "ok", tab$notes, paste("Failed:", tab$error))),
                    "</tr>")
html <- c("<!DOCTYPE html>", "<html lang=\"en\">", "<head>", "<meta charset=\"utf-8\">",
          "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
          paste0("<meta name=\"generator\" content=\"dplR ", dplr_version, "\">"),
          "<title>Crossdating overview: ITRDB ring-width files</title>",
          "<style>",
          "body{font-family:system-ui,-apple-system,\"Segoe UI\",Helvetica,Arial,sans-serif;color:#222;background:#fff;margin:1.5em;line-height:1.4;max-width:80em}",
          "h1{font-size:1.4em;margin-bottom:.2em}h2{font-size:1.15em;margin-top:1.8em;border-bottom:1px solid #ccc;padding-bottom:.2em}",
          "p.gen{color:#555;margin-top:0}.wrap{overflow-x:auto}",
          "table{border-collapse:collapse;font-size:.85em;margin:.4em 0 1em}th,td{border:1px solid #ddd;padding:.2em .5em;text-align:left;vertical-align:top}",
          "th{background:#f3f3f3}td.n{text-align:right;font-variant-numeric:tabular-nums}tr.hi td{background:#fff4d6}tr.fail td{background:#fde2e2}",
          "</style>", "</head>", "<body>",
          "<h1>Crossdating overview: ITRDB ring-width files</h1>",
          paste0("<p class=\"gen\">", h(run_line), "<br>", h(set_line), "</p>"),
          paste0("<p>", h(paste(about[1:3], collapse = " ")), "</p>"),
          paste0("<p>", h(paste(about[5:7], collapse = " ")), "</p>"),
          "<h2>Summary</h2>", "<table>",
          paste0("<tr><th>", h(names(headline)), "</th>", td(unlist(headline), TRUE), "</tr>"),
          "</table>",
          "<h2>By region</h2>", "<table>",
          "<tr><th>Region</th><th>Files</th><th>Crossdated</th><th>Segments</th><th>Flagged</th><th>Median r</th></tr>",
          paste0("<tr>", td(by_cont$region), td(by_cont$files, TRUE), td(by_cont$crossdated, TRUE),
                 td(by_cont$segments, TRUE), td(by_cont$flagged, TRUE),
                 td(fmt_num(by_cont$medianCor, 3), TRUE), "</tr>"),
          "</table>",
          "<h2>Every file</h2>",
          "<p>Most flagged first. Shaded: 10% or more of segments flagged. Red: the report failed.</p>",
          "<div class=\"wrap\"><table>",
          "<tr><th>File</th><th>Study</th><th>Folder</th><th>Species</th><th>Series</th><th>Years</th><th>A</th><th>B</th><th>% flagged</th><th>r</th><th>Notes</th></tr>",
          html_rows, "</table></div>", "</body>", "</html>")
writeLines(html, file.path(out_root, "correlation-overview.html"))

## -- README.md files, which GitHub renders. They link the .txt reports,
## because GitHub shows .html files as source, not as pages.
##
## One table of 6,800 files made an 840 KB README, past the size GitHub will
## render. So the top README holds the summary and the files that need a
## look, and each report folder gets its own README listing every report in
## it. GitHub shows a folder's README below its file list, even when the list
## stops at 1,000 files.
md_esc <- function(x) gsub("|", "\\|", x, fixed = TRUE)
md_rows <- function(t, link_from) {
  rel <- function(path) {
    ## path relative to the folder the README is in
    if (link_from == ".") sub(paste0("^", out_root, "/"), "", path) else basename(path)
  }
  paste0("| ", ifelse(t$status == "ok", paste0("[", t$file, "](", rel(t$txt), ")"), t$file),
         " | ", t$code, " | ", ifelse(is.na(t$species), "", md_esc(t$species)), " | ",
         fmt_num(t$nSeries, 0), " | ",
         ifelse(is.na(t$firstYear), "", paste0(t$firstYear, "–", t$lastYear)), " | ",
         fmt_num(t$nA, 0), " | ", fmt_num(t$nB, 0), " | ", fmt_num(t$pctFlagged, 1), " | ",
         fmt_num(t$interseriesCor, 3), " | ",
         md_esc(ifelse(t$status == "ok", t$notes, paste("Failed:", t$error))), " |")
}
md_head <- c("| File | Study | Species | Series | Years | A | B | % flagged | r | Notes |",
             "|---|---|---|---:|---|---:|---:|---:|---:|---|")
look <- tab[tab$status != "ok" | !xd[ord] | (!is.na(tab$pctFlagged) & tab$pctFlagged >= 10), ]

md <- c("# Crossdating overview: ITRDB ring-width files", "", run_line, "", set_line, "",
        paste(about[1:3], collapse = " "), "", paste(about[5:7], collapse = " "), "",
        "The full overview, every file in one table, is in",
        "[`correlation-overview.txt`](correlation-overview.txt) and `correlation-overview.html`",
        "(GitHub shows `.html` as source: download it to read it as a page). Every number is also",
        "in [`../xdate-summary.csv`](../xdate-summary.csv), one row per file.", "",
        "## Summary", "", "| | |", "|---|---:|",
        paste0("| ", names(headline), " | ", unlist(headline), " |"), "",
        "## By folder", "",
        "Each folder lists its reports, most flagged first. The `.html` versions sit under `html/`",
        "at the same path.", "",
        "| Folder | Files | Crossdated | Segments | Flagged | Median r |", "|---|---:|---:|---:|---:|---:|",
        paste0("| [", by_cont$region, "](txt/", by_cont$region, "/) | ", by_cont$files, " | ",
               by_cont$crossdated, " | ", by_cont$segments, " | ", by_cont$flagged, " | ",
               fmt_num(by_cont$medianCor, 3), " |"), "",
        "## Files that need a look", "",
        sprintf(paste("%d files: those with 10%% or more of segments flagged, those with too few",
                      "series or years to crossdate, and those that failed. Most flagged first."),
                nrow(look)), "",
        md_head, md_rows(look, "."))
writeLines(md, file.path(out_root, "README.md"))

for (d in unique(summ$subdir)) {
  t <- tab[tab$subdir == d, ]
  writeLines(c(paste0("# Crossdating reports: ", d), "",
               run_line, "",
               paste0("Most flagged first. Back to the [overview](",
                      paste(rep("..", length(strsplit(d, "/")[[1]]) + 1), collapse = "/"), "/)."), "",
               md_head, md_rows(t, d)),
             file.path(out_root, "txt", d, "README.md"))
}

## ---- 6. report ----------------------------------------------------------
cat("\n"); for (k in names(headline)) cat(sprintf("%-56s %s\n", paste0(k, ":"), headline[[k]]))
if (any(!ok)) {
  cat("\nfailed:\n")
  print(data.frame(file = summ$file[!ok], error = substr(summ$error[!ok], 1, 80)), row.names = FALSE)
}
sz <- sum(file.size(list.files(out_root, recursive = TRUE, full.names = TRUE))) / 2^20
cat(sprintf("\n%s: %.0f MB\n", out_root, sz))
