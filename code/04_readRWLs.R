# AGB -- Oct 2026
# Read every .rwl file in the index with dplR::read.tucson().
#
## AGB Oct 2026: replaces 02_getRWLs.R. The reading itself is unchanged: same
## reader, same settings, same provenance kept on each object, same report.
## What changed is which files get read and how the result is keyed.
##
##   * Every file, not one per study. 02_getRWLs.R picked the shortest file
##     name in each study, on the theory that it would be the whole-ring file.
##     When names tie on length (russ146x, russ146w, russ146e ...) which.min()
##     takes the first, and for 304 studies that was not ring width: 253 were
##     maximum density. rwls.rds shipped them as ring width.
##   * rwls is keyed by file name without .rwl: rwls[["russ146w"]]. Where a
##     study has one plain file the key is the old study code in lower case, so
##     rwls[["ca671"]] still works. rwls_meta has one row per file, in the same
##     order, with the variable NOAA says the file holds. To get ring width:
##       rw <- rwls[rwls_meta$variableShort %in% "total ring width"]
##   * Files that did not read are left out of rwls and kept in rwls_bad, as
##     before. A file the index lists but the sync could not fetch has status
##     "missing".
##
## Needs dplR 1.8.0 (dev): strict, fill.internal.NA = NULL and the
## dplR.provenance attribute are not in 1.7.x.
##
## Reads in parallel with mclapply (forks, so macOS/Linux only). About 20
## minutes on 10 cores, then a few minutes to compress rwls.rds (79 MB with
## bzip2 on 1 Oct 2026).

rm(list=ls())
library(dplR)
library(parallel)
stopifnot(packageVersion("dplR") >= "1.8.0")

load("Rdatafiles/itrdb_meta.Rdata")

## AGB Sep 2026 (carried over from 02_getRWLs.R): interior gaps come back as NA
## and the reader invents nothing. fill.internal.NA = 0 reproduces
## read.tucson.legacy() exactly, which wrote a zero -- "this tree grew no ring
## here" -- into 252,371 cells across 3,255 files where the archive only ever
## said "unknown". Do not change this silently; it moves real numbers.
fill_internal_NA <- NULL

## AGB Sep 2026 (carried over): strict is OFF, deliberately. strict turns every
## recoverable problem into an error; on this archive that stops 283 of 6,846
## studies from reading at all. We want the whole archive plus an honest account
## of what is wrong with it, which is what the read report is for.
read_strict <- FALSE

n_cores <- max(1, detectCores() - 1)

read_one <- function(path) {
  if (!file.exists(path)) {
    return(list(status = "missing", error = "file not on disk", warnings = character(0),
                rwl = NULL))
  }
  ## Keep the warnings. The reader warns where the old one guessed, so a
  ## warning is the reader telling us something about the file.
  w <- character(0)
  res <- withCallingHandlers(
    try(read.tucson(path, fill.internal.NA = fill_internal_NA,
                    strict = read_strict, verbose = FALSE),
        silent = TRUE),
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    })
  if (inherits(res, "try-error")) {
    list(status = "error", error = conditionMessage(attr(res, "condition")),
         warnings = w, rwl = NULL)
  } else {
    list(status = "ok", error = NA_character_, warnings = w, rwl = res)
  }
}

cat("reading", nrow(itrdb_files), "files on", n_cores, "cores\n")
t0 <- Sys.time()
## Prescheduled: each worker is forked once and takes every n_cores-th file.
## On 1,000 files that took 40 s against 46 s for one fork per file. The
## reader itself sets the pace, at about 0.4 s a file.
out <- mclapply(itrdb_files$localPath, read_one, mc.cores = n_cores,
                mc.preschedule = TRUE)
print(Sys.time() - t0)

## mclapply returns a try-error in place of a result when a worker dies.
## That is our failure, not the file's. Stop rather than report it as one.
died <- vapply(out, function(x) !is.list(x) || is.null(x$status), logical(1))
if (any(died)) {
  stop(sum(died), " workers failed, e.g. on ", itrdb_files$localPath[which(died)[1]],
       ". Re-run, or set n_cores <- 1 to see the error.")
}

status <- vapply(out, `[[`, "", "status")
names(out) <- itrdb_files$file
print(table(status))

## ---- report -------------------------------------------------------------
prov_field <- function(x, what) {
  if (!inherits(x, "rwl")) return(NULL)
  attr(x, "dplR.provenance")[[what]]
}
rwl_of <- lapply(out, `[[`, "rwl")
num_or_na <- function(f, type = numeric(1)) vapply(rwl_of, function(x) if (inherits(x, "rwl")) f(x) else type[NA], type)

readReport <- data.frame(
  file = itrdb_files$file,
  code = itrdb_files$code,
  variable = itrdb_files$variableShort,
  localPath = itrdb_files$localPath,
  status = status,
  error = vapply(out, `[[`, "", "error"),
  nSeries = num_or_na(ncol, integer(1)),
  nCells = num_or_na(function(x) sum(!is.na(x)), integer(1)),
  firstYear = num_or_na(function(x) min(as.numeric(rownames(x)))),
  lastYear = num_or_na(function(x) max(as.numeric(rownames(x)))),
  precision = vapply(rwl_of, function(x) {
    p <- prov_field(x, "precision")
    if (is.null(p)) NA_character_ else paste(sort(unique(p$precision)), collapse = " and ")
  }, character(1)),
  nGaps = vapply(rwl_of, function(x) {
    g <- prov_field(x, "gaps"); if (is.null(g)) NA_integer_ else nrow(g)
  }, integer(1)),
  nGapCells = vapply(rwl_of, function(x) {
    g <- prov_field(x, "gaps"); if (is.null(g)) NA_integer_ else as.integer(sum(g$n))
  }, integer(1)),
  nRenamed = vapply(rwl_of, function(x) {
    r <- prov_field(x, "renames"); if (is.null(r)) NA_integer_ else nrow(r)
  }, integer(1)),
  events = vapply(rwl_of, function(x) {
    e <- prov_field(x, "events")
    if (is.null(e) || nrow(e) == 0) "" else paste(sort(unique(e$event)), collapse = ";")
  }, character(1)),
  nWarnings = vapply(out, function(x) length(x$warnings), integer(1)),
  warnings = vapply(out, function(x) paste(x$warnings, collapse = " | "), character(1)),
  stringsAsFactors = FALSE)

## NOAA's metadata gives a year range for the data table each file belongs to.
## It is a range for the table, not the file, so a mismatch is a lead, not a
## defect: on 1 Oct 2026, 942 files differed, most by a year or two. The big
## ones are the floating chronologies (swit195-198, newz116, wi013), whose
## files count years from 1 while the metadata gives cal yr BP.
readReport$indexFirstYear <- itrdb_files$firstYear
readReport$indexLastYear <- itrdb_files$lastYear
year_mismatch <- readReport$status == "ok" & !is.na(readReport$indexFirstYear) &
  (readReport$firstYear != readReport$indexFirstYear |
     readReport$lastYear != readReport$indexLastYear)
cat("\nfiles whose years differ from the range NOAA gives for their data table:",
    sum(year_mismatch), "\n")

cat("\nwhat the reader had to say:\n")
print(sort(table(unlist(strsplit(readReport$events[nzchar(readReport$events)], ";"))),
           decreasing = TRUE))
cat("files raising any warning:", sum(readReport$nWarnings > 0), "\n")
## The reader's errors run to a paragraph; the full text is in the CSV.
not_ok <- subset(readReport, status != "ok", c(file, status, error))
not_ok$error <- substr(not_ok$error, 1, 80)
print(not_ok, row.names = FALSE)

write.csv(readReport, "reports/rwl-read-report.csv", row.names = FALSE)

## ---- deliverables -------------------------------------------------------
ok <- status == "ok"
rwls <- rwl_of[ok]
rwls_bad <- out[!ok]

study_cols <- c("studyName", "siteName", "Lat", "Long", "Altitude", "AltitudeSource",
                "GenusSpp", "Genus", "Family", "Order", "Group", "investigators",
                "doi", "landingPage")
rwls_meta <- cbind(itrdb_files[ok, c("file", "code", "NOAAStudyId", "variableShort",
                                     "variable", "unit", "timeUnit", "scientificName",
                                     "speciesCode", "url")],
                   itrdb_studies[itrdb_files$code[ok], study_cols])
rownames(rwls_meta) <- rwls_meta$file
stopifnot(identical(names(rwls), rownames(rwls_meta)))

## gzip put the old, smaller rwls.rds over GitHub's 100 MB limit, so it was
## saved with bzip2. This one holds every file, so check the size.
saveRDS(rwls, file = "Rdatafiles/rwls.rds", compress = "bzip2")
saveRDS(rwls_meta, file = "Rdatafiles/rwls_meta.rds", compress = "bzip2")
saveRDS(rwls_bad, file = "Rdatafiles/rwls_bad.rds", compress = "bzip2")
mb <- file.size("Rdatafiles/rwls.rds") / 2^20
cat(sprintf("\nrwls.rds: %d files, %.1f MB\n", length(rwls), mb))
if (mb > 95) {
  warning(sprintf("rwls.rds is %.0f MB. GitHub rejects files over 100 MB.", mb))
}
