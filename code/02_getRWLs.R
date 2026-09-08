# AGB -- Nov 2017, April 2024, Aug 2026, Sep 2026
# Take the cleaned data and loop through each study to
# read in the rwl file for each one.
# if there is >1 rwl file, get the one with the shortest
# filename. Tends to be the cleanest one (not EW/LW etc.)

## AGB Sep 2026: rewritten for the new dplR::read.tucson(). The old loop tried
## each file up to four times -- UTF-8, then ASCII, then latin1, then
## long = TRUE -- and recorded which attempt worked in a `fileEncoding` vector.
## None of that is needed now:
##
##   * Encoding is resolved by the reader. It tests the file for valid UTF-8,
##     which is a decision rather than a guess, and falls back to latin1 only
##     when the test fails. latin1 is total and lossless, so the fallback cannot
##     drop a byte. The reader says which path it took, so we still record it --
##     see the ENCODING_ASSUMED event below -- but we no longer discover it by
##     failing three times first.
##   * `long` is ignored. The reader detects the two column layouts line by
##     line, so a file can hold 8-character series IDs and years before -999 at
##     the same time. The retry could only ever have masked a real error.
##   * The reader no longer fails where the old one did. It returns NA and warns
##     rather than guessing, so a file that used to error now usually reads with
##     a message attached. That moves the interesting output from "did this
##     throw" to "what did the reader have to say", which is what the report
##     built at the bottom of this script now collects.
##
## Everything the reader noticed rides back on the object as
## attr(x, "dplR.provenance"): the header lines, the precision of each series,
## any series it renamed, every interior gap and what the file held there, and
## a table of events with stable ids. We keep the provenance on each rwl in
## rwls.rds and summarise it into QA_Stuff/rwl-read-report.csv.
##
## Runs in about 20 minutes on one core.

rm(list=ls())
library(dplR)
library(utils)
library(tidyverse)
load("RdataFiles/cleaned_itrdb.Rdata")
head(itrdb_meta)
# get sites with RWL files
sites2get <- itrdb_meta$RWL_Count > 0
summary(sites2get)
sites_gt_one <- itrdb_meta$RWL_Count > 1
summary(sites_gt_one) # hmmm

rwls_meta <- itrdb_meta[sites2get,]
rwls_meta <- droplevels(rwls_meta)
head(rwls_meta)

rwls2get <- itrdb_rwl[sites2get]

nstudies <- nrow(rwls_meta)

## AGB Sep 2026: interior gaps come back as NA and the reader invents nothing.
## fill.internal.NA = 0 reproduces read.tucson.legacy() exactly, which wrote a
## zero -- "this tree grew no ring here" -- into 252,371 cells across 3,255
## files where the archive only ever said "unknown". Set this to 0 if you need
## the old deliverable back; do not change it silently, it moves real numbers.
fill_internal_NA <- NULL

## AGB Sep 2026: strict is OFF, deliberately and explicitly. It is dplR's
## default too, but the clone should not inherit this one silently: strict
## turns every recoverable problem the reader reports into an error, and on
## this archive that means 283 of 6,846 studies stop reading rather than
## reading with a warning attached. Those warnings are worth having and the
## data behind them is worth keeping -- a misaligned column in one decade of
## one core is no reason to throw away the other 40 series in the file. We
## want the whole archive plus an honest account of what is wrong with it,
## which is what QA_Stuff/rwl-read-report.csv is for.
read_strict <- FALSE

rwls <- vector("list", nstudies)
fnameUsed <- character(nstudies)
readStatus <- character(nstudies)   # ok / missing / error
readError <- rep(NA_character_, nstudies)
readWarnings <- vector("list", nstudies)

pb <- txtProgressBar(0, nstudies, style = 3)
for(i in 1:nstudies){
  # what to do with > 1 rwl file? Is there a metadata approach?
  fname <- rwls2get[[i]]
  if(length(fname) > 1) {
    # get shortest rwl name -- usually the file that has all the series
    # and not broken out by EW/LW etc
    getShorty <- which.min(nchar(fname))
    fname <- fname[getShorty]
  }
  else {
    fname <- fname[1]
  }

  fname <- paste("./data_files/",fname,sep="")
  fnameUsed[i] <- fname

  ## AGB Sep 2026: a file the metadata lists but the download never fetched used
  ## to arrive here as an ordinary try-error, indistinguishable from a parse
  ## failure. Name it for what it is instead. wy081 is the current case.
  if(!file.exists(fname)){
    ## rwls[i] <- list(NULL), not rwls[[i]] <- NULL: the second deletes the
    ## element and shifts every later study one place down the list.
    rwls[i] <- list(NULL)
    readStatus[i] <- "missing"
    readError[i] <- "file not on disk"
    readWarnings[[i]] <- character(0)
    setTxtProgressBar(pb, i)
    next
  }

  ## AGB Sep 2026: keep the warnings. The new reader warns where the old one
  ## guessed, so a warning here is the reader telling us something about the
  ## file, not noise to be muffled and forgotten.
  w <- character(0)
  res <- withCallingHandlers(
    try(read.tucson(fname,
                    fill.internal.NA = fill_internal_NA,
                    strict = read_strict,
                    verbose = FALSE),
        silent = TRUE),
    warning = function(cond){
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    })

  readWarnings[[i]] <- w
  if(inherits(res, "try-error")){
    readStatus[i] <- "error"
    readError[i] <- conditionMessage(attr(res, "condition"))
    rwls[[i]] <- res
  }
  else {
    readStatus[i] <- "ok"
    rwls[[i]] <- res
  }
  setTxtProgressBar(pb, i)
}

close(pb)

table(readStatus)

## AGB Sep 2026: one report, built once, from the provenance the reader
## returned. The old script built `studies2check` three times -- twice before
## the objects it referenced existed -- and reported only which encoding
## attempt had succeeded. This says what the reader actually found.
prov_field <- function(x, what){
  if(!inherits(x, "rwl")) return(NULL)
  attr(x, "dplR.provenance")[[what]]
}

readReport <- data.frame(
  Study = rownames(rwls_meta),
  XML_FileName = rwls_meta$XML_FileName,
  rwlFile = fnameUsed,
  status = readStatus,
  error = readError,
  nSeries = vapply(rwls, function(x) if(inherits(x,"rwl")) ncol(x) else NA_integer_,
                   integer(1)),
  nCells = vapply(rwls, function(x) if(inherits(x,"rwl")) sum(!is.na(x)) else NA_integer_,
                  integer(1)),
  firstYear = vapply(rwls, function(x) if(inherits(x,"rwl")) min(as.numeric(rownames(x))) else NA_real_,
                     numeric(1)),
  lastYear = vapply(rwls, function(x) if(inherits(x,"rwl")) max(as.numeric(rownames(x))) else NA_real_,
                    numeric(1)),
  precision = vapply(rwls, function(x){
    p <- prov_field(x, "precision")
    if(is.null(p)) return(NA_character_)
    paste(sort(unique(p$precision)), collapse = " and ")
  }, character(1)),
  nGaps = vapply(rwls, function(x){
    g <- prov_field(x, "gaps")
    if(is.null(g)) NA_integer_ else nrow(g)
  }, integer(1)),
  nGapCells = vapply(rwls, function(x){
    g <- prov_field(x, "gaps")
    if(is.null(g)) NA_integer_ else as.integer(sum(g$n))
  }, integer(1)),
  nRenamed = vapply(rwls, function(x){
    r <- prov_field(x, "renames")
    if(is.null(r)) NA_integer_ else nrow(r)
  }, integer(1)),
  events = vapply(rwls, function(x){
    e <- prov_field(x, "events")
    if(is.null(e) || nrow(e) == 0) return("")
    paste(sort(unique(e$event)), collapse = ";")
  }, character(1)),
  nWarnings = lengths(readWarnings),
  warnings = vapply(readWarnings, function(w) paste(w, collapse = " | "), character(1)),
  stringsAsFactors = FALSE
)

# what did the reader have to say?
sort(table(unlist(strsplit(readReport$events[nzchar(readReport$events)], ";"))),
     decreasing = TRUE)
sum(readReport$nWarnings > 0, na.rm = TRUE)      # files raising any warning

## AGB Sep 2026: count the NAs separately. nGaps is NA for any study whose rwl
## carries no provenance, and sum(nGaps > 0, na.rm = TRUE) reports 0 for that
## just as it does for a clean archive -- so the bare sum cannot tell "no gaps"
## from "no provenance", and reads as good news either way. Provenance is
## absent from every build before Sep 2026, so a stale `rwls` in the workspace
## silently produces a reassuring zero.
cat("studies with an interior gap:", sum(readReport$nGaps > 0, na.rm = TRUE),
    "of", sum(!is.na(readReport$nGaps)), "with provenance;",
    sum(is.na(readReport$nGaps)), "without\n")
sum(readReport$nGapCells, na.rm = TRUE)          # interior gap cells in total
subset(readReport, status != "ok", c(Study, rwlFile, status, error))

write.csv(readReport, "QA_Stuff/rwl-read-report.csv", row.names = FALSE)

# write output

## AGB Aug 2026: name the list by study code. rwls is built with rwls[[i]] <- ...
## in the loop above, which loses the names process_itrdb.R put on itrdb_rwl, so
## every earlier build shipped an unnamed list and users had to index by position.
stopifnot(length(rwls) == nrow(rwls_meta))
names(rwls) <- rownames(rwls_meta)

## AGB Sep 2026: the studies that did not read are kept separately rather than
## shipped as try-error objects inside rwls.rds. 03_demo.R has always read
## Rdatafiles/rwls_bad.rds and no version of this script ever wrote it.
rwls_bad <- rwls[readStatus != "ok"]

# sadly, the rwls are >100 MB with gzip. which means github balks. So try bzip2.
saveRDS(rwls,file = "Rdatafiles/rwls.rds",compress = "bzip2")
saveRDS(rwls_meta,file = "Rdatafiles/rwls_meta.rds",compress = "bzip2")
saveRDS(rwls_bad,file = "Rdatafiles/rwls_bad.rds",compress = "bzip2")
