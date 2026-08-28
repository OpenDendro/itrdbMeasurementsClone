## AGB. Modifying in April 2024, Aug 2026
## Orchestrates the download and the parse. Run sparingly -- it leans on the
## NOAA server. Sourced scripts live alongside this one and do not clear the
## workspace; this script does.

rm(list=ls())

## AGB Aug 2026: stage switches. The goal is a current copy of the archive, so
## both default to TRUE.
##
## refresh_metadata -- delete dif_files/ and study_files/ and pull them again.
##   The DIFs are regenerated daily by a cron job, so Last-Modified tells us
##   nothing about them and a re-pull is the only way to be current.
##
## refresh_stale -- ask the server which data files have changed since we
##   fetched them, delete those, and download them again. Without this,
##   download_itrdb.R never updates a file it already has and the clone drifts
##   further from the archive with every run.
##
## To check every data file rather than just the .rwl files, set
##   stale_pattern <- NULL
## before running. That is ~54k requests instead of ~9k.
refresh_metadata <- TRUE
refresh_stale <- TRUE

## ---- 1. clear the old metadata so it downloads again --------------------
if (refresh_metadata) {
  for (d in c("dif_files", "study_files")) {
    if (dir.exists(d)) {
      unlink(d, recursive = TRUE)
      message("removed ", d)
    }
  }
}

## ---- 2. download anything we do not have --------------------------------
# run sparingly. takes awhile
source("code/download_itrdb.R")
save.image("RdataFiles/download_itrdb_has_run.Rdata")

## ---- 3. refresh the files the server has changed ------------------------
## Replaces the old note here about needing a script to look for updates.
if (refresh_stale) {
  ## not clearing the workspace here: that would discard a stale_pattern set
  ## above before check_stale_files.R could see it.
  ## check_stale_files.R downloads the new copies itself, in place.
  source("code/check_stale_files.R")   # leaves `stale_files`
  save.image("RdataFiles/download_itrdb_has_run.Rdata")
}

## ---- 4. parse the metadata ----------------------------------------------
rm(list=ls())
load("RdataFiles/download_itrdb_has_run.Rdata")
source("code/process_itrdb.R")
save(itrdb_crn,itrdb_meta,itrdb_rwl,
     file = "RdataFiles/process_itrdb_has_run.Rdata")
