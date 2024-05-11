# AGB -- testing Hung's new read func. It's great!
#
# Take the cleaned data and loop through each study to
# read in the rwl file for each one.
# if there is >1 rwl file, get the one with the shortest
# filename. Tends to be the cleanest one (not EW/LW etc.)


# rm(list=ls())
# require(dplR)

library(doFuture)
source("QA_Stuff/read_tucson2.R")

load("RdataFiles/cleaned_itrdb.Rdata")
head(itrdb_meta)
# get studies with RWL files
studies2get <- itrdb_meta$RWL_Count > 0
summary(studies2get)
studies_gt_one <- itrdb_meta$RWL_Count > 1
summary(studies_gt_one)

rwls_meta <- itrdb_meta[studies2get,]
rwls_meta <- droplevels(rwls_meta)
head(rwls_meta)

rwls2get <- itrdb_rwl[studies2get]

# Try and read.
# There are still bad files.
# Many would be dealt with usung the  fix dups. But let's try them all with
# defaults and save the error codes

# HN: changed to parallel operation to speed up
fnameUsed <- sapply(rwls2get, \(s) {
   fname <- if (length(s) > 1) {
      getShorty <- which.min(nchar(s))
      s[getShorty]
   } else s
   paste0("./data_files/", fname)
})

registerDoFuture()
plan(multisession)

rwls <- foreach(fname = fnameUsed) %dopar%
   try(read.tucson2(fname, verbose = FALSE), silent = TRUE)

errors <- lapply(rwls, \(x)
                 if (inherits(x, "try-error")) attr(x, "condition")[[1]])                 

badIdx <- which(!sapply(errors, is.null))
# studies that failed
studies2check <- data.frame(XML_FileName = rwls_meta$XML_FileName[badIdx],
                            rwlfilename = fnameUsed[badIdx],
                            errorThrown = sapply(badIdx, \(ii) errors[[ii]]))
write.csv(studies2check,"QA_Stuff/studies_that_failed_20240509.csv")
