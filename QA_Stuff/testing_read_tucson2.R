# AGB -- testing Hung's new read func. It's great!
#
# Take the cleaned data and loop through each study to
# read in the rwl file for each one.
# if there is >1 rwl file, get the one with the shortest
# filename. Tends to be the cleanest one (not EW/LW etc.)


rm(list=ls())
require(dplR)
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

nstudies <- nrow(rwls_meta)
rwls <- list()
fnameUsed <- character()
# Try and read.
# There are still bad files.
# Many would be dealt with usung the  fix dups. But let's try them all with
# defaults and save the error codes
for(i in 1:nstudies){
 # read each rwl
 fname <- rwls2get[[i]]
 # if there is more than one rwl in a study just grab the shortest filename
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
 res <- try(read.tucson2(fname,verbose = FALSE),silent = TRUE)
 rwls[[i]] <- res
}
# let's look at the errors (can sapply this later if feeling frisky)
errors_when_reading <- rep("OK",nstudies)
badIdx <- rep(FALSE,nstudies)
for(i in 1:nstudies){
  if(inherits(rwls[[i]], "try-error")){
    badIdx[i] <- TRUE
    errors_when_reading[i] <- attr(rwls[[i]],"condition")[[1]]
  }
}
# studies that failed
studies2check <- data.frame(XML_FileName = rwls_meta$XML_FileName[badIdx],
                            rwlfilename = fnameUsed[badIdx],
                            errorThrown = errors_when_reading[badIdx])
write.csv(studies2check,"QA_Stuff/studies_that_failed.csv")
