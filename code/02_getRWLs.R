# AGB -- Nov 2017, April 2024
# Take the cleaned data and loop through each study to
# read in the rwl file for each one.
# if there is >1 rwl file, get the one with the shortest
# filename. Tends to be the cleanest one (not EW/LW etc.)

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

rwls <- list()
fnameUsed <- character()
fileEncoding <- character()

#options(warn=2) #0 is default
pb <- txtProgressBar(0, nstudies, style = 3)
# Try and read as UTF-8 first, then ASCII, then latin1.
# There are still bad files.
for(i in 1:nstudies){ #nstudies){
  # what to do with > 1 rwl file? Is there a metadata approach?
  fname <- rwls2get[[i]]
  if(length(fname) > 1) {
    # get shortest rwl name -- usually the file that has all the series
    # and not broekn out by EW/LW etc
    getShorty <- which.min(nchar(fname))
    fname <- fname[getShorty]
  }
  else {
    fname <- fname[1]
  }

  fname <- paste("./data_files/",fname,sep="")
  fnameUsed[i] <- fname

  res <- try(read.tucson(fname,encoding = "UTF-8",verbose = FALSE),silent = T)
  fileEncoding[i] <- "UTF-8"
  if(any(class(res)=="try-error")){
    res <- try(read.tucson(fname,encoding = "ASCII",verbose = FALSE),silent = T)
    if(any(class(res)=="rwl")) {
      fileEncoding[i] <- "ASCII"
      stop()
    }
  }
  if(any(class(res)=="try-error")){
    res <- try(read.tucson(fname,encoding = "latin1",verbose = FALSE),silent = T)
    if(any(class(res)=="rwl")) {
      fileEncoding[i] <- "latin1"
    }
  }
  if(any(class(res)=="try-error")){
    res <- try(read.tucson(fname,long=T,verbose = FALSE),silent = T)
    if(any(class(res)=="rwl")) {
      fileEncoding[i] <- "long UTF-8"
    }
  }
  if(any(class(res)=="try-error")){
    fileEncoding[i] <- "error"
  }
  rwls[[i]] <- res
  setTxtProgressBar(pb, i)
}

close(pb)

studies2check <- data.frame(i = 1:nstudies,
                            XML_FileName = rwls_meta$XML_FileName,
                            rwlfilename = fnameUsed,
                            fileEncoding = fileEncoding)
table(studies2check$fileEncoding)
# are all those really long?

studies2checkLong <- studies2check %>% filter(fileEncoding == "long UTF-8")

studies2checkLong[20,]
foo <- read.tucson(studies2checkLong$rwlfilename[20],long = TRUE)
dif_url <-
  "http://www1.ncdc.noaa.gov/pub/data/metadata/published/paleo/dif/xml/"

paste0(dif_url,studies2checkLong$XML_FileName[1])
table(fileEncoding)

# let's look at the errors (can sapply this later if feeling frisky)
# there are none at the moment. Seems odd.
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

studies2check
table(studies2check$errorThrown)

rwls_bad_long <- sapply(rwls, class)
class(rwls[[1]])

rwls_bad <- sapply(rwls, function(x) { class(x)[[1]] %in% "try-error" }, simplify = T)
summary(rwls_bad)


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


# write output

# sadly, the rwls are >100 MB with gzip. which means github balks. So try bzip2.
saveRDS(rwls,file = "Rdatafiles/rwls.rds",compress = "bzip2")
saveRDS(rwls_meta,file = "Rdatafiles/rwls_meta.rds",compress = "bzip2")

