# AGB -- Nov 2017, April 2024
# Take the cleaned data and loop through each study to
# read in the rwl file for each one.
# if there is >1 rwl file, get the one with the shortest
# filename. Tends to be the cleanest one (not EW/LW etc.)

rm(list=ls())
require(dplR)
source("code/read_tucson2.R")
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

nsites <- nrow(rwls_meta)
rwls <- list()

# Try and read as UTF-8 first, then ASCII, then latin1.
# There are still bad files.
for(i in 1:nsites){
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
 #res <- try(read.tucson(fname,encoding = "UTF-8"),silent = T)
 res <- try(read.tucson2(fname,verbose =FALSE),silent = T)
 # if(any(class(res)=="try-error")){
 #   res <- try(read.tucson(fname,encoding = "ASCII"),silent = T)
 # }
 # if(any(class(res)=="try-error")){
 #   res <- try(read.tucson(fname,encoding = "latin1"),silent = T)
 # }
 # if(any(class(res)=="try-error")){
 #   res <- try(read.tucson(fname,long=T),silent = T)
 # }
 if(any(class(res)=="try-error")){print(i)}
 rwls[[i]] <- res

}

rwls_bad_long <- sapply(rwls, class)
class(rwls[[1]])

rwls_bad <- sapply(rwls, function(x) { class(x)[[1]] %in% "try-error" }, simplify = T)
summary(rwls_bad)

# write output

# sadly, the rwls are >100 MB with gzip. which means github balks. So try bzip2.
saveRDS(rwls,file = "Rdatafiles/rwls.rds",compress = "bzip2")
saveRDS(rwls_meta,file = "Rdatafiles/rwls_meta.rds",compress = "bzip2")
saveRDS(rwls_bad,file = "Rdatafiles/rwls_bad.rds",compress = "bzip2")
# save(rwls,rwls_meta,rwls_bad,
#      file = "RdataFiles/rwls.Rdata")
