## AGB. Modifying in April 2024
rm(list=ls())
# run sparingly. takes awhile
source("code/download_itrdb.R")
save.image("RdataFiles/download_itrdb_has_run.Rdata")

# we need a script to look for updates. can't use date modified on server since
# the dif files are generated daily from a chron job.
# so we need to open each dif file and compare it to the remote version
# then delete those files and run the download script again.


rm(list=ls())
load("RdataFiles/download_itrdb_has_run.Rdata")
source("code/process_itrdb.R")
save(itrdb_crn,itrdb_meta,itrdb_rwl,
     file = "RdataFiles/process_itrdb_has_run.Rdata")

