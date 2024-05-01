## AGB. Modifying in April 2024
rm(list=ls())
source("code/download_itrdb.R")
save.image("RdataFiles/download_itrdb_has_run.Rdata")
#n_dif <- length(list.files("./dif_files/"))
load("RdataFiles/download_itrdb_has_run.Rdata")
source("code/process_itrdb.R")
save(itrdb_crn,itrdb_meta,itrdb_rwl,
     file = "RdataFiles/process_itrdb_has_run.Rdata")

