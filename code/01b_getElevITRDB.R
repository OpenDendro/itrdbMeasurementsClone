# AGB -- Nov 2017 -- redone in 2023 and 2024
# In order to fill in missing gaps in the ITRDB elev data,
# go and get elevs from elevatr
# this takes a bit to run (minutes)

rm(list=ls())
load("RdataFiles/cleaned_itrdb.Rdata")
library(elevatr)

sites <- data.frame(x=itrdb_meta$Long,y=itrdb_meta$Lat)
sites_aws <- get_elev_point(sites, prj = 4326, src = "aws")
summary(sites_aws)
offshoreMaybe <- itrdb_meta[sites_aws$elevation<0,]
# all well? not as such....there are a handful
offshoreMaybe


altElev <- sites_aws
save(altElev,
     file = "RdataFiles/altElev_itrdb.Rdata")


