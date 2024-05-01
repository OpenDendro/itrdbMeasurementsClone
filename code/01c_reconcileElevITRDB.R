# AGB -- Nov 2017, Apr 2024
# Continue cleaning the ITRDB data by adding elevations
# for sites that do not have them recorded in the metadata
rm(list=ls())
load("RdataFiles/cleaned_itrdb.Rdata")
load("RdataFiles/altElev_itrdb.Rdata")

mask <- is.na(itrdb_meta$Altitude)
summary(mask)
# otherwise use MapZen
mask <- is.na(itrdb_meta$Altitude)
summary(mask)
itrdb_meta$Altitude[mask] <- altElev$elevation[mask]
summary(itrdb_meta$Altitude) # still some funnies.

save(itrdb_crn,itrdb_meta,itrdb_rwl,itrdb_rwl,
     file = "RdataFiles/cleaned_itrdb.Rdata")
