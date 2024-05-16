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
summary(sites_aws$elevation)
elevBins <- seq(-200,max(sites_aws$elevation),by=200)
ggplot(sites_aws) +
  geom_histogram(mapping = aes(x=elevation),
                 breaks = elevBins,alpha=0.8) +
  theme_minimal()
summary(sites_aws$elevation<0)
offshoreMaybe <- itrdb_meta[sites_aws$elevation<0,]
# all well? not as such....there are a handful
# send to ed?
offshoreMaybe


altElev <- sites_aws
save(altElev,
     file = "RdataFiles/altElev_itrdb.Rdata")


