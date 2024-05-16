# AGB -- Nov 2017, Apr 2024
# Continue cleaning the ITRDB data by adding elevations
# for sites that do not have them recorded in the metadata
rm(list=ls())
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = 110, returnclass = "sf")
#coast <- ne_coastline(scale = 10, returnclass = "sf")

load("RdataFiles/cleaned_itrdb.Rdata")
load("RdataFiles/altElev_itrdb.Rdata")

itrdb_sf <- itrdb_meta %>%
  st_as_sf(coords = c("Long","Lat"), crs = 4326)

# make a map of sites where elevatr elev is <0 or is NA
funnyElev <- itrdb_meta
funnyElev$altAltitude <- altElev$elevation
funnyElev <- funnyElev %>% filter(altAltitude < 0 | is.na(Altitude))
funnyElev <- funnyElev %>%
  st_as_sf(coords = c("Long","Lat"), crs = 4326)

missingElev <- funnyElev %>% filter(is.na(Altitude))

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = missingElev) +
  labs(title = "Missing Elevations in ITRDB") +
  theme_minimal()

write_csv(missingElev[,c(1:2,11)],file = "QA_Stuff/missingElev.csv")

mask <- is.na(itrdb_meta$Altitude)
summary(mask)
# otherwise use MapZen
mask <- is.na(itrdb_meta$Altitude)
summary(mask)
itrdb_meta$Altitude[mask] <- altElev$elevation[mask]
summary(itrdb_meta$Altitude) # still some funnies.

save(itrdb_crn,itrdb_meta,itrdb_rwl,itrdb_rwl,
     file = "RdataFiles/cleaned_itrdb.Rdata")
