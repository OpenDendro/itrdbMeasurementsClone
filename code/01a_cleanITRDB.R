# AGB -- Nov 2017, April 2024
# Clean up the ITRDB data by improving the taxonomy
# filling in missing names, adding family, etc.
# Remove any studies with bad spp data
rm(list=ls())
load("RdataFiles/process_itrdb_has_run.Rdata")
library(tidyverse)
library(devtools)
#install_github("ropenscilabs/datastorr")
#install_github("wcornwell/taxonlookup")
library(taxonlookup)

# ironic that this one bums out itrdb_meta because it's about the ITRDB writ large
#https://www.ncei.noaa.gov/access/paleo-search/study/25570

badID <- which(is.na(itrdb_meta$Species))
itrdb_meta[badID,]
itrdb_crn <- itrdb_crn[-badID]
itrdb_meta <- itrdb_meta[-badID,]
itrdb_rwl <- itrdb_rwl[-badID]

# fix a few names? There are a few without full tax. But I'm not sure that this is worth it. Unless you want to get into the plot tax this might not matter but worth asking Ed about?
levels(itrdb_meta$Species)

# Make more useful species handles
names(itrdb_meta)[1] <- "SpeciesLong"
foo <- strsplit(as.character(itrdb_meta$SpeciesLong)," ")
itrdb_meta$Genus <- sapply(foo,`[`,1)
itrdb_meta$Species <- sapply(foo,`[`,2)

itrdb_meta$GenusSpp <- paste(itrdb_meta$Genus,itrdb_meta$Species,sep=" ")

itrdb_meta$Genus <- as.factor(itrdb_meta$Genus)
itrdb_meta$Species <- as.factor(itrdb_meta$Species)
itrdb_meta$GenusSpp <- as.factor(itrdb_meta$GenusSpp)
# read in tax lookup table
allSpp <- levels(itrdb_meta$GenusSpp)
nGenus <- levels(itrdb_meta$Genus)
generaTax <- lookup_table(allSpp,missing_action = "NA")
names(generaTax) <- stringr::str_to_title(names(generaTax))
generaTax[is.na(generaTax)] <- "Unkown"
itrdb_meta <- inner_join(itrdb_meta,generaTax,by="Genus")
# reorder
names(itrdb_meta)
itrdb_meta[1,]
itrdb_meta <- itrdb_meta %>% relocate(XML_FileName)
itrdb_meta <- itrdb_meta %>% relocate(SpeciesLong,.after = CRN_Count)
itrdb_meta$Species <- NULL
itrdb_meta <- itrdb_meta %>% relocate(Genus,.after = GenusSpp)


rownames(itrdb_meta) <- names(itrdb_rwl)

save(itrdb_crn,itrdb_meta,itrdb_rwl,itrdb_rwl,
     file = "RdataFiles/cleaned_itrdb.Rdata")
