# AGB -- Oct 2026
# Add higher taxonomy and fill missing elevations in the study table.
#
## AGB Oct 2026: replaces 01a_cleanITRDB.R, 01b_getElevITRDB.R and
## 01c_reconcileElevITRDB.R. Changes from those:
##
##   * Species names come from the API (01_indexITRDB.R). They match what the
##     DIF XML gave for every study where both have one (6,821 of 6,845 on
##     1 Oct 2026). The API has no species for 23 studies; those stay NA and are
##     listed in reports/index-flags.csv. 01a dropped any study with no species.
##     A study with no species still has measurements, so it stays.
##   * Genus, family, order and group still come from taxonlookup. Where it
##     cannot classify a name they are NA. 01a wrote "Unkown" there, which a
##     user filtering on Family would take for a real family. The 9 names it
##     failed on before ("Various taxa", SWE347's "Norway spruce") are among
##     the 23 the API leaves blank, so on 1 Oct 2026 it placed every name.
##   * 01a used inner_join() on genus, which would silently drop any study
##     whose genus taxonlookup did not return. merge(all.x = TRUE) keeps them.
##   * Elevation: the API has one for all but 12 studies (1 Oct 2026); the DIF
##     route left far more blank. The gaps are filled from the AWS terrain
##     tiles through elevatr, as 01b did, but at z = 12 (about 30 m) rather than
##     the default z = 5 (about 3.5 km). At z = 5, 17 of 37 sites 01b called
##     offshore were on land. AltitudeSource says where each value came from.
##   * Nothing here overwrites its own input, so it can be run twice. 01c
##     overwrote cleaned_itrdb.Rdata, the file 01a wrote.
##
## Writes Rdatafiles/itrdb_meta.Rdata: itrdb_studies with the new columns, and
## itrdb_files unchanged.

rm(list=ls())
library(taxonlookup)
library(elevatr)

load("Rdatafiles/itrdb_index.Rdata")

## ---- 1. taxonomy ----------------------------------------------------------
## A few studies list more than one species ("Cedrela odorata L. | Juglans
## neotropica Diels" for PER002). Classify by the first and count them all.
sp <- strsplit(itrdb_studies$scientificName, " | ", fixed = TRUE)
itrdb_studies$nSpecies <- ifelse(is.na(itrdb_studies$scientificName), 0L, lengths(sp))
first_sp <- vapply(sp, `[`, "", 1)
words <- strsplit(trimws(first_sp), "[[:space:]]+")
itrdb_studies$Genus <- vapply(words, `[`, "", 1)
itrdb_studies$GenusSpp <- ifelse(is.na(first_sp), NA_character_,
                                 paste(itrdb_studies$Genus, vapply(words, `[`, "", 2)))

genus_tax <- lookup_table(unique(na.omit(itrdb_studies$GenusSpp)), missing_action = "NA")
names(genus_tax) <- c("Genus", "Family", "Order", "Group")
genus_tax <- unique(genus_tax)
stopifnot(!anyDuplicated(genus_tax$Genus))
n_before <- nrow(itrdb_studies)
itrdb_studies <- merge(itrdb_studies, genus_tax, by = "Genus", all.x = TRUE, sort = FALSE)
stopifnot(nrow(itrdb_studies) == n_before)

cat("studies with no species from the API:", sum(is.na(itrdb_studies$scientificName)), "\n")
cat("studies taxonlookup could not place in a family:",
    sum(!is.na(itrdb_studies$Genus) & is.na(itrdb_studies$Family)), "\n")
print(itrdb_studies[!is.na(itrdb_studies$Genus) & is.na(itrdb_studies$Family),
                    c("code", "scientificName")], row.names = FALSE)

## ---- 2. elevation -------------------------------------------------------
itrdb_studies$AltitudeSource <- ifelse(is.na(itrdb_studies$Altitude), NA, "NOAA")
fill <- which(is.na(itrdb_studies$Altitude))
cat("\nstudies with no elevation from NOAA:", length(fill), "\n")
if (length(fill) > 0) {
  ## One point at a time. Given several, elevatr fetches one raster covering
  ## all of them, which at z = 12 for sites on different continents is ~100 GB.
  z <- vapply(fill, function(i) {
    pt <- data.frame(x = itrdb_studies$Long[i], y = itrdb_studies$Lat[i])
    e <- try(get_elev_point(pt, prj = 4326, src = "aws", z = 12)$elevation, silent = TRUE)
    if (inherits(e, "try-error")) NA_real_ else e
  }, numeric(1))
  itrdb_studies$Altitude[fill] <- round(z)
  itrdb_studies$AltitudeSource[fill] <- ifelse(is.na(z), NA,
                                               "terrain model (elevatr, AWS, z = 12)")
  print(itrdb_studies[fill, c("code", "Lat", "Long", "Altitude")], row.names = FALSE)
  if (anyNA(z)) {
    warning(sum(is.na(z)), " sites got no elevation from the terrain model either")
  }
}

## ---- 3. tidy and save ----------------------------------------------------
itrdb_studies <- itrdb_studies[order(itrdb_studies$code), ]
rownames(itrdb_studies) <- itrdb_studies$code
first <- c("code", "NOAAStudyId", "studyName", "siteName", "Lat", "Long", "Altitude",
           "AltitudeSource", "scientificName", "speciesCode", "nSpecies", "GenusSpp",
           "Genus", "Family", "Order", "Group")
itrdb_studies <- itrdb_studies[, c(first, setdiff(names(itrdb_studies), first))]

save(itrdb_studies, itrdb_files, index_flags, file = "Rdatafiles/itrdb_meta.Rdata")
