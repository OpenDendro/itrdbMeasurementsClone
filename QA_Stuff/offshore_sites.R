# AGB -- Aug 2026
# Work out which ITRDB sites really sit in open water, and how far offshore.
#
# 01b_getElevITRDB.R calls elevatr::get_elev_point(src = "aws") without a zoom
# level, and the default is z = 5 -- roughly 3.5 km per pixel at mid latitudes.
# At that resolution a site a kilometre or two inland can land in a pixel whose
# average elevation is below zero, so the "offshore" flag from 01b overstates
# the problem. This script re-samples the flagged sites at z = 12 (about 27 m
# per pixel) and measures how far each one is from land.
#
# Coordinate precision matters as much as distance. Many of these studies
# predate common GPS use, so their coordinates were read off paper maps and
# rounded. A site given to two decimal places is only good to about a
# kilometre, so landing just offshore tells us nothing. A site given to five
# decimals implies a GPS fix, and if that lands in open water something is
# genuinely wrong. A site is only called out when it is further offshore than
# its own coordinates are precise.

rm(list=ls())
suppressPackageStartupMessages({
  library(elevatr); library(sf)
  library(rnaturalearth); library(rnaturalearthdata)
})

load("Rdatafiles/cleaned_itrdb.Rdata")
load("Rdatafiles/altElev_itrdb.Rdata")

meta <- itrdb_meta
meta$StudyID <- rownames(meta)
meta$elev_coarse <- altElev$elevation           # z = 5, from 01b

flagged <- meta[!is.na(meta$elev_coarse) & meta$elev_coarse < 0, ]
cat("sites flagged offshore at z = 5:", nrow(flagged), "\n")

## ---- 1. re-sample at a resolution that can see a coastline ---------------
## One site per call. Passing them all at once makes elevatr build a bounding
## box spanning every site on earth and try to download the planet.
flagged$elev_fine <- NA_real_
pb <- txtProgressBar(0, nrow(flagged), style = 3)
for (i in seq_len(nrow(flagged))) {
  e <- try(get_elev_point(data.frame(x = flagged$Long[i], y = flagged$Lat[i]),
                          prj = 4326, src = "aws", z = 12), silent = TRUE)
  if (!inherits(e, "try-error")) flagged$elev_fine[i] <- e$elevation
  setTxtProgressBar(pb, i)
}
close(pb)
cat("\nstill below sea level at z = 12:", sum(flagged$elev_fine < 0, na.rm = TRUE), "\n")

## ---- 2. how far to the nearest land? -------------------------------------
## Against the 1:50m coastline, which is good to roughly a kilometre. That is
## the same order as the coordinate precision of most of these studies, so
## there is no point reaching for anything finer.
land <- ne_countries(scale = 50, returnclass = "sf")
land <- st_make_valid(st_union(land))
pts <- st_as_sf(flagged, coords = c("Long","Lat"), crs = 4326, remove = FALSE)
flagged$dist_to_land_km <- as.numeric(st_distance(pts, land)) / 1000

## ---- 3. coordinate precision --------------------------------------------
## Trailing zeros are gone by the time the value is numeric, so this is a lower
## bound on what the author actually wrote down.
## AGB Aug 2026: do this one value at a time. format() is vectorised and pads
## every element to the widest in the vector, so a single 5-decimal coordinate
## made the whole column look like it had 5 decimals. all.equal() rather than ==
## because Lat/Long are means of two DIF fields and carry floating point noise.
decimals <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_integer_)
    k <- 0L
    while (k < 8L && !isTRUE(all.equal(v, round(v, k), tolerance = 1e-9))) k <- k + 1L
    k
  }, integer(1))
}
flagged$coord_decimals <- pmin(decimals(flagged$Lat), decimals(flagged$Long))
flagged$coord_precision_km <- 111 * 10^(-flagged$coord_decimals)

## ---- 4. verdict and output ----------------------------------------------
flagged$verdict <- with(flagged, ifelse(
  !is.na(elev_fine) & elev_fine >= 0, "on land at fine resolution -- z=5 artefact",
  ifelse(dist_to_land_km <= coord_precision_km,
         "offshore by less than the coordinate precision -- probably fine",
         "genuinely offshore")))

out <- flagged[order(-flagged$dist_to_land_km),
               c("StudyID","XML_FileName","Lat","Long","Altitude",
                 "elev_coarse","elev_fine","dist_to_land_km",
                 "coord_decimals","coord_precision_km","GenusSpp","verdict")]
write.csv(out, "QA_Stuff/offshore-sites.csv", row.names = FALSE)

cat("\n"); print(table(out$verdict))
cat("\n=== the ones worth reporting ===\n")
print(out[out$verdict == "genuinely offshore",
          c("StudyID","Lat","Long","Altitude","elev_fine",
            "dist_to_land_km","coord_decimals")], row.names = FALSE, digits = 4)
cat("\nwrote QA_Stuff/offshore-sites.csv\n")
