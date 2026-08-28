# AGB -- Aug 2026
# Look for implausible elevations in the ITRDB metadata.
#
# Three tests, run against the elevations NOAA reports (studies whose elevation
# we filled ourselves in 01c are excluded):
#
#   1. below sea level
#   2. reported as exactly 0 m where the terrain says otherwise -- almost
#      certainly "not recorded" entered as a zero
#   3. feet recorded as metres. No DIF carries an Altitude_Unit field, so
#      nothing in the archive declares what the number means. Where reading a
#      value as feet lands close to the terrain and reading it as metres does
#      not, feet is the better explanation.
#
# Caveat on test 3: the comparison terrain comes from 01b, sampled at elevatr's
# default z = 5, about 3.5 km per pixel. In steep country that average is far
# from any real point, so mountain sites throw false positives. The output
# carries a relief estimate so those can be set aside.

rm(list=ls())
suppressPackageStartupMessages(library(elevatr))

load("Rdatafiles/cleaned_itrdb.Rdata")
load("Rdatafiles/altElev_itrdb.Rdata")

m <- itrdb_meta
m$StudyID <- rownames(m)
m$terrain <- altElev$elevation
filled <- read.csv("QA_Stuff/missingElev.csv")$XML_FileName
r <- m[!(m$XML_FileName %in% filled), ]
cat("studies with a NOAA-reported elevation:", nrow(r), "\n")

## ---- 1. below sea level --------------------------------------------------
below <- r[r$Altitude < 0, ]
cat("below sea level:", nrow(below), "\n")

## ---- 2. exactly zero -----------------------------------------------------
zero <- r[r$Altitude == 0 & !is.na(r$terrain) & r$terrain > 100, ]
cat("reported 0 m where terrain is above 100 m:", nrow(zero), "\n")

## ---- 3. feet as metres ---------------------------------------------------
ok <- !is.na(r$terrain) & r$terrain > 50 & r$Altitude > 150
s <- r[ok, ]
s$as_metres <- round(s$Altitude * 0.3048)
s$err_if_feet <- abs(s$as_metres - s$terrain)
s$err_as_is <- abs(s$Altitude - s$terrain)
feet <- s[s$err_if_feet < 0.15 * s$terrain & s$err_as_is > 2 * s$err_if_feet, ]
cat("feet-as-metres candidates:", nrow(feet), "\n")

## Local relief, to separate a real unit error from coarse sampling in
## mountains. Sample a small ring around each candidate and take the spread.
relief <- function(lat, long, d = 5) {
  dlat <- (d / 6371) * (180 / pi)
  dlon <- dlat / cos(lat * pi / 180)
  b <- seq(0, 315, by = 45)
  pts <- data.frame(x = long + dlon * sin(b * pi/180),
                    y = lat  + dlat * cos(b * pi/180))
  e <- try(get_elev_point(pts, prj = 4326, src = "aws", z = 10)$elevation, silent = TRUE)
  if (inherits(e, "try-error")) return(NA_real_)
  diff(range(e, na.rm = TRUE))
}
feet$relief_m <- NA_real_
if (nrow(feet) > 0) {
  pb <- txtProgressBar(0, nrow(feet), style = 3)
  for (i in seq_len(nrow(feet))) {
    feet$relief_m[i] <- relief(feet$Lat[i], feet$Long[i])
    setTxtProgressBar(pb, i)
  }
  close(pb)
}
## Flat country plus a close feet fit is a convincing unit error. Rough country
## means the terrain average is unreliable and the case is not made.
feet$confidence <- ifelse(feet$err_if_feet <= 20 & feet$relief_m < 600,
                          "strong", "uncertain -- steep terrain")

out <- rbind(
  data.frame(StudyID = below$StudyID, XML_FileName = below$XML_FileName,
             Lat = below$Lat, Long = below$Long, Altitude = below$Altitude,
             terrain = below$terrain, note = "reported below sea level"),
  data.frame(StudyID = zero$StudyID, XML_FileName = zero$XML_FileName,
             Lat = zero$Lat, Long = zero$Long, Altitude = zero$Altitude,
             terrain = zero$terrain, note = "reported exactly 0 m"),
  data.frame(StudyID = feet$StudyID, XML_FileName = feet$XML_FileName,
             Lat = feet$Lat, Long = feet$Long, Altitude = feet$Altitude,
             terrain = feet$terrain,
             note = paste0("reads as ", feet$as_metres, " m if feet (",
                           feet$confidence, ")")))
write.csv(out, "QA_Stuff/elevation-oddities.csv", row.names = FALSE)

cat("\n=== strong feet-as-metres cases ===\n")
print(feet[feet$confidence == "strong",
           c("StudyID","Lat","Long","Altitude","as_metres","terrain","err_if_feet","relief_m")],
      row.names = FALSE)
cat("\nwrote QA_Stuff/elevation-oddities.csv (", nrow(out), "rows )\n")
