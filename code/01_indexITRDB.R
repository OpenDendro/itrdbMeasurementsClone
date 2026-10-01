# AGB -- Oct 2026
# Build the index of ITRDB measurement files from NOAA's paleo-search API.
#
## AGB Oct 2026: replaces download_itrdb.R and process_itrdb.R. Those scraped
## a directory of ~7,200 DIF XML files and ~7,200 study pages to find the data
## files, then guessed which .rwl in a study was ring width from the length of
## its name. That guess was wrong for 304 studies (253 of them max density),
## and the DIF route dropped CHN087 because its XML lacks a ring-width keyword.
## Chris Guiterman (NOAA) pointed us at the API, which is the route NOAA
## maintains. It returns every study, its site, species and files, and it says
## which variable each file holds. The data files themselves are the same:
## every fileUrl points into /pub/data/paleo/treering/measurements/.
##
## One request, ~240 MB of JSON, a minute or two. The JSON is cached in
## api_cache/ (not in git). Set use_cached_api <- TRUE to rebuild the index from
## the cached copy without asking NOAA again.
##
## Writes Rdatafiles/itrdb_index.Rdata with:
##   itrdb_studies  one row per ITRDB study
##   itrdb_files    one row per .rwl file
##   index_flags    one row per problem found in NOAA's metadata
## and reports/index-flags.csv, a copy of index_flags meant for NOAA staff.

rm(list=ls())
library(jsonlite)
library(curl)

use_cached_api <- FALSE

api_url <- paste0("https://www.ncei.noaa.gov/access/paleo-search/study/search.json",
                  "?metadataOnly=true&dataPublisher=NOAA&dataTypeId=18")
## dataTypeId 18 is TREE RING. The address Chris's script uses
## (/paleo-search/... without /access/) now answers with a 301 to this one.
cache_dir <- "api_cache"
cache_file <- file.path(cache_dir, "paleo-search-treering.json")

## ---- 1. fetch ------------------------------------------------------------
if (!dir.exists(cache_dir)) dir.create(cache_dir)
if (!use_cached_api || !file.exists(cache_file)) {
  cat("asking the API for every NOAA tree-ring study (slow, ~240 MB)\n")
  tmp <- paste0(cache_file, ".tmp")
  curl_download(api_url, tmp, handle = new_handle(timeout = 900L))
  file.rename(tmp, cache_file)
}
cat("API response from", format(file.mtime(cache_file)), "\n")
api <- fromJSON(cache_file, simplifyVector = FALSE)$study
cat(length(api), "tree-ring studies in the response\n")

## ---- 2. helpers ----------------------------------------------------------
## The JSON has nulls everywhere. Turn NULL into NA so every field has a value.
nz <- function(x) if (is.null(x)) NA_character_ else as.character(x)
num <- function(x) suppressWarnings(as.numeric(nz(x)))

## The ITRDB code is the last word of the study name: "... - LAGM - ITRDB RUSS146".
## We take the code from there and only cross-check NOAA's studyCode field,
## because each one is wrong somewhere the other is right:
##   UT576 and NC036 have no studyCode, but their names end "ITRDB UT576" and
##   "ITRDB NC036". IITRDB32 has a studyCode but is a carbon isotope study from
##   Lapland, not an ITRDB site, and its name has no ITRDB code.
title_code <- function(title) {
  m <- regmatches(title, regexec("ITRDB[[:space:]]+([[:alnum:]]+)[[:space:]]*$", title))[[1]]
  if (length(m) == 2) toupper(m[2]) else NA_character_
}

## NOAA's variable names are paths: "physical property>width>total ring width".
## The last step names the variable; keep the whole path as well. One path ends
## in a bare "latewood": "biological material>tissue>wood>latewood", in percent.
## That is latewood percent, so call it that.
short_var <- function(x) {
  out <- ifelse(is.na(x), NA_character_, sub(".*>", "", x))
  out[x %in% "biological material>tissue>wood>latewood"] <- "latewood percent"
  out
}

## ---- 3. walk the studies -------------------------------------------------
flags <- list()
flag <- function(code, NOAAStudyId, problem, detail = "") {
  flags[[length(flags) + 1]] <<- data.frame(code = code, NOAAStudyId = NOAAStudyId,
                                            problem = problem, detail = detail)
}

study_rows <- list()
file_rows <- list()
n_not_itrdb <- 0

for (x in api) {
  id <- nz(x$NOAAStudyId)
  title <- nz(x$studyName)
  code <- title_code(title)
  api_code <- toupper(nz(x$studyCode))

  ## Gather every .rwl file in the study. A study can have several sites and
  ## several data tables; each file carries the species of its own table.
  files <- list()
  species <- list()
  for (st in x$site) for (pd in st$paleoData) {
    sp <- pd$species
    sp_code <- paste(vapply(sp, function(s) nz(s$speciesCode), ""), collapse = " | ")
    sp_name <- paste(vapply(sp, function(s) nz(s$scientificName), ""), collapse = " | ")
    if (length(sp) == 0) sp_code <- sp_name <- NA_character_
    species[[length(species) + 1]] <- c(sp_code, sp_name)
    for (f in pd$dataFile) {
      url <- nz(f$fileUrl)
      if (is.na(url) || !grepl("\\.rwl$", url, ignore.case = TRUE)) next
      v <- Filter(function(z) !identical(z$cvWhat, "age variable>age"), f$variables)
      files[[length(files) + 1]] <- data.frame(
        file = sub("\\.rwl$", "", tolower(basename(url)), ignore.case = TRUE),
        url = url,
        variable = if (length(v)) paste(vapply(v, function(z) nz(z$cvWhat), ""), collapse = "; ") else NA_character_,
        unit = if (length(v)) paste(vapply(v, function(z) nz(z$cvUnit), ""), collapse = "; ") else NA_character_,
        timeUnit = nz(pd$timeUnit),
        firstYear = num(pd$earliestYearCE),
        lastYear = num(pd$mostRecentYearCE),
        speciesCode = sp_code,
        scientificName = sp_name,
        urlDescription = nz(f$urlDescription),
        nVariables = length(v))
    }
  }
  if (length(files) == 0) next          # no measurements: chronology-only etc.

  ## Not an ITRDB study. Contributed datasets without an ITRDB code are how NOAA
  ## archives tree material that does not meet ITRDB standards (Ed Gille, Aug
  ## 2026). Note any that carry a studyCode anyway.
  if (is.na(code)) {
    n_not_itrdb <- n_not_itrdb + 1
    if (!is.na(api_code)) {
      flag(api_code, id, "studyCode but no ITRDB code in the study name",
           sprintf("studyCode %s; name: %s. Left out.", api_code, title))
    }
    next
  }
  if (is.na(api_code)) {
    flag(code, id, "no studyCode", sprintf("name ends ITRDB %s. Kept, using that code.", code))
  } else if (api_code != code) {
    flag(code, id, "studyCode disagrees with study name",
         sprintf("studyCode %s, name ends ITRDB %s. Kept, using the name.", api_code, code))
  }

  files <- do.call(rbind, files)
  files$code <- code
  files$NOAAStudyId <- id
  file_rows[[length(file_rows) + 1]] <- files

  ## One site for nearly every study. Where there are several, average the
  ## first site's bounding box as the old script did and say so.
  if (length(x$site) > 1) {
    flag(code, id, "more than one site", sprintf("%d sites; coordinates from the first", length(x$site)))
  }
  pr <- x$site[[1]]$geo$properties
  elev <- mean(c(num(pr$minElevationMeters), num(pr$maxElevationMeters)))

  species <- unique(do.call(rbind, species))
  study_rows[[length(study_rows) + 1]] <- data.frame(
    code = code,
    NOAAStudyId = id,
    studyName = title,
    siteName = nz(x$site[[1]]$siteName),
    investigators = nz(x$investigators),
    Lat = mean(c(num(pr$southernmostLatitude), num(pr$northernmostLatitude))),
    Long = mean(c(num(pr$westernmostLongitude), num(pr$easternmostLongitude))),
    geometry = nz(x$site[[1]]$geo$geometry$type),
    Altitude = elev,
    speciesCode = paste(na.omit(unique(species[, 1])), collapse = " | "),
    scientificName = paste(na.omit(unique(species[, 2])), collapse = " | "),
    firstYear = num(x$earliestYearCE),
    lastYear = num(x$mostRecentYearCE),
    contributionDate = nz(x$contributionDate),
    doi = nz(x$doi),
    landingPage = nz(x$onlineResourceLink),
    nRWL = nrow(files))
}

itrdb_studies <- do.call(rbind, study_rows)
itrdb_files <- do.call(rbind, file_rows)
itrdb_studies$speciesCode[!nzchar(itrdb_studies$speciesCode)] <- NA
itrdb_studies$scientificName[!nzchar(itrdb_studies$scientificName)] <- NA
itrdb_files$variableShort <- short_var(itrdb_files$variable)

## Local path mirrors the server: .../pub/data/paleo/treering/... becomes
## data_files/treering/...
itrdb_files$localPath <- file.path("data_files", sub("^.*/pub/data/paleo/", "", itrdb_files$url))
itrdb_files <- itrdb_files[, c("file", "code", "NOAAStudyId", "variableShort", "variable",
                               "unit", "timeUnit", "firstYear", "lastYear",
                               "speciesCode", "scientificName", "urlDescription",
                               "nVariables", "url", "localPath")]

## ---- 4. check the index before anything trusts it -------------------------
## These would break the steps that follow. Stop rather than carry on.
stopifnot(!anyDuplicated(itrdb_studies$code))
stopifnot(!anyDuplicated(itrdb_files$url))
## rwls.rds is keyed by file name, so file names must be unique across folders.
stopifnot(!anyDuplicated(itrdb_files$file))
stopifnot(all(grepl("^data_files/treering/measurements/", itrdb_files$localPath)))

## These are NOAA metadata problems. Keep the study, report the problem.
## AGB Oct 2026: twelve old studies end "ITRDB TN", "ITRDB BRIT" and so on, with
## the number missing, and their files are named to match (tn.rwl). The codes
## are unique, so they work as keys here, but they are not ITRDB codes. Five
## of these files also carry a variable (min density, cell wall thickness)
## whose values look like ring widths in mm. Reported, not changed.
for (i in which(!grepl("[0-9]", itrdb_studies$code))) {
  f <- itrdb_files$code == itrdb_studies$code[i]
  flag(itrdb_studies$code[i], itrdb_studies$NOAAStudyId[i], "ITRDB code has no number",
       paste0(itrdb_files$file[f], ".rwl: ", itrdb_files$variableShort[f], collapse = "; "))
}
for (i in which(is.na(itrdb_studies$scientificName))) {
  flag(itrdb_studies$code[i], itrdb_studies$NOAAStudyId[i], "no species")
}
for (i in which(is.na(itrdb_studies$Altitude))) {
  flag(itrdb_studies$code[i], itrdb_studies$NOAAStudyId[i], "no elevation",
       "filled from a terrain model in 03_metaITRDB.R")
}
for (i in which(itrdb_files$nVariables == 0)) {
  flag(itrdb_files$code[i], itrdb_files$NOAAStudyId[i], "file has no variable",
       paste0(itrdb_files$file[i], ".rwl"))
}
for (i in which(itrdb_files$nVariables > 1)) {
  flag(itrdb_files$code[i], itrdb_files$NOAAStudyId[i], "file has more than one variable",
       paste0(itrdb_files$file[i], ".rwl: ", itrdb_files$variable[i]))
}
for (i in which(itrdb_files$timeUnit != "CE")) {
  flag(itrdb_files$code[i], itrdb_files$NOAAStudyId[i], "years are not CE",
       paste0(itrdb_files$file[i], ".rwl: ", itrdb_files$timeUnit[i]))
}
has_rw <- tapply(itrdb_files$variableShort %in% "total ring width", itrdb_files$code, any)
n_rw <- tapply(itrdb_files$variableShort %in% "total ring width", itrdb_files$code, sum)
for (cd in names(has_rw)[!has_rw]) {
  flag(cd, itrdb_studies$NOAAStudyId[itrdb_studies$code == cd], "no total ring width file",
       paste(itrdb_files$variableShort[itrdb_files$code == cd], collapse = "; "))
}
for (cd in names(n_rw)[n_rw > 1]) {
  flag(cd, itrdb_studies$NOAAStudyId[itrdb_studies$code == cd], "more than one total ring width file",
       paste0(itrdb_files$file[itrdb_files$code == cd &
                                 itrdb_files$variableShort %in% "total ring width"], ".rwl", collapse = ", "))
}

index_flags <- do.call(rbind, flags)
index_flags <- index_flags[order(index_flags$problem, index_flags$code), ]
rownames(index_flags) <- NULL
rownames(itrdb_studies) <- itrdb_studies$code

## ---- 5. report -----------------------------------------------------------
cat("\nITRDB studies with a .rwl file:", nrow(itrdb_studies), "\n")
cat(".rwl files:", nrow(itrdb_files), "\n")
cat("tree-ring studies with a .rwl file but no ITRDB code (left out):", n_not_itrdb, "\n\n")
cat("files by variable:\n")
print(sort(table(itrdb_files$variableShort, useNA = "ifany"), decreasing = TRUE))
cat("\nproblems in NOAA's metadata (reports/index-flags.csv):\n")
print(table(index_flags$problem))

write.csv(index_flags, "reports/index-flags.csv", row.names = FALSE)
save(itrdb_studies, itrdb_files, index_flags, file = "Rdatafiles/itrdb_index.Rdata")
