# itrdbMeasurementsClone

A copy of the tree-ring measurement data from the [International Tree-Ring Data
Bank](https://www.ncei.noaa.gov/products/paleoclimatology/tree-ring) (ITRDB), parsed into R
objects you can load in one line.

Part of the [openDendro](https://opendendro.org) project.

## Why this exists

The ITRDB is the archive of record for tree-ring measurements. It holds several thousand
studies contributed over decades. Getting all of it into R, though, means downloading
thousands of files from a public FTP tree, matching each one to its metadata in a separate
XML record, and parsing a fixed-width format that has drifted over forty years of use.

Every project that wants to work across the whole database repeats that work. This repo does
it once and publishes the result: **two files that load in a second and give you every
readable ring-width study with its metadata attached.**

That makes questions across the whole archive tractable — how sample depth varies by species,
what the age structure of the network looks like, how many series cover a given period —
without each researcher rebuilding the same pipeline.

## What you get

| File | What it holds |
|---|---|
| `Rdatafiles/rwls.rds` | A list of `rwl` objects, one per `.rwl` file in the ITRDB. Each is a data frame of measurements with years as row names and one column per series. |
| `Rdatafiles/rwls_meta.rds` | A data frame with one row per file: what the file measures, its study, location, elevation, and taxonomy. |

The two line up by position: `rwls[[i]]` is the data for `rwls_meta[i, ]`. Both are keyed by
file name without `.rwl`, so you can pull a file by name:

```r
rwls[["ca671"]]
rwls_meta["ca671", ]
```

**Not every file is ring width.** The ITRDB also holds earlywood and latewood width, density,
blue intensity and other measurements, often several files per study. `variableShort` says
what each file holds, as NOAA records it. To get ring width:

```r
rw <- rwls[rwls_meta$variableShort %in% "total ring width"]
```

Main `rwls_meta` columns: `file`, `code` (the ITRDB study code), `variableShort`, `unit`,
`studyName`, `Lat`, `Long`, `Altitude`, `AltitudeSource`, `scientificName`, `GenusSpp`,
`Genus`, `Family`, `Order`, `Group`, `doi`, `landingPage`.

## Quick start

```r
library(dplR)

rwls <- readRDS("Rdatafiles/rwls.rds")
rwls_meta <- readRDS("Rdatafiles/rwls_meta.rds")

# ring-width files only
rw <- rwls_meta$variableShort %in% "total ring width"
rwls_meta[rw, ][1, ]
aRWL <- rwls[rw][[1]]

rwl.report(aRWL)
summary(aRWL)
plot(aRWL)
```

Everything after that is ordinary [dplR](https://github.com/OpenDendro/dplR). To pull every
ring-width file of a given species:

```r
piab <- rwls[rw & rwls_meta$GenusSpp %in% "Picea abies"]
length(piab)
```

## Current build

Built 1 October 2026 from the ITRDB as it stood then. Figures from `code/04_readRWLs.R`.

- **10,168** `.rwl` files from **6,862** ITRDB studies, of which **10,166** read
- **6,807** total ring width files, covering **6,781** studies
- In those ring-width files: **275,873** series and **53.4 million** ring measurements
- **358** species
- Years spanned: 6000 BCE to 2025

Two files are not in the data. WY081 NOAA has temporarily withdrawn at the contributor's
request. KYRG014 will not read: one of its series carries two different precision flags, so
there is no single reading of it to hand back.

**This build is not compatible with builds before October 2026.** Those held one file per
study, keyed by upper-case study code (`rwls[["CA671"]]`). The file was chosen by the shortest
name, and for 304 studies that was not ring width; 253 were maximum density. If you used an
earlier build, check which file you were reading.

## How it is built

Scripts in `code/`, run in order:

1. **`01_indexITRDB.R`** asks NOAA's paleo-search API for every tree-ring study, keeps the
   ITRDB ones, and lists their `.rwl` files with the variable each holds. This is the route
   NOAA recommends.
2. **`02_syncFiles.R`** brings `data_files/` up to date with that list: it fetches new and
   changed files and deletes any NOAA no longer lists. `Rdatafiles/sync_manifest.csv`
   records what the server reported for each file.
3. **`03_metaITRDB.R`** adds family, order and group to each species, and fills the few
   missing elevations from a terrain model.
4. **`04_readRWLs.R`** reads every file into an `rwl` object.

Reading is done by `dplR::read.tucson()` (dplR 1.8.0), which records what it found in each
file and returns that alongside the data. Each run writes three reports to `reports/`:

- `index-flags.csv`: problems in NOAA's metadata, such as a study with no species recorded.
- `sync-log.csv`: which files the last run fetched, refreshed or deleted.
- `rwl-read-report.csv`: one row per file, with series and measurement counts, year span,
  precision, interior gaps, and any warning the reader raised.

We send what the first and last of these find to NOAA.

## What is left out, and why

- **Studies with no `.rwl` file.** Chronology-only studies are not included, nor are
  contributed datasets published only as NOAA template `.txt` files. The latter is deliberate on
  NOAA's part: that is how they archive tree-ring contributions that do not meet ITRDB
  standards, such as subfossil material with no calendar dating, or collections mixing
  several species.
- **Files that will not read.** Currently one, KYRG014. `Rdatafiles/rwls_bad.rds` holds
  every file that did not read, with the reason.

Things to know about the metadata:

- **Species, coordinates and elevation are NOAA's.** Where NOAA records no species (23
  studies in this build), `scientificName` is `NA`. Where it records no elevation (12
  studies), the value comes from a terrain model, and `AltitudeSource` says so.
- **Some files hold years that are not calendar years.** Seven files are floating
  chronologies that NOAA dates in calibrated years BP; `timeUnit` marks them.
- **Some coordinates are wrong in the archive itself.** This repo copies what the ITRDB
  holds rather than silently correcting it.

This is a snapshot, not a live mirror. The ITRDB is the authority. When the two disagree, the
ITRDB is right and this repo is stale.

## Citing the data

The measurements are not ours. Cite the original investigators and the ITRDB, following
[NOAA's guidance](https://www.ncei.noaa.gov/products/paleoclimatology/tree-ring). For each
file, `rwls_meta` gives the study's `doi` and `landingPage`, which carries the citation.

If this repo saved you time, a mention is welcome, but the credit belongs to the people who
collected and contributed the cores.

## Related

- [dplR](https://github.com/OpenDendro/dplR) — the R package for tree-ring analysis these
  objects are built for
- [dplPy](https://github.com/OpenDendro/dplPy) — the Python counterpart
- [openDendro](https://opendendro.org) — the wider project

## License

MIT, for the code in this repository. The measurement data comes from the ITRDB and carries
its own terms of use.
