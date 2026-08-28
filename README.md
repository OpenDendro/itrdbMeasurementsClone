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
| `Rdatafiles/rwls.rds` | A list of `rwl` objects, one per study. Each is a data frame of ring widths with years as row names and one column per series. |
| `Rdatafiles/rwls_meta.rds` | A data frame with one row per study: location, elevation, taxonomy, and file counts. |

The two line up by position: `rwls[[i]]` is the data for `rwls_meta[i, ]`. Both are keyed by
ITRDB study code as well, so you can pull a study by name:

```r
rwls[["CA671"]]
rwls_meta["CA671", ]
```

`rwls_meta` columns: `XML_FileName`, `Lat`, `Long`, `Altitude`, `RWL_Count`, `CRN_Count`,
`SpeciesLong`, `GenusSpp`, `Genus`, `Family`, `Order`, `Group`.

## Quick start

```r
library(dplR)

rwls <- readRDS("Rdatafiles/rwls.rds")
rwls_meta <- readRDS("Rdatafiles/rwls_meta.rds")

# the first study
rwls_meta[1, ]
aRWL <- rwls[[1]]

rwl.report(aRWL)
summary(aRWL)
plot(aRWL)
```

Everything after that is ordinary [dplR](https://github.com/OpenDendro/dplR). To pull every
study of a given species:

```r
piab <- rwls[rwls_meta$GenusSpp == "Picea abies"]
length(piab)
```

## Current build

Built August 2026 from the ITRDB as it stood then.

- **6,846** studies
- **277,746** individual series
- **368** species
- Years spanned: 6000 BCE to 2024

One study is missing: WY081, whose files NOAA has temporarily withdrawn at the
contributor's request. Every other study the archive links to is here and parses.

## How it is built

Scripts in `code/`, run in order:

1. **`00_goGetITRDB.R`** downloads the XML metadata records, the study pages, and the data
   files, then parses the metadata into study lists.
2. **`01a_cleanITRDB.R`** improves the taxonomy — splits the species name, adds family and
   order.
3. **`01b_getElevITRDB.R`** and **`01c_reconcileElevITRDB.R`** fill in missing site
   elevations from a terrain model.
4. **`02_getRWLs.R`** reads each study's `.rwl` file into an `rwl` object.

`QA_Stuff/` holds the checks: which files fail to parse, which sites have implausible
coordinates, and a comparison of two Tucson-format readers.

## What is left out, and why

- **Studies with no `.rwl` file.** Chronology-only studies are not included, nor are
  contributed datasets published as NOAA template `.txt` files. The latter is deliberate on
  NOAA's part: that is how they archive tree-ring contributions that do not meet ITRDB
  standards, such as subfossil material with no calendar dating, or collections mixing
  several species. If you want ITRDB data, these are the ones to skip.
- **Only one measurement file per study.** Where a study offers several, the shortest filename
  is used. That is usually the whole-ring file rather than the earlywood/latewood splits.
- **Files that will not parse.** A small number defeat the reader, usually through character
  encoding or conflicting precision flags. They are listed in `QA_Stuff/`.

Two things to know about the metadata:

- **Some elevations are derived, not reported.** Where the ITRDB record has no elevation, the
  value comes from an AWS terrain model at the site coordinates. If you need measured
  elevations, check the original study.
- **Some coordinates are wrong in the archive itself.** A few sites plot in open water. We
  report these upstream as we find them, but this repo copies what the ITRDB holds rather than
  silently correcting it.

This is a snapshot, not a live mirror. The ITRDB is the authority. When the two disagree, the
ITRDB is right and this repo is stale.

## Citing the data

The measurements are not ours. Cite the original investigators and the ITRDB, following
[NOAA's guidance](https://www.ncei.noaa.gov/products/paleoclimatology/tree-ring). Each study's
`XML_FileName` in `rwls_meta` maps to its NOAA landing page, which carries the citation.

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
