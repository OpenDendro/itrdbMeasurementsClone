# Crossdating overview: ITRDB ring-width files

Run 01 October 2026 with dplR 1.8.1 on 6808 ring-width files from the ITRDB clone.

Settings: 50-year segments, first segment at a multiple of 100 years, 32-year spline, prewhitened (AR order up to 3), Pearson's r, p < 0.01, lags of up to 10 years searched.

These reports were made by dplR's xdate.report(), a crossdating report in the layout of the COFECHA output in the ITRDB's correlation-stats files. They are a demonstration that the function can be run across the whole archive, not NOAA's published statistics.

A segment is flagged B when some other position correlates better with the master than the dated one, and A when the dated position is the best tested but under the critical value. A B flag that is weak at every lag marks a poorly correlated segment, not a dating error.

The full overview, every file in one table, is in
[`correlation-overview.txt`](correlation-overview.txt) and `correlation-overview.html`
(GitHub shows `.html` as source: download it to read it as a page). Every number is also
in [`../xdate-summary.csv`](../xdate-summary.csv), one row per file.

## Summary

| | |
|---|---:|
| Files reported | 6807 |
| Files that failed | 1 |
| Files crossdated | 6764 |
| Files with too few series or years to crossdate | 43 |
| Files on a shortened segment length | 506 |
| Series | 275873 |
| Segments tested | 1514739 |
| Segments flagged (A or B) | 73129 (4.8%) |
| B flags weak at every lag | 23792 |
| Files with no flagged segment | 1814 |
| Files with 10% or more of segments flagged | 1278 |
| Series intercorrelation, 10th / 50th / 90th percentile | 0.477 / 0.606 / 0.759 |

## By folder

Each folder lists its reports, most flagged first. The `.html` versions sit under `html/`
at the same path.

| Folder | Files | Crossdated | Segments | Flagged | Median r |
|---|---:|---:|---:|---:|---:|
| [northamerica/usa](txt/northamerica/usa/) | 2555 | 2546 | 682856 | 26568 | 0.625 |
| [europe](txt/europe/) | 2013 | 1985 | 254189 | 13203 | 0.598 |
| [asia](txt/asia/) | 791 | 789 | 214725 | 10731 | 0.607 |
| [northamerica/canada](txt/northamerica/canada/) | 686 | 685 | 180284 | 8308 | 0.601 |
| [southamerica](txt/southamerica/) | 287 | 287 | 45670 | 5443 | 0.536 |
| [australia](txt/australia/) | 190 | 186 | 74536 | 6002 | 0.574 |
| [northamerica/mexico](txt/northamerica/mexico/) | 142 | 142 | 29352 | 1616 | 0.633 |
| [africa](txt/africa/) | 135 | 135 | 32712 | 1118 | 0.656 |
| [centralamerica](txt/centralamerica/) | 8 | 8 | 394 | 128 | 0.373 |
| [atlantic](txt/atlantic/) | 1 | 1 | 21 | 12 | 0.498 |

## Files that need a look

1322 files: those with 10% or more of segments flagged, those with too few series or years to crossdate, and those that failed. Most flagged first.

| File | Study | Species | Series | Years | A | B | % flagged | r | Notes |
|---|---|---|---:|---|---:|---:|---:|---:|---|
| [ak037](txt/northamerica/usa/ak037.txt) | AK037 | Picea glauca (Moench) Voss | 8 | 1833–1996 | 3 | 13 | 100.0 | 0.080 |  |
| [bol023](txt/southamerica/bol023.txt) | BOL023 | Cedrelinga cateniformis (Ducke) Ducke | 22 | 1909–2002 | 2 | 17 | 100.0 | 0.144 | Segment length reduced from 50 to 46 years: the record is only 94 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [bra028](txt/southamerica/bra028.txt) | BRA028 | Swietenia macrophylla King | 17 | 1866–2000 | 3 | 32 | 100.0 | 0.057 |  |
| [ca729](txt/northamerica/usa/ca729.txt) | CA729 | Pinus ponderosa Douglas ex C. Lawson | 63 | 1734–2021 | 0 | 6 | 100.0 | 0.571 |  |
| [can719](txt/northamerica/canada/can719.txt) | CAN719 | Pseudotsuga menziesii (Mirb.) Franco | 4 | 1958–2022 | 0 | 2 | 100.0 | 0.340 | Segment length reduced from 50 to 32 years: the record is only 65 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [cod001](txt/africa/cod001.txt) | COD001 | Aidia ochroleuca (K.Schum.) E.M.A.Petit | 10 | 1922–2005 | 1 | 3 | 100.0 | 0.294 | Segment length reduced from 50 to 42 years: the record is only 84 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [cod002](txt/africa/cod002.txt) | COD002 | Xylopia wilwerthii De Wild. & T.Durand | 10 | 1957–2006 | 1 | 9 | 100.0 | 0.181 | Segment length reduced from 50 to 24 years: the record is only 50 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [cod004](txt/africa/cod004.txt) | COD004 | Prioria balsamifera (Vermoesen) Breteler | 5 | 1778–2006 | 1 | 15 | 100.0 | 0.074 |  |
| [cod006](txt/africa/cod006.txt) | COD006 | Terminalia superba Engl. & Diels | 12 | 1948–2008 | 0 | 6 | 100.0 | 0.248 | Segment length reduced from 50 to 30 years: the record is only 61 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [germ078](txt/europe/germ078.txt) | GERM078 | Abies alba Mill. | 4 | 1534–1659 | 0 | 1 | 100.0 | 0.180 | First segment floored to 0 years, not 100: at 100 no segment could be tested. |
| [germ128](txt/europe/germ128.txt) | GERM128 | Picea abies (L.) H. Karst. | 10 | 1617–1682 | 2 | 0 | 100.0 | 0.511 | Segment length reduced from 50 to 32 years: the record is only 66 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [nam001](txt/africa/nam001.txt) | NAM001 | Burkea africana Hook. | 15 | 1812–1998 | 1 | 12 | 100.0 | 0.086 |  |
| [nam002](txt/africa/nam002.txt) | NAM002 | Pterocarpus angolensis DC. | 9 | 1876–2000 | 0 | 18 | 100.0 | 0.084 |  |
| [zmb004](txt/africa/zmb004.txt) | ZMB004 | Brachystegia boehmii Taub. | 6 | 1893–2000 | 2 | 12 | 100.0 | 0.046 |  |
| [zmb007](txt/africa/zmb007.txt) | ZMB007 | Brachystegia spiciformis Benth. | 8 | 1896–2000 | 4 | 12 | 100.0 | 0.035 |  |
| [zmb013](txt/africa/zmb013.txt) | ZMB013 | Erythrophleum africanum (Benth.) Harms | 5 | 1903–2000 | 0 | 4 | 100.0 | -0.003 | Segment length reduced from 50 to 48 years: the record is only 98 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [az561](txt/northamerica/usa/az561.txt) | AZ561 | Pinus ponderosa Douglas ex C. Lawson | 17 | 0–338 | 2 | 66 | 98.6 | 0.011 |  |
| [bra021](txt/southamerica/bra021.txt) | BRA021 | Cedrela odorata L. | 51 | 1856–2000 | 12 | 55 | 98.5 | 0.048 |  |
| [zmb008](txt/africa/zmb008.txt) | ZMB008 | Brachystegia spiciformis Benth. | 14 | 1847–2002 | 9 | 31 | 97.6 | 0.111 |  |
| [zmb012](txt/africa/zmb012.txt) | ZMB012 | Brachystegia spiciformis Benth. | 29 | 1856–2002 | 9 | 48 | 96.6 | 0.087 |  |
| [ak045](txt/northamerica/usa/ak045.txt) | AK045 | Picea glauca (Moench) Voss | 15 | 1774–1996 | 4 | 69 | 94.8 | 0.116 |  |
| [zmb010](txt/africa/zmb010.txt) | ZMB010 | Brachystegia spiciformis Benth. | 16 | 1953–2000 | 3 | 14 | 94.4 | 0.208 | Segment length reduced from 50 to 24 years: the record is only 48 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [zmb009](txt/africa/zmb009.txt) | ZMB009 | Brachystegia spiciformis Benth. | 18 | 1940–2002 | 3 | 13 | 94.1 | 0.131 | Segment length reduced from 50 to 30 years: the record is only 63 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ita064](txt/europe/ita064.txt) | ITA064 | Fagus sylvatica L. | 10 | 1960–2019 | 2 | 12 | 93.3 | 0.162 | Segment length reduced from 50 to 30 years: the record is only 60 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [tha023](txt/asia/tha023.txt) | THA023 | Afzelia xylocarpa (Kurz) Craib | 138 | 1825–2011 | 5 | 35 | 93.0 | 0.267 |  |
| [cod007](txt/africa/cod007.txt) | COD007 | Pericopsis elata (Harms) Meeuwen | 24 | 1808–2008 | 11 | 55 | 93.0 | 0.166 |  |
| [col006](txt/southamerica/col006.txt) | COL006 | Apeiba macropetala Ducke | 12 | 1983–2016 | 2 | 9 | 91.7 | 0.384 | Segment length reduced from 50 to 16 years: the record is only 34 years long. |
| [zmb011](txt/africa/zmb011.txt) | ZMB011 | Brachystegia spiciformis Benth. | 13 | 1968–2000 | 2 | 9 | 91.7 | 0.232 | Segment length reduced from 50 to 16 years: the record is only 33 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [az562](txt/northamerica/usa/az562.txt) | AZ562 | Pinus ponderosa Douglas ex C. Lawson | 18 | 0–289 | 3 | 58 | 91.0 | 0.078 |  |
| [cod003](txt/africa/cod003.txt) | COD003 | Corynanthe paniculata Welw. | 10 | 1894–2006 | 6 | 14 | 90.9 | 0.194 |  |
| [zmb005](txt/africa/zmb005.txt) | ZMB005 | Brachystegia boehmii Taub. | 9 | 1854–2000 | 0 | 20 | 90.9 | -0.011 |  |
| [zmb006](txt/africa/zmb006.txt) | ZMB006 | Brachystegia spiciformis Benth. | 11 | 1917–2002 | 1 | 9 | 90.9 | 0.133 | Segment length reduced from 50 to 42 years: the record is only 86 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit248](txt/europe/swit248.txt) | SWIT248 | Fraxinus excelsior L. | 4 | 1949–1991 | 3 | 6 | 90.0 | 0.304 | Segment length reduced from 50 to 20 years: the record is only 43 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [bol024](txt/southamerica/bol024.txt) | BOL024 | Macrolobium acaciifolium (Benth.) Benth. | 29 | 1965–2014 | 9 | 42 | 89.5 | 0.293 | Segment length reduced from 50 to 24 years: the record is only 50 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [bra049](txt/southamerica/bra049.txt) | BRA049 | Cedrela fissilis Vell. | 14 | 1971–2011 | 4 | 12 | 88.9 | 0.281 | Segment length reduced from 50 to 20 years: the record is only 41 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [bra047](txt/southamerica/bra047.txt) | BRA047 | Cedrela fissilis Vell. | 34 | 1937–2011 | 4 | 3 | 87.5 | 0.287 | Segment length reduced from 50 to 36 years: the record is only 75 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [col003](txt/southamerica/col003.txt) | COL003 | Cedrela odorata L. | 79 | 1971–2005 | 17 | 71 | 87.1 | 0.327 | Segment length reduced from 50 to 16 years: the record is only 35 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [sur001](txt/southamerica/sur001.txt) | SUR001 | Goupia glabra Aubl. | 41 | 1760–2016 | 24 | 114 | 86.8 | 0.114 |  |
| [ital029](txt/europe/ital029.txt) | ITAL029 | Pistacia lentiscus  L. | 17 | 1978–2007 | 8 | 11 | 86.4 | 0.480 | Segment length reduced from 50 to 14 years: the record is only 30 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [bgd005](txt/asia/bgd005.txt) | BGD005 | Sonneratia apetala Banks | 18 | 1929–2012 | 1 | 5 | 85.7 | 0.302 | Segment length reduced from 50 to 42 years: the record is only 84 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [brit10](txt/europe/brit10.txt) | BRIT10 | Quercus robur L. = Quercus pendunculata Ehrl. | 4 | 1816–1977 | 2 | 4 | 85.7 | 0.303 |  |
| [tza002](txt/africa/tza002.txt) | TZA002 | Brachystegia spiciformis Benth. | 9 | 1946–1998 | 0 | 6 | 85.7 | 0.256 | Segment length reduced from 50 to 26 years: the record is only 53 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [per004](txt/southamerica/per004.txt) | PER004 | Macrolobium acaciifolium (Benth.) Benth. | 45 | 1965–2014 | 15 | 53 | 85.0 | 0.289 | Segment length reduced from 50 to 24 years: the record is only 50 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [bra035](txt/southamerica/bra035.txt) | BRA035 | Hymenaea stigonocarpa Mart. ex Hayne | 18 | 1985–2020 | 7 | 2 | 81.8 | 0.478 | Segment length reduced from 50 to 18 years: the record is only 36 years long. |
| [civ001](txt/africa/civ001.txt) | CIV001 | Terminalia superba Engl. & Diels | 14 | 1971–2006 | 2 | 16 | 81.8 | 0.368 | Segment length reduced from 50 to 18 years: the record is only 36 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [cod005](txt/africa/cod005.txt) | COD005 | Terminalia superba Engl. & Diels | 14 | 1971–2006 | 2 | 16 | 81.8 | 0.368 | Segment length reduced from 50 to 18 years: the record is only 36 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [pan003](txt/centralamerica/pan003.txt) | PAN003 | Tetragastris panamensis (Engl.) Kuntze | 30 | 1892–2014 | 11 | 16 | 81.8 | 0.221 |  |
| [bwa001](txt/africa/bwa001.txt) | BWA001 | Adansonia digitata | 16 | 1960–2010 | 10 | 14 | 80.0 | 0.440 | Segment length reduced from 50 to 24 years: the record is only 51 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [ital052](txt/europe/ital052.txt) | ITAL052 | Quercus robur L. = Quercus pendunculata Ehrl. | 11 | 1961–2018 | 6 | 2 | 80.0 | 0.352 | Segment length reduced from 50 to 28 years: the record is only 58 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [swit226](txt/europe/swit226.txt) | SWIT226 | Picea abies (L.) H. Karst. | 21 | 1968–2000 | 7 | 16 | 79.3 | 0.372 | Segment length reduced from 50 to 16 years: the record is only 33 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [cmr004](txt/africa/cmr004.txt) | CMR004 | Triplochiton scleroxylon K.Schum. | 8 | 1881–2014 | 4 | 11 | 78.9 | 0.133 |  |
| [bol022](txt/southamerica/bol022.txt) | BOL022 | Amburana cearensis (Allemão) A.C.Sm. | 20 | 1850–2001 | 11 | 32 | 78.2 | 0.123 |  |
| [bra033](txt/southamerica/bra033.txt) | BRA033 | Araucaria angustifolia (Bertol.) Kuntze | 31 | 1800–2016 | 6 | 8 | 77.8 | 0.318 |  |
| [pan002](txt/centralamerica/pan002.txt) | PAN002 | Trichilia tuberculata (Triana & Planch.) C.DC. | 39 | 1863–2014 | 10 | 28 | 77.6 | 0.217 |  |
| [nc016](txt/northamerica/usa/nc016.txt) | NC016 | Picea rubens Sarg. | 30 | 1650–1986 | 23 | 161 | 76.3 | 0.211 |  |
| [che585](txt/europe/che585.txt) | CHE585 | Fagus sylvatica L. | 5 | 1928–2009 | 0 | 6 | 75.0 | 0.131 | Segment length reduced from 50 to 40 years: the record is only 82 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [pan001](txt/centralamerica/pan001.txt) | PAN001 | Jacaranda copaia (Aubl.) D.Don | 31 | 1909–2014 | 2 | 1 | 75.0 | 0.359 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ita073be25](txt/europe/ita073be25.txt) | ITA073 | Larix decidua Mill. | 28 | 1502–2016 | 18 | 80 | 74.8 | 0.179 |  |
| [col005](txt/southamerica/col005.txt) | COL005 | Humiriastrum procerum (Little) Cuatrec. | 14 | 1973–2016 | 9 | 8 | 73.9 | 0.469 | Segment length reduced from 50 to 22 years: the record is only 44 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [mex142](txt/northamerica/mexico/mex142.txt) | MEX142 | Taxodium mucronatum Ten. | 46 | 1770–2020 | 13 | 21 | 73.9 | 0.269 |  |
| [tha024](txt/asia/tha024.txt) | THA024 | Chukrasia tabularis A.Juss. | 70 | 1904–2010 | 4 | 24 | 73.7 | 0.282 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [cmr003](txt/africa/cmr003.txt) | CMR003 | Entandrophragma cylindricum (Sprague) Sprague | 21 | 1604–2010 | 24 | 97 | 72.9 | 0.214 |  |
| [co562](txt/northamerica/usa/co562.txt) | CO562 | Pinus aristata Engelm. | 18 | 1180–1994 | 10 | 167 | 72.5 | 0.209 |  |
| [bra052](txt/southamerica/bra052.txt) | BRA052 | Araucaria angustifolia (Bertol.) Kuntze | 35 | 1887–2013 | 0 | 15 | 71.4 | 0.273 |  |
| [cri001](txt/centralamerica/cri001.txt) | CRI001 | Genipa americana L. | 13 | 1926–1997 | 0 | 5 | 71.4 | 0.235 | Segment length reduced from 50 to 36 years: the record is only 72 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [mex125](txt/northamerica/mexico/mex125.txt) | MEX125 | Pinus oocarpa Schiede | 42 | 1912–2014 | 2 | 3 | 71.4 | 0.381 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [cmr002](txt/africa/cmr002.txt) | CMR002 | Triplochiton schleroxylon | 20 | 1788–2010 | 15 | 31 | 70.8 | 0.237 |  |
| [che517](txt/europe/che517.txt) | CHE517 | Picea abies (L.) H. Karst. | 5 | 1941–2008 | 4 | 3 | 70.0 | 0.502 | Segment length reduced from 50 to 34 years: the record is only 68 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ita073be50](txt/europe/ita073be50.txt) | ITA073 | Larix decidua Mill. | 28 | 1502–2016 | 11 | 78 | 68.5 | 0.196 |  |
| [ita073be75](txt/europe/ita073be75.txt) | ITA073 | Larix decidua Mill. | 28 | 1502–2016 | 13 | 75 | 67.7 | 0.215 |  |
| [ita072be25](txt/europe/ita072be25.txt) | ITA072 | Larix decidua Mill. | 23 | 1418–2015 | 25 | 94 | 67.6 | 0.223 |  |
| [bgd004](txt/asia/bgd004.txt) | BGD004 | Heritiera fomes Banks | 14 | 1935–2012 | 2 | 6 | 66.7 | 0.317 | Segment length reduced from 50 to 38 years: the record is only 78 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [bra024](txt/southamerica/bra024.txt) | BRA024 | Hymenaea courbaril L. | 24 | 1765–2013 | 11 | 23 | 66.7 | 0.250 |  |
| [bra051](txt/southamerica/bra051.txt) | BRA051 | Araucaria angustifolia (Bertol.) Kuntze | 78 | 1954–2013 | 4 | 16 | 66.7 | 0.369 | Segment length reduced from 50 to 30 years: the record is only 60 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [che439](txt/europe/che439.txt) | CHE439 | Fagus sylvatica L. | 6 | 1924–2013 | 2 | 0 | 66.7 | 0.427 | Segment length reduced from 50 to 44 years: the record is only 90 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ok039](txt/northamerica/usa/ok039.txt) | OK039 | Pinus taeda L. | 262 | 1977–2004 | 53 | 93 | 66.7 | 0.589 | Segment length reduced from 50 to 14 years: the record is only 28 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [russ179w](txt/asia/russ179w.txt) | RUSS179 | Larix gmelinii (Rupr.) Kuzen.=L.kurulensis(Maxim ex Regel)Pilg.=L.dahuricaTurcz.exTratv=Larix cajanderi Mayr | 14 | 1976–1998 | 6 | 8 | 66.7 | 0.538 | Segment length reduced from 50 to 10 years: the record is only 23 years long. \| Lags searched reduced from 10 to 9 years to fit the segment length. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [swit249](txt/europe/swit249.txt) | SWIT249 | Picea abies (L.) H. Karst. | 6 | 1968–1991 | 1 | 5 | 66.7 | 0.478 | Segment length reduced from 50 to 12 years: the record is only 24 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [ury001](txt/southamerica/ury001.txt) | URY001 | Scutia buxifolia Reissek | 29 | 1919–2012 | 1 | 3 | 66.7 | 0.436 | Segment length reduced from 50 to 46 years: the record is only 94 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [zmb001](txt/africa/zmb001.txt) | ZMB001 | Baikiaea plurijuga Harms | 8 | 1958–2013 | 1 | 3 | 66.7 | 0.391 | Segment length reduced from 50 to 28 years: the record is only 56 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [ak053](txt/northamerica/usa/ak053.txt) | AK053 | Picea glauca (Moench) Voss | 153 | 1715–2000 | 28 | 246 | 66.0 | 0.216 |  |
| [ita072be50](txt/europe/ita072be50.txt) | ITA072 | Larix decidua Mill. | 23 | 1418–2015 | 23 | 93 | 65.9 | 0.223 |  |
| [deu464](txt/europe/deu464.txt) | DEU464 | Picea abies (L.) H. Karst. | 33 | 1971–2017 | 9 | 17 | 65.0 | 0.435 | Segment length reduced from 50 to 22 years: the record is only 47 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [mex156](txt/northamerica/mexico/mex156.txt) | MEX156 | Pinus hartwegii Lindl. | 45 | 1873–2015 | 10 | 38 | 64.9 | 0.233 |  |
| [ak163](txt/northamerica/usa/ak163.txt) | AK163 | Salix pulchra Cham. | 35 | 1974–2016 | 3 | 8 | 64.7 | 0.555 | Segment length reduced from 50 to 20 years: the record is only 43 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [wi035](txt/northamerica/usa/wi035.txt) | WI035 | Quercus alba L. | 6 | 1839–2014 | 1 | 8 | 64.3 | 0.311 |  |
| [ita072be75](txt/europe/ita072be75.txt) | ITA072 | Larix decidua Mill. | 23 | 1418–2015 | 20 | 93 | 64.2 | 0.221 |  |
| [prt002](txt/europe/prt002.txt) | PRT002 | Quercus suber L. | 8 | 1963–2012 | 3 | 7 | 62.5 | 0.433 | Segment length reduced from 50 to 24 years: the record is only 50 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [bra050](txt/southamerica/bra050.txt) | BRA050 | Araucaria angustifolia (Bertol.) Kuntze | 90 | 1641–2010 | 20 | 72 | 62.2 | 0.355 |  |
| [ita072be100](txt/europe/ita072be100.txt) | ITA072 | Larix decidua Mill. | 23 | 1418–2015 | 15 | 94 | 61.9 | 0.222 |  |
| [bol026](txt/southamerica/bol026.txt) | BOL026 | Amburana cearensis (Allemão) A.C.Sm. | 24 | 1788–2010 | 9 | 30 | 61.9 | 0.321 |  |
| [bra034](txt/southamerica/bra034.txt) | BRA034 | Cedrela fissilis Vell. | 35 | 1899–2015 | 2 | 6 | 61.5 | 0.300 |  |
| [cze044](txt/europe/cze044.txt) | CZE044 | Pinus sylvestris L. | 26 | 1847–2019 | 0 | 8 | 61.5 | 0.391 |  |
| [per005](txt/southamerica/per005.txt) | PER005 | Jacaranda copaia (Aubl.) D.Don | 47 | 1952–2019 | 9 | 11 | 60.6 | 0.394 | Segment length reduced from 50 to 34 years: the record is only 68 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [chin078](txt/asia/chin078.txt) | CHIN078 | Pinus yunnanensis Franch. | 14 | 1956–2016 | 1 | 5 | 60.0 | 0.568 | Segment length reduced from 50 to 30 years: the record is only 61 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [nv524](txt/northamerica/usa/nv524.txt) | NV524 | Populus fremontii Wats. | 27 | 1964–2020 | 0 | 3 | 60.0 | 0.392 | Segment length reduced from 50 to 28 years: the record is only 57 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [swit227](txt/europe/swit227.txt) | SWIT227 | Pinus strobus L. | 20 | 1963–2000 | 7 | 17 | 60.0 | 0.453 | Segment length reduced from 50 to 18 years: the record is only 38 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [swit349](txt/europe/swit349.txt) | SWIT349 | Abies alba Mill. | 6 | 1895–1992 | 2 | 1 | 60.0 | 0.297 | Segment length reduced from 50 to 48 years: the record is only 98 years long. |
| [tha025](txt/asia/tha025.txt) | THA025 | Melia azedarach L. | 256 | 1914–2011 | 2 | 1 | 60.0 | 0.444 | Segment length reduced from 50 to 48 years: the record is only 98 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ita073be100](txt/europe/ita073be100.txt) | ITA073 | Larix decidua Mill. | 28 | 1502–2016 | 11 | 66 | 59.2 | 0.235 |  |
| [cypr013w](txt/europe/cypr013w.txt) | CYPR013 | Pinus brutia Ten. | 20 | 1926–1981 | 3 | 7 | 58.8 | 0.301 | Segment length reduced from 50 to 28 years: the record is only 56 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [mexi041](txt/northamerica/mexico/mexi041.txt) | MEXI041 | Taxodium mucronatum Ten. | 4 | 1842–2001 | 5 | 2 | 58.3 | 0.323 |  |
| [bhs001](txt/atlantic/bhs001.txt) | BHS001 | Pinus elliottii Engelm. | 72 | 1935–2002 | 7 | 5 | 57.1 | 0.498 | Segment length reduced from 50 to 34 years: the record is only 68 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [can720](txt/northamerica/canada/can720.txt) | CAN720 | Pseudotsuga menziesii (Mirb.) Franco | 4 | 1961–2022 | 1 | 3 | 57.1 | 0.454 | Segment length reduced from 50 to 30 years: the record is only 62 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [mex154](txt/northamerica/mexico/mex154.txt) | MEX154 | Taxodium mucronatum Ten. | 34 | 1256–2019 | 23 | 65 | 56.8 | 0.302 |  |
| [mex135](txt/northamerica/mexico/mex135.txt) | MEX135 | Taxodium mucronatum Ten. | 23 | 1500–2018 | 13 | 41 | 55.7 | 0.311 |  |
| [ury003](txt/southamerica/ury003.txt) | URY003 | Gleditsia triacanthos L. | 47 | 1970–2024 | 1 | 4 | 55.6 | 0.486 | Segment length reduced from 50 to 26 years: the record is only 55 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [bra044](txt/southamerica/bra044.txt) | BRA044 | Aspidosperma polyneuron Müll.Arg. | 39 | 1660–2010 | 30 | 48 | 54.5 | 0.310 |  |
| [ak164](txt/northamerica/usa/ak164.txt) | AK164 | Salix pulchra Cham. | 31 | 1962–2015 | 2 | 5 | 53.8 | 0.555 | Segment length reduced from 50 to 26 years: the record is only 54 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [ak175](txt/northamerica/usa/ak175.txt) | AK175 | Salix spp. | 40 | 1977–2010 | 15 | 13 | 53.8 | 0.506 | Segment length reduced from 50 to 16 years: the record is only 34 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [cmr001](txt/africa/cmr001.txt) | CMR001 | Erythrophleum ivorense A.Chev. | 11 | 1886–2010 | 0 | 7 | 53.8 | 0.332 |  |
| [bra045](txt/southamerica/bra045.txt) | BRA045 | Aspidosperma polyneuron Müll.Arg. | 27 | 1686–2010 | 17 | 37 | 53.5 | 0.316 |  |
| [bol025](txt/southamerica/bol025.txt) | BOL025 | Machaerium scleroxylon Tul. | 30 | 1913–2009 | 7 | 9 | 53.3 | 0.309 | Segment length reduced from 50 to 48 years: the record is only 97 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ecu002](txt/southamerica/ecu002.txt) | ECU002 | Cedrela montana Moritz ex Turcz. | 40 | 1947–2011 | 5 | 3 | 53.3 | 0.385 | Segment length reduced from 50 to 32 years: the record is only 65 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [nor050](txt/europe/nor050.txt) | NOR050 | Juniperus communis L. | 18 | 1900–2022 | 4 | 13 | 53.1 | 0.357 |  |
| [ita074be75](txt/europe/ita074be75.txt) | ITA074 | Larix decidua Mill. | 25 | 1566–2015 | 40 | 91 | 52.6 | 0.282 |  |
| [ita074be25](txt/europe/ita074be25.txt) | ITA074 | Larix decidua Mill. | 25 | 1566–2015 | 34 | 96 | 52.2 | 0.281 |  |
| [fra058](txt/europe/fra058.txt) | FRA058 | Pinus nigra J.F. Arnold | 61 | 1486–2013 | 28 | 189 | 51.8 | 0.444 |  |
| [bol009](txt/southamerica/bol009.txt) | BOL009 | Cenostigma pluviosum (DC.) Gagnon & G.P.Lewis = Caesalpinia pluviosa DC. | 31 | 1844–2011 | 10 | 36 | 51.7 | 0.377 |  |
| [ital022](txt/europe/ital022.txt) | ITAL022 | Abies spp. Mill. | 7 | 1539–1973 | 4 | 17 | 51.2 | 0.343 |  |
| [indo003](txt/asia/indo003.txt) | INDO003 | Tectona grandis L. f. | 13 | 1714–2004 | 13 | 32 | 51.1 | 0.373 |  |
| [ita074be50](txt/europe/ita074be50.txt) | ITA074 | Larix decidua Mill. | 25 | 1566–2015 | 32 | 94 | 50.6 | 0.281 |  |
| [ak043](txt/northamerica/usa/ak043.txt) | AK043 | Picea glauca (Moench) Voss | 12 | 1713–1997 | 2 | 24 | 50.0 | 0.254 |  |
| [bra006](txt/southamerica/bra006.txt) | BRA006 | Cedrela fissilis Vell. | 16 | 1896–2006 | 3 | 4 | 50.0 | 0.358 |  |
| [bra018](txt/southamerica/bra018.txt) | BRA018 | Aspidosperma pyrifolium Mart. | 10 | 1967–2012 | 1 | 0 | 50.0 | 0.561 | Segment length reduced from 50 to 22 years: the record is only 46 years long. \| First segment floored to 0 years, not 100: at 100 no segment could be tested. |
| [brit033](txt/europe/brit033.txt) | BRIT033 | Quercus spp. L. | 6 | 1818–1987 | 1 | 2 | 50.0 | 0.352 |  |
| [germ100](txt/europe/germ100.txt) | GERM100 | Abies alba Mill. | 3 | 1508–1579 | 0 | 1 | 50.0 | 0.448 | Segment length reduced from 50 to 36 years: the record is only 72 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [germ124](txt/europe/germ124.txt) | GERM124 | Picea abies (L.) H. Karst. | 5 | 1604–1721 | 0 | 1 | 50.0 | 0.339 | First segment floored to 0 years, not 100: at 100 no segment could be tested. |
| [germ137](txt/europe/germ137.txt) | GERM137 | Picea abies (L.) H. Karst. | 3 | 1714–1780 | 0 | 1 | 50.0 | 0.525 | Segment length reduced from 50 to 32 years: the record is only 67 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [ital053](txt/europe/ital053.txt) | ITAL053 | Quercus rubra L. | 10 | 1980–2018 | 3 | 2 | 50.0 | 0.422 | Segment length reduced from 50 to 18 years: the record is only 39 years long. |
| [keny002](txt/africa/keny002.txt) | KENY002 |  | 13 | 1935–1994 | 3 | 6 | 50.0 | 0.451 | Segment length reduced from 50 to 30 years: the record is only 60 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [mexi115](txt/northamerica/mexico/mexi115.txt) | MEXI115 | Pinus leiophylla var. chihuahuana (Engelm.) | 18 | 1918–2015 | 1 | 1 | 50.0 | 0.595 | Segment length reduced from 50 to 48 years: the record is only 98 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [nv523](txt/northamerica/usa/nv523.txt) | NV523 | Populus fremontii Wats. | 13 | 1912–2020 | 1 | 1 | 50.0 | 0.335 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit232](txt/europe/swit232.txt) | SWIT232 | Quercus petraea (Matt.) Liebl. = Quercus sessiliflora Salisb. | 6 | 1941–1995 | 3 | 3 | 50.0 | 0.567 | Segment length reduced from 50 to 26 years: the record is only 55 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ury002](txt/southamerica/ury002.txt) | URY002 | Scutia buxifolia Reissek | 25 | 1900–2012 | 4 | 5 | 50.0 | 0.371 |  |
| [arge003](txt/southamerica/arge003.txt) | ARGE003 | Araucaria araucana (Molina) K. Koch | 8 | 1486–1974 | 11 | 25 | 49.3 | 0.325 |  |
| [russ158w](txt/asia/russ158w.txt) | RUSS158 | Pinus sylvestris L. | 24 | 1934–1996 | 5 | 7 | 48.0 | 0.506 | Segment length reduced from 50 to 30 years: the record is only 63 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ind027](txt/asia/ind027.txt) | IND027 | Tectona grandis L. f. | 18 | 1864–2004 | 8 | 14 | 47.8 | 0.355 |  |
| [ita074be100](txt/europe/ita074be100.txt) | ITA074 | Larix decidua Mill. | 25 | 1566–2015 | 35 | 84 | 47.8 | 0.290 |  |
| [che558](txt/europe/che558.txt) | CHE558 | Fagus sylvatica L. | 17 | 1718–2018 | 3 | 17 | 46.5 | 0.311 |  |
| [ak165](txt/northamerica/usa/ak165.txt) | AK165 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 60 | 1971–2017 | 15 | 23 | 46.3 | 0.503 | Segment length reduced from 50 to 22 years: the record is only 47 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [lbn009](txt/asia/lbn009.txt) | LBN009 | Cedrus libani A. Rich. | 29 | 1852–2010 | 3 | 15 | 46.2 | 0.348 |  |
| [or120](txt/northamerica/usa/or120.txt) | OR120 | Pseudotsuga menziesii (Mirb.) Franco | 4 | 1570–1844 | 0 | 12 | 46.2 | 0.540 |  |
| [swit149w](txt/europe/swit149w.txt) | SWIT149 | Picea abies (L.) H. Karst. | 26 | 1917–1979 | 9 | 3 | 46.2 | 0.477 | Segment length reduced from 50 to 30 years: the record is only 63 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [spai080](txt/europe/spai080.txt) | SPAI080 | Pinus nigra J.F. Arnold | 19 | 1701–2010 | 4 | 17 | 45.7 | 0.387 |  |
| [ak173](txt/northamerica/usa/ak173.txt) | AK173 | Picea sitchensis (Bong.) Carrière | 38 | 1950–2016 | 3 | 17 | 45.5 | 0.406 | Segment length reduced from 50 to 32 years: the record is only 67 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [cana039](txt/northamerica/canada/cana039.txt) | CANA039 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 26 | 1819–1988 | 2 | 12 | 45.2 | 0.417 |  |
| [swit254](txt/europe/swit254.txt) | SWIT254 | Pinus cembra L. | 22 | 1939–1996 | 5 | 9 | 45.2 | 0.481 | Segment length reduced from 50 to 28 years: the record is only 58 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [mex157](txt/northamerica/mexico/mex157.txt) | MEX157 | Pinus hartwegii Lindl. | 45 | 1878–2015 | 16 | 21 | 45.1 | 0.337 |  |
| [rus392](txt/asia/rus392.txt) | RUS392 | Pinus spp. L. | 10 | 1544–1669 | 0 | 4 | 44.4 | 0.328 |  |
| [swit235](txt/europe/swit235.txt) | SWIT235 | Quercus spp. L. | 12 | 1971–1992 | 2 | 2 | 44.4 | 0.684 | Segment length reduced from 50 to 10 years: the record is only 22 years long. \| Lags searched reduced from 10 to 9 years to fit the segment length. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [zmb003](txt/africa/zmb003.txt) | ZMB003 | Baikiaea plurijuga Harms | 10 | 1958–2013 | 2 | 2 | 44.4 | 0.517 | Segment length reduced from 50 to 28 years: the record is only 56 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [prt001](txt/europe/prt001.txt) | PRT001 | Quercus suber L. | 8 | 1964–2012 | 1 | 6 | 43.8 | 0.340 | Segment length reduced from 50 to 24 years: the record is only 49 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [wa133](txt/northamerica/usa/wa133.txt) | WA133 | Thuja plicata Donn ex D. Don | 4 | 1378–1685 | 4 | 10 | 43.8 | 0.391 |  |
| [wa164](txt/northamerica/usa/wa164.txt) | WA164 | Thuja plicata Donn ex D. Don | 52 | 1937–2020 | 4 | 13 | 43.6 | 0.405 | Segment length reduced from 50 to 42 years: the record is only 84 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ak10](txt/northamerica/usa/ak10.txt) | AK10 | Picea sitchensis (Bong.) Carrière | 10 | 1934–1985 | 1 | 2 | 42.9 | 0.521 | Segment length reduced from 50 to 26 years: the record is only 52 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [germ239](txt/europe/germ239.txt) | GERM239 | Pinus sylvestris L. | 70 | 1902–2011 | 1 | 2 | 42.9 | 0.439 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [tn026](txt/northamerica/usa/tn026.txt) | TN026 | Tsuga canadensis (L.) Carr. | 16 | 1895–1995 | 0 | 3 | 42.9 | 0.424 |  |
| [arg159](txt/southamerica/arg159.txt) | ARG159 | Juglans australis Griseb. | 6 | 1930–2006 | 2 | 3 | 41.7 | 0.410 | Segment length reduced from 50 to 38 years: the record is only 77 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [che521](txt/europe/che521.txt) | CHE521 | Picea abies (L.) H. Karst. | 7 | 1883–2010 | 2 | 3 | 41.7 | 0.406 |  |
| [aus126g](txt/australia/aus126g.txt) | AUS126 | Phyllocladus aspleniifolius (Labill.) Hook. f. | 39 | 1616–2011 | 25 | 84 | 41.6 | 0.340 |  |
| [chin044](txt/asia/chin044.txt) | CHIN044 | Juniperus tibetica Kom. | 10 | 1047–1993 | 19 | 63 | 41.2 | 0.333 |  |
| [ak155](txt/northamerica/usa/ak155.txt) | AK155 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 213 | 1776–2013 | 52 | 125 | 40.8 | 0.345 |  |
| [ca705](txt/northamerica/usa/ca705.txt) | CA705 | Sequoiadendron giganteum (Lindl.) Buchholz | 24 | 1590–1991 | 19 | 37 | 40.6 | 0.349 |  |
| [co561](txt/northamerica/usa/co561.txt) | CO561 | Pinus flexilis E. James | 17 | 1484–1994 | 2 | 32 | 40.5 | 0.363 |  |
| [mexi110](txt/northamerica/mexico/mexi110.txt) | MEXI110 | Taxodium mucronatum Ten. | 18 | 1701–2009 | 10 | 24 | 40.5 | 0.339 |  |
| [nor032](txt/europe/nor032.txt) | NOR032 | Juniperus communis L. | 25 | 1877–2022 | 7 | 18 | 40.3 | 0.336 |  |
| [ak154](txt/northamerica/usa/ak154.txt) | AK154 | Picea glauca (Moench) Voss | 339 | 1648–2013 | 100 | 218 | 40.2 | 0.380 |  |
| [mn013](txt/northamerica/usa/mn013.txt) | MN013 | Pinus resinosa Aiton | 16 | 1687–1982 | 16 | 41 | 40.1 | 0.374 |  |
| [al004](txt/northamerica/usa/al004.txt) | AL004 | Quercus boyntonii Beadle | 10 | 1925–2018 | 2 | 0 | 40.0 | 0.497 | Segment length reduced from 50 to 46 years: the record is only 94 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [bra020](txt/southamerica/bra020.txt) | BRA020 | Cedrela odorata L. | 24 | 1930–2014 | 1 | 3 | 40.0 | 0.310 | Segment length reduced from 50 to 42 years: the record is only 85 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [germ096](txt/europe/germ096.txt) | GERM096 | Abies alba Mill. | 5 | 1538–1625 | 1 | 1 | 40.0 | 0.603 | Segment length reduced from 50 to 44 years: the record is only 88 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [mex153](txt/northamerica/mexico/mex153.txt) | MEX153 | Pinus oocarpa Schiede | 42 | 1867–2019 | 4 | 6 | 40.0 | 0.386 |  |
| [prt004](txt/europe/prt004.txt) | PRT004 | Quercus suber L. | 8 | 1905–2022 | 0 | 2 | 40.0 | 0.506 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [tn025](txt/northamerica/usa/tn025.txt) | TN025 | Castanea dentata (Marsh.) Borkh. | 8 | 1713–1923 | 3 | 3 | 40.0 | 0.466 |  |
| [nor051](txt/europe/nor051.txt) | NOR051 | Juniperus communis L. | 30 | 1710–2023 | 13 | 39 | 39.7 | 0.364 |  |
| [mex128](txt/northamerica/mexico/mex128.txt) | MEX128 | Picea martinezii T.F.Patt. | 27 | 1848–2020 | 3 | 8 | 39.3 | 0.347 |  |
| [wy071](txt/northamerica/usa/wy071.txt) | WY071 | Pinus albicaulis Engelm. | 142 | 1829–2018 | 25 | 41 | 39.3 | 0.383 |  |
| [kyrg010](txt/asia/kyrg010.txt) | KYRG010 | Juniperus turkestanica Komar. | 18 | 1427–1987 | 23 | 48 | 39.0 | 0.371 |  |
| [ak159](txt/northamerica/usa/ak159.txt) | AK159 | Betula papyrifera var. neoalaskana (Sarg.) Raup | 231 | 1829–2015 | 6 | 68 | 38.9 | 0.386 |  |
| [spai065](txt/europe/spai065.txt) | SPAI065 | Quercus faginea Lam. | 20 | 1966–2007 | 1 | 6 | 38.9 | 0.666 | Segment length reduced from 50 to 20 years: the record is only 42 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [turk029](txt/europe/turk029.txt) | TURK029 | Juniperus spp. L. | 3 | -1767–-742 | 1 | 23 | 38.7 | 0.332 |  |
| [nepa044](txt/asia/nepa044.txt) | NEPA044 | Abies pindrow (Royle ex D. Don) Royle | 39 | 1649–2012 | 15 | 67 | 38.5 | 0.357 |  |
| [ak188](txt/northamerica/usa/ak188.txt) | AK188 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 39 | 1859–2017 | 1 | 4 | 38.5 | 0.514 |  |
| [cana278](txt/northamerica/canada/cana278.txt) | CANA278 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 76 | 1737–2004 | 16 | 44 | 38.5 | 0.369 |  |
| [deu462](txt/europe/deu462.txt) | DEU462 | Abies procera Rehder | 100 | 1982–2020 | 15 | 23 | 38.4 | 0.557 | Segment length reduced from 50 to 18 years: the record is only 39 years long. |
| [bra012](txt/southamerica/bra012.txt) | BRA012 | Hymenaea courbaril L. | 35 | 1822–2011 | 13 | 16 | 38.2 | 0.386 |  |
| [wi071](txt/northamerica/usa/wi071.txt) | WI071 | Fraxinus spp. L. | 11 | 1943–2019 | 1 | 7 | 38.1 | 0.508 | Segment length reduced from 50 to 38 years: the record is only 77 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ital055](txt/europe/ital055.txt) | ITAL055 | Castanea sativa Mill. | 53 | 1557–2012 | 13 | 17 | 38.0 | 0.407 |  |
| [ak187](txt/northamerica/usa/ak187.txt) | AK187 | Alnus spp. Mill. | 53 | 1936–2017 | 2 | 12 | 37.8 | 0.496 | Segment length reduced from 50 to 40 years: the record is only 82 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [bol019](txt/southamerica/bol019.txt) | BOL019 | Schinopsis brasiliensis Engl. | 15 | 1838–2011 | 2 | 15 | 37.8 | 0.347 |  |
| [czec2](txt/europe/czec2.txt) | CZEC2 | Fagus sylvatica L. | 9 | 1684–1943 | 3 | 17 | 37.7 | 0.390 |  |
| [ak176](txt/northamerica/usa/ak176.txt) | AK176 | Salix spp. | 55 | 1964–2010 | 3 | 6 | 37.5 | 0.590 | Segment length reduced from 50 to 22 years: the record is only 47 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [alb004](txt/europe/alb004.txt) | ALB004 | Pinus halepensis Mill. | 40 | 1949–2010 | 7 | 17 | 37.5 | 0.440 | Segment length reduced from 50 to 30 years: the record is only 62 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [bra009](txt/southamerica/bra009.txt) | BRA009 | Cedrela odorata L. | 19 | 1954–2015 | 1 | 2 | 37.5 | 0.391 | Segment length reduced from 50 to 30 years: the record is only 62 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [che574](txt/europe/che574.txt) | CHE574 | Fagus sylvatica L. | 9 | 1897–2008 | 2 | 4 | 37.5 | 0.310 |  |
| [germ074](txt/europe/germ074.txt) | GERM074 | Abies alba Mill. | 15 | 1233–1332 | 1 | 2 | 37.5 | 0.606 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ma007](txt/northamerica/usa/ma007.txt) | MA007 | Quercus spp. L. | 8 | 1579–1695 | 1 | 2 | 37.5 | 0.416 |  |
| [ma010](txt/northamerica/usa/ma010.txt) | MA010 | Quercus spp. L. | 8 | 1555–1704 | 1 | 2 | 37.5 | 0.515 |  |
| [swit312](txt/europe/swit312.txt) | SWIT312 | Castanea sativa Mill. | 421 | 1944–2003 | 24 | 48 | 37.3 | 0.385 | Segment length reduced from 50 to 30 years: the record is only 60 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [wy072](txt/northamerica/usa/wy072.txt) | WY072 | Pinus contorta Douglas ex Loudon | 118 | 1814–2018 | 27 | 59 | 37.2 | 0.370 |  |
| [wi013](txt/northamerica/usa/wi013.txt) | WI013 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 21 | 1–252 | 7 | 10 | 37.0 | 0.356 |  |
| [cana132](txt/northamerica/canada/cana132.txt) | CANA132 | Pinus banksiana Lamb. | 10 | 1825–1992 | 1 | 6 | 36.8 | 0.315 |  |
| [pse002](txt/asia/pse002.txt) | PSE002 | Quercus spp. L. | 33 | 1396–1848 | 6 | 16 | 36.7 | 0.385 |  |
| [fl017](txt/northamerica/usa/fl017.txt) | FL017 | Pinus palustris Mill. | 60 | 1936–2017 | 2 | 29 | 36.5 | 0.485 | Segment length reduced from 50 to 40 years: the record is only 82 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [bra022](txt/southamerica/bra022.txt) | BRA022 | Copaifera lucens Dwyer | 14 | 1966–2012 | 2 | 2 | 36.4 | 0.511 | Segment length reduced from 50 to 22 years: the record is only 47 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [swit313](txt/europe/swit313.txt) | SWIT313 | Castanea sativa Mill. | 452 | 1940–2004 | 44 | 51 | 36.3 | 0.404 | Segment length reduced from 50 to 32 years: the record is only 65 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [col007](txt/southamerica/col007.txt) | COL007 | Qualea lineata Stafleu | 14 | 1960–2016 | 5 | 4 | 36.0 | 0.399 | Segment length reduced from 50 to 28 years: the record is only 57 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [ak186](txt/northamerica/usa/ak186.txt) | AK186 | Alnus spp. Mill. | 50 | 1980–2017 | 11 | 4 | 35.7 | 0.543 | Segment length reduced from 50 to 18 years: the record is only 38 years long. |
| [swit238](txt/europe/swit238.txt) | SWIT238 | Fagus sylvatica L. | 24 | 1938–1987 | 6 | 4 | 35.7 | 0.532 | Segment length reduced from 50 to 24 years: the record is only 50 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit251](txt/europe/swit251.txt) | SWIT251 | Quercus robur L. = Quercus pendunculata Ehrl. | 14 | 1932–1991 | 3 | 2 | 35.7 | 0.557 | Segment length reduced from 50 to 30 years: the record is only 60 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [nepa053](txt/asia/nepa053.txt) | NEPA053 | Abies spectabilis (D. Don) Spach | 40 | 1802–2013 | 14 | 27 | 35.7 | 0.352 |  |
| [gree010](txt/europe/gree010.txt) | GREE010 | Juniperus foetidissima Willd. | 8 | 1332–2003 | 5 | 26 | 35.6 | 0.414 |  |
| [aus121](txt/australia/aus121.txt) | AUS121 | Lagarostrobos franklinii (Hook. f.) Quinn | 39 | 1147–2011 | 53 | 82 | 35.3 | 0.355 |  |
| [gtm001](txt/centralamerica/gtm001.txt) | GTM001 | Taxodium mucronatum Ten. | 12 | 1800–2009 | 4 | 8 | 35.3 | 0.386 |  |
| [mexi111](txt/northamerica/mexico/mexi111.txt) | MEXI111 | Taxodium mucronatum Ten. | 12 | 1800–2009 | 4 | 8 | 35.3 | 0.386 |  |
| [cypr003](txt/europe/cypr003.txt) | CYPR003 | Pinus nigra J.F. Arnold | 24 | 1608–1981 | 1 | 43 | 35.2 | 0.462 |  |
| [per008](txt/southamerica/per008.txt) | PER008 | Drypetes sp. | 71 | 1888–2019 | 13 | 22 | 35.0 | 0.388 |  |
| [bol013](txt/southamerica/bol013.txt) | BOL013 | Centrolobium microchaete (Benth.) H.C. Lima | 26 | 1798–2010 | 16 | 27 | 35.0 | 0.445 |  |
| [lith012](txt/europe/lith012.txt) | LITH012 | Pinus sylvestris L. | 39 | 1816–2002 | 0 | 39 | 34.8 | 0.331 |  |
| [swit383](txt/europe/swit383.txt) | SWIT383 | Picea abies (L.) H. Karst. | 60 | 1975–2017 | 11 | 15 | 34.7 | 0.481 | Segment length reduced from 50 to 20 years: the record is only 43 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [wi069](txt/northamerica/usa/wi069.txt) | WI069 | Acer saccharinum L. | 18 | 1945–2017 | 4 | 6 | 34.5 | 0.453 | Segment length reduced from 50 to 36 years: the record is only 73 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ak152](txt/northamerica/usa/ak152.txt) | AK152 | Salix pulchra Cham. | 20 | 1965–2015 | 1 | 2 | 33.3 | 0.520 | Segment length reduced from 50 to 24 years: the record is only 51 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [ak162](txt/northamerica/usa/ak162.txt) | AK162 | Salix pulchra Cham. | 37 | 1968–2016 | 1 | 1 | 33.3 | 0.590 | Segment length reduced from 50 to 24 years: the record is only 49 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [ak189](txt/northamerica/usa/ak189.txt) | AK189 | Betula spp. L. | 30 | 1957–2016 | 0 | 2 | 33.3 | 0.512 | Segment length reduced from 50 to 30 years: the record is only 60 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [bol007](txt/southamerica/bol007.txt) | BOL007 | Anadenanthera colubrina var. cebil (Griseb.) Altschul = Anadenanthera macrocarpa (Benth.) Brenan | 24 | 1854–2010 | 8 | 9 | 33.3 | 0.404 |  |
| [bol008](txt/southamerica/bol008.txt) | BOL008 | Aspidosperma tomentosum Mart. | 21 | 1913–2010 | 0 | 6 | 33.3 | 0.441 | Segment length reduced from 50 to 48 years: the record is only 98 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [bol014](txt/southamerica/bol014.txt) | BOL014 | Centrolobium microchaete (Benth.) H.C. Lima | 22 | 1900–2007 | 1 | 4 | 33.3 | 0.442 |  |
| [bra010](txt/southamerica/bra010.txt) | BRA010 | Cedrela odorata L. | 29 | 1946–2015 | 3 | 1 | 33.3 | 0.409 | Segment length reduced from 50 to 34 years: the record is only 70 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [germ066](txt/europe/germ066.txt) | GERM066 | Abies alba Mill. | 5 | 1264–1338 | 1 | 0 | 33.3 | 0.407 | Segment length reduced from 50 to 36 years: the record is only 75 years long. |
| [germ140](txt/europe/germ140.txt) | GERM140 | Picea abies (L.) H. Karst. | 10 | 1750–1841 | 1 | 0 | 33.3 | 0.423 | Segment length reduced from 50 to 46 years: the record is only 92 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [germ16](txt/europe/germ16.txt) | GERM16 | Picea abies (L.) H. Karst. | 16 | 1830–1955 | 1 | 4 | 33.3 | 0.419 |  |
| [in046](txt/northamerica/usa/in046.txt) | IN046 | Pinus strobus L. | 39 | 1954–2021 | 11 | 7 | 33.3 | 0.530 | Segment length reduced from 50 to 34 years: the record is only 68 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [ita062](txt/europe/ita062.txt) | ITA062 | Fagus sylvatica L. | 12 | 1955–2019 | 2 | 1 | 33.3 | 0.460 | Segment length reduced from 50 to 32 years: the record is only 65 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [mex127](txt/northamerica/mexico/mex127.txt) | MEX127 | Simira salvadorensis (Standl.) Steyerm. | 40 | 1853–2015 | 5 | 3 | 33.3 | 0.588 |  |
| [nc8](txt/northamerica/usa/nc8.txt) | NC8 | Pinus echinata Mill. | 33 | 1879–1992 | 1 | 14 | 33.3 | 0.398 |  |
| [rus393](txt/asia/rus393.txt) | RUS393 | Betula spp. L. | 37 | 1934–2017 | 1 | 0 | 33.3 | 0.320 | Segment length reduced from 50 to 42 years: the record is only 84 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit299](txt/europe/swit299.txt) | SWIT299 | Picea abies (L.) H. Karst. | 13 | 1878–1989 | 1 | 3 | 33.3 | 0.328 |  |
| [swit301](txt/europe/swit301.txt) | SWIT301 | Abies alba Mill. | 5 | 1764–1990 | 0 | 4 | 33.3 | 0.431 |  |
| [wi050](txt/northamerica/usa/wi050.txt) | WI050 | Quercus macrocarpa Michx. | 18 | 1767–2014 | 5 | 20 | 33.3 | 0.425 |  |
| [npl065](txt/asia/npl065.txt) | NPL065 | Rhododendron campanulatum D.Don | 53 | 1787–2013 | 23 | 30 | 33.1 | 0.409 |  |
| [th005](txt/asia/th005.txt) | TH005 | Pinus merkusii Jungh. & De Vriese | 74 | 1673–2001 | 40 | 61 | 33.1 | 0.381 |  |
| [ital034](txt/europe/ital034.txt) | ITAL034 | Pinus pinaster Aiton | 88 | 1953–2002 | 19 | 23 | 33.1 | 0.546 | Segment length reduced from 50 to 24 years: the record is only 50 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [fl007](txt/northamerica/usa/fl007.txt) | FL007 | Taxodium distichum (L.) Rich. | 28 | 1618–2005 | 9 | 24 | 33.0 | 0.378 |  |
| [wa129](txt/northamerica/usa/wa129.txt) | WA129 | Thuja plicata Donn ex D. Don | 21 | 991–1986 | 42 | 62 | 32.8 | 0.381 |  |
| [nepa059](txt/asia/nepa059.txt) | NEPA059 | Pinus wallichiana A.B. Jacks. | 43 | 1936–2016 | 13 | 7 | 32.8 | 0.431 | Segment length reduced from 50 to 40 years: the record is only 81 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [brit9](txt/europe/brit9.txt) | BRIT9 | Quercus spp. L. | 20 | 862–1193 | 1 | 17 | 32.7 | 0.358 |  |
| [va044](txt/northamerica/usa/va044.txt) | VA044 | Platanus occidentalis L. | 43 | 1960–2019 | 10 | 8 | 32.7 | 0.518 | Segment length reduced from 50 to 30 years: the record is only 60 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [ca712](txt/northamerica/usa/ca712.txt) | CA712 | Sequoiadendron giganteum (Lindl.) Buchholz | 17 | 1475–1991 | 16 | 33 | 32.7 | 0.376 |  |
| [ca644](txt/northamerica/usa/ca644.txt) | CA644 |  | 49 | 890–1467 | 29 | 58 | 32.3 | 0.379 |  |
| [indi007](txt/asia/indi007.txt) | INDI007 | Abies pindrow (Royle ex D. Don) Royle | 14 | 1682–1981 | 10 | 9 | 32.2 | 0.378 |  |
| [russ1](txt/asia/russ1.txt) | RUSS1 | Pinus sylvestris L. | 99 | 884–1461 | 9 | 66 | 32.2 | 0.424 |  |
| [isl011](txt/europe/isl011.txt) | ISL011 | Picea sitchensis (Bong.) Carrière | 28 | 1963–2017 | 1 | 8 | 32.1 | 0.609 | Segment length reduced from 50 to 26 years: the record is only 55 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [che456](txt/europe/che456.txt) | CHE456 | Quercus spp. L. | 24 | 1960–2009 | 4 | 4 | 32.0 | 0.500 | Segment length reduced from 50 to 24 years: the record is only 50 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [indo002](txt/asia/indo002.txt) | INDO002 | Tectona grandis L. f. | 20 | 1668–2004 | 21 | 11 | 32.0 | 0.363 |  |
| [nv527](txt/northamerica/usa/nv527.txt) | NV527 | Populus fremontii Wats. | 20 | 1953–2020 | 3 | 4 | 31.8 | 0.451 | Segment length reduced from 50 to 34 years: the record is only 68 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [wi012](txt/northamerica/usa/wi012.txt) | WI012 | Pinus resinosa Aiton | 19 | 1638–1871 | 8 | 6 | 31.8 | 0.359 |  |
| [wi011](txt/northamerica/usa/wi011.txt) | WI011 | Pinus resinosa Aiton | 32 | 1652–2008 | 7 | 27 | 31.8 | 0.386 |  |
| [arge012](txt/southamerica/arge012.txt) | ARGE012 | Araucaria araucana (Molina) K. Koch | 9 | 1717–1974 | 2 | 11 | 31.7 | 0.512 |  |
| [isl016](txt/europe/isl016.txt) | ISL016 | Picea sitchensis (Bong.) Carrière | 40 | 1961–2017 | 3 | 10 | 31.7 | 0.505 | Segment length reduced from 50 to 28 years: the record is only 57 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [bra036](txt/southamerica/bra036.txt) | BRA036 | Araucaria angustifolia (Bertol.) Kuntze | 13 | 1947–2017 | 2 | 4 | 31.6 | 0.478 | Segment length reduced from 50 to 34 years: the record is only 71 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [cana554](txt/northamerica/canada/cana554.txt) | CANA554 | Pinus banksiana Lamb. | 20 | 1922–2010 | 2 | 4 | 31.6 | 0.448 | Segment length reduced from 50 to 44 years: the record is only 89 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [cana610.plotb](txt/northamerica/canada/cana610.plotb.txt) | CANA610 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 33 | 1897–2015 | 4 | 12 | 31.4 | 0.438 |  |
| [arge111](txt/southamerica/arge111.txt) | ARGE111 | Prosopis spp. L. | 37 | 1930–2002 | 1 | 3 | 30.8 | 0.513 | Segment length reduced from 50 to 36 years: the record is only 73 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [kore005](txt/asia/kore005.txt) | KORE005 | Abies koreana Wils. | 15 | 1944–2016 | 2 | 2 | 30.8 | 0.518 | Segment length reduced from 50 to 36 years: the record is only 73 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [mexi042](txt/northamerica/mexico/mexi042.txt) | MEXI042 | Pinus montezumae Lamb. | 20 | 1865–2006 | 6 | 2 | 30.8 | 0.439 |  |
| [nepa046](txt/asia/nepa046.txt) | NEPA046 | Abies spectabilis (D. Don) Spach | 84 | 1782–2010 | 8 | 56 | 30.8 | 0.398 |  |
| [tha028](txt/asia/tha028.txt) | THA028 | Tectona grandis L. f. | 20 | 1898–1996 | 1 | 7 | 30.8 | 0.397 | Segment length reduced from 50 to 48 years: the record is only 99 years long. |
| [aus120](txt/australia/aus120.txt) | AUS120 | Lagarostrobos franklinii (Hook. f.) Quinn | 69 | 955–2007 | 91 | 148 | 30.6 | 0.378 |  |
| [swit225](txt/europe/swit225.txt) | SWIT225 | Larix decidua Mill. | 20 | 1957–1999 | 8 | 10 | 30.5 | 0.592 | Segment length reduced from 50 to 20 years: the record is only 43 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [arg149](txt/southamerica/arg149.txt) | ARG149 | Cedrela angustifolia Sesse & Mocino ex DC.= Cedrela balansae C. DC. | 24 | 1729–1982 | 5 | 15 | 30.3 | 0.455 |  |
| [arge042](txt/southamerica/arge042.txt) | ARGE042 |  | 24 | 1729–1982 | 5 | 15 | 30.3 | 0.455 |  |
| [mex130](txt/northamerica/mexico/mex130.txt) | MEX130 | Pinus teocote Cham. & Schltdl. | 44 | 1880–2019 | 8 | 5 | 30.2 | 0.420 |  |
| [wa017](txt/northamerica/usa/wa017.txt) | WA017 | Pseudotsuga menziesii (Mirb.) Franco | 20 | 1420–1975 | 12 | 41 | 30.1 | 0.457 |  |
| [wa040](txt/northamerica/usa/wa040.txt) | WA040 | Pinus albicaulis Engelm. | 16 | 1605–1976 | 12 | 19 | 30.1 | 0.407 |  |
| [ak11](txt/northamerica/usa/ak11.txt) | AK11 | Picea sitchensis (Bong.) Carrière | 14 | 1932–1985 | 2 | 1 | 30.0 | 0.494 | Segment length reduced from 50 to 26 years: the record is only 54 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ak147](txt/northamerica/usa/ak147.txt) | AK147 | Tsuga heterophylla (Raf.) Sarg. | 5 | 1749–1996 | 1 | 8 | 30.0 | 0.455 |  |
| [bgd002](txt/asia/bgd002.txt) | BGD002 | Lagerstroemia speciosa (L.) Pers. | 38 | 1912–2016 | 0 | 6 | 30.0 | 0.380 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [germ173](txt/europe/germ173.txt) | GERM173 | Quercus petraea (Matt.) Liebl. = Quercus sessiliflora Salisb. | 10 | 1828–2004 | 0 | 9 | 30.0 | 0.522 |  |
| [swit358](txt/europe/swit358.txt) | SWIT358 | Abies alba Mill. | 44 | 1918–1995 | 1 | 5 | 30.0 | 0.472 | Segment length reduced from 50 to 38 years: the record is only 78 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit381](txt/europe/swit381.txt) | SWIT381 | Picea abies (L.) H. Karst. | 30 | 1904–2014 | 6 | 3 | 30.0 | 0.501 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [turk027](txt/europe/turk027.txt) | TURK027 | Quercus spp. L. | 35 | 1773–2004 | 17 | 23 | 29.9 | 0.396 |  |
| [cana280](txt/northamerica/canada/cana280.txt) | CANA280 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 76 | 1743–2004 | 22 | 39 | 29.8 | 0.394 |  |
| [isl004](txt/europe/isl004.txt) | ISL004 | Pinus contorta Douglas ex Loudon | 24 | 1967–2017 | 9 | 7 | 29.6 | 0.526 | Segment length reduced from 50 to 24 years: the record is only 51 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [swit152w](txt/europe/swit152w.txt) | SWIT152 | Picea abies (L.) H. Karst. | 24 | 1904–1979 | 1 | 7 | 29.6 | 0.454 | Segment length reduced from 50 to 38 years: the record is only 76 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [sc008](txt/northamerica/usa/sc008.txt) | SC008 | Pinus palustris Mill. | 38 | 1636–2017 | 8 | 13 | 29.6 | 0.448 |  |
| [bol006](txt/southamerica/bol006.txt) | BOL006 | Acosmium cardenasii H.S.Irwin & Arroyo | 38 | 1881–2011 | 0 | 18 | 29.5 | 0.461 |  |
| [newz109](txt/australia/newz109.txt) | NEWZ109 | Agathis australis (D. Don) Loudon | 11 | 1154–1846 | 8 | 10 | 29.5 | 0.381 |  |
| [ak055](txt/northamerica/usa/ak055.txt) | AK055 | Picea glauca (Moench) Voss | 97 | 1788–2002 | 23 | 17 | 29.4 | 0.375 |  |
| [lbn010](txt/asia/lbn010.txt) | LBN010 | Cedrus libani A. Rich. | 38 | 1808–2009 | 15 | 12 | 29.3 | 0.423 |  |
| [ca700](txt/northamerica/usa/ca700.txt) | CA700 | Calocedrus decurrens (Torr.) Florin = Libocedrus decurrens Torr. | 75 | 1811–2018 | 5 | 16 | 29.2 | 0.466 |  |
| [nc2](txt/northamerica/usa/nc2.txt) | NC2 | Pinus palustris Mill. | 31 | 1608–1829 | 2 | 12 | 29.2 | 0.438 |  |
| [swit224](txt/europe/swit224.txt) | SWIT224 | Castanea sativa Mill. | 24 | 1964–2000 | 8 | 6 | 29.2 | 0.705 | Segment length reduced from 50 to 18 years: the record is only 37 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [mex152](txt/northamerica/mexico/mex152.txt) | MEX152 | Pinus hartwegii Lindl. | 41 | 1700–2016 | 12 | 13 | 29.1 | 0.377 |  |
| [ak157](txt/northamerica/usa/ak157.txt) | AK157 | Chamaecyparis nootkatensis (D. Don) Spach | 10 | 1700–2004 | 3 | 15 | 29.0 | 0.340 |  |
| [lbn008](txt/asia/lbn008.txt) | LBN008 | Cedrus libani A. Rich. | 39 | 1918–2011 | 0 | 9 | 29.0 | 0.416 | Segment length reduced from 50 to 46 years: the record is only 94 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ecu001](txt/southamerica/ecu001.txt) | ECU001 | Bursera graveolens (Kunth) Triana & Planch. | 51 | 1809–2011 | 5 | 17 | 28.9 | 0.438 |  |
| [russ210](txt/asia/russ210.txt) | RUSS210 | Larix gmelinii (Rupr.) Kuzen.=L.kurulensis(Maxim ex Regel)Pilg.=L.dahuricaTurcz.exTratv=Larix cajanderi Mayr | 10 | 1664–1991 | 0 | 15 | 28.8 | 0.438 |  |
| [wa021](txt/northamerica/usa/wa021.txt) | WA021 | Tsuga heterophylla (Raf.) Sarg. | 28 | 1668–1976 | 4 | 39 | 28.7 | 0.452 |  |
| [cana130](txt/northamerica/canada/cana130.txt) | CANA130 | Picea glauca (Moench) Voss | 30 | 1814–1989 | 2 | 12 | 28.6 | 0.433 |  |
| [cana615](txt/northamerica/canada/cana615.txt) | CANA615 | Acer saccharum Marsh. | 6 | 1885–2017 | 1 | 3 | 28.6 | 0.423 |  |
| [co634](txt/northamerica/usa/co634.txt) | CO634 | Pinus contorta Douglas ex Loudon | 8 | 1747–2003 | 9 | 7 | 28.6 | 0.373 |  |
| [col008](txt/southamerica/col008.txt) | COL008 | Goupia glabra Aubl. | 38 | 1867–2019 | 4 | 2 | 28.6 | 0.417 |  |
| [germ079](txt/europe/germ079.txt) | GERM079 | Abies alba Mill. | 4 | 1490–1538 | 1 | 1 | 28.6 | 0.561 | Segment length reduced from 50 to 24 years: the record is only 49 years long. |
| [ital044](txt/europe/ital044.txt) | ITAL044 | Hedera helix L. | 54 | 1935–2013 | 1 | 7 | 28.6 | 0.427 | Segment length reduced from 50 to 38 years: the record is only 79 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [nv525](txt/northamerica/usa/nv525.txt) | NV525 | Populus fremontii Wats. | 27 | 1948–2020 | 1 | 1 | 28.6 | 0.402 | Segment length reduced from 50 to 36 years: the record is only 73 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [sd019](txt/northamerica/usa/sd019.txt) | SD019 | Quercus macrocarpa Michx. | 9 | 1747–1990 | 2 | 4 | 28.6 | 0.539 |  |
| [chin041](txt/asia/chin041.txt) | CHIN041 | Tsuga dumosa (D. Don) Eichler | 35 | 1530–2005 | 33 | 61 | 28.3 | 0.401 |  |
| [cana272](txt/northamerica/canada/cana272.txt) | CANA272 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 64 | 1716–2006 | 17 | 34 | 28.2 | 0.417 |  |
| [japa017](txt/asia/japa017.txt) | JAPA017 | Cryptomeria japonica (Thunb. ex L.f.) D. Don | 99 | 982–2002 | 30 | 59 | 28.2 | 0.415 |  |
| [arge094](txt/southamerica/arge094.txt) | ARGE094 | Fitzroya cupressoides (Molina) I.M. Johnst. | 37 | 568–1993 | 61 | 148 | 28.1 | 0.411 |  |
| [keny001](txt/africa/keny001.txt) | KENY001 | Vitex keniensis Turr | 20 | 1939–1994 | 4 | 5 | 28.1 | 0.521 | Segment length reduced from 50 to 28 years: the record is only 56 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [rus335](txt/asia/rus335.txt) | RUS335 |  | 67 | 1376–1767 | 19 | 44 | 28.1 | 0.409 |  |
| [kyrg004](txt/asia/kyrg004.txt) | KYRG004 | Juniperus spp. L. | 65 | 1378–1995 | 35 | 78 | 28.1 | 0.403 |  |
| [can675](txt/northamerica/canada/can675.txt) | CAN675 | Tsuga mertensiana (Bong.) Carrière | 74 | 1669–2017 | 2 | 123 | 28.0 | 0.574 |  |
| [swit344](txt/europe/swit344.txt) | SWIT344 | Castanea sativa Mill. | 44 | 1955–2006 | 9 | 15 | 27.9 | 0.506 | Segment length reduced from 50 to 26 years: the record is only 52 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [eth004](txt/africa/eth004.txt) | ETH004 | Juniperus procera Hochst. ex Endl. | 10 | 1849–2003 | 2 | 3 | 27.8 | 0.430 |  |
| [isl002](txt/europe/isl002.txt) | ISL002 | Picea sitchensis (Bong.) Carrière | 28 | 1975–2017 | 4 | 6 | 27.8 | 0.703 | Segment length reduced from 50 to 20 years: the record is only 43 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [indo006](txt/asia/indo006.txt) | INDO006 | Tectona grandis L. f. | 17 | 1776–2004 | 8 | 15 | 27.7 | 0.398 |  |
| [turk003](txt/europe/turk003.txt) | TURK003 | Picea orientalis (L.) Peterm. | 4 | 1686–1989 | 3 | 5 | 27.6 | 0.359 |  |
| [leba002](txt/asia/leba002.txt) | LEBA002 | Abies cilicica (Ant. et Kotschy) Carr. | 19 | 1722–2001 | 14 | 5 | 27.5 | 0.437 |  |
| [chn087](txt/asia/chn087.txt) | CHN087 | Tsuga dumosa (D. Don) Eichler | 33 | 1520–2016 | 26 | 58 | 27.4 | 0.400 |  |
| [lbn011](txt/asia/lbn011.txt) | LBN011 | Pinus pinea L. | 37 | 1921–2011 | 2 | 4 | 27.3 | 0.412 | Segment length reduced from 50 to 44 years: the record is only 91 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [arge142](txt/southamerica/arge142.txt) | ARGE142 | Fitzroya cupressoides (Molina) I.M. Johnst. | 125 | 888–2011 | 150 | 257 | 27.0 | 0.420 |  |
| [nepa061](txt/asia/nepa061.txt) | NEPA061 | Pinus wallichiana A.B. Jacks. | 26 | 1898–2015 | 3 | 4 | 26.9 | 0.478 |  |
| [az027](txt/northamerica/usa/az027.txt) | AZ027 | Pseudotsuga menziesii (Mirb.) Franco | 15 | 1593–1965 | 9 | 9 | 26.9 | 0.449 |  |
| [cypr002](txt/europe/cypr002.txt) | CYPR002 | Pinus nigra J.F. Arnold | 21 | 1616–1981 | 3 | 31 | 26.8 | 0.449 |  |
| [ak122](txt/northamerica/usa/ak122.txt) | AK122 | Picea glauca (Moench) Voss | 29 | 1835–2009 | 0 | 20 | 26.7 | 0.482 |  |
| [ak146](txt/northamerica/usa/ak146.txt) | AK146 | Picea sitchensis (Bong.) Carrière | 6 | 1526–1995 | 0 | 8 | 26.7 | 0.411 |  |
| [cze043](txt/europe/cze043.txt) | CZE043 | Pinus sylvestris L. | 64 | 1877–2019 | 4 | 42 | 26.6 | 0.478 |  |
| [chin083](txt/asia/chin083.txt) | CHIN083 | Pinus taiwanensis Hayata | 14 | 1842–2014 | 3 | 6 | 26.5 | 0.410 |  |
| [cze064](txt/europe/cze064.txt) | CZE064 | Picea abies (L.) H. Karst. | 23 | 1881–2021 | 4 | 5 | 26.5 | 0.528 |  |
| [sd016](txt/northamerica/usa/sd016.txt) | SD016 | Pinus ponderosa Douglas ex C. Lawson | 13 | 1–1991 | 2 | 25 | 26.5 | 0.433 |  |
| [tha022](txt/asia/tha022.txt) | THA022 | Toona ciliata | 173 | 1860–2011 | 6 | 8 | 26.4 | 0.498 |  |
| [or053](txt/northamerica/usa/or053.txt) | OR053 | Pinus ponderosa Douglas ex C. Lawson | 19 | 1639–1995 | 6 | 18 | 26.4 | 0.455 |  |
| [arge090](txt/southamerica/arge090.txt) | ARGE090 | Fitzroya cupressoides (Molina) I.M. Johnst. | 24 | 574–1993 | 44 | 99 | 26.3 | 0.432 |  |
| [swit206](txt/europe/swit206.txt) | SWIT206 | Picea abies (L.) H. Karst. | 20 | 1907–1995 | 3 | 2 | 26.3 | 0.410 | Segment length reduced from 50 to 44 years: the record is only 89 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [bol016](txt/southamerica/bol016.txt) | BOL016 | Centrolobium microchaete (Benth.) H.C. Lima | 23 | 1882–2007 | 4 | 7 | 26.2 | 0.422 |  |
| [bol020](txt/southamerica/bol020.txt) | BOL020 | Handroanthus impetiginosus (Mart. ex DC.) Mattos = Tabebuia impetiginosa (Mart. ex DC.) Standl. | 24 | 1880–2010 | 3 | 8 | 26.2 | 0.342 |  |
| [swit241](txt/europe/swit241.txt) | SWIT241 | Pinus sylvestris L. | 112 | 1915–1994 | 20 | 9 | 26.1 | 0.441 | Segment length reduced from 50 to 40 years: the record is only 80 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [turk001](txt/europe/turk001.txt) | TURK001 | Pinus nigra J.F. Arnold | 37 | 1292–2001 | 22 | 27 | 26.1 | 0.407 |  |
| [bra005](txt/southamerica/bra005.txt) | BRA005 | Alchornea triplinervia (Spreng.) Müll.Arg. | 61 | 1889–2013 | 8 | 5 | 26.0 | 0.446 |  |
| [mt144](txt/northamerica/usa/mt144.txt) | MT144 | Pinus ponderosa Douglas ex C. Lawson | 75 | 1558–2002 | 7 | 89 | 25.9 | 0.540 |  |
| [sd013](txt/northamerica/usa/sd013.txt) | SD013 | Quercus macrocarpa Michx. | 12 | 1733–1991 | 2 | 5 | 25.9 | 0.433 |  |
| [wa038](txt/northamerica/usa/wa038.txt) | WA038 | Pinus ponderosa Douglas ex C. Lawson | 16 | 1490–1975 | 4 | 33 | 25.9 | 0.431 |  |
| [ak083](txt/northamerica/usa/ak083.txt) | AK083 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 31 | 1912–2001 | 3 | 5 | 25.8 | 0.371 | Segment length reduced from 50 to 44 years: the record is only 90 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ak044](txt/northamerica/usa/ak044.txt) | AK044 | Picea glauca (Moench) Voss | 15 | 1784–1996 | 2 | 15 | 25.8 | 0.437 |  |
| [chin027](txt/asia/chin027.txt) | CHIN027 | Abies forestii Rogers | 40 | 1348–2007 | 36 | 94 | 25.7 | 0.428 |  |
| [mex117](txt/northamerica/mexico/mex117.txt) | MEX117 | Cedrela salvadorensis Standl. | 39 | 1890–2007 | 0 | 10 | 25.6 | 0.518 |  |
| [or027](txt/northamerica/usa/or027.txt) | OR027 | Pinus ponderosa Douglas ex C. Lawson | 24 | 1495–1981 | 15 | 52 | 25.6 | 0.447 |  |
| [mo2](txt/northamerica/usa/mo2.txt) | MO2 | Juniperus virginiana L. | 20 | 1131–1991 | 13 | 21 | 25.6 | 0.456 |  |
| [czec004](txt/europe/czec004.txt) | CZEC004 | Fagus sylvatica L. | 178 | 1603–2010 | 27 | 137 | 25.5 | 0.432 |  |
| [fran041](txt/europe/fran041.txt) | FRAN041 | Fagus sylvatica L. | 52 | 1781–2003 | 5 | 42 | 25.5 | 0.514 |  |
| [ny040](txt/northamerica/usa/ny040.txt) | NY040 | Quercus spp. L. | 18 | 1507–1838 | 5 | 11 | 25.4 | 0.455 |  |
| [ak213](txt/northamerica/usa/ak213.txt) | AK213 | Pinus sibirica Du Tour | 61 | 1741–2024 | 14 | 19 | 25.4 | 0.401 |  |
| [bt019](txt/asia/bt019.txt) | BT019 | Juniperus recurva Buch.-Ham. ex D. Don | 15 | 1660–2006 | 15 | 11 | 25.2 | 0.438 |  |
| [az620](txt/northamerica/usa/az620.txt) | AZ620 | Pinus ponderosa Douglas ex C. Lawson | 16 | 1666–2018 | 11 | 16 | 25.2 | 0.456 |  |
| [turk023](txt/europe/turk023.txt) | TURK023 | Pinus nigra J.F. Arnold | 22 | 1444–2003 | 14 | 50 | 25.1 | 0.408 |  |
| [chin028](txt/asia/chin028.txt) | CHIN028 | Abies forestii Rogers | 43 | 1348–2007 | 31 | 45 | 25.1 | 0.412 |  |
| [arg145](txt/southamerica/arg145.txt) | ARG145 | Alnus acuminata Kunth | 31 | 1867–2003 | 3 | 6 | 25.0 | 0.439 |  |
| [az026](txt/northamerica/usa/az026.txt) | AZ026 | Pinus ponderosa Douglas ex C. Lawson | 9 | 1620–1965 | 2 | 10 | 25.0 | 0.481 |  |
| [cana306](txt/northamerica/canada/cana306.txt) | CANA306 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 16 | 1929–2008 | 1 | 1 | 25.0 | 0.561 | Segment length reduced from 50 to 40 years: the record is only 80 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [cze041](txt/europe/cze041.txt) | CZE041 | Pinus sylvestris L. | 26 | 1858–2019 | 0 | 3 | 25.0 | 0.494 |  |
| [fl012](txt/northamerica/usa/fl012.txt) | FL012 | Pinus palustris Mill. | 21 | 1934–1997 | 2 | 1 | 25.0 | 0.500 | Segment length reduced from 50 to 32 years: the record is only 64 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ital028](txt/europe/ital028.txt) | ITAL028 | Pinus mughus Scop. = Pinus mugo Turra | 47 | 1901–2007 | 2 | 2 | 25.0 | 0.419 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [mex151](txt/northamerica/mexico/mex151.txt) | MEX151 | Pinus pseudostrobus Lindl. | 66 | 1942–2015 | 0 | 3 | 25.0 | 0.484 | Segment length reduced from 50 to 36 years: the record is only 74 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [nepa056](txt/asia/nepa056.txt) | NEPA056 | Pinus wallichiana A.B. Jacks. | 59 | 1917–2015 | 1 | 1 | 25.0 | 0.471 | Segment length reduced from 50 to 48 years: the record is only 99 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [newz025](txt/australia/newz025.txt) | NEWZ025 | Nothofagus solanderi (Hook f.) Oerst. | 13 | 1710–1979 | 5 | 7 | 25.0 | 0.393 |  |
| [prt003](txt/europe/prt003.txt) | PRT003 | Quercus suber L. | 4 | 1848–1937 | 2 | 1 | 25.0 | 0.624 | Segment length reduced from 50 to 44 years: the record is only 90 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit147w](txt/europe/swit147w.txt) | SWIT147 | Abies alba Mill. | 28 | 1889–1979 | 1 | 3 | 25.0 | 0.512 | Segment length reduced from 50 to 44 years: the record is only 91 years long. |
| [swit199](txt/europe/swit199.txt) | SWIT199 | Fagus sylvatica L. | 10 | 1882–1994 | 1 | 3 | 25.0 | 0.450 |  |
| [swit214](txt/europe/swit214.txt) | SWIT214 | Castanea sativa Mill. | 20 | 1941–1997 | 2 | 1 | 25.0 | 0.436 | Segment length reduced from 50 to 28 years: the record is only 57 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit245](txt/europe/swit245.txt) | SWIT245 | Castanea sativa Mill. | 20 | 1956–1998 | 2 | 8 | 25.0 | 0.549 | Segment length reduced from 50 to 20 years: the record is only 43 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [japa020](txt/asia/japa020.txt) | JAPA020 | Cryptomeria japonica (Thunb. ex L.f.) D. Don | 46 | 1–1999 | 43 | 83 | 24.9 | 0.423 |  |
| [chin024](txt/asia/chin024.txt) | CHIN024 | Juniperus tibetica Kom. | 42 | 1628–2007 | 35 | 49 | 24.8 | 0.390 |  |
| [indo008](txt/asia/indo008.txt) | INDO008 | Tectona grandis L. f. | 35 | 1760–2000 | 10 | 18 | 24.8 | 0.489 |  |
| [cze045](txt/europe/cze045.txt) | CZE045 | Pinus sylvestris L. | 26 | 1856–2020 | 0 | 19 | 24.7 | 0.419 |  |
| [mong019](txt/asia/mong019.txt) | MONG019 | Larix sibirica Ledeb. = Larix russica (Endl.) Sabine ex Trautv. | 43 | 1375–2004 | 57 | 74 | 24.7 | 0.452 |  |
| [ital046](txt/europe/ital046.txt) | ITAL046 | Pinus nigra J.F. Arnold | 104 | 1785–2013 | 5 | 42 | 24.6 | 0.482 |  |
| [ky002](txt/northamerica/usa/ky002.txt) | KY002 | Quercus alba L. | 20 | 1648–1966 | 4 | 22 | 24.5 | 0.511 |  |
| [me018](txt/northamerica/usa/me018.txt) | ME018 | Picea rubens Sarg. | 11 | 1665–1982 | 8 | 18 | 24.5 | 0.414 |  |
| [per010](txt/southamerica/per010.txt) | PER010 | Swietenia macrophylla King | 89 | 1781–2018 | 12 | 13 | 24.5 | 0.405 |  |
| [nepa052](txt/asia/nepa052.txt) | NEPA052 | Abies spectabilis (D. Don) Spach | 40 | 1846–2013 | 4 | 6 | 24.4 | 0.415 |  |
| [chn090](txt/asia/chn090.txt) | CHN090 | Quercus mongolica Fisch. ex Turcz. | 20 | 1963–2020 | 5 | 4 | 24.3 | 0.547 | Segment length reduced from 50 to 28 years: the record is only 58 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [th006](txt/asia/th006.txt) | TH006 | Pinus merkusii Jungh. & De Vriese | 25 | 1648–2004 | 12 | 24 | 24.3 | 0.388 |  |
| [ausl055](txt/australia/ausl055.txt) | AUSL055 | Callitris intratropica R. Baker & H.G. Smith | 69 | 1845–1973 | 4 | 4 | 24.2 | 0.430 |  |
| [mo075](txt/northamerica/usa/mo075.txt) | MO075 | Pinus echinata Mill. | 166 | 1810–2002 | 9 | 48 | 24.2 | 0.421 |  |
| [ital036](txt/europe/ital036.txt) | ITAL036 | Fagus sylvatica L. | 34 | 1949–2002 | 4 | 9 | 24.1 | 0.530 | Segment length reduced from 50 to 26 years: the record is only 54 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [cana663](txt/northamerica/canada/cana663.txt) | CANA663 | Pinus strobus L. | 53 | 1979–2018 | 7 | 5 | 24.0 | 0.667 | Segment length reduced from 50 to 20 years: the record is only 40 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [va034](txt/northamerica/usa/va034.txt) | VA034 | Quercus alba L. | 20 | 1814–2002 | 2 | 4 | 24.0 | 0.422 |  |
| [indo005](txt/asia/indo005.txt) | INDO005 | Tectona grandis L. f. | 66 | 1565–2005 | 24 | 59 | 23.9 | 0.446 |  |
| [che522](txt/europe/che522.txt) | CHE522 | Picea abies (L.) H. Karst. | 10 | 1892–2009 | 1 | 4 | 23.8 | 0.494 |  |
| [co681](txt/northamerica/usa/co681.txt) | CO681 | Populus tremuloides Michx. | 29 | 1890–2016 | 2 | 3 | 23.8 | 0.409 |  |
| [nepa048](txt/asia/nepa048.txt) | NEPA048 | Abies spectabilis (D. Don) Spach | 40 | 1934–2012 | 9 | 6 | 23.8 | 0.514 | Segment length reduced from 50 to 38 years: the record is only 79 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit403](txt/europe/swit403.txt) | SWIT403 | Abies alba Mill. | 6 | 1779–2011 | 2 | 3 | 23.8 | 0.454 |  |
| [zmb002](txt/africa/zmb002.txt) | ZMB002 | Baikiaea plurijuga Harms | 11 | 1970–2013 | 3 | 2 | 23.8 | 0.466 | Segment length reduced from 50 to 22 years: the record is only 44 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [ak219](txt/northamerica/usa/ak219.txt) | AK219 | Chamaecyparis nootkatensis (D. Don) Spach | 76 | 1263–2022 | 50 | 100 | 23.7 | 0.415 |  |
| [rus356](txt/asia/rus356.txt) | RUS356 | Pinus sylvestris L. | 14 | 1387–1624 | 5 | 4 | 23.7 | 0.411 |  |
| [cana7rw](txt/northamerica/canada/cana7rw.txt) | CANA7 | Pinus banksiana Lamb. | 9 | 1938–1994 | 1 | 3 | 23.5 | 0.507 | Segment length reduced from 50 to 28 years: the record is only 57 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [co684](txt/northamerica/usa/co684.txt) | CO684 | Populus tremuloides Michx. | 30 | 1890–2016 | 3 | 1 | 23.5 | 0.511 |  |
| [mo060](txt/northamerica/usa/mo060.txt) | MO060 | Quercus velutina Lam. | 18 | 1829–2002 | 1 | 7 | 23.5 | 0.403 |  |
| [newz094](txt/australia/newz094.txt) | NEWZ094 | Agathis australis (D. Don) Loudon | 13 | 1517–1854 | 0 | 4 | 23.5 | 0.487 |  |
| [swit305](txt/europe/swit305.txt) | SWIT305 | Castanea sativa Mill. | 17 | 1956–1995 | 3 | 5 | 23.5 | 0.531 | Segment length reduced from 50 to 20 years: the record is only 40 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [th001](txt/asia/th001.txt) | TH001 | Tectona grandis L. f. | 77 | 1558–2005 | 38 | 40 | 23.5 | 0.409 |  |
| [chin021](txt/asia/chin021.txt) | CHIN021 | Abies forestii Rogers | 48 | 1380–2007 | 45 | 51 | 23.5 | 0.418 |  |
| [kore002](txt/asia/kore002.txt) | KORE002 | Pinus densiflora Siebold & Zucc. | 51 | 1962–2018 | 13 | 2 | 23.4 | 0.528 | Segment length reduced from 50 to 28 years: the record is only 57 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [finl025](txt/europe/finl025.txt) | FINL025 | Pinus sylvestris L. | 16 | 1381–1823 | 14 | 15 | 23.4 | 0.444 |  |
| [nc036](txt/northamerica/usa/nc036.txt) | NC036 | Tsuga canadensis (L.) Carr. | 24 | 1509–2025 | 1 | 17 | 23.4 | 0.410 |  |
| [cana133](txt/northamerica/canada/cana133.txt) | CANA133 | Pinus banksiana Lamb. | 28 | 1849–1998 | 1 | 6 | 23.3 | 0.441 |  |
| [fran037](txt/europe/fran037.txt) | FRAN037 | Larix decidua Mill. | 5 | 1754–2000 | 1 | 6 | 23.3 | 0.452 |  |
| [ak156](txt/northamerica/usa/ak156.txt) | AK156 | Chamaecyparis nootkatensis (D. Don) Spach | 14 | 1700–2004 | 7 | 19 | 23.2 | 0.393 |  |
| [arge102](txt/southamerica/arge102.txt) | ARGE102 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 21 | 1890–1991 | 2 | 1 | 23.1 | 0.481 |  |
| [bra004](txt/southamerica/bra004.txt) | BRA004 | Alchornea triplinervia (Spreng.) Müll.Arg. | 36 | 1904–2013 | 1 | 2 | 23.1 | 0.548 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [bra023](txt/southamerica/bra023.txt) | BRA023 | Hymenaea courbaril L. | 28 | 1830–2012 | 4 | 8 | 23.1 | 0.409 |  |
| [cana052](txt/northamerica/canada/cana052.txt) | CANA052 | Picea glauca (Moench) Voss | 15 | 1759–1988 | 5 | 10 | 23.1 | 0.417 |  |
| [cana146](txt/northamerica/canada/cana146.txt) | CANA146 | Abies balsamea (L.) Mill. | 23 | 1903–1994 | 1 | 2 | 23.1 | 0.516 | Segment length reduced from 50 to 46 years: the record is only 92 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [newz107](txt/australia/newz107.txt) | NEWZ107 | Agathis australis (D. Don) Loudon | 6 | 1501–1849 | 3 | 3 | 23.1 | 0.447 |  |
| [npl062](txt/asia/npl062.txt) | NPL062 | Picea smithiana (Wall.) Boiss. | 42 | 1724–2013 | 8 | 23 | 23.0 | 0.433 |  |
| [wy049](txt/northamerica/usa/wy049.txt) | WY049 | Pinus flexilis E. James | 18 | 471–1998 | 17 | 28 | 23.0 | 0.408 |  |
| [nepa050](txt/asia/nepa050.txt) | NEPA050 | Abies spectabilis (D. Don) Spach | 41 | 1857–2012 | 6 | 8 | 23.0 | 0.470 |  |
| [cana275](txt/northamerica/canada/cana275.txt) | CANA275 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 80 | 1716–2006 | 30 | 20 | 22.9 | 0.459 |  |
| [swit171w](txt/europe/swit171w.txt) | SWIT171 | Picea abies (L.) H. Karst. | 23 | 1695–1988 | 7 | 18 | 22.9 | 0.443 |  |
| [az560](txt/northamerica/usa/az560.txt) | AZ560 | Pinus ponderosa Douglas ex C. Lawson | 15 | 1800–1995 | 8 | 3 | 22.9 | 0.443 |  |
| [cana603](txt/northamerica/canada/cana603.txt) | CANA603 | Picea glauca (Moench) Voss | 25 | 1651–1999 | 6 | 18 | 22.9 | 0.456 |  |
| [deu491](txt/europe/deu491.txt) | DEU491 | Picea abies (L.) H. Karst. | 56 | 1914–2020 | 5 | 3 | 22.9 | 0.450 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit346](txt/europe/swit346.txt) | SWIT346 | Quercus petraea (Matt.) Liebl. = Quercus sessiliflora Salisb. | 48 | 1956–2006 | 8 | 13 | 22.8 | 0.535 | Segment length reduced from 50 to 24 years: the record is only 51 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [che580](txt/europe/che580.txt) | CHE580 | Fagus sylvatica L. | 11 | 1928–2006 | 4 | 1 | 22.7 | 0.440 | Segment length reduced from 50 to 38 years: the record is only 79 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [deu310](txt/europe/deu310.txt) | DEU310 | Tilia cordata Mill. | 16 | 1955–2017 | 1 | 4 | 22.7 | 0.633 | Segment length reduced from 50 to 30 years: the record is only 63 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [wa2](txt/northamerica/usa/wa2.txt) | WA2 | Pseudotsuga menziesii (Mirb.) Franco | 12 | 1791–1986 | 4 | 6 | 22.7 | 0.483 |  |
| [newz074](txt/australia/newz074.txt) | NEWZ074 | Libocedrus bidwillii Hook. f. | 25 | 1213–1992 | 28 | 48 | 22.6 | 0.448 |  |
| [brit026](txt/europe/brit026.txt) | BRIT026 | Pinus sylvestris L. | 21 | 1671–1978 | 4 | 12 | 22.5 | 0.427 |  |
| [brit11](txt/europe/brit11.txt) | BRIT11 | Quercus spp. L. | 9 | 1708–1972 | 1 | 8 | 22.5 | 0.419 |  |
| [isl007](txt/europe/isl007.txt) | ISL007 | Picea sitchensis (Bong.) Carrière | 34 | 1962–2016 | 2 | 7 | 22.5 | 0.541 | Segment length reduced from 50 to 26 years: the record is only 55 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [wa3](txt/northamerica/usa/wa3.txt) | WA3 | Pseudotsuga menziesii (Mirb.) Franco | 17 | 1788–1986 | 3 | 8 | 22.4 | 0.626 |  |
| [wa059](txt/northamerica/usa/wa059.txt) | WA059 | Pinus ponderosa Douglas ex C. Lawson | 39 | 1433–1980 | 15 | 84 | 22.4 | 0.477 |  |
| [chil012](txt/southamerica/chil012.txt) | CHIL012 | Araucaria araucana (Molina) K. Koch | 43 | 1239–1975 | 31 | 52 | 22.4 | 0.442 |  |
| [cana591](txt/northamerica/canada/cana591.txt) | CANA591 | Pinus contorta Douglas ex Loudon | 153 | 1772–2013 | 13 | 97 | 22.4 | 0.497 |  |
| [chin046](txt/asia/chin046.txt) | CHIN046 | Juniperus tibetica Kom. | 48 | 449–2004 | 78 | 131 | 22.3 | 0.452 |  |
| [or052](txt/northamerica/usa/or052.txt) | OR052 | Pinus ponderosa Douglas ex C. Lawson | 27 | 1419–1995 | 20 | 60 | 22.3 | 0.430 |  |
| [az625](txt/northamerica/usa/az625.txt) | AZ625 | Quercus gambelii Nutt. | 60 | 1764–2022 | 8 | 33 | 22.3 | 0.451 |  |
| [bt002](txt/asia/bt002.txt) | BT002 | Tsuga dumosa (D. Don) Eichler | 43 | 1454–2003 | 48 | 64 | 22.2 | 0.417 |  |
| [cana556](txt/northamerica/canada/cana556.txt) | CANA556 | Pinus banksiana Lamb. | 19 | 1912–2010 | 0 | 4 | 22.2 | 0.453 | Segment length reduced from 50 to 48 years: the record is only 99 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [che590](txt/europe/che590.txt) | CHE590 | Fagus sylvatica L. | 11 | 1938–2012 | 2 | 2 | 22.2 | 0.498 | Segment length reduced from 50 to 36 years: the record is only 75 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [finl007](txt/europe/finl007.txt) | FINL007 | Pinus sylvestris L. | 20 | 1472–1909 | 12 | 28 | 22.2 | 0.445 |  |
| [germ204](txt/europe/germ204.txt) | GERM204 | Quercus robur L. = Quercus pendunculata Ehrl. | 15 | 1845–2005 | 1 | 9 | 22.2 | 0.559 |  |
| [ita075](txt/europe/ita075.txt) | ITA075 | Quercus robur L. = Quercus pendunculata Ehrl. | 21 | 1911–2023 | 1 | 1 | 22.2 | 0.482 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [slov003](txt/europe/slov003.txt) | SLOV003 | Abies alba Mill. | 21 | 1890–1993 | 0 | 4 | 22.2 | 0.378 |  |
| [kyrg008](txt/asia/kyrg008.txt) | KYRG008 | Juniperus turkestanica Komar. | 27 | 1420–1987 | 35 | 37 | 22.1 | 0.444 |  |
| [syri003](txt/asia/syri003.txt) | SYRI003 | Pinus brutia Ten. | 40 | 1882–2001 | 8 | 9 | 22.1 | 0.448 |  |
| [chin045](txt/asia/chin045.txt) | CHIN045 | Juniperus tibetica Kom. | 12 | 1568–1993 | 13 | 17 | 22.1 | 0.419 |  |
| [mexi087](txt/northamerica/mexico/mexi087.txt) | MEXI087 | Taxodium mucronatum Ten. | 40 | 1880–2007 | 7 | 4 | 22.0 | 0.359 |  |
| [spai074](txt/europe/spai074.txt) | SPAI074 | Abies pinsapo Boiss. | 32 | 1785–1998 | 1 | 10 | 22.0 | 0.448 |  |
| [brit055](txt/europe/brit055.txt) | BRIT055 | Quercus spp. L. | 18 | 1642–2004 | 11 | 9 | 22.0 | 0.405 |  |
| [or047](txt/northamerica/usa/or047.txt) | OR047 | Pinus ponderosa Douglas ex C. Lawson | 30 | 1354–1991 | 16 | 66 | 21.9 | 0.443 |  |
| [chil009](txt/southamerica/chil009.txt) | CHIL009 | Araucaria araucana (Molina) K. Koch | 27 | 1440–1975 | 5 | 27 | 21.9 | 0.477 |  |
| [nepa049](txt/asia/nepa049.txt) | NEPA049 | Abies spectabilis (D. Don) Spach | 39 | 1875–2012 | 1 | 6 | 21.9 | 0.424 |  |
| [swit377](txt/europe/swit377.txt) | SWIT377 | Picea abies (L.) H. Karst. | 30 | 1881–2016 | 3 | 4 | 21.9 | 0.653 |  |
| [ca561](txt/northamerica/usa/ca561.txt) | CA561 | Pinus lambertiana Douglas | 40 | 1880–1990 | 8 | 9 | 21.8 | 0.527 |  |
| [indi010](txt/asia/indi010.txt) | INDI010 | Cedrus deodara (D. Don) G. Don | 27 | 1711–1988 | 5 | 17 | 21.8 | 0.514 |  |
| [swit380](txt/europe/swit380.txt) | SWIT380 | Picea abies (L.) H. Karst. | 30 | 1938–2016 | 5 | 5 | 21.7 | 0.557 | Segment length reduced from 50 to 38 years: the record is only 79 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [va031](txt/northamerica/usa/va031.txt) | VA031 | Fagus grandifolia Ehrh. | 27 | 1854–2004 | 1 | 9 | 21.7 | 0.389 |  |
| [bt006](txt/asia/bt006.txt) | BT006 | Picea spinulosa (Griff.) Beissn. | 17 | 1400–2005 | 20 | 18 | 21.7 | 0.426 |  |
| [mt180](txt/northamerica/usa/mt180.txt) | MT180 | Pinus albicaulis Engelm. | 438 | 1608–2012 | 162 | 222 | 21.7 | 0.413 |  |
| [ak047](txt/northamerica/usa/ak047.txt) | AK047 | Picea glauca (Moench) Voss | 16 | 1676–2002 | 4 | 20 | 21.6 | 0.480 |  |
| [chil015](txt/southamerica/chil015.txt) | CHIL015 | Fitzroya cupressoides (Molina) I.M. Johnst. | 33 | 1386–1987 | 19 | 29 | 21.6 | 0.423 |  |
| [ital037](txt/europe/ital037.txt) | ITAL037 | Fagus sylvatica L. | 33 | 1938–2002 | 5 | 3 | 21.6 | 0.576 | Segment length reduced from 50 to 32 years: the record is only 65 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [nc035](txt/northamerica/usa/nc035.txt) | NC035 | Pinus palustris Mill. | 20 | 1805–2016 | 5 | 3 | 21.6 | 0.458 |  |
| [ita073bl25](txt/europe/ita073bl25.txt) | ITA073 | Larix decidua Mill. | 28 | 1502–2016 | 9 | 19 | 21.5 | 0.426 |  |
| [bgd003](txt/asia/bgd003.txt) | BGD003 | Toona ciliata | 40 | 1930–2016 | 3 | 3 | 21.4 | 0.445 | Segment length reduced from 50 to 42 years: the record is only 87 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [brit003](txt/europe/brit003.txt) | BRIT003 | Quercus petraea (Matt.) Liebl. = Quercus sessiliflora Salisb. | 14 | 1813–1978 | 0 | 6 | 21.4 | 0.423 |  |
| [ga023](txt/northamerica/usa/ga023.txt) | GA023 | Tsuga canadensis (L.) Carr. | 40 | 1947–2011 | 2 | 4 | 21.4 | 0.469 | Segment length reduced from 50 to 32 years: the record is only 65 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [wy083](txt/northamerica/usa/wy083.txt) | WY083 | Pinus contorta Douglas ex Loudon | 27 | 1849–2023 | 1 | 5 | 21.4 | 0.419 |  |
| [mexi029](txt/northamerica/mexico/mexi029.txt) | MEXI029 | Taxodium mucronatum Ten. | 61 | 1462–2000 | 42 | 54 | 21.4 | 0.447 |  |
| [cze047](txt/europe/cze047.txt) | CZE047 | Pinus sylvestris L. | 35 | 1890–2019 | 3 | 16 | 21.3 | 0.480 |  |
| [bol018](txt/southamerica/bol018.txt) | BOL018 | Cedrela odorata L. | 63 | 1850–2001 | 8 | 18 | 21.3 | 0.429 |  |
| [paki013](txt/asia/paki013.txt) | PAKI013 | Juniperus turkestanica Komar. | 12 | 1540–1999 | 10 | 13 | 21.3 | 0.459 |  |
| [newz087](txt/australia/newz087.txt) | NEWZ087 | Agathis australis (D. Don) Loudon | 19 | 1710–1996 | 9 | 1 | 21.3 | 0.429 |  |
| [bol003](txt/southamerica/bol003.txt) | BOL003 | Centrolobium microchaete (Benth.) H.C. Lima | 18 | 1798–2010 | 7 | 7 | 21.2 | 0.533 |  |
| [cana164](txt/northamerica/canada/cana164.txt) | CANA164 | Pseudotsuga menziesii (Mirb.) Franco | 8 | -866–-606 | 2 | 5 | 21.2 | 0.465 |  |
| [cana252](txt/northamerica/canada/cana252.txt) | CANA252 | Pinus resinosa Aiton | 34 | 1759–2004 | 10 | 25 | 21.2 | 0.445 |  |
| [me050](txt/northamerica/usa/me050.txt) | ME050 | Picea rubens Sarg. | 12 | 1830–2009 | 3 | 4 | 21.2 | 0.458 |  |
| [chin038](txt/asia/chin038.txt) | CHIN038 | Tsuga dumosa (D. Don) Eichler | 42 | 1591–2005 | 17 | 41 | 21.2 | 0.455 |  |
| [czec008](txt/europe/czec008.txt) | CZEC008 | Pinus sylvestris L. | 59 | 1778–2019 | 4 | 49 | 21.1 | 0.524 |  |
| [bgd001](txt/asia/bgd001.txt) | BGD001 | Chukrasia tabularis A.Juss. | 40 | 1911–2012 | 2 | 2 | 21.1 | 0.392 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ital038](txt/europe/ital038.txt) | ITAL038 | Fagus sylvatica L. | 38 | 1942–2002 | 2 | 6 | 21.1 | 0.571 | Segment length reduced from 50 to 30 years: the record is only 61 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [lith037](txt/europe/lith037.txt) | LITH037 | Quercus robur L. = Quercus pendunculata Ehrl. | 24 | 1821–1997 | 4 | 4 | 21.1 | 0.531 |  |
| [per002](txt/southamerica/per002.txt) | PER002 | Cedrela odorata L. \| Juglans neotropica Diels | 47 | 1794–2009 | 8 | 13 | 21.0 | 0.451 |  |
| [nepa011](txt/asia/nepa011.txt) | NEPA011 | Abies spectabilis (D. Don) Spach | 22 | 1681–1996 | 11 | 27 | 21.0 | 0.443 |  |
| [cana5](txt/northamerica/canada/cana5.txt) | CANA5 | Abies amabilis Douglas ex J. Forbes | 17 | 1711–1986 | 9 | 4 | 21.0 | 0.474 |  |
| [nepa025](txt/asia/nepa025.txt) | NEPA025 | Ulmus wallichiana Planch. | 22 | 1566–1997 | 21 | 14 | 21.0 | 0.435 |  |
| [ital054](txt/europe/ital054.txt) | ITAL054 | Pinus pinea L. | 69 | 1828–2015 | 9 | 31 | 20.9 | 0.396 |  |
| [fl021](txt/northamerica/usa/fl021.txt) | FL021 | Pinus palustris Mill. | 56 | 1809–2017 | 2 | 7 | 20.9 | 0.462 |  |
| [wa101](txt/northamerica/usa/wa101.txt) | WA101 | Tsuga mertensiana (Bong.) Carrière | 18 | 1406–1992 | 9 | 27 | 20.9 | 0.498 |  |
| [cana610.plota](txt/northamerica/canada/cana610.plota.txt) | CANA610 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 62 | 1894–2015 | 2 | 22 | 20.9 | 0.475 |  |
| [ak3](txt/northamerica/usa/ak3.txt) | AK3 | Picea sitchensis (Bong.) Carrière | 14 | 1807–1986 | 1 | 4 | 20.8 | 0.526 |  |
| [brit028](txt/europe/brit028.txt) | BRIT028 | Quercus spp. L. | 10 | 1776–1990 | 6 | 4 | 20.8 | 0.479 |  |
| [che569](txt/europe/che569.txt) | CHE569 | Fagus sylvatica L. | 10 | 1889–2008 | 2 | 3 | 20.8 | 0.521 |  |
| [russ287](txt/asia/russ287.txt) | RUSS287 | Pinus sylvestris L. | 71 | 1863–2016 | 2 | 28 | 20.8 | 0.555 |  |
| [spai063](txt/europe/spai063.txt) | SPAI063 | Pinus sylvestris L. | 29 | 1956–2008 | 3 | 2 | 20.8 | 0.647 | Segment length reduced from 50 to 26 years: the record is only 53 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [swit163w](txt/europe/swit163w.txt) | SWIT163 | Picea abies (L.) H. Karst. | 26 | 1841–1979 | 6 | 4 | 20.8 | 0.428 |  |
| [wa6](txt/northamerica/usa/wa6.txt) | WA6 | Tsuga heterophylla (Raf.) Sarg. | 15 | 1630–1986 | 3 | 19 | 20.8 | 0.467 |  |
| [cana149](txt/northamerica/canada/cana149.txt) | CANA149 | Pinus strobus L. | 14 | 1187–1852 | 11 | 6 | 20.7 | 0.420 |  |
| [wa009](txt/northamerica/usa/wa009.txt) | WA009 | Pinus ponderosa Douglas ex C. Lawson | 16 | 1695–1975 | 1 | 22 | 20.7 | 0.545 |  |
| [spai066](txt/europe/spai066.txt) | SPAI066 | Quercus faginea Lam. | 16 | 1958–2007 | 2 | 4 | 20.7 | 0.644 | Segment length reduced from 50 to 24 years: the record is only 50 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [turk011](txt/europe/turk011.txt) | TURK011 | Pinus nigra J.F. Arnold | 26 | 1568–1999 | 9 | 22 | 20.7 | 0.442 |  |
| [cze057](txt/europe/cze057.txt) | CZE057 | Picea abies (L.) H. Karst. | 25 | 1788–2021 | 9 | 16 | 20.7 | 0.471 |  |
| [deu490](txt/europe/deu490.txt) | DEU490 | Abies grandis (Dougl. ex D. Don) Lindl. | 97 | 1967–2020 | 8 | 11 | 20.7 | 0.587 | Segment length reduced from 50 to 26 years: the record is only 54 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [che482](txt/europe/che482.txt) | CHE482 | Picea abies (L.) H. Karst. | 10 | 1752–2023 | 4 | 4 | 20.5 | 0.449 |  |
| [arg157](txt/southamerica/arg157.txt) | ARG157 | Juglans australis Griseb. | 22 | 1930–2003 | 4 | 5 | 20.5 | 0.593 | Segment length reduced from 50 to 36 years: the record is only 74 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ca608](txt/northamerica/usa/ca608.txt) | CA608 | Pseudotsuga menziesii (Mirb.) Franco | 19 | 1760–1997 | 6 | 4 | 20.4 | 0.487 |  |
| [fl019](txt/northamerica/usa/fl019.txt) | FL019 | Pinus palustris Mill. | 60 | 1921–2017 | 5 | 5 | 20.4 | 0.467 | Segment length reduced from 50 to 48 years: the record is only 97 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit359](txt/europe/swit359.txt) | SWIT359 | Quercus petraea (Matt.) Liebl. = Quercus sessiliflora Salisb. | 25 | 1810–1996 | 2 | 8 | 20.4 | 0.594 |  |
| [swit180](txt/europe/swit180.txt) | SWIT180 | Picea abies (L.) H. Karst. | 15 | 1750–1999 | 5 | 6 | 20.4 | 0.479 |  |
| [ny052](txt/northamerica/usa/ny052.txt) | NY052 | Acer saccharinum L. | 51 | 1884–2023 | 5 | 11 | 20.3 | 0.438 |  |
| [bt021](txt/asia/bt021.txt) | BT021 | Picea spinulosa (Griff.) Beissn. | 77 | 1280–2013 | 78 | 122 | 20.2 | 0.449 |  |
| [ak194](txt/northamerica/usa/ak194.txt) | AK194 | Chamaecyparis nootkatensis (D. Don) Spach | 26 | 1520–2011 | 30 | 21 | 20.2 | 0.408 |  |
| [paki008](txt/asia/paki008.txt) | PAKI008 | Juniperus spp. L. | 8 | 568–1990 | 16 | 23 | 20.1 | 0.437 |  |
| [ca703](txt/northamerica/usa/ca703.txt) | CA703 | Sequoiadendron giganteum (Lindl.) Buchholz | 87 | -40–2012 | 91 | 134 | 20.1 | 0.446 |  |
| [ak198](txt/northamerica/usa/ak198.txt) | AK198 | Pinus contorta Douglas ex Loudon | 63 | 1559–2012 | 44 | 45 | 20.0 | 0.431 |  |
| [ausl008](txt/australia/ausl008.txt) | AUSL008 | Callitris robusta (A. Cunn. ex Parl.) F.M. Bailey  = Callitris preissii Miq. | 14 | 1912–1975 | 1 | 2 | 20.0 | 0.528 | Segment length reduced from 50 to 32 years: the record is only 64 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [brit032](txt/europe/brit032.txt) | BRIT032 | Fraxinus excelsior L. | 5 | 1825–1987 | 1 | 1 | 20.0 | 0.396 |  |
| [brit073](txt/europe/brit073.txt) | BRIT073 | Quercus spp. L. | 28 | 1203–1372 | 0 | 4 | 20.0 | 0.579 |  |
| [che471](txt/europe/che471.txt) | CHE471 | Picea abies (L.) H. Karst. | 11 | 1806–2023 | 2 | 2 | 20.0 | 0.455 |  |
| [che496](txt/europe/che496.txt) | CHE496 | Picea abies (L.) H. Karst. | 15 | 1919–2010 | 1 | 2 | 20.0 | 0.602 | Segment length reduced from 50 to 46 years: the record is only 92 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [che514](txt/europe/che514.txt) | CHE514 | Picea abies (L.) H. Karst. | 10 | 1909–2010 | 0 | 2 | 20.0 | 0.497 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [che553](txt/europe/che553.txt) | CHE553 | Abies alba Mill. | 10 | 1904–2008 | 2 | 0 | 20.0 | 0.520 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [in024](txt/northamerica/usa/in024.txt) | IN024 | Acer saccharum Marsh. | 17 | 1907–2015 | 1 | 2 | 20.0 | 0.478 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [me028](txt/northamerica/usa/me028.txt) | ME028 | Fraxinus nigra Marsh. | 44 | 1896–1994 | 0 | 1 | 20.0 | 0.592 | Segment length reduced from 50 to 48 years: the record is only 99 years long. |
| [russ160w](txt/asia/russ160w.txt) | RUSS160 | Picea obovata Ledeb. = Picea abies (L.) H. Karst. subsp. obovata (Ledeb.) Hultén | 22 | 1881–1990 | 1 | 3 | 20.0 | 0.631 |  |
| [swit263](txt/europe/swit263.txt) | SWIT263 | Populus tremula L. | 6 | 1938–1998 | 0 | 2 | 20.0 | 0.562 | Segment length reduced from 50 to 30 years: the record is only 61 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [va035](txt/northamerica/usa/va035.txt) | VA035 | Quercus alba L. | 20 | 1919–2006 | 0 | 1 | 20.0 | 0.432 | Segment length reduced from 50 to 44 years: the record is only 88 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [wi006](txt/northamerica/usa/wi006.txt) | WI006 | Quercus alba L. | 15 | 1807–2000 | 3 | 6 | 20.0 | 0.425 |  |
| [kyrg003](txt/asia/kyrg003.txt) | KYRG003 | Juniperus spp. L. | 33 | 1591–1995 | 16 | 18 | 19.9 | 0.436 |  |
| [turk025](txt/europe/turk025.txt) | TURK025 | Cedrus libani A. Rich. | 23 | 1423–2003 | 10 | 17 | 19.9 | 0.416 |  |
| [nepa042](txt/asia/nepa042.txt) | NEPA042 | Tsuga dumosa (D. Don) Eichler | 21 | 1500–1999 | 20 | 24 | 19.8 | 0.479 |  |
| [paki004](txt/asia/paki004.txt) | PAKI004 | Juniperus turkestanica Komar. | 25 | 1240–1999 | 18 | 20 | 19.7 | 0.423 |  |
| [che502](txt/europe/che502.txt) | CHE502 | Picea abies (L.) H. Karst. | 30 | 1675–2014 | 4 | 14 | 19.6 | 0.465 |  |
| [mex131](txt/northamerica/mexico/mex131.txt) | MEX131 | Picea martinezii T.F.Patt. | 109 | 1736–2020 | 12 | 32 | 19.6 | 0.551 |  |
| [newz071](txt/australia/newz071.txt) | NEWZ071 | Libocedrus bidwillii Hook. f. | 11 | 1626–1990 | 7 | 8 | 19.5 | 0.461 |  |
| [can693](txt/northamerica/canada/can693.txt) | CAN693 | Picea glauca (Moench) Voss | 53 | 1859–2022 | 6 | 16 | 19.5 | 0.410 |  |
| [arge140](txt/southamerica/arge140.txt) | ARGE140 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 138 | 962–2011 | 26 | 52 | 19.5 | 0.474 |  |
| [cana439](txt/northamerica/canada/cana439.txt) | CANA439 | Picea engelmannii Parry ex Engelm. | 43 | 1428–2000 | 24 | 51 | 19.4 | 0.455 |  |
| [bol015](txt/southamerica/bol015.txt) | BOL015 | Centrolobium microchaete (Benth.) H.C. Lima | 36 | 1881–2007 | 3 | 10 | 19.4 | 0.461 |  |
| [paki007](txt/asia/paki007.txt) | PAKI007 | Juniperus spp. L. | 18 | 1141–1993 | 14 | 41 | 19.4 | 0.499 |  |
| [turk033](txt/europe/turk033.txt) | TURK033 | Pinus nigra J.F. Arnold | 36 | 1567–1995 | 19 | 42 | 19.4 | 0.467 |  |
| [cana557](txt/northamerica/canada/cana557.txt) | CANA557 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 445 | 1842–2013 | 82 | 56 | 19.4 | 0.452 |  |
| [cypr014](txt/europe/cypr014.txt) | CYPR014 | Pinus nigra J.F. Arnold | 12 | 1594–1978 | 7 | 11 | 19.4 | 0.489 |  |
| [newz010](txt/australia/newz010.txt) | NEWZ010 | Dacrydium biforme (Hook.) Pilg. = Halocarpus biformis (Hook.) Quinn = Halocarpus biformis Hook. | 25 | 1567–1976 | 12 | 18 | 19.4 | 0.426 |  |
| [ak215](txt/northamerica/usa/ak215.txt) | AK215 | Tsuga heterophylla (Raf.) Sarg. | 49 | 1444–2016 | 35 | 60 | 19.3 | 0.453 |  |
| [mex150](txt/northamerica/mexico/mex150.txt) | MEX150 | Pinus montezumae Lamb. | 60 | 1790–2017 | 2 | 15 | 19.3 | 0.536 |  |
| [swed338](txt/europe/swed338.txt) | SWED338 | Pinus sylvestris L. | 19 | 1684–2010 | 7 | 14 | 19.3 | 0.459 |  |
| [wy051](txt/northamerica/usa/wy051.txt) | WY051 | Pinus contorta Douglas ex Loudon | 27 | 1597–2009 | 3 | 18 | 19.3 | 0.460 |  |
| [ita073bl50](txt/europe/ita073bl50.txt) | ITA073 | Larix decidua Mill. | 28 | 1502–2016 | 5 | 20 | 19.2 | 0.441 |  |
| [mexi103](txt/northamerica/mexico/mexi103.txt) | MEXI103 | Pinus hartwegii Lindl. | 18 | 1546–2009 | 13 | 6 | 19.2 | 0.468 |  |
| [nepa029](txt/asia/nepa029.txt) | NEPA029 | Tsuga dumosa (D. Don) Eichler | 38 | 1559–1994 | 27 | 30 | 19.1 | 0.455 |  |
| [mex145](txt/northamerica/mexico/mex145.txt) | MEX145 | Pinus patula Schiede & Deppe | 58 | 1786–2015 | 13 | 12 | 19.1 | 0.488 |  |
| [ut547](txt/northamerica/usa/ut547.txt) | UT547 | Pinus flexilis E. James | 21 | 1406–2019 | 10 | 15 | 19.1 | 0.473 |  |
| [va009](txt/northamerica/usa/va009.txt) | VA009 | Quercus prinus L. | 35 | 1587–1982 | 22 | 35 | 19.1 | 0.482 |  |
| [mex149](txt/northamerica/mexico/mex149.txt) | MEX149 | Pinus rudis Endl. | 49 | 1879–2015 | 7 | 5 | 19.0 | 0.454 |  |
| [swit177w](txt/europe/swit177w.txt) | SWIT177 | Picea abies (L.) H. Karst. | 37 | 982–1976 | 3 | 13 | 19.0 | 0.439 |  |
| [swit211](txt/europe/swit211.txt) | SWIT211 | Abies alba Mill. | 22 | 1829–1992 | 1 | 3 | 19.0 | 0.475 |  |
| [tha009](txt/asia/tha009.txt) | THA009 | Pinus kesiya Royle ex Gordon | 55 | 1930–2001 | 2 | 2 | 19.0 | 0.471 | Segment length reduced from 50 to 36 years: the record is only 72 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [mi021](txt/northamerica/usa/mi021.txt) | MI021 | Pinus resinosa Aiton | 43 | 1–2006 | 22 | 29 | 19.0 | 0.418 |  |
| [ca682](txt/northamerica/usa/ca682.txt) | CA682 | Pseudotsuga menziesii (Mirb.) Franco | 28 | 1900–2009 | 5 | 6 | 19.0 | 0.533 |  |
| [cana131](txt/northamerica/canada/cana131.txt) | CANA131 | Picea glauca (Moench) Voss | 21 | 1865–1989 | 0 | 7 | 18.9 | 0.535 |  |
| [mexi047](txt/northamerica/mexico/mexi047.txt) | MEXI047 | Taxodium mucronatum Ten. | 74 | 771–2008 | 61 | 105 | 18.9 | 0.468 |  |
| [aus118](txt/australia/aus118.txt) | AUS118 | Lagarostrobos franklinii (Hook. f.) Quinn | 174 | -571–1992 | 183 | 351 | 18.9 | 0.460 |  |
| [cze073](txt/europe/cze073.txt) | CZE073 | Picea abies (L.) H. Karst. | 26 | 1947–2021 | 3 | 7 | 18.9 | 0.562 | Segment length reduced from 50 to 36 years: the record is only 75 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [mex132](txt/northamerica/mexico/mex132.txt) | MEX132 | Pinus hartwegii Lindl. | 60 | 1478–2013 | 37 | 32 | 18.9 | 0.463 |  |
| [ar070](txt/northamerica/usa/ar070.txt) | AR070 | Juniperus virginiana L. | 24 | 1480–1991 | 7 | 12 | 18.8 | 0.497 |  |
| [brit8](txt/europe/brit8.txt) | BRIT8 | Quercus spp. L. | 9 | 1359–1591 | 1 | 2 | 18.8 | 0.536 |  |
| [gree004](txt/europe/gree004.txt) | GREE004 | Abies cephalonica Loudon | 20 | 1832–1981 | 0 | 6 | 18.8 | 0.419 |  |
| [indi020](txt/asia/indi020.txt) | INDI020 | Pinus roxburghii Sarg. = Pinus longifolia Roxb. | 25 | 1796–1990 | 9 | 6 | 18.8 | 0.539 |  |
| [lith039](txt/europe/lith039.txt) | LITH039 | Quercus robur L. = Quercus pendunculata Ehrl. | 16 | 1827–1971 | 2 | 1 | 18.8 | 0.496 |  |
| [nc3](txt/northamerica/usa/nc3.txt) | NC3 | Pinus echinata Mill. | 15 | 1648–1847 | 1 | 2 | 18.8 | 0.430 |  |
| [or129](txt/northamerica/usa/or129.txt) | OR129 | Thuja plicata Donn ex D. Don | 47 | 1924–2020 | 0 | 3 | 18.8 | 0.451 | Segment length reduced from 50 to 48 years: the record is only 97 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [or130](txt/northamerica/usa/or130.txt) | OR130 | Thuja plicata Donn ex D. Don | 57 | 1907–2020 | 1 | 2 | 18.8 | 0.480 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [tn014](txt/northamerica/usa/tn014.txt) | TN014 | Castanea dentata (Marsh.) Borkh. | 22 | 1687–1918 | 5 | 10 | 18.8 | 0.516 |  |
| [mexi088](txt/northamerica/mexico/mexi088.txt) | MEXI088 | Taxodium mucronatum Ten. | 36 | 1770–2006 | 10 | 13 | 18.7 | 0.471 |  |
| [ak197](txt/northamerica/usa/ak197.txt) | AK197 | Chamaecyparis nootkatensis (D. Don) Spach | 71 | 1108–2021 | 53 | 102 | 18.7 | 0.450 |  |
| [deu370](txt/europe/deu370.txt) | DEU370 | Picea abies (L.) H. Karst. | 20 | 1783–2010 | 5 | 15 | 18.7 | 0.456 |  |
| [sd021](txt/northamerica/usa/sd021.txt) | SD021 | Quercus macrocarpa Michx. | 16 | 1609–2008 | 7 | 18 | 18.7 | 0.464 |  |
| [deu378](txt/europe/deu378.txt) | DEU378 | Picea abies (L.) H. Karst. | 20 | 1813–2010 | 9 | 2 | 18.6 | 0.442 |  |
| [wa130](txt/northamerica/usa/wa130.txt) | WA130 | Thuja plicata Donn ex D. Don | 8 | 1305–1680 | 2 | 9 | 18.6 | 0.390 |  |
| [me017](txt/northamerica/usa/me017.txt) | ME017 | Picea rubens Sarg. | 20 | 1665–1982 | 7 | 20 | 18.6 | 0.450 |  |
| [can696](txt/northamerica/canada/can696.txt) | CAN696 | Larix laricina (Du Roi) K. Koch | 61 | 1762–1992 | 9 | 23 | 18.6 | 0.553 |  |
| [mexi066](txt/northamerica/mexico/mexi066.txt) | MEXI066 | Pinus culminicola Andresen et Beaman | 25 | 1805–2007 | 6 | 2 | 18.6 | 0.453 |  |
| [arge143](txt/southamerica/arge143.txt) | ARGE143 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 88 | 1644–2011 | 25 | 63 | 18.6 | 0.458 |  |
| [ca718](txt/northamerica/usa/ca718.txt) | CA718 | Sequoiadendron giganteum (Lindl.) Buchholz | 52 | 413–1992 | 75 | 117 | 18.5 | 0.469 |  |
| [ca545](txt/northamerica/usa/ca545.txt) | CA545 | Pseudotsuga macrocarpa (Vasey) Mayr | 20 | 1772–1988 | 1 | 9 | 18.5 | 0.495 |  |
| [japa018](txt/asia/japa018.txt) | JAPA018 | Cryptomeria japonica (Thunb. ex L.f.) D. Don | 36 | 969–2005 | 11 | 24 | 18.5 | 0.493 |  |
| [per006](txt/southamerica/per006.txt) | PER006 | Hura crepitans L. | 28 | 1744–2018 | 4 | 11 | 18.5 | 0.420 |  |
| [slov002](txt/europe/slov002.txt) | SLOV002 | Abies alba Mill. | 16 | 1859–1994 | 1 | 4 | 18.5 | 0.524 |  |
| [swit109](txt/europe/swit109.txt) | SWIT109 | Pinus cembra L. | 9 | 1788–1974 | 2 | 3 | 18.5 | 0.509 |  |
| [cana269](txt/northamerica/canada/cana269.txt) | CANA269 | Thuja occidentalis L. | 63 | 1530–2005 | 3 | 31 | 18.5 | 0.439 |  |
| [ca741](txt/northamerica/usa/ca741.txt) | CA741 | Abies magnifica A. Murray | 36 | 1809–2022 | 2 | 7 | 18.4 | 0.526 |  |
| [wa060](txt/northamerica/usa/wa060.txt) | WA060 | Pinus ponderosa Douglas ex C. Lawson | 40 | 1646–1980 | 10 | 41 | 18.3 | 0.453 |  |
| [ca724](txt/northamerica/usa/ca724.txt) | CA724 | Sequoiadendron giganteum (Lindl.) Buchholz | 28 | 792–1992 | 34 | 39 | 18.3 | 0.471 |  |
| [brit057](txt/europe/brit057.txt) | BRIT057 | Quercus spp. L. | 35 | 1736–2006 | 7 | 21 | 18.3 | 0.480 |  |
| [cana175](txt/northamerica/canada/cana175.txt) | CANA175 | Chamaecyparis nootkatensis (D. Don) Spach | 81 | 1200–1999 | 135 | 171 | 18.3 | 0.451 |  |
| [indo007](txt/asia/indo007.txt) | INDO007 | Tectona grandis L. f. | 29 | 1689–2000 | 7 | 16 | 18.3 | 0.457 |  |
| [arg147](txt/southamerica/arg147.txt) | ARG147 | Cedrela angustifolia Sesse & Mocino ex DC.= Cedrela balansae C. DC. | 32 | 1855–2002 | 3 | 3 | 18.2 | 0.505 |  |
| [arge101](txt/southamerica/arge101.txt) | ARGE101 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 19 | 1861–1991 | 1 | 3 | 18.2 | 0.440 |  |
| [ma005](txt/northamerica/usa/ma005.txt) | MA005 | Quercus spp. L. | 6 | 1454–1683 | 0 | 4 | 18.2 | 0.603 |  |
| [nc015](txt/northamerica/usa/nc015.txt) | NC015 | Castanea dentata (Marsh.) Borkh. | 18 | 1780–1939 | 2 | 2 | 18.2 | 0.455 |  |
| [or084](txt/northamerica/usa/or084.txt) | OR084 | Abies lasiocarpa (Hook.) Nutt. | 30 | 1865–2002 | 2 | 2 | 18.2 | 0.495 |  |
| [swit237](txt/europe/swit237.txt) | SWIT237 | Fagus sylvatica L. | 24 | 1924–1987 | 1 | 3 | 18.2 | 0.528 | Segment length reduced from 50 to 32 years: the record is only 64 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit297](txt/europe/swit297.txt) | SWIT297 | Abies alba Mill. | 42 | 1825–1989 | 2 | 8 | 18.2 | 0.376 |  |
| [wv030](txt/northamerica/usa/wv030.txt) | WV030 | Quercus alba L. | 12 | 1725–1844 | 0 | 2 | 18.2 | 0.406 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit240](txt/europe/swit240.txt) | SWIT240 | Pinus sylvestris L. | 95 | 53–1434 | 47 | 82 | 18.2 | 0.444 |  |
| [newz039](txt/australia/newz039.txt) | NEWZ039 | Libocedrus bidwillii Hook. f. | 25 | 1460–1978 | 8 | 35 | 18.1 | 0.457 |  |
| [cana223](txt/northamerica/canada/cana223.txt) | CANA223 | Picea engelmannii Parry ex Engelm. | 32 | 1798–1997 | 4 | 3 | 17.9 | 0.470 |  |
| [czec3](txt/europe/czec3.txt) | CZEC3 | Picea abies (L.) H. Karst. | 14 | 1725–1943 | 2 | 5 | 17.9 | 0.462 |  |
| [fl003](txt/northamerica/usa/fl003.txt) | FL003 | Quercus stellata Wangenh. | 24 | 1852–1993 | 3 | 4 | 17.9 | 0.466 |  |
| [bol001](txt/southamerica/bol001.txt) | BOL001 | Centrolobium microchaete (Benth.) H.C. Lima | 32 | 1836–2005 | 6 | 6 | 17.9 | 0.468 |  |
| [che508](txt/europe/che508.txt) | CHE508 | Picea abies (L.) H. Karst. | 15 | 1884–2011 | 1 | 4 | 17.9 | 0.511 |  |
| [cypr004](txt/europe/cypr004.txt) | CYPR004 | Pinus nigra J.F. Arnold | 22 | 1742–1981 | 3 | 7 | 17.9 | 0.493 |  |
| [swit304](txt/europe/swit304.txt) | SWIT304 | Castanea sativa Mill. | 191 | 1914–1995 | 2 | 3 | 17.9 | 0.523 | Segment length reduced from 50 to 40 years: the record is only 82 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [germ223](txt/europe/germ223.txt) | GERM223 | Pseudotsuga menziesii (Mirb.) Franco | 281 | 1965–2015 | 28 | 36 | 17.8 | 0.618 | Segment length reduced from 50 to 24 years: the record is only 51 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [czec031](txt/europe/czec031.txt) | CZEC031 | Quercus robur L. = Quercus pendunculata Ehrl. | 27 | 1823–2015 | 3 | 10 | 17.8 | 0.543 |  |
| [or136](txt/northamerica/usa/or136.txt) | OR136 | Tsuga mertensiana (Bong.) Carrière | 22 | 1771–2019 | 7 | 14 | 17.8 | 0.435 |  |
| [ausl022](txt/australia/ausl022.txt) | AUSL022 | Lagarostrobos franklinii (Hook. f.) Quinn | 23 | 1542–1992 | 17 | 28 | 17.8 | 0.451 |  |
| [cana117](txt/northamerica/canada/cana117.txt) | CANA117 | Picea glauca (Moench) Voss | 75 | 1409–1991 | 36 | 60 | 17.8 | 0.437 |  |
| [indi004](txt/asia/indi004.txt) | INDI004 | Abies pindrow (Royle ex D. Don) Royle | 10 | 1643–1981 | 3 | 5 | 17.8 | 0.431 |  |
| [norw027](txt/europe/norw027.txt) | NORW027 | Pinus sylvestris L. | 98 | 1801–2013 | 4 | 12 | 17.8 | 0.459 |  |
| [finl009](txt/europe/finl009.txt) | FINL009 | Pinus sylvestris L. | 18 | 1375–1786 | 4 | 24 | 17.7 | 0.488 |  |
| [ita073bl75](txt/europe/ita073bl75.txt) | ITA073 | Larix decidua Mill. | 28 | 1502–2016 | 4 | 19 | 17.7 | 0.453 |  |
| [czec005](txt/europe/czec005.txt) | CZEC005 | Picea abies (L.) H. Karst. | 100 | 1569–2010 | 36 | 90 | 17.7 | 0.475 |  |
| [ma012](txt/northamerica/usa/ma012.txt) | MA012 | Quercus spp. L. | 22 | 1362–1711 | 2 | 4 | 17.6 | 0.580 |  |
| [mex119](txt/northamerica/mexico/mex119.txt) | MEX119 | Pinus oocarpa Schiede | 40 | 1880–2005 | 2 | 1 | 17.6 | 0.461 |  |
| [swit369](txt/europe/swit369.txt) | SWIT369 | Picea abies (L.) H. Karst. | 24 | 1860–2016 | 3 | 3 | 17.6 | 0.623 |  |
| [tha029](txt/asia/tha029.txt) | THA029 | Tectona grandis L. f. | 25 | 1867–1996 | 3 | 3 | 17.6 | 0.504 |  |
| [kyrg002](txt/asia/kyrg002.txt) | KYRG002 | Juniperus spp. L. | 33 | 1346–1995 | 32 | 24 | 17.6 | 0.441 |  |
| [or079](txt/northamerica/usa/or079.txt) | OR079 | Tsuga mertensiana (Bong.) Carrière | 13 | 1577–1992 | 2 | 17 | 17.6 | 0.441 |  |
| [nepa032](txt/asia/nepa032.txt) | NEPA032 | Abies spectabilis (D. Don) Spach | 26 | 1546–1996 | 14 | 9 | 17.6 | 0.451 |  |
| [vt003](txt/northamerica/usa/vt003.txt) | VT003 | Acer saccharum Marsh. | 182 | 1808–2005 | 22 | 24 | 17.6 | 0.419 |  |
| [swit269](txt/europe/swit269.txt) | SWIT269 | Pinus sylvestris L. | 19 | 1816–2009 | 5 | 5 | 17.5 | 0.535 |  |
| [swit386](txt/europe/swit386.txt) | SWIT386 | Pinus sylvestris L. | 19 | 1816–2009 | 5 | 5 | 17.5 | 0.535 |  |
| [brit6](txt/europe/brit6.txt) | BRIT6 | Quercus spp. L. | 28 | 1649–1972 | 14 | 7 | 17.5 | 0.414 |  |
| [nc4](txt/northamerica/usa/nc4.txt) | NC4 | Tsuga canadensis (L.) Carr. | 32 | 1660–1992 | 13 | 26 | 17.5 | 0.488 |  |
| [mexi092](txt/northamerica/mexico/mexi092.txt) | MEXI092 | Taxodium mucronatum Ten. | 90 | 1850–2008 | 13 | 18 | 17.4 | 0.446 |  |
| [co683](txt/northamerica/usa/co683.txt) | CO683 | Populus tremuloides Michx. | 30 | 1890–2016 | 3 | 1 | 17.4 | 0.521 |  |
| [per001](txt/southamerica/per001.txt) | PER001 | Cedrela nebulosa T.D.Penn & Daza | 22 | 1883–2015 | 2 | 2 | 17.4 | 0.403 |  |
| [ca589](txt/northamerica/usa/ca589.txt) | CA589 | Abies magnifica A. Murray | 38 | 1880–1990 | 4 | 9 | 17.3 | 0.479 |  |
| [ny051](txt/northamerica/usa/ny051.txt) | NY051 | Pinus palustris Mill. | 16 | 1512–1891 | 5 | 8 | 17.3 | 0.406 |  |
| [wa131](txt/northamerica/usa/wa131.txt) | WA131 | Thuja plicata Donn ex D. Don | 6 | 1291–1691 | 6 | 3 | 17.3 | 0.488 |  |
| [ca717](txt/northamerica/usa/ca717.txt) | CA717 | Sequoiadendron giganteum (Lindl.) Buchholz | 77 | -1095–2012 | 88 | 135 | 17.3 | 0.497 |  |
| [ar073](txt/northamerica/usa/ar073.txt) | AR073 | Juniperus virginiana L. | 42 | 1729–1991 | 2 | 23 | 17.2 | 0.495 |  |
| [indi001](txt/asia/indi001.txt) | INDI001 | Abies pindrow (Royle ex D. Don) Royle | 10 | 1777–1981 | 1 | 4 | 17.2 | 0.497 |  |
| [bt015](txt/asia/bt015.txt) | BT015 | Pinus roxburghii Sarg. = Pinus longifolia Roxb. | 30 | 1777–2003 | 11 | 10 | 17.2 | 0.532 |  |
| [czec003](txt/europe/czec003.txt) | CZEC003 | Abies alba Mill. | 28 | 1587–2010 | 4 | 36 | 17.2 | 0.466 |  |
| [nepa058](txt/asia/nepa058.txt) | NEPA058 | Pinus wallichiana A.B. Jacks. | 50 | 1913–2016 | 4 | 2 | 17.1 | 0.434 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ca709](txt/northamerica/usa/ca709.txt) | CA709 | Sequoiadendron giganteum (Lindl.) Buchholz | 66 | 754–1991 | 42 | 67 | 17.1 | 0.490 |  |
| [che545](txt/europe/che545.txt) | CHE545 | Larix decidua Mill. | 15 | 1876–2011 | 4 | 3 | 17.1 | 0.519 |  |
| [deu385](txt/europe/deu385.txt) | DEU385 | Fagus sylvatica L. | 20 | 1896–2008 | 1 | 6 | 17.1 | 0.508 |  |
| [cana075](txt/northamerica/canada/cana075.txt) | CANA075 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 24 | 1797–1988 | 1 | 8 | 17.0 | 0.446 |  |
| [che467](txt/europe/che467.txt) | CHE467 | Abies alba Mill. | 9 | 1778–2023 | 5 | 4 | 17.0 | 0.459 |  |
| [brit041](txt/europe/brit041.txt) | BRIT041 | Quercus spp. L. | 13 | 1764–1992 | 5 | 5 | 16.9 | 0.470 |  |
| [cana279](txt/northamerica/canada/cana279.txt) | CANA279 | Picea glauca (Moench) Voss | 39 | 1729–2004 | 10 | 10 | 16.9 | 0.489 |  |
| [mexi065](txt/northamerica/mexico/mexi065.txt) | MEXI065 | Pseudotsuga menziesii (Mirb.) Franco | 41 | 1680–2007 | 6 | 24 | 16.9 | 0.501 |  |
| [nepa057](txt/asia/nepa057.txt) | NEPA057 | Pinus roxburghii Sarg. = Pinus longifolia Roxb. | 58 | 1925–2015 | 8 | 2 | 16.9 | 0.443 | Segment length reduced from 50 to 44 years: the record is only 91 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [indi006](txt/asia/indi006.txt) | INDI006 | Abies pindrow (Royle ex D. Don) Royle | 16 | 1654–1981 | 3 | 8 | 16.9 | 0.415 |  |
| [turk026](txt/europe/turk026.txt) | TURK026 | Juniperus spp. L. | 11 | 1728–2003 | 0 | 11 | 16.9 | 0.409 |  |
| [ak050](txt/northamerica/usa/ak050.txt) | AK050 | Picea glauca (Moench) Voss | 154 | 1731–2000 | 19 | 61 | 16.9 | 0.492 |  |
| [cana277](txt/northamerica/canada/cana277.txt) | CANA277 | Picea glauca (Moench) Voss | 60 | 1771–2004 | 6 | 9 | 16.9 | 0.427 |  |
| [indi013](txt/asia/indi013.txt) | INDI013 | Cedrus deodara (D. Don) G. Don | 42 | 1676–1988 | 13 | 17 | 16.9 | 0.469 |  |
| [mex155](txt/northamerica/mexico/mex155.txt) | MEX155 | Pinus greggii Engelm. ex Parl. | 227 | 1803–2019 | 5 | 10 | 16.9 | 0.660 |  |
| [co083](txt/northamerica/usa/co083.txt) | CO083 | Picea engelmannii Parry ex Engelm. | 50 | 1539–1979 | 22 | 26 | 16.8 | 0.502 |  |
| [swit348](txt/europe/swit348.txt) | SWIT348 | Pinus sylvestris L. | 55 | 1622–2002 | 25 | 41 | 16.8 | 0.452 |  |
| [ak052](txt/northamerica/usa/ak052.txt) | AK052 | Picea glauca (Moench) Voss | 186 | 1730–2002 | 34 | 68 | 16.8 | 0.497 |  |
| [cana546](txt/northamerica/canada/cana546.txt) | CANA546 | Picea glauca (Moench) Voss | 40 | 1615–2007 | 13 | 9 | 16.8 | 0.489 |  |
| [ca726](txt/northamerica/usa/ca726.txt) | CA726 | Sequoiadendron giganteum (Lindl.) Buchholz | 258 | -1372–2012 | 314 | 498 | 16.8 | 0.490 |  |
| [mex139](txt/northamerica/mexico/mex139.txt) | MEX139 | Pinus pinceana Gordon & Glend. | 125 | 0–2019 | 8 | 21 | 16.8 | 0.561 |  |
| [gtm002](txt/centralamerica/gtm002.txt) | GTM002 | Taxodium mucronatum Ten. | 37 | 1515–2009 | 16 | 22 | 16.7 | 0.448 |  |
| [mexi112](txt/northamerica/mexico/mexi112.txt) | MEXI112 | Taxodium mucronatum Ten. | 37 | 1515–2009 | 16 | 22 | 16.7 | 0.448 |  |
| [rus350](txt/asia/rus350.txt) | RUS350 |  | 103 | 1149–1814 | 12 | 29 | 16.7 | 0.473 |  |
| [ar003](txt/northamerica/usa/ar003.txt) | AR003 | Juniperus virginiana L. | 15 | 1710–1859 | 0 | 1 | 16.7 | 0.516 |  |
| [ar085](txt/northamerica/usa/ar085.txt) | AR085 | Pinus echinata Mill. | 66 | 1795–2022 | 0 | 1 | 16.7 | 0.641 |  |
| [bol021](txt/southamerica/bol021.txt) | BOL021 | Zeyheria tuberculosa (Vell.) Bureau ex Verl. | 20 | 1872–2010 | 2 | 6 | 16.7 | 0.466 |  |
| [cana670](txt/northamerica/canada/cana670.txt) | CANA670 | Acer saccharum Marsh. | 15 | 1933–2018 | 0 | 4 | 16.7 | 0.499 | Segment length reduced from 50 to 42 years: the record is only 86 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [che491](txt/europe/che491.txt) | CHE491 | Picea abies (L.) H. Karst. | 12 | 1910–2011 | 1 | 1 | 16.7 | 0.491 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [cze038](txt/europe/cze038.txt) | CZE038 | Pinus sylvestris L. | 26 | 1836–2020 | 0 | 3 | 16.7 | 0.537 |  |
| [deu374](txt/europe/deu374.txt) | DEU374 | Picea abies (L.) H. Karst. | 20 | 1805–2007 | 6 | 4 | 16.7 | 0.471 |  |
| [deu425](txt/europe/deu425.txt) | DEU425 | Pinus sylvestris L. | 60 | 1976–2023 | 2 | 3 | 16.7 | 0.650 | Segment length reduced from 50 to 24 years: the record is only 48 years long. |
| [deu475](txt/europe/deu475.txt) | DEU475 | Fagus sylvatica L. | 14 | 1898–2019 | 0 | 1 | 16.7 | 0.579 |  |
| [ital013](txt/europe/ital013.txt) | ITAL013 | Pinus nigra J.F. Arnold | 20 | 1773–1980 | 1 | 1 | 16.7 | 0.477 |  |
| [ital027](txt/europe/ital027.txt) | ITAL027 | Abies alba Mill. | 5 | 1729–1963 | 3 | 0 | 16.7 | 0.560 |  |
| [ks012](txt/northamerica/usa/ks012.txt) | KS012 | Quercus macrocarpa Michx. | 15 | 1820–2006 | 2 | 3 | 16.7 | 0.533 |  |
| [mex118](txt/northamerica/mexico/mex118.txt) | MEX118 | Pinus oocarpa Schiede | 30 | 1925–2015 | 0 | 1 | 16.7 | 0.396 | Segment length reduced from 50 to 44 years: the record is only 91 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [mi014](txt/northamerica/usa/mi014.txt) | MI014 | Tsuga canadensis (L.) Carr. | 5 | 1898–2002 | 1 | 0 | 16.7 | 0.434 |  |
| [mn008](txt/northamerica/usa/mn008.txt) | MN008 | Pinus resinosa Aiton | 16 | 1727–1971 | 2 | 9 | 16.7 | 0.464 |  |
| [swit198](txt/europe/swit198.txt) | SWIT198 | Pinus sylvestris L. | 20 | 1–286 | 1 | 3 | 16.7 | 0.625 |  |
| [wv026](txt/northamerica/usa/wv026.txt) | WV026 | Quercus alba L. | 18 | 1714–1840 | 3 | 1 | 16.7 | 0.543 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [cana528](txt/northamerica/canada/cana528.txt) | CANA528 | Larix lyallii Parl. | 68 | 1616–2012 | 13 | 77 | 16.6 | 0.550 |  |
| [brit024](txt/europe/brit024.txt) | BRIT024 | Pinus sylvestris L. | 25 | 1735–1976 | 6 | 13 | 16.5 | 0.423 |  |
| [pola005](txt/europe/pola005.txt) | POLA005 | Quercus robur L. = Quercus pendunculata Ehrl. | 45 | 1762–1985 | 13 | 23 | 16.4 | 0.497 |  |
| [wa080](txt/northamerica/usa/wa080.txt) | WA080 | Abies lasiocarpa (Hook.) Nutt. | 20 | 1756–1992 | 3 | 8 | 16.4 | 0.529 |  |
| [arge061](txt/southamerica/arge061.txt) | ARGE061 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 33 | 1657–1984 | 17 | 15 | 16.4 | 0.427 |  |
| [czec020](txt/europe/czec020.txt) | CZEC020 | Pinus sylvestris L. | 43 | 1804–2019 | 4 | 17 | 16.4 | 0.470 |  |
| [ak119](txt/northamerica/usa/ak119.txt) | AK119 | Chamaecyparis nootkatensis (D. Don) Spach | 54 | 1324–2009 | 36 | 53 | 16.4 | 0.455 |  |
| [bt010](txt/asia/bt010.txt) | BT010 | Picea spinulosa (Griff.) Beissn. | 17 | 1484–2005 | 12 | 23 | 16.4 | 0.453 |  |
| [grc035](txt/europe/grc035.txt) | GRC035 | Pinus heldreichii Christ | 6 | 1133–2019 | 1 | 15 | 16.3 | 0.485 |  |
| [ak081](txt/northamerica/usa/ak081.txt) | AK081 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 91 | 1848–2004 | 4 | 19 | 16.3 | 0.446 |  |
| [swed007](txt/europe/swed007.txt) | SWED007 | Pinus sylvestris L. | 40 | 1553–1974 | 6 | 78 | 16.3 | 0.540 |  |
| [swed009](txt/europe/swed009.txt) | SWED009 | Pinus sylvestris L. | 24 | 1614–1971 | 1 | 21 | 16.3 | 0.533 |  |
| [cze069](txt/europe/cze069.txt) | CZE069 | Picea abies (L.) H. Karst. | 26 | 1827–2021 | 2 | 5 | 16.3 | 0.467 |  |
| [germ036](txt/europe/germ036.txt) | GERM036 | Picea abies (L.) H. Karst. | 28 | 1806–1996 | 3 | 4 | 16.3 | 0.462 |  |
| [germ228](txt/europe/germ228.txt) | GERM228 | Abies alba Mill. | 35 | 1893–2009 | 1 | 6 | 16.3 | 0.502 |  |
| [ita071](txt/europe/ita071.txt) | ITA071 | Pinus lambertiana Douglas | 53 | 1830–2014 | 9 | 11 | 16.3 | 0.483 |  |
| [il006](txt/northamerica/usa/il006.txt) | IL006 | Pinus echinata Mill. | 20 | 1724–1965 | 3 | 10 | 16.2 | 0.532 |  |
| [germ6](txt/europe/germ6.txt) | GERM6 | Quercus robur L. = Quercus pendunculata Ehrl. | 138 | 1376–1972 | 11 | 33 | 16.2 | 0.462 |  |
| [paki014](txt/asia/paki014.txt) | PAKI014 | Juniperus spp. L. | 14 | 1412–1993 | 9 | 15 | 16.2 | 0.498 |  |
| [aus119](txt/australia/aus119.txt) | AUS119 | Lagarostrobos franklinii (Hook. f.) Quinn | 71 | 1107–2011 | 67 | 126 | 16.2 | 0.483 |  |
| [cana613](txt/northamerica/canada/cana613.txt) | CANA613 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 225 | 1884–2014 | 23 | 37 | 16.2 | 0.462 |  |
| [mn033](txt/northamerica/usa/mn033.txt) | MN033 | Pinus resinosa Aiton | 29 | 1766–2020 | 6 | 15 | 16.2 | 0.479 |  |
| [ak080](txt/northamerica/usa/ak080.txt) | AK080 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 76 | 1861–2003 | 4 | 22 | 16.1 | 0.432 |  |
| [arge014](txt/southamerica/arge014.txt) | ARGE014 | Araucaria araucana (Molina) K. Koch | 4 | 1700–1974 | 0 | 5 | 16.1 | 0.403 |  |
| [che483](txt/europe/che483.txt) | CHE483 | Picea abies (L.) H. Karst. | 12 | 1802–2023 | 2 | 3 | 16.1 | 0.539 |  |
| [ital026](txt/europe/ital026.txt) | ITAL026 | Taxus baccata L. | 17 | 1896–2005 | 3 | 2 | 16.1 | 0.416 |  |
| [or083](txt/northamerica/usa/or083.txt) | OR083 | Pinus contorta Douglas ex Loudon | 43 | 1892–2002 | 1 | 4 | 16.1 | 0.562 |  |
| [bol012](txt/southamerica/bol012.txt) | BOL012 | Centrolobium microchaete (Benth.) H.C. Lima | 38 | 1836–2005 | 5 | 8 | 16.0 | 0.481 |  |
| [ak151](txt/northamerica/usa/ak151.txt) | AK151 | Salix pulchra Cham. | 25 | 1972–2015 | 3 | 1 | 16.0 | 0.581 | Segment length reduced from 50 to 22 years: the record is only 44 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [in034](txt/northamerica/usa/in034.txt) | IN034 | Acer saccharum Marsh. | 24 | 1937–2016 | 2 | 2 | 16.0 | 0.521 | Segment length reduced from 50 to 40 years: the record is only 80 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [arge087](txt/southamerica/arge087.txt) | ARGE087 | Fitzroya cupressoides (Molina) I.M. Johnst. | 17 | 888–1991 | 22 | 33 | 15.9 | 0.491 |  |
| [ak116](txt/northamerica/usa/ak116.txt) | AK116 | Picea glauca (Moench) Voss | 159 | 1471–1999 | 63 | 116 | 15.9 | 0.482 |  |
| [ga013](txt/northamerica/usa/ga013.txt) | GA013 | Quercus spp. L. | 26 | 1646–2009 | 15 | 17 | 15.9 | 0.455 |  |
| [cana114](txt/northamerica/canada/cana114.txt) | CANA114 | Picea engelmannii Parry ex Engelm. | 24 | 1804–1983 | 2 | 5 | 15.9 | 0.561 |  |
| [russ286](txt/asia/russ286.txt) | RUSS286 | Pinus spp. L. | 28 | 1405–1713 | 11 | 13 | 15.9 | 0.461 |  |
| [czec012](txt/europe/czec012.txt) | CZEC012 | Picea abies (L.) H. Karst. | 36 | 1747–2017 | 7 | 13 | 15.9 | 0.462 |  |
| [me015](txt/northamerica/usa/me015.txt) | ME015 | Picea rubens Sarg. | 28 | 1728–1976 | 6 | 7 | 15.9 | 0.426 |  |
| [bt011](txt/asia/bt011.txt) | BT011 | Picea spinulosa (Griff.) Beissn. | 13 | 1456–2005 | 4 | 12 | 15.8 | 0.422 |  |
| [paki026](txt/asia/paki026.txt) | PAKI026 | Pinus gerardiana Wall. ex D. Don. | 25 | 1738–2007 | 0 | 22 | 15.8 | 0.638 |  |
| [ak193](txt/northamerica/usa/ak193.txt) | AK193 | Chamaecyparis nootkatensis (D. Don) Spach | 58 | 1611–2017 | 29 | 40 | 15.8 | 0.457 |  |
| [ak148](txt/northamerica/usa/ak148.txt) | AK148 | Picea glauca (Moench) Voss | 25 | 1855–2012 | 1 | 5 | 15.8 | 0.515 |  |
| [cana2](txt/northamerica/canada/cana2.txt) | CANA2 | Pinus contorta Douglas ex Loudon | 21 | 1750–1969 | 1 | 2 | 15.8 | 0.502 |  |
| [chil023](txt/southamerica/chil023.txt) | CHIL023 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 24 | 1770–1995 | 4 | 2 | 15.8 | 0.422 |  |
| [germ11](txt/europe/germ11.txt) | GERM11 | Picea abies (L.) H. Karst. | 8 | 1573–1961 | 2 | 7 | 15.8 | 0.391 |  |
| [germ236](txt/europe/germ236.txt) | GERM236 | Picea abies (L.) H. Karst. | 42 | 1951–2009 | 8 | 1 | 15.8 | 0.624 | Segment length reduced from 50 to 28 years: the record is only 59 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [lith006](txt/europe/lith006.txt) | LITH006 | Picea abies (L.) H. Karst. | 28 | 1876–1998 | 3 | 3 | 15.8 | 0.508 |  |
| [mexi072](txt/northamerica/mexico/mexi072.txt) | MEXI072 | Taxodium mucronatum Ten. | 28 | 1834–2010 | 2 | 4 | 15.8 | 0.443 |  |
| [swit134](txt/europe/swit134.txt) | SWIT134 | Pinus sylvestris L. | 32 | 1843–1979 | 1 | 5 | 15.8 | 0.559 |  |
| [arge028](txt/southamerica/arge028.txt) | ARGE028 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 25 | 1734–1974 | 6 | 5 | 15.7 | 0.484 |  |
| [wa039](txt/northamerica/usa/wa039.txt) | WA039 | Tsuga mertensiana (Bong.) Carrière | 28 | 1331–1976 | 24 | 38 | 15.7 | 0.527 |  |
| [bt012](txt/asia/bt012.txt) | BT012 | Pinus wallichiana A.B. Jacks. | 37 | 1625–2003 | 14 | 22 | 15.7 | 0.488 |  |
| [mi022](txt/northamerica/usa/mi022.txt) | MI022 | Pinus resinosa Aiton | 41 | 1446–2009 | 12 | 18 | 15.6 | 0.468 |  |
| [wa037](txt/northamerica/usa/wa037.txt) | WA037 | Pinus ponderosa Douglas ex C. Lawson | 20 | 1648–1975 | 1 | 19 | 15.6 | 0.588 |  |
| [arge144](txt/southamerica/arge144.txt) | ARGE144 | Fitzroya cupressoides (Molina) I.M. Johnst. | 179 | 836–2011 | 148 | 259 | 15.6 | 0.482 |  |
| [turk008](txt/europe/turk008.txt) | TURK008 |  | 18 | 1623–2004 | 6 | 11 | 15.6 | 0.477 |  |
| [wi048](txt/northamerica/usa/wi048.txt) | WI048 | Quercus alba L. | 21 | 1763–2014 | 7 | 5 | 15.6 | 0.440 |  |
| [isl001](txt/europe/isl001.txt) | ISL001 | Picea abies (L.) H. Karst. | 24 | 1970–2017 | 5 | 2 | 15.6 | 0.685 | Segment length reduced from 50 to 24 years: the record is only 48 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [mexi091](txt/northamerica/mexico/mexi091.txt) | MEXI091 | Taxodium mucronatum Ten. | 56 | 1810–2008 | 11 | 3 | 15.6 | 0.446 |  |
| [kyrg007](txt/asia/kyrg007.txt) | KYRG007 | Juniperus spp. L. | 47 | 1157–1995 | 28 | 62 | 15.5 | 0.457 |  |
| [swit340](txt/europe/swit340.txt) | SWIT340 | Picea abies (L.) H. Karst. | 42 | 1711–1997 | 5 | 11 | 15.5 | 0.449 |  |
| [ausl024](txt/australia/ausl024.txt) | AUSL024 | Lagarostrobos franklinii (Hook. f.) Quinn | 305 | -2145–1990 | 288 | 635 | 15.5 | 0.499 |  |
| [sc003](txt/northamerica/usa/sc003.txt) | SC003 | Pinus echinata Mill. | 33 | 1684–1973 | 4 | 7 | 15.5 | 0.482 |  |
| [az628](txt/northamerica/usa/az628.txt) | AZ628 | Pinus ponderosa Douglas ex C. Lawson | 60 | 1845–2022 | 0 | 15 | 15.5 | 0.677 |  |
| [svk027](txt/europe/svk027.txt) | SVK027 | Picea abies (L.) H. Karst. | 54 | 1793–2020 | 4 | 15 | 15.4 | 0.471 |  |
| [arg151](txt/southamerica/arg151.txt) | ARG151 | Juglans australis Griseb. | 20 | 1709–1999 | 5 | 3 | 15.4 | 0.519 |  |
| [germ301](txt/europe/germ301.txt) | GERM301 | Fagus sylvatica L. | 21 | 1654–1952 | 5 | 15 | 15.4 | 0.490 |  |
| [in042](txt/northamerica/usa/in042.txt) | IN042 | Acer saccharum Marsh. | 19 | 1884–2013 | 1 | 5 | 15.4 | 0.518 |  |
| [kore003](txt/asia/kore003.txt) | KORE003 | Pinus densiflora Siebold & Zucc. | 27 | 1961–2018 | 2 | 4 | 15.4 | 0.559 | Segment length reduced from 50 to 28 years: the record is only 58 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [lith005](txt/europe/lith005.txt) | LITH005 | Picea abies (L.) H. Karst. | 49 | 1877–1997 | 0 | 6 | 15.4 | 0.573 |  |
| [mo063](txt/northamerica/usa/mo063.txt) | MO063 | Quercus coccinea Muenchh. | 9 | 1937–2002 | 1 | 1 | 15.4 | 0.523 | Segment length reduced from 50 to 32 years: the record is only 66 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [paki032](txt/asia/paki032.txt) | PAKI032 | Abies pindrow (Royle ex D. Don) Royle | 13 | 1678–2005 | 4 | 10 | 15.4 | 0.534 |  |
| [per003](txt/southamerica/per003.txt) | PER003 | Juglans neotropica Diels | 54 | 1805–2009 | 8 | 12 | 15.4 | 0.469 |  |
| [sd010](txt/northamerica/usa/sd010.txt) | SD010 | Quercus macrocarpa Michx. | 12 | 1858–1990 | 1 | 1 | 15.4 | 0.610 |  |
| [swit186](txt/europe/swit186.txt) | SWIT186 | Picea abies (L.) H. Karst. | 12 | 1736–2007 | 4 | 6 | 15.4 | 0.452 |  |
| [turk024](txt/europe/turk024.txt) | TURK024 | Abies cilicica (Ant. et Kotschy) Carr. | 7 | 1797–2003 | 2 | 2 | 15.4 | 0.487 |  |
| [bt022](txt/asia/bt022.txt) | BT022 | Taxus baccata L. | 30 | 1630–2003 | 22 | 8 | 15.3 | 0.470 |  |
| [rus334](txt/asia/rus334.txt) | RUS334 | Pinus sylvestris L. | 34 | 1735–2013 | 14 | 12 | 15.3 | 0.477 |  |
| [aust113](txt/europe/aust113.txt) | AUST113 | Quercus spp. L. | 272 | 1748–2011 | 44 | 61 | 15.3 | 0.517 |  |
| [turk006](txt/europe/turk006.txt) | TURK006 | Juniperus spp. L. | 18 | 1360–1988 | 16 | 31 | 15.3 | 0.544 |  |
| [va012](txt/northamerica/usa/va012.txt) | VA012 | Tsuga canadensis (L.) Carr. | 26 | 1645–1982 | 14 | 19 | 15.2 | 0.462 |  |
| [kyrg009](txt/asia/kyrg009.txt) | KYRG009 | Juniperus turkestanica Komar. | 23 | 1019–1987 | 17 | 44 | 15.2 | 0.464 |  |
| [wa019](txt/northamerica/usa/wa019.txt) | WA019 | Pseudotsuga menziesii (Mirb.) Franco | 28 | 1548–1976 | 15 | 34 | 15.2 | 0.556 |  |
| [arg153](txt/southamerica/arg153.txt) | ARG153 | Juglans australis Griseb. | 19 | 1814–1994 | 0 | 5 | 15.2 | 0.519 |  |
| [arge091](txt/southamerica/arge091.txt) | ARGE091 | Fitzroya cupressoides (Molina) I.M. Johnst. | 31 | 320–1993 | 41 | 59 | 15.2 | 0.476 |  |
| [brit039](txt/europe/brit039.txt) | BRIT039 | Pinus sylvestris L. | 52 | -1689–-1488 | 0 | 5 | 15.2 | 0.633 |  |
| [cana6](txt/northamerica/canada/cana6.txt) | CANA6 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 17 | 1705–1986 | 3 | 7 | 15.2 | 0.532 |  |
| [che556](txt/europe/che556.txt) | CHE556 | Fagus sylvatica L. | 29 | 1787–2018 | 3 | 7 | 15.2 | 0.516 |  |
| [deu339](txt/europe/deu339.txt) | DEU339 | Fagus sylvatica L. | 38 | 1903–2021 | 1 | 4 | 15.2 | 0.552 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [czec019](txt/europe/czec019.txt) | CZEC019 | Picea abies (L.) H. Karst. | 42 | 1870–2019 | 3 | 13 | 15.1 | 0.529 |  |
| [finl087](txt/europe/finl087.txt) | FINL087 | Pinus sylvestris L. | 87 | 1710–2011 | 1 | 7 | 15.1 | 0.502 |  |
| [ak065](txt/northamerica/usa/ak065.txt) | AK065 | Picea glauca (Moench) Voss | 20 | 1400–2002 | 12 | 15 | 15.1 | 0.508 |  |
| [nm619](txt/northamerica/usa/nm619.txt) | NM619 | Pinus ponderosa Douglas ex C. Lawson | 42 | 1653–2012 | 7 | 12 | 15.1 | 0.603 |  |
| [deu396](txt/europe/deu396.txt) | DEU396 | Picea abies (L.) H. Karst. | 20 | 1845–2010 | 2 | 7 | 15.0 | 0.494 |  |
| [fran033w](txt/europe/fran033w.txt) | FRAN033 | Picea abies (L.) H. Karst. | 20 | 1931–1994 | 0 | 3 | 15.0 | 0.624 | Segment length reduced from 50 to 32 years: the record is only 64 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [nc1](txt/northamerica/usa/nc1.txt) | NC1 | Quercus alba L. | 32 | 1770–1992 | 3 | 12 | 15.0 | 0.501 |  |
| [tha027](txt/asia/tha027.txt) | THA027 | Tectona grandis L. f. | 31 | 1872–2009 | 3 | 3 | 15.0 | 0.462 |  |
| [wa162](txt/northamerica/usa/wa162.txt) | WA162 | Thuja plicata Donn ex D. Don | 29 | 1913–2020 | 1 | 2 | 15.0 | 0.436 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [mo077](txt/northamerica/usa/mo077.txt) | MO077 | Quercus spp. L. | 264 | 912–2008 | 66 | 79 | 15.0 | 0.477 |  |
| [mo051](txt/northamerica/usa/mo051.txt) | MO051 | Pinus echinata Mill. | 87 | 1806–2002 | 3 | 16 | 15.0 | 0.445 |  |
| [mn019](txt/northamerica/usa/mn019.txt) | MN019 | Pinus resinosa Aiton | 45 | 1672–1971 | 8 | 24 | 15.0 | 0.486 |  |
| [che466](txt/europe/che466.txt) | CHE466 | Abies alba Mill. | 9 | 1776–2023 | 1 | 6 | 14.9 | 0.432 |  |
| [md008](txt/northamerica/usa/md008.txt) | MD008 | Quercus stellata Wangenh. | 38 | 1804–1977 | 6 | 5 | 14.9 | 0.563 |  |
| [czec006](txt/europe/czec006.txt) | CZEC006 | Picea abies (L.) H. Karst. | 143 | 1799–2019 | 9 | 32 | 14.9 | 0.534 |  |
| [ital020](txt/europe/ital020.txt) | ITAL020 | Quercus robur L. = Quercus pendunculata Ehrl. | 24 | 1875–1989 | 1 | 3 | 14.8 | 0.450 |  |
| [ital035](txt/europe/ital035.txt) | ITAL035 | Pinus pinea L. | 95 | 1910–2002 | 7 | 5 | 14.8 | 0.503 | Segment length reduced from 50 to 46 years: the record is only 93 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [per007](txt/southamerica/per007.txt) | PER007 | Myroxylon balsamum (L.) Harms | 50 | 1927–2019 | 6 | 2 | 14.8 | 0.429 | Segment length reduced from 50 to 46 years: the record is only 93 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ca711](txt/northamerica/usa/ca711.txt) | CA711 | Sequoiadendron giganteum (Lindl.) Buchholz | 54 | 947–1991 | 26 | 35 | 14.8 | 0.546 |  |
| [russ245](txt/asia/russ245.txt) | RUSS245 | Larix sibirica Ledeb. = Larix russica (Endl.) Sabine ex Trautv. | 51 | 1423–2013 | 1 | 44 | 14.8 | 0.622 |  |
| [wi027](txt/northamerica/usa/wi027.txt) | WI027 | Quercus alba L. | 25 | 1788–2014 | 5 | 9 | 14.7 | 0.465 |  |
| [bol002](txt/southamerica/bol002.txt) | BOL002 | Centrolobium microchaete (Benth.) H.C. Lima | 40 | 1924–2008 | 3 | 2 | 14.7 | 0.568 | Segment length reduced from 50 to 42 years: the record is only 85 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ca727](txt/northamerica/usa/ca727.txt) | CA727 | Pinus lambertiana Douglas | 50 | 1591–2017 | 10 | 28 | 14.7 | 0.512 |  |
| [wa058](txt/northamerica/usa/wa058.txt) | WA058 | Tsuga mertensiana (Bong.) Carrière | 32 | 1417–1980 | 26 | 50 | 14.7 | 0.506 |  |
| [nc023](txt/northamerica/usa/nc023.txt) | NC023 | Quercus alba L. | 28 | 1599–2003 | 9 | 9 | 14.6 | 0.476 |  |
| [oh021](txt/northamerica/usa/oh021.txt) | OH021 | Metasequoia glyptostroboides Hu & W.C. Cheng | 32 | 1955–2010 | 1 | 5 | 14.6 | 0.609 | Segment length reduced from 50 to 28 years: the record is only 56 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [rus391](txt/asia/rus391.txt) | RUS391 | Pinus spp. L. | 16 | 1432–1617 | 2 | 5 | 14.6 | 0.450 |  |
| [swit228](txt/europe/swit228.txt) | SWIT228 | Pinus sylvestris L. | 24 | 1963–2000 | 4 | 3 | 14.6 | 0.708 | Segment length reduced from 50 to 18 years: the record is only 38 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [ar075](txt/northamerica/usa/ar075.txt) | AR075 | Taxodium distichum (L.) Rich. | 25 | 1538–1882 | 1 | 8 | 14.5 | 0.545 |  |
| [chin020](txt/asia/chin020.txt) | CHIN020 | Juniperus tibetica Kom. | 44 | 1306–2007 | 36 | 36 | 14.5 | 0.487 |  |
| [arge089](txt/southamerica/arge089.txt) | ARGE089 | Fitzroya cupressoides (Molina) I.M. Johnst. | 32 | 539–1993 | 37 | 60 | 14.5 | 0.484 |  |
| [wa062](txt/northamerica/usa/wa062.txt) | WA062 | Pseudotsuga menziesii (Mirb.) Franco | 32 | 1567–1980 | 14 | 31 | 14.5 | 0.524 |  |
| [cana602](txt/northamerica/canada/cana602.txt) | CANA602 | Picea glauca (Moench) Voss | 34 | 1676–2001 | 7 | 18 | 14.5 | 0.483 |  |
| [swit283](txt/europe/swit283.txt) | SWIT283 | Pinus mugo Turra = Pinus montana Mill. | 44 | 1681–2008 | 17 | 21 | 14.4 | 0.455 |  |
| [nepa034](txt/asia/nepa034.txt) | NEPA034 | Abies spectabilis (D. Don) Spach | 24 | 1672–1996 | 9 | 8 | 14.4 | 0.449 |  |
| [ca704](txt/northamerica/usa/ca704.txt) | CA704 | Sequoiadendron giganteum (Lindl.) Buchholz | 37 | 1171–1991 | 18 | 18 | 14.4 | 0.482 |  |
| [indi014](txt/asia/indi014.txt) | INDI014 | Cedrus deodara (D. Don) G. Don | 20 | 1685–1989 | 8 | 10 | 14.4 | 0.484 |  |
| [brit099](txt/europe/brit099.txt) | BRIT099 | Taxus baccata L. | 32 | 840–1252 | 9 | 11 | 14.4 | 0.453 |  |
| [bra027](txt/southamerica/bra027.txt) | BRA027 | Schinopsis brasiliensis Engl. | 50 | 1963–2015 | 2 | 1 | 14.3 | 0.596 | Segment length reduced from 50 to 26 years: the record is only 53 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [cana344](txt/northamerica/canada/cana344.txt) | CANA344 | Pinus banksiana Lamb. | 30 | 1860–2006 | 3 | 3 | 14.3 | 0.441 |  |
| [col001](txt/southamerica/col001.txt) | COL001 | Morisonia odoratissima (Jacq.) Christenh. & Byng = Capparis odoratissima Jacq. | 45 | 1955–2004 | 2 | 1 | 14.3 | 0.620 | Segment length reduced from 50 to 24 years: the record is only 50 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [cze048](txt/europe/cze048.txt) | CZE048 | Pinus sylvestris L. | 26 | 1856–2019 | 0 | 4 | 14.3 | 0.529 |  |
| [czec1](txt/europe/czec1.txt) | CZEC1 | Abies alba Mill. | 10 | 1849–1965 | 1 | 0 | 14.3 | 0.524 |  |
| [japa1](txt/asia/japa1.txt) | JAPA1 | Pinus densiflora Siebold & Zucc. | 7 | 1881–1987 | 0 | 2 | 14.3 | 0.560 |  |
| [ma011](txt/northamerica/usa/ma011.txt) | MA011 | Quercus spp. L. | 6 | 1495–1671 | 1 | 1 | 14.3 | 0.367 |  |
| [mex136](txt/northamerica/mexico/mex136.txt) | MEX136 | Picea martinezii T.F.Patt. | 59 | 1835–2020 | 0 | 7 | 14.3 | 0.614 |  |
| [mo079](txt/northamerica/usa/mo079.txt) | MO079 | Acer saccharum Marsh. | 15 | 1894–2015 | 2 | 2 | 14.3 | 0.474 |  |
| [nepa060](txt/asia/nepa060.txt) | NEPA060 | Pinus roxburghii Sarg. = Pinus longifolia Roxb. | 61 | 1910–2015 | 1 | 0 | 14.3 | 0.458 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [or055](txt/northamerica/usa/or055.txt) | OR055 | Pinus ponderosa Douglas ex C. Lawson | 28 | 1423–1995 | 16 | 23 | 14.3 | 0.544 |  |
| [russ296](txt/asia/russ296.txt) | RUSS296 | Pinus sylvestris L. | 8 | 1844–2004 | 0 | 3 | 14.3 | 0.733 |  |
| [spai075](txt/europe/spai075.txt) | SPAI075 | Abies pinsapo Boiss. | 34 | 1689–1999 | 9 | 14 | 14.3 | 0.539 |  |
| [swit310](txt/europe/swit310.txt) | SWIT310 | Larix decidua Mill. | 11 | 1821–2005 | 1 | 2 | 14.3 | 0.615 |  |
| [swit354](txt/europe/swit354.txt) | SWIT354 | Fagus sylvatica L. | 12 | 1899–1998 | 1 | 0 | 14.3 | 0.524 |  |
| [swit406](txt/europe/swit406.txt) | SWIT406 | Picea abies (L.) H. Karst. | 15 | 1771–2011 | 2 | 4 | 14.3 | 0.477 |  |
| [safr001](txt/africa/safr001.txt) | SAFR001 | Widdringtonia cedarbergensis J.A. Marsh | 55 | 1564–1976 | 13 | 29 | 14.2 | 0.523 |  |
| [ak214](txt/northamerica/usa/ak214.txt) | AK214 | Tsuga heterophylla (Raf.) Sarg. | 38 | 1336–2012 | 20 | 33 | 14.2 | 0.462 |  |
| [indo004](txt/asia/indo004.txt) | INDO004 | Tectona grandis L. f. | 59 | 1644–2005 | 21 | 27 | 14.2 | 0.479 |  |
| [fran042](txt/europe/fran042.txt) | FRAN042 | Pinus uncinata Mill. ex Mirb. | 83 | 1736–2004 | 11 | 12 | 14.2 | 0.506 |  |
| [arge030](txt/southamerica/arge030.txt) | ARGE030 | Fitzroya cupressoides (Molina) I.M. Johnst. | 42 | 441–1974 | 40 | 73 | 14.2 | 0.477 |  |
| [chin040](txt/asia/chin040.txt) | CHIN040 | Tsuga dumosa (D. Don) Eichler | 30 | 1393–2005 | 13 | 29 | 14.2 | 0.488 |  |
| [deu437](txt/europe/deu437.txt) | DEU437 | Picea abies (L.) H. Karst. | 94 | 1975–2016 | 6 | 13 | 14.2 | 0.633 | Segment length reduced from 50 to 20 years: the record is only 42 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [indi021](txt/asia/indi021.txt) | INDI021 | Cedrus deodara (D. Don) G. Don | 13 | 1657–1990 | 6 | 8 | 14.1 | 0.501 |  |
| [rus327](txt/asia/rus327.txt) | RUS327 | Pinus sylvestris L. | 21 | 1726–2009 | 8 | 6 | 14.1 | 0.438 |  |
| [ak017](txt/northamerica/usa/ak017.txt) | AK017 | Picea sitchensis (Bong.) Carrière | 17 | 1719–1988 | 3 | 10 | 14.1 | 0.469 |  |
| [swit181](txt/europe/swit181.txt) | SWIT181 | Picea abies (L.) H. Karst. | 18 | 1668–1999 | 8 | 5 | 14.1 | 0.479 |  |
| [gree012](txt/europe/gree012.txt) | GREE012 | Picea abies (L.) H. Karst. | 9 | 1635–1979 | 7 | 5 | 14.1 | 0.477 |  |
| [finl008](txt/europe/finl008.txt) | FINL008 | Pinus sylvestris L. | 27 | 1621–1985 | 6 | 20 | 14.1 | 0.460 |  |
| [mexi036](txt/northamerica/mexico/mexi036.txt) | MEXI036 | Taxodium mucronatum Ten. | 28 | 1574–1996 | 16 | 9 | 14.0 | 0.468 |  |
| [che557](txt/europe/che557.txt) | CHE557 | Fagus sylvatica L. | 30 | 1863–2018 | 1 | 7 | 14.0 | 0.491 |  |
| [cana200](txt/northamerica/canada/cana200.txt) | CANA200 | Pinus banksiana Lamb. | 30 | 1766–2002 | 5 | 9 | 14.0 | 0.468 |  |
| [tha015](txt/asia/tha015.txt) | THA015 | Tectona grandis L. f. | 34 | 1870–2009 | 3 | 4 | 14.0 | 0.426 |  |
| [wy033](txt/northamerica/usa/wy033.txt) | WY033 | Pinus albicaulis Engelm. | 26 | 1280–2005 | 14 | 14 | 14.0 | 0.509 |  |
| [nd015](txt/northamerica/usa/nd015.txt) | ND015 | Populus deltoides Bartr. ex Marsh. | 43 | 1826–2015 | 7 | 6 | 14.0 | 0.438 |  |
| [ak143](txt/northamerica/usa/ak143.txt) | AK143 | Tsuga heterophylla (Raf.) Sarg. | 9 | 1453–1996 | 4 | 13 | 13.9 | 0.468 |  |
| [bol017](txt/southamerica/bol017.txt) | BOL017 | Centrolobium microchaete (Benth.) H.C. Lima | 42 | 1900–2010 | 3 | 7 | 13.9 | 0.472 |  |
| [cze053](txt/europe/cze053.txt) | CZE053 | Pinus sylvestris L. | 26 | 1877–2019 | 3 | 2 | 13.9 | 0.555 |  |
| [nv515](txt/northamerica/usa/nv515.txt) | NV515 | Pinus longaeva D.K. Bailey = Pinus aristata var. longaeva | 112 | -2370–1980 | 67 | 375 | 13.9 | 0.540 |  |
| [aus116](txt/australia/aus116.txt) | AUS116 | Toona ciliata | 53 | 1591–2000 | 2 | 7 | 13.8 | 0.535 |  |
| [ita073bl100](txt/europe/ita073bl100.txt) | ITA073 | Larix decidua Mill. | 28 | 1502–2016 | 3 | 15 | 13.8 | 0.468 |  |
| [wa165](txt/northamerica/usa/wa165.txt) | WA165 | Thuja plicata Donn ex D. Don | 38 | 1792–2020 | 4 | 14 | 13.8 | 0.469 |  |
| [wa056](txt/northamerica/usa/wa056.txt) | WA056 | Tsuga mertensiana (Bong.) Carrière | 32 | 1259–1980 | 8 | 49 | 13.8 | 0.520 |  |
| [arge139](txt/southamerica/arge139.txt) | ARGE139 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 170 | 1539–2011 | 57 | 99 | 13.8 | 0.503 |  |
| [russ295](txt/asia/russ295.txt) | RUSS295 | Pinus sylvestris L. | 15 | 1844–2010 | 0 | 4 | 13.8 | 0.507 |  |
| [tur075](txt/europe/tur075.txt) | TUR075 | Pinus nigra J.F. Arnold | 48 | 1695–2010 | 14 | 18 | 13.8 | 0.523 |  |
| [nm015](txt/northamerica/usa/nm015.txt) | NM015 | Pinus aristata Engelm. | 15 | 1535–1968 | 6 | 13 | 13.8 | 0.505 |  |
| [wy052](txt/northamerica/usa/wy052.txt) | WY052 | Pseudotsuga menziesii (Mirb.) Franco | 12 | 1076–2007 | 9 | 10 | 13.8 | 0.547 |  |
| [nm020](txt/northamerica/usa/nm020.txt) | NM020 | Pseudotsuga menziesii (Mirb.) Franco | 16 | 1597–1970 | 1 | 14 | 13.8 | 0.635 |  |
| [nepa041](txt/asia/nepa041.txt) | NEPA041 | Tsuga dumosa (D. Don) Eichler | 36 | 1612–1994 | 10 | 8 | 13.7 | 0.466 |  |
| [or046](txt/northamerica/usa/or046.txt) | OR046 | Pinus ponderosa Douglas ex C. Lawson | 19 | 1529–1995 | 10 | 19 | 13.7 | 0.501 |  |
| [wi039](txt/northamerica/usa/wi039.txt) | WI039 | Quercus spp. L. | 30 | 1736–2013 | 4 | 12 | 13.7 | 0.525 |  |
| [mexi035](txt/northamerica/mexico/mexi035.txt) | MEXI035 | Taxodium mucronatum Ten. | 66 | 1474–1995 | 31 | 35 | 13.7 | 0.492 |  |
| [ca538](txt/northamerica/usa/ca538.txt) | CA538 | Pseudotsuga macrocarpa (Vasey) Mayr | 12 | 1784–1988 | 1 | 5 | 13.6 | 0.499 |  |
| [mn035](txt/northamerica/usa/mn035.txt) | MN035 | Larix laricina (Du Roi) K. Koch | 30 | 1918–2020 | 1 | 2 | 13.6 | 0.647 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [sd023](txt/northamerica/usa/sd023.txt) | SD023 | Quercus macrocarpa Michx. | 6 | 1770–2008 | 2 | 1 | 13.6 | 0.465 |  |
| [spai076](txt/europe/spai076.txt) | SPAI076 | Abies pinsapo Boiss. | 25 | 1886–1998 | 0 | 3 | 13.6 | 0.525 |  |
| [wa090](txt/northamerica/usa/wa090.txt) | WA090 | Pseudotsuga menziesii (Mirb.) Franco | 53 | 1326–1987 | 52 | 88 | 13.6 | 0.488 |  |
| [or059](txt/northamerica/usa/or059.txt) | OR059 | Pinus ponderosa Douglas ex C. Lawson | 20 | 1653–1995 | 2 | 17 | 13.6 | 0.500 |  |
| [eth005](txt/africa/eth005.txt) | ETH005 | Juniperus procera Hochst. ex Endl. | 22 | 1869–2004 | 1 | 4 | 13.5 | 0.404 |  |
| [ital019](txt/europe/ital019.txt) | ITAL019 | Quercus robur L. = Quercus pendunculata Ehrl. | 16 | 1779–1989 | 1 | 6 | 13.5 | 0.525 |  |
| [mar052](txt/africa/mar052.txt) | MAR052 | Cedrus atlantica (Endl.) Manetti ex Carrière | 21 | 1835–2009 | 3 | 4 | 13.5 | 0.532 |  |
| [russ241](txt/asia/russ241.txt) | RUSS241 | Larix sibirica Ledeb. = Larix russica (Endl.) Sabine ex Trautv. | 19 | 1523–2013 | 0 | 7 | 13.5 | 0.504 |  |
| [wa057](txt/northamerica/usa/wa057.txt) | WA057 | Pinus albicaulis Engelm. | 22 | 1430–1980 | 10 | 25 | 13.5 | 0.501 |  |
| [paki041](txt/asia/paki041.txt) | PAKI041 | Cedrus deodara (D. Don) G. Don | 93 | 1550–2017 | 35 | 69 | 13.4 | 0.558 |  |
| [ak185](txt/northamerica/usa/ak185.txt) | AK185 | Alnus spp. Mill. | 39 | 1979–2017 | 2 | 0 | 13.3 | 0.572 | Segment length reduced from 50 to 18 years: the record is only 39 years long. |
| [ar006](txt/northamerica/usa/ar006.txt) | AR006 | Pinus echinata Mill. | 10 | 1750–1881 | 1 | 1 | 13.3 | 0.567 |  |
| [deu424](txt/europe/deu424.txt) | DEU424 | Quercus robur L. = Quercus pendunculata Ehrl. | 60 | 1976–2023 | 2 | 2 | 13.3 | 0.595 | Segment length reduced from 50 to 24 years: the record is only 48 years long. |
| [in037](txt/northamerica/usa/in037.txt) | IN037 | Acer saccharum Marsh. | 20 | 1917–2016 | 1 | 1 | 13.3 | 0.462 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [or131](txt/northamerica/usa/or131.txt) | OR131 | Thuja plicata Donn ex D. Don | 53 | 1913–2020 | 0 | 2 | 13.3 | 0.444 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [tza001](txt/africa/tza001.txt) | TZA001 | Brachystegia spiciformis Benth. | 95 | 1908–2012 | 0 | 4 | 13.3 | 0.494 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ak218](txt/northamerica/usa/ak218.txt) | AK218 | Chamaecyparis nootkatensis (D. Don) Spach | 78 | 1286–2023 | 58 | 69 | 13.3 | 0.463 |  |
| [arge093](txt/southamerica/arge093.txt) | ARGE093 | Fitzroya cupressoides (Molina) I.M. Johnst. | 58 | 311–1992 | 56 | 98 | 13.3 | 0.485 |  |
| [ga010](txt/northamerica/usa/ga010.txt) | GA010 | Liriodendron tulipifera L. | 26 | 1552–2009 | 5 | 17 | 13.3 | 0.541 |  |
| [ak054](txt/northamerica/usa/ak054.txt) | AK054 | Picea glauca (Moench) Voss | 111 | 1735–2000 | 11 | 31 | 13.2 | 0.456 |  |
| [ak056](txt/northamerica/usa/ak056.txt) | AK056 | Picea glauca (Moench) Voss | 111 | 1735–2000 | 11 | 31 | 13.2 | 0.456 |  |
| [cze054](txt/europe/cze054.txt) | CZE054 | Picea abies (L.) H. Karst. | 26 | 1896–2019 | 1 | 6 | 13.2 | 0.515 |  |
| [czec010](txt/europe/czec010.txt) | CZEC010 | Picea abies (L.) H. Karst. | 28 | 1860–2018 | 1 | 6 | 13.2 | 0.571 |  |
| [mn040](txt/northamerica/usa/mn040.txt) | MN040 | Acer saccharinum L. | 51 | 1928–2018 | 1 | 6 | 13.2 | 0.593 | Segment length reduced from 50 to 44 years: the record is only 91 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [id001](txt/northamerica/usa/id001.txt) | ID001 | Pinus ponderosa Douglas ex C. Lawson | 26 | 1712–1976 | 1 | 18 | 13.2 | 0.540 |  |
| [nc7](txt/northamerica/usa/nc7.txt) | NC7 | Pinus palustris Mill. | 31 | 1893–1992 | 0 | 5 | 13.2 | 0.514 |  |
| [cana125](txt/northamerica/canada/cana125.txt) | CANA125 | Picea glauca (Moench) Voss | 93 | 1470–1992 | 20 | 44 | 13.1 | 0.495 |  |
| [arge017](txt/southamerica/arge017.txt) | ARGE017 | Araucaria araucana (Molina) K. Koch | 28 | 1689–1976 | 6 | 12 | 13.1 | 0.533 |  |
| [chn088](txt/asia/chn088.txt) | CHN088 | Pinus tabulaeformis Carr. | 32 | 1872–2018 | 1 | 7 | 13.1 | 0.428 |  |
| [germ14](txt/europe/germ14.txt) | GERM14 | Picea abies (L.) H. Karst. | 21 | 1622–1953 | 2 | 12 | 13.1 | 0.416 |  |
| [brit009](txt/europe/brit009.txt) | BRIT009 | Quercus petraea (Matt.) Liebl. = Quercus sessiliflora Salisb. | 12 | 1850–1978 | 3 | 0 | 13.0 | 0.527 |  |
| [cana204](txt/northamerica/canada/cana204.txt) | CANA204 | Pinus banksiana Lamb. | 24 | 1854–2002 | 4 | 5 | 13.0 | 0.470 |  |
| [finl068](txt/europe/finl068.txt) | FINL068 | Pinus sylvestris L. | 10 | 1602–1841 | 0 | 3 | 13.0 | 0.455 |  |
| [gtm003](txt/centralamerica/gtm003.txt) | GTM003 | Swietenia macrophylla King | 18 | 1857–2022 | 2 | 1 | 13.0 | 0.422 |  |
| [mex147](txt/northamerica/mexico/mex147.txt) | MEX147 | Pinus oocarpa Schiede | 157 | 1856–2018 | 6 | 3 | 13.0 | 0.400 |  |
| [neth013](txt/europe/neth013.txt) | NETH013 | Quercus spp. L. | 6 | -103–123 | 1 | 2 | 13.0 | 0.479 |  |
| [turk002](txt/europe/turk002.txt) | TURK002 | Abies nordmanniana (Steven) Spach | 7 | 1582–1988 | 0 | 6 | 13.0 | 0.600 |  |
| [cana489](txt/northamerica/canada/cana489.txt) | CANA489 | Abies lasiocarpa (Hook.) Nutt. | 21 | 1740–2003 | 4 | 9 | 13.0 | 0.521 |  |
| [ak160](txt/northamerica/usa/ak160.txt) | AK160 | Populus tremuloides Michx. | 111 | 1825–2015 | 1 | 19 | 13.0 | 0.587 |  |
| [bol005](txt/southamerica/bol005.txt) | BOL005 | Aspidosperma tomentosum Mart. | 24 | 1843–2015 | 3 | 4 | 13.0 | 0.526 |  |
| [tn017](txt/northamerica/usa/tn017.txt) | TN017 | Quercus prinus L. | 15 | 1654–1994 | 1 | 10 | 12.9 | 0.471 |  |
| [arge013](txt/southamerica/arge013.txt) | ARGE013 | Araucaria araucana (Molina) K. Koch | 42 | 1306–1974 | 13 | 32 | 12.9 | 0.507 |  |
| [kyrg011](txt/asia/kyrg011.txt) | KYRG011 | Juniperus turkestanica Komar. | 24 | 694–1987 | 19 | 45 | 12.9 | 0.495 |  |
| [swed311](txt/europe/swed311.txt) | SWED311 | Picea abies (L.) H. Karst. | 61 | 1759–2004 | 3 | 13 | 12.9 | 0.574 |  |
| [va016](txt/northamerica/usa/va016.txt) | VA016 | Quercus prinus L. | 25 | 1642–1981 | 5 | 21 | 12.9 | 0.519 |  |
| [rus347](txt/asia/rus347.txt) | RUS347 | Pinus sylvestris L. | 35 | 1763–2014 | 14 | 8 | 12.9 | 0.483 |  |
| [mn009](txt/northamerica/usa/mn009.txt) | MN009 | Pinus resinosa Aiton | 10 | 1672–1971 | 1 | 8 | 12.9 | 0.508 |  |
| [ls001](txt/asia/ls001.txt) | LS001 | Pinus merkusii Jungh. & De Vriese | 30 | 1743–2005 | 12 | 7 | 12.8 | 0.482 |  |
| [arge001](txt/southamerica/arge001.txt) | ARGE001 | Araucaria araucana (Molina) K. Koch | 29 | 200–1974 | 14 | 20 | 12.8 | 0.514 |  |
| [ca744](txt/northamerica/usa/ca744.txt) | CA744 | Pinus ponderosa Douglas ex C. Lawson | 16 | 1776–2023 | 2 | 3 | 12.8 | 0.489 |  |
| [cana380](txt/northamerica/canada/cana380.txt) | CANA380 | Abies lasiocarpa (Hook.) Nutt. | 39 | 1848–2002 | 5 | 5 | 12.8 | 0.478 |  |
| [newz108](txt/australia/newz108.txt) | NEWZ108 | Agathis australis (D. Don) Loudon | 46 | 1515–1861 | 3 | 2 | 12.8 | 0.516 |  |
| [chin017](txt/asia/chin017.txt) | CHIN017 | Juniperus tibetica Kom. | 43 | 1452–2007 | 16 | 35 | 12.8 | 0.504 |  |
| [ak20](txt/northamerica/usa/ak20.txt) | AK20 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 16 | 1616–1986 | 4 | 8 | 12.8 | 0.561 |  |
| [deu411](txt/europe/deu411.txt) | DEU411 | Larix decidua Mill. | 20 | 1947–2009 | 5 | 1 | 12.8 | 0.696 | Segment length reduced from 50 to 30 years: the record is only 63 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ma032](txt/northamerica/usa/ma032.txt) | MA032 | Chamaecyparis thyoides (L.) B.S.P. | 37 | 1854–2016 | 6 | 6 | 12.8 | 0.478 |  |
| [nepa019](txt/asia/nepa019.txt) | NEPA019 | Abies spectabilis (D. Don) Spach | 26 | 1530–1997 | 12 | 13 | 12.8 | 0.479 |  |
| [ca722](txt/northamerica/usa/ca722.txt) | CA722 | Sequoiadendron giganteum (Lindl.) Buchholz | 60 | 326–1992 | 30 | 58 | 12.8 | 0.513 |  |
| [cana274](txt/northamerica/canada/cana274.txt) | CANA274 | Picea glauca (Moench) Voss | 150 | 1743–2003 | 10 | 17 | 12.7 | 0.524 |  |
| [tur063](txt/europe/tur063.txt) | TUR063 | Cedrus libani A. Rich. | 33 | 1770–2012 | 4 | 3 | 12.7 | 0.553 |  |
| [ca723](txt/northamerica/usa/ca723.txt) | CA723 | Sequoiadendron giganteum (Lindl.) Buchholz | 66 | 341–1992 | 44 | 70 | 12.7 | 0.511 |  |
| [swit322](txt/europe/swit322.txt) | SWIT322 | Fagus sylvatica L. | 44 | 1801–1997 | 2 | 8 | 12.7 | 0.640 |  |
| [swit368](txt/europe/swit368.txt) | SWIT368 | Picea abies (L.) H. Karst. | 27 | 1885–2016 | 5 | 5 | 12.7 | 0.552 |  |
| [wa086](txt/northamerica/usa/wa086.txt) | WA086 | Pseudotsuga menziesii (Mirb.) Franco | 47 | 1288–1987 | 39 | 57 | 12.6 | 0.520 |  |
| [va026](txt/northamerica/usa/va026.txt) | VA026 | Quercus alba L. | 24 | 1713–2000 | 7 | 5 | 12.6 | 0.483 |  |
| [mong033](txt/asia/mong033.txt) | MONG033 | Larix sibirica Ledeb. = Larix russica (Endl.) Sabine ex Trautv. | 43 | 1265–1997 | 6 | 43 | 12.6 | 0.516 |  |
| [cana448](txt/northamerica/canada/cana448.txt) | CANA448 | Picea engelmannii Parry ex Engelm. | 48 | 867–1345 | 9 | 17 | 12.6 | 0.456 |  |
| [pa009](txt/northamerica/usa/pa009.txt) | PA009 | Quercus prinus L. | 18 | 1631–1981 | 7 | 12 | 12.6 | 0.515 |  |
| [wa145](txt/northamerica/usa/wa145.txt) | WA145 | Tsuga mertensiana (Bong.) Carrière | 17 | 1346–2011 | 13 | 15 | 12.6 | 0.469 |  |
| [arge092](txt/southamerica/arge092.txt) | ARGE092 | Fitzroya cupressoides (Molina) I.M. Johnst. | 57 | -342–1995 | 35 | 79 | 12.5 | 0.523 |  |
| [arge056](txt/southamerica/arge056.txt) | ARGE056 | Pilgerodendron uviferum (D. Don) Florin | 22 | 1765–1982 | 6 | 2 | 12.5 | 0.468 |  |
| [aust005](txt/europe/aust005.txt) | AUST005 | Picea abies (L.) H. Karst. | 12 | 1838–1975 | 0 | 3 | 12.5 | 0.583 |  |
| [bol010](txt/southamerica/bol010.txt) | BOL010 | Centrolobium microchaete (Benth.) H.C. Lima | 26 | 1909–2006 | 0 | 2 | 12.5 | 0.460 | Segment length reduced from 50 to 48 years: the record is only 98 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [can718](txt/northamerica/canada/can718.txt) | CAN718 | Picea engelmannii Parry ex Engelm. | 17 | 1771–2023 | 1 | 9 | 12.5 | 0.463 |  |
| [cana088](txt/northamerica/canada/cana088.txt) | CANA088 | Picea glauca (Moench) Voss | 25 | 1770–1983 | 2 | 10 | 12.5 | 0.524 |  |
| [cana617](txt/northamerica/canada/cana617.txt) | CANA617 | Acer saccharum Marsh. | 13 | 1845–2017 | 2 | 2 | 12.5 | 0.460 |  |
| [che476](txt/europe/che476.txt) | CHE476 | Picea abies (L.) H. Karst. | 15 | 1845–2023 | 2 | 1 | 12.5 | 0.537 |  |
| [che582](txt/europe/che582.txt) | CHE582 | Fagus sylvatica L. | 11 | 1889–2007 | 1 | 3 | 12.5 | 0.456 |  |
| [cze037](txt/europe/cze037.txt) | CZE037 | Pinus sylvestris L. | 27 | 1852–2019 | 1 | 7 | 12.5 | 0.538 |  |
| [czec032](txt/europe/czec032.txt) | CZEC032 | Quercus robur L. = Quercus pendunculata Ehrl. | 23 | 1856–2013 | 4 | 2 | 12.5 | 0.502 |  |
| [deu472](txt/europe/deu472.txt) | DEU472 | Fagus sylvatica L. | 5 | 1929–2019 | 1 | 0 | 12.5 | 0.583 | Segment length reduced from 50 to 44 years: the record is only 91 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [indi005](txt/asia/indi005.txt) | INDI005 | Abies pindrow (Royle ex D. Don) Royle | 4 | 1732–1980 | 1 | 2 | 12.5 | 0.332 |  |
| [indi008](txt/asia/indi008.txt) | INDI008 | Abies pindrow (Royle ex D. Don) Royle | 8 | 1609–1982 | 4 | 6 | 12.5 | 0.500 |  |
| [mo057](txt/northamerica/usa/mo057.txt) | MO057 | Quercus coccinea Muenchh. | 44 | 1901–2002 | 2 | 1 | 12.5 | 0.436 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [neth017](txt/europe/neth017.txt) | NETH017 | Quercus spp. L. | 9 | 1455–1650 | 1 | 1 | 12.5 | 0.479 |  |
| [newz104](txt/australia/newz104.txt) | NEWZ104 | Agathis australis (D. Don) Loudon | 10 | 1207–1872 | 2 | 1 | 12.5 | 0.479 |  |
| [russ289](txt/asia/russ289.txt) | RUSS289 | Larix sibirica Ledeb. = Larix russica (Endl.) Sabine ex Trautv. | 11 | 1877–2004 | 1 | 2 | 12.5 | 0.603 |  |
| [spai004](txt/europe/spai004.txt) | SPAI004 | Pinus mughus Scop. = Pinus mugo Turra | 20 | 1808–1977 | 2 | 2 | 12.5 | 0.551 |  |
| [swit170w](txt/europe/swit170w.txt) | SWIT170 | Pinus mughus Scop. = Pinus mugo Turra | 20 | 1671–1987 | 5 | 5 | 12.5 | 0.467 |  |
| [swit239](txt/europe/swit239.txt) | SWIT239 | Fagus sylvatica L. | 24 | 1920–1987 | 1 | 2 | 12.5 | 0.564 | Segment length reduced from 50 to 34 years: the record is only 68 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit298](txt/europe/swit298.txt) | SWIT298 | Fagus sylvatica L. | 17 | 1920–1989 | 2 | 0 | 12.5 | 0.502 | Segment length reduced from 50 to 34 years: the record is only 70 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [syri002](txt/asia/syri002.txt) | SYRI002 | Abies cilicica (Ant. et Kotschy) Carr. | 35 | 1795–2001 | 8 | 4 | 12.5 | 0.482 |  |
| [wi032](txt/northamerica/usa/wi032.txt) | WI032 | Quercus spp. L. | 10 | 1764–1851 | 0 | 1 | 12.5 | 0.521 | Segment length reduced from 50 to 44 years: the record is only 88 years long. |
| [wi062](txt/northamerica/usa/wi062.txt) | WI062 | Fraxinus americana L. | 13 | 1939–2023 | 2 | 1 | 12.5 | 0.524 | Segment length reduced from 50 to 42 years: the record is only 85 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [wy084](txt/northamerica/usa/wy084.txt) | WY084 | Pinus contorta Douglas ex Loudon | 42 | 1781–2023 | 2 | 7 | 12.5 | 0.456 |  |
| [zimb001](txt/africa/zimb001.txt) | ZIMB001 | Canthium burttii | 22 | 1846–1994 | 0 | 1 | 12.5 | 0.707 |  |
| [ut591](txt/northamerica/usa/ut591.txt) | UT591 | Pinus flexilis E. James | 40 | 1282–2012 | 17 | 29 | 12.5 | 0.511 |  |
| [czec009](txt/europe/czec009.txt) | CZEC009 | Pinus sylvestris L. | 65 | 1785–2017 | 5 | 19 | 12.4 | 0.587 |  |
| [viet002](txt/asia/viet002.txt) | VIET002 | Fokienia hodginsii | 42 | 1470–2004 | 18 | 27 | 12.4 | 0.478 |  |
| [arge062](txt/southamerica/arge062.txt) | ARGE062 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 33 | 1593–1984 | 12 | 14 | 12.4 | 0.451 |  |
| [swit409](txt/europe/swit409.txt) | SWIT409 | Picea abies (L.) H. Karst. | 26 | 1750–2015 | 6 | 7 | 12.4 | 0.473 |  |
| [russ274](txt/asia/russ274.txt) | RUSS274 | Pinus sylvestris L. | 47 | 1824–2009 | 7 | 5 | 12.4 | 0.441 |  |
| [wa035](txt/northamerica/usa/wa035.txt) | WA035 | Abies lasiocarpa (Hook.) Nutt. | 24 | 1788–1976 | 6 | 6 | 12.4 | 0.520 |  |
| [nc007](txt/northamerica/usa/nc007.txt) | NC007 | Quercus alba L. | 40 | 1617–1977 | 16 | 18 | 12.4 | 0.510 |  |
| [nor049](txt/europe/nor049.txt) | NOR049 | Pinus sylvestris L. | 41 | 1664–2022 | 6 | 28 | 12.4 | 0.559 |  |
| [ausl048](txt/australia/ausl048.txt) | AUSL048 | Athrotaxis selaginoides D. Don | 163 | 1412–2009 | 50 | 69 | 12.4 | 0.484 |  |
| [per012](txt/southamerica/per012.txt) | PER012 | Polylepis tarapacana Phil. | 34 | 1602–2015 | 11 | 17 | 12.3 | 0.487 |  |
| [mexi090](txt/northamerica/mexico/mexi090.txt) | MEXI090 | Taxodium mucronatum Ten. | 25 | 1826–2006 | 0 | 7 | 12.3 | 0.426 |  |
| [nc012](txt/northamerica/usa/nc012.txt) | NC012 | Liriodendron tulipifera L. | 16 | 1672–1997 | 0 | 7 | 12.3 | 0.539 |  |
| [spai002](txt/europe/spai002.txt) | SPAI002 | Pinus sylvestris L. | 24 | 1663–1977 | 5 | 9 | 12.3 | 0.519 |  |
| [spai079](txt/europe/spai079.txt) | SPAI079 | Pinus nigra J.F. Arnold | 22 | 1890–2009 | 3 | 4 | 12.3 | 0.546 |  |
| [mexi067](txt/northamerica/mexico/mexi067.txt) | MEXI067 | Taxodium mucronatum Ten. | 31 | 1800–2006 | 1 | 10 | 12.2 | 0.477 |  |
| [brit044](txt/europe/brit044.txt) | BRIT044 | Quercus spp. L. | 16 | 1708–1984 | 5 | 5 | 12.2 | 0.414 |  |
| [germ186](txt/europe/germ186.txt) | GERM186 | Pinus sylvestris L. | 14 | 1854–2005 | 3 | 2 | 12.2 | 0.562 |  |
| [or082](txt/northamerica/usa/or082.txt) | OR082 | Pinus albicaulis Engelm. | 49 | 1740–2002 | 9 | 6 | 12.2 | 0.468 |  |
| [rus332](txt/asia/rus332.txt) | RUS332 | Larix sibirica Ledeb. = Larix russica (Endl.) Sabine ex Trautv. | 31 | 1670–2012 | 9 | 10 | 12.2 | 0.476 |  |
| [arg158](txt/southamerica/arg158.txt) | ARG158 | Juglans australis Griseb. | 37 | 1930–2004 | 4 | 5 | 12.2 | 0.626 | Segment length reduced from 50 to 36 years: the record is only 75 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [mex121](txt/northamerica/mexico/mex121.txt) | MEX121 | Taxodium mucronatum Ten. | 20 | 1719–2005 | 7 | 6 | 12.1 | 0.517 |  |
| [mexi038](txt/northamerica/mexico/mexi038.txt) | MEXI038 | Taxodium mucronatum Ten. | 20 | 1719–2005 | 7 | 6 | 12.1 | 0.517 |  |
| [rus385](txt/asia/rus385.txt) | RUS385 | Pinus sylvestris L. | 44 | 1802–2014 | 8 | 5 | 12.1 | 0.538 |  |
| [ausl023](txt/australia/ausl023.txt) | AUSL023 | Lagarostrobos franklinii (Hook. f.) Quinn | 23 | 1058–1992 | 17 | 24 | 12.1 | 0.522 |  |
| [che567](txt/europe/che567.txt) | CHE567 | Fagus sylvatica L. | 10 | 1773–2007 | 2 | 2 | 12.1 | 0.530 |  |
| [chil019](txt/southamerica/chil019.txt) | CHIL019 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 50 | 1745–1996 | 3 | 13 | 12.1 | 0.566 |  |
| [russ004](txt/asia/russ004.txt) | RUSS004 | Larix sibirica Ledeb. = Larix russica (Endl.) Sabine ex Trautv. | 11 | 1770–1972 | 0 | 4 | 12.1 | 0.620 |  |
| [swit141w](txt/europe/swit141w.txt) | SWIT141 | Abies alba Mill. | 24 | 1861–1977 | 3 | 1 | 12.1 | 0.430 |  |
| [ca720](txt/northamerica/usa/ca720.txt) | CA720 | Sequoiadendron giganteum (Lindl.) Buchholz | 80 | -384–1992 | 74 | 95 | 12.1 | 0.509 |  |
| [brit054](txt/europe/brit054.txt) | BRIT054 | Quercus spp. L. | 14 | 1666–2008 | 7 | 4 | 12.1 | 0.472 |  |
| [cana586](txt/northamerica/canada/cana586.txt) | CANA586 | Pinus flexilis E. James | 32 | 1009–2010 | 11 | 18 | 12.1 | 0.509 |  |
| [deu324](txt/europe/deu324.txt) | DEU324 | Fagus sylvatica L. | 20 | 1842–2008 | 5 | 2 | 12.1 | 0.529 |  |
| [id025](txt/northamerica/usa/id025.txt) | ID025 | Thuja plicata Donn ex D. Don | 39 | 1859–2020 | 3 | 4 | 12.1 | 0.464 |  |
| [rus317](txt/asia/rus317.txt) | RUS317 |  | 121 | 1367–2020 | 30 | 54 | 12.1 | 0.490 |  |
| [arge125](txt/southamerica/arge125.txt) | ARGE125 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 34 | 1767–2006 | 4 | 5 | 12.0 | 0.558 |  |
| [indi003](txt/asia/indi003.txt) | INDI003 | Abies pindrow (Royle ex D. Don) Royle | 10 | 1608–1980 | 6 | 3 | 12.0 | 0.502 |  |
| [russ173w](txt/asia/russ173w.txt) | RUSS173 | Pinus sylvestris L. | 26 | 1917–1996 | 2 | 1 | 12.0 | 0.575 | Segment length reduced from 50 to 40 years: the record is only 80 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit129](txt/europe/swit129.txt) | SWIT129 | Pinus sylvestris L. | 22 | 1802–1982 | 1 | 2 | 12.0 | 0.522 |  |
| [ital048](txt/europe/ital048.txt) | ITAL048 | Pinus nigra J.F. Arnold | 76 | 1820–2014 | 2 | 6 | 11.9 | 0.489 |  |
| [paki012](txt/asia/paki012.txt) | PAKI012 | Juniperus spp. L. | 18 | 1069–1990 | 5 | 24 | 11.9 | 0.529 |  |
| [nepa039](txt/asia/nepa039.txt) | NEPA039 | Tsuga dumosa (D. Don) Eichler | 32 | 1605–1996 | 13 | 8 | 11.9 | 0.502 |  |
| [ca639](txt/northamerica/usa/ca639.txt) | CA639 | Pinus flexilis E. James | 15 | 526–1995 | 13 | 10 | 11.9 | 0.535 |  |
| [bra048](txt/southamerica/bra048.txt) | BRA048 | Cedrela fissilis Vell. | 96 | 1940–2015 | 0 | 5 | 11.9 | 0.527 | Segment length reduced from 50 to 38 years: the record is only 76 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [cana046](txt/northamerica/canada/cana046.txt) | CANA046 | Picea glauca (Moench) Voss | 24 | 1795–1988 | 3 | 2 | 11.9 | 0.510 |  |
| [cana066](txt/northamerica/canada/cana066.txt) | CANA066 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 24 | 1748–1988 | 1 | 4 | 11.9 | 0.476 |  |
| [cana10](txt/northamerica/canada/cana10.txt) | CANA10 | Pinus ponderosa Douglas ex C. Lawson | 27 | 1496–1995 | 16 | 14 | 11.9 | 0.504 |  |
| [cana309](txt/northamerica/canada/cana309.txt) | CANA309 | Picea glauca (Moench) Voss | 16 | 1517–1988 | 2 | 8 | 11.9 | 0.465 |  |
| [cana552](txt/northamerica/canada/cana552.txt) | CANA552 | Picea glauca (Moench) Voss | 77 | 1522–2007 | 21 | 39 | 11.9 | 0.492 |  |
| [va20](txt/northamerica/usa/va20.txt) | VA20 | Pinus taeda L. | 23 | 1836–1975 | 0 | 5 | 11.9 | 0.666 |  |
| [vt008](txt/northamerica/usa/vt008.txt) | VT008 | Abies balsamea (L.) Mill. | 78 | 1879–2005 | 2 | 3 | 11.9 | 0.451 |  |
| [cana099](txt/northamerica/canada/cana099.txt) | CANA099 | Picea engelmannii Parry ex Engelm. | 63 | 1499–1992 | 17 | 45 | 11.9 | 0.526 |  |
| [mo046](txt/northamerica/usa/mo046.txt) | MO046 | Quercus macrocarpa Michx. | 50 | 1687–2004 | 13 | 20 | 11.9 | 0.525 |  |
| [ca708](txt/northamerica/usa/ca708.txt) | CA708 | Sequoiadendron giganteum (Lindl.) Buchholz | 69 | -1372–1895 | 119 | 191 | 11.9 | 0.522 |  |
| [paki025](txt/asia/paki025.txt) | PAKI025 | Juniperus excelsa M.-Bieb. | 35 | 1290–2007 | 13 | 22 | 11.9 | 0.539 |  |
| [wi028](txt/northamerica/usa/wi028.txt) | WI028 | Quercus macrocarpa Michx. | 14 | 1747–2014 | 3 | 4 | 11.9 | 0.484 |  |
| [swed022w](txt/europe/swed022w.txt) | SWED022 | Pinus sylvestris L. | 86 | 1127–1987 | 11 | 14 | 11.8 | 0.542 |  |
| [cze049](txt/europe/cze049.txt) | CZE049 | Pinus sylvestris L. | 26 | 1855–2018 | 0 | 9 | 11.8 | 0.554 |  |
| [indi016](txt/asia/indi016.txt) | INDI016 | Picea smithiana (Wall.) Boiss. | 14 | 1724–1989 | 6 | 3 | 11.8 | 0.500 |  |
| [brit008](txt/europe/brit008.txt) | BRIT008 | Quercus spp. L. | 15 | 1652–1975 | 4 | 7 | 11.8 | 0.464 |  |
| [or054](txt/northamerica/usa/or054.txt) | OR054 | Pinus ponderosa Douglas ex C. Lawson | 20 | 1513–1995 | 10 | 19 | 11.8 | 0.510 |  |
| [chin025](txt/asia/chin025.txt) | CHIN025 | Abies forestii Rogers | 27 | 1483–2007 | 16 | 17 | 11.8 | 0.459 |  |
| [ak5](txt/northamerica/usa/ak5.txt) | AK5 | Picea sitchensis (Bong.) Carrière | 38 | 1848–1987 | 1 | 3 | 11.8 | 0.537 |  |
| [ausl060](txt/australia/ausl060.txt) | AUSL060 | Callitris intratropica R. Baker & H.G. Smith | 23 | 1877–2007 | 2 | 2 | 11.8 | 0.485 |  |
| [cana555](txt/northamerica/canada/cana555.txt) | CANA555 | Pinus banksiana Lamb. | 20 | 1921–2011 | 1 | 1 | 11.8 | 0.495 | Segment length reduced from 50 to 44 years: the record is only 91 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [chin079](txt/asia/chin079.txt) | CHIN079 | Pinus yunnanensis Franch. | 15 | 1976–2016 | 1 | 1 | 11.8 | 0.695 | Segment length reduced from 50 to 20 years: the record is only 41 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [esp094](txt/europe/esp094.txt) | ESP094 | Pinus uncinata Mill. ex Mirb. | 66 | 1529–2009 | 6 | 24 | 11.8 | 0.527 |  |
| [gtm004](txt/centralamerica/gtm004.txt) | GTM004 | Cedrela odorata L. | 27 | 1893–2022 | 2 | 0 | 11.8 | 0.462 |  |
| [il026](txt/northamerica/usa/il026.txt) | IL026 | Acer saccharum Marsh. | 16 | 1900–2016 | 1 | 1 | 11.8 | 0.537 |  |
| [me027](txt/northamerica/usa/me027.txt) | ME027 | Fraxinus nigra Marsh. | 36 | 1872–1994 | 0 | 4 | 11.8 | 0.621 |  |
| [mi017](txt/northamerica/usa/mi017.txt) | MI017 | Pinus resinosa Aiton | 19 | 1862–2002 | 1 | 3 | 11.8 | 0.463 |  |
| [nc024](txt/northamerica/usa/nc024.txt) | NC024 | Tsuga canadensis (L.) Carr. | 37 | 1910–2010 | 0 | 2 | 11.8 | 0.512 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [nj008](txt/northamerica/usa/nj008.txt) | NJ008 | Quercus spp. L. | 29 | 1563–1838 | 2 | 6 | 11.8 | 0.477 |  |
| [oh013](txt/northamerica/usa/oh013.txt) | OH013 | Fagus grandifolia Ehrh. | 18 | 1724–2016 | 5 | 3 | 11.8 | 0.561 |  |
| [swit187](txt/europe/swit187.txt) | SWIT187 | Pinus nigra J.F. Arnold | 17 | 1893–2007 | 3 | 1 | 11.8 | 0.481 |  |
| [newz059](txt/australia/newz059.txt) | NEWZ059 | Libocedrus bidwillii Hook. f. | 58 | 1525–1992 | 25 | 26 | 11.8 | 0.501 |  |
| [cana445](txt/northamerica/canada/cana445.txt) | CANA445 | Picea engelmannii Parry ex Engelm. | 15 | 760–1326 | 3 | 12 | 11.7 | 0.514 |  |
| [wa153](txt/northamerica/usa/wa153.txt) | WA153 | Tsuga mertensiana (Bong.) Carrière | 47 | 1659–2018 | 20 | 26 | 11.7 | 0.528 |  |
| [paki040](txt/asia/paki040.txt) | PAKI040 | Cedrus deodara (D. Don) G. Don | 21 | 1472–2005 | 13 | 10 | 11.7 | 0.523 |  |
| [nc9](txt/northamerica/usa/nc9.txt) | NC9 | Pinus palustris Mill. | 34 | 1671–1979 | 5 | 25 | 11.7 | 0.497 |  |
| [deu379](txt/europe/deu379.txt) | DEU379 | Pinus sylvestris L. | 20 | 1816–2010 | 3 | 4 | 11.7 | 0.457 |  |
| [can687](txt/northamerica/canada/can687.txt) | CAN687 | Picea glauca (Moench) Voss | 41 | 1756–2022 | 5 | 14 | 11.7 | 0.458 |  |
| [paki002](txt/asia/paki002.txt) | PAKI002 | Juniperus spp. L. | 38 | 1369–1993 | 9 | 25 | 11.6 | 0.505 |  |
| [brit018](txt/europe/brit018.txt) | BRIT018 | Pinus sylvestris L. | 16 | 1756–1978 | 1 | 4 | 11.6 | 0.450 |  |
| [germ12](txt/europe/germ12.txt) | GERM12 | Abies alba Mill. | 11 | 1586–1961 | 3 | 7 | 11.6 | 0.497 |  |
| [germ15](txt/europe/germ15.txt) | GERM15 | Fagus sylvatica L. | 7 | 1684–1949 | 3 | 2 | 11.6 | 0.570 |  |
| [germ166](txt/europe/germ166.txt) | GERM166 | Quercus petraea (Matt.) Liebl. = Quercus sessiliflora Salisb. | 16 | 1868–2004 | 1 | 4 | 11.6 | 0.668 |  |
| [cze068](txt/europe/cze068.txt) | CZE068 | Picea abies (L.) H. Karst. | 25 | 1811–2021 | 4 | 4 | 11.6 | 0.490 |  |
| [chn086](txt/asia/chn086.txt) | CHN086 | Tetracentron sinense Oliv. | 62 | 1789–2015 | 7 | 10 | 11.6 | 0.455 |  |
| [vt005](txt/northamerica/usa/vt005.txt) | VT005 | Picea rubens Sarg. | 102 | 1668–2005 | 12 | 26 | 11.6 | 0.476 |  |
| [ca598](txt/northamerica/usa/ca598.txt) | CA598 | Pinus ponderosa Douglas ex C. Lawson | 26 | 1830–1995 | 1 | 5 | 11.5 | 0.584 |  |
| [cana560](txt/northamerica/canada/cana560.txt) | CANA560 | Pinus banksiana Lamb. | 23 | 1927–2017 | 3 | 0 | 11.5 | 0.531 | Segment length reduced from 50 to 44 years: the record is only 91 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ma017](txt/northamerica/usa/ma017.txt) | MA017 | Tsuga canadensis (L.) Carr. | 8 | 1650–2003 | 4 | 5 | 11.5 | 0.479 |  |
| [id003](txt/northamerica/usa/id003.txt) | ID003 | Pinus ponderosa Douglas ex C. Lawson | 52 | 1672–1975 | 2 | 18 | 11.5 | 0.577 |  |
| [tur089](txt/europe/tur089.txt) | TUR089 | Pinus nigra J.F. Arnold | 40 | 1856–2010 | 3 | 7 | 11.5 | 0.520 |  |
| [germ240](txt/europe/germ240.txt) | GERM240 | Picea abies (L.) H. Karst. | 40 | 1938–2010 | 3 | 4 | 11.5 | 0.548 | Segment length reduced from 50 to 36 years: the record is only 73 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ut592](txt/northamerica/usa/ut592.txt) | UT592 | Cercocarpus ledifolius Nutt. | 74 | 1653–2008 | 9 | 12 | 11.5 | 0.481 |  |
| [cana392](txt/northamerica/canada/cana392.txt) | CANA392 | Tsuga mertensiana (Bong.) Carrière | 35 | 1419–2002 | 9 | 24 | 11.5 | 0.465 |  |
| [russ222](txt/asia/russ222.txt) | RUSS222 | Pinus sibirica Du Tour | 52 | 1598–2011 | 18 | 14 | 11.4 | 0.491 |  |
| [ut545](txt/northamerica/usa/ut545.txt) | UT545 | Abies bifolia A.Murray | 42 | 1802–2019 | 2 | 6 | 11.4 | 0.483 |  |
| [newz118](txt/australia/newz118.txt) | NEWZ118 | Halocarpus biformis (Hook.) Quinn | 107 | 1457–2010 | 36 | 63 | 11.4 | 0.500 |  |
| [tn031](txt/northamerica/usa/tn031.txt) | TN031 | Juniperus virginiana L. | 47 | 1378–2002 | 12 | 18 | 11.4 | 0.491 |  |
| [arge011](txt/southamerica/arge011.txt) | ARGE011 | Araucaria araucana (Molina) K. Koch | 13 | 459–1974 | 3 | 6 | 11.4 | 0.498 |  |
| [russ240](txt/asia/russ240.txt) | RUSS240 | Larix sibirica Ledeb. = Larix russica (Endl.) Sabine ex Trautv. | 8 | 1603–2013 | 0 | 9 | 11.4 | 0.673 |  |
| [ak19](txt/northamerica/usa/ak19.txt) | AK19 | Picea sitchensis (Bong.) Carrière | 17 | 1599–1985 | 3 | 11 | 11.4 | 0.559 |  |
| [ca702](txt/northamerica/usa/ca702.txt) | CA702 | Pinus ponderosa Douglas ex C. Lawson | 83 | 1800–2018 | 2 | 8 | 11.4 | 0.569 |  |
| [swit234](txt/europe/swit234.txt) | SWIT234 | Pinus sylvestris L. | 78 | 1917–1995 | 1 | 4 | 11.4 | 0.575 | Segment length reduced from 50 to 38 years: the record is only 79 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [ausl051](txt/australia/ausl051.txt) | AUSL051 | Toona ciliata | 53 | 1812–2000 | 2 | 4 | 11.3 | 0.532 |  |
| [swit370](txt/europe/swit370.txt) | SWIT370 | Picea abies (L.) H. Karst. | 26 | 1875–2016 | 2 | 4 | 11.3 | 0.549 |  |
| [wa146](txt/northamerica/usa/wa146.txt) | WA146 | Tsuga mertensiana (Bong.) Carrière | 18 | 1615–2011 | 7 | 11 | 11.3 | 0.486 |  |
| [yugo001](txt/europe/yugo001.txt) | YUGO001 | Picea abies (L.) H. Karst. | 24 | 1757–1981 | 3 | 3 | 11.3 | 0.539 |  |
| [can691](txt/northamerica/canada/can691.txt) | CAN691 | Picea glauca (Moench) Voss | 45 | 1811–2022 | 4 | 3 | 11.3 | 0.495 |  |
| [chil013](txt/southamerica/chil013.txt) | CHIL013 | Austrocedrus chilensis (D. Don) Pic. Serm. & Bizzarri | 74 | 1568–1975 | 6 | 45 | 11.3 | 0.569 |  |
| [me041](txt/northamerica/usa/me041.txt) | ME041 | Chamaecyparis thyoides (L.) B.S.P. | 41 | 1846–2016 | 5 | 4 | 11.2 | 0.523 |  |
| [finl001](txt/europe/finl001.txt) | FINL001 | Pinus sylvestris L. | 50 | 1573–1984 | 8 | 66 | 11.2 | 0.525 |  |
| [ausl039](txt/australia/ausl039.txt) | AUSL039 | Athrotaxis selaginoides D. Don | 63 | 1481–2011 | 30 | 19 | 11.2 | 0.537 |  |
| [finl006](txt/europe/finl006.txt) | FINL006 | Pinus sylvestris L. | 16 | 1448–1766 | 4 | 7 | 11.2 | 0.485 |  |
| [wa013](txt/northamerica/usa/wa013.txt) | WA013 | Pinus ponderosa Douglas ex C. Lawson | 18 | 1623–1975 | 2 | 10 | 11.2 | 0.551 |  |
| [russ205](txt/asia/russ205.txt) | RUSS205 | Picea obovata Ledeb. = Picea abies (L.) H. Karst. subsp. obovata (Ledeb.) Hultén | 21 | 1551–2007 | 7 | 9 | 11.2 | 0.458 |  |
| [nepa027](txt/asia/nepa027.txt) | NEPA027 | Abies spectabilis (D. Don) Spach | 19 | 1561–1999 | 11 | 11 | 11.2 | 0.491 |  |
| [swit209](txt/europe/swit209.txt) | SWIT209 | Pinus sylvestris L. | 45 | 1701–2005 | 9 | 14 | 11.2 | 0.472 |  |
| [ak158](txt/northamerica/usa/ak158.txt) | AK158 | Chamaecyparis nootkatensis (D. Don) Spach | 12 | 1700–1995 | 8 | 4 | 11.1 | 0.462 |  |
| [ak18](txt/northamerica/usa/ak18.txt) | AK18 | Picea sitchensis (Bong.) Carrière | 17 | 1807–1986 | 0 | 3 | 11.1 | 0.575 |  |
| [ak4](txt/northamerica/usa/ak4.txt) | AK4 | Picea sitchensis (Bong.) Carrière | 6 | 1895–1986 | 0 | 1 | 11.1 | 0.531 | Segment length reduced from 50 to 46 years: the record is only 92 years long. |
| [bel001](txt/europe/bel001.txt) | BEL001 | Fagus sylvatica L. | 39 | 1774–2021 | 5 | 20 | 11.1 | 0.605 |  |
| [cana044](txt/northamerica/canada/cana044.txt) | CANA044 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 23 | 1830–1988 | 0 | 3 | 11.1 | 0.549 |  |
| [cana064](txt/northamerica/canada/cana064.txt) | CANA064 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 23 | 1841–1988 | 3 | 1 | 11.1 | 0.530 |  |
| [cana531](txt/northamerica/canada/cana531.txt) | CANA531 | Picea glauca (Moench) Voss | 46 | 1711–2007 | 16 | 13 | 11.1 | 0.544 |  |
| [cana620](txt/northamerica/canada/cana620.txt) | CANA620 | Acer saccharum Marsh. | 12 | 1760–2017 | 1 | 4 | 11.1 | 0.461 |  |
| [che434](txt/europe/che434.txt) | CHE434 | Picea abies (L.) H. Karst. | 11 | 1744–2013 | 5 | 2 | 11.1 | 0.493 |  |
| [che594](txt/europe/che594.txt) | CHE594 | Quercus robur L. = Quercus pendunculata Ehrl. | 9 | 1901–2008 | 0 | 1 | 11.1 | 0.608 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [che601](txt/europe/che601.txt) | CHE601 | Fraxinus excelsior L. | 21 | 1917–2022 | 2 | 0 | 11.1 | 0.576 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [eth002](txt/africa/eth002.txt) | ETH002 | Juniperus procera Hochst. ex Endl. | 35 | 1901–2006 | 0 | 2 | 11.1 | 0.501 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [germ086](txt/europe/germ086.txt) | GERM086 | Abies alba Mill. | 12 | 1719–1825 | 1 | 0 | 11.1 | 0.646 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [germ088](txt/europe/germ088.txt) | GERM088 | Abies alba Mill. | 10 | 1712–1861 | 1 | 0 | 11.1 | 0.630 |  |
| [germ165](txt/europe/germ165.txt) | GERM165 | Quercus petraea (Matt.) Liebl. = Quercus sessiliflora Salisb. | 18 | 1847–2004 | 0 | 6 | 11.1 | 0.591 |  |
| [lith010](txt/europe/lith010.txt) | LITH010 | Picea abies (L.) H. Karst. | 25 | 1836–1997 | 0 | 3 | 11.1 | 0.461 |  |
| [ma004](txt/northamerica/usa/ma004.txt) | MA004 | Quercus spp. L. | 13 | 1527–1712 | 1 | 1 | 11.1 | 0.533 |  |
| [ny046](txt/northamerica/usa/ny046.txt) | NY046 | Quercus spp. L. | 6 | 1507–1706 | 1 | 0 | 11.1 | 0.536 |  |
| [pa019](txt/northamerica/usa/pa019.txt) | PA019 | Acer saccharum Marsh. | 38 | 1893–2006 | 0 | 6 | 11.1 | 0.566 |  |
| [spai031](txt/europe/spai031.txt) | SPAI031 | Pinus nigra J.F. Arnold | 6 | 1728–1984 | 1 | 2 | 11.1 | 0.519 |  |
| [tha021](txt/asia/tha021.txt) | THA021 | Tectona grandis L. f. | 23 | 1855–1996 | 1 | 1 | 11.1 | 0.509 |  |
| [wi034](txt/northamerica/usa/wi034.txt) | WI034 | Quercus alba L. | 10 | 1921–2013 | 0 | 1 | 11.1 | 0.532 | Segment length reduced from 50 to 46 years: the record is only 93 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [wv017](txt/northamerica/usa/wv017.txt) | WV017 | Quercus alba L. | 17 | 1682–1824 | 2 | 3 | 11.1 | 0.568 |  |
| [wy035](txt/northamerica/usa/wy035.txt) | WY035 | Pinus contorta Douglas ex Loudon | 17 | 1900–2008 | 2 | 0 | 11.1 | 0.495 |  |
| [ca706](txt/northamerica/usa/ca706.txt) | CA706 | Sequoiadendron giganteum (Lindl.) Buchholz | 39 | 1442–1992 | 15 | 23 | 11.0 | 0.503 |  |
| [cana545](txt/northamerica/canada/cana545.txt) | CANA545 | Picea glauca (Moench) Voss | 81 | 1648–2007 | 15 | 34 | 11.0 | 0.491 |  |
| [cana322](txt/northamerica/canada/cana322.txt) | CANA322 | Picea glauca (Moench) Voss | 569 | 1046–2003 | 208 | 326 | 11.0 | 0.556 |  |
| [ny023](txt/northamerica/usa/ny023.txt) | NY023 | Betula lenta L. | 33 | 1614–2002 | 3 | 10 | 11.0 | 0.545 |  |
| [germ211](txt/europe/germ211.txt) | GERM211 | Fagus sylvatica L. | 19 | 1799–2006 | 0 | 12 | 11.0 | 0.661 |  |
| [gree009](txt/europe/gree009.txt) | GREE009 | Pinus nigra J.F. Arnold | 31 | 1657–1999 | 12 | 12 | 11.0 | 0.526 |  |
| [brit065](txt/europe/brit065.txt) | BRIT065 | Quercus spp. L. | 74 | 1670–2008 | 22 | 25 | 11.0 | 0.506 |  |
| [ca051](txt/northamerica/usa/ca051.txt) | CA051 | Pinus flexilis E. James | 26 | -42–1970 | 18 | 40 | 11.0 | 0.575 |  |
| [bel002](txt/europe/bel002.txt) | BEL002 | Fagus sylvatica L. | 16 | 1788–2020 | 2 | 9 | 11.0 | 0.594 |  |
| [mi020](txt/northamerica/usa/mi020.txt) | MI020 | Pinus resinosa Aiton | 36 | 1723–2007 | 4 | 17 | 11.0 | 0.514 |  |
| [swit365](txt/europe/swit365.txt) | SWIT365 | Pinus sylvestris L. | 92 | 1929–1996 | 5 | 5 | 11.0 | 0.594 | Segment length reduced from 50 to 34 years: the record is only 68 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [chin039](txt/asia/chin039.txt) | CHIN039 | Abies recurvata Mast. | 19 | 1489–2005 | 8 | 10 | 11.0 | 0.522 |  |
| [ca527](txt/northamerica/usa/ca527.txt) | CA527 | Pinus ponderosa Douglas ex C. Lawson | 36 | 1400–1980 | 18 | 26 | 10.9 | 0.507 |  |
| [arge027](txt/southamerica/arge027.txt) | ARGE027 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 21 | 1626–1982 | 5 | 9 | 10.9 | 0.484 |  |
| [ak136](txt/northamerica/usa/ak136.txt) | AK136 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 26 | 1857–2012 | 2 | 4 | 10.9 | 0.447 |  |
| [or096](txt/northamerica/usa/or096.txt) | OR096 | Pseudotsuga menziesii (Mirb.) Franco | 27 | 1838–2013 | 2 | 3 | 10.9 | 0.513 |  |
| [swit197](txt/europe/swit197.txt) | SWIT197 | Pinus sylvestris L. | 9 | 1–318 | 0 | 5 | 10.9 | 0.641 |  |
| [mexi109](txt/northamerica/mexico/mexi109.txt) | MEXI109 | Taxodium mucronatum Ten. | 41 | 1284–2009 | 19 | 15 | 10.9 | 0.483 |  |
| [cana532](txt/northamerica/canada/cana532.txt) | CANA532 | Picea glauca (Moench) Voss | 42 | 1735–2006 | 12 | 15 | 10.8 | 0.543 |  |
| [ny013](txt/northamerica/usa/ny013.txt) | NY013 | Pinus strobus L. | 26 | 1632–1981 | 11 | 11 | 10.8 | 0.542 |  |
| [newz064](txt/australia/newz064.txt) | NEWZ064 | Libocedrus bidwillii Hook. f. | 45 | 1450–1991 | 13 | 29 | 10.8 | 0.530 |  |
| [ky006](txt/northamerica/usa/ky006.txt) | KY006 | Pinus spp. L. | 42 | 1750–2004 | 2 | 6 | 10.8 | 0.529 |  |
| [mong018](txt/asia/mong018.txt) | MONG018 | Larix sibirica Ledeb. = Larix russica (Endl.) Sabine ex Trautv. | 24 | 1519–2005 | 1 | 11 | 10.8 | 0.598 |  |
| [nc6](txt/northamerica/usa/nc6.txt) | NC6 | Pinus taeda L. | 35 | 1891–1994 | 2 | 2 | 10.8 | 0.499 |  |
| [neth007](txt/europe/neth007.txt) | NETH007 | Quercus spp. L. | 16 | 1092–1287 | 3 | 1 | 10.8 | 0.497 |  |
| [ar009](txt/northamerica/usa/ar009.txt) | AR009 | Quercus alba L. | 25 | 1713–1972 | 2 | 9 | 10.8 | 0.573 |  |
| [or002](txt/northamerica/usa/or002.txt) | OR002 | Pinus ponderosa Douglas ex C. Lawson | 20 | 1421–1964 | 5 | 17 | 10.8 | 0.528 |  |
| [ca721](txt/northamerica/usa/ca721.txt) | CA721 | Sequoiadendron giganteum (Lindl.) Buchholz | 39 | -300–1991 | 27 | 68 | 10.8 | 0.573 |  |
| [az626](txt/northamerica/usa/az626.txt) | AZ626 | Pinus ponderosa Douglas ex C. Lawson | 60 | 1894–2022 | 0 | 7 | 10.8 | 0.708 |  |
| [che441](txt/europe/che441.txt) | CHE441 | Pinus cembra L. | 11 | 1610–2014 | 3 | 4 | 10.8 | 0.473 |  |
| [cze039](txt/europe/cze039.txt) | CZE039 | Pinus sylvestris L. | 26 | 1852–2020 | 0 | 7 | 10.8 | 0.560 |  |
| [cana542](txt/northamerica/canada/cana542.txt) | CANA542 | Picea glauca (Moench) Voss | 45 | 1668–2007 | 12 | 15 | 10.8 | 0.515 |  |
| [ny002](txt/northamerica/usa/ny002.txt) | NY002 | Quercus alba L. | 22 | 1648–1977 | 9 | 11 | 10.8 | 0.550 |  |
| [paki035](txt/asia/paki035.txt) | PAKI035 | Juniperus excelsa M.-Bieb. | 40 | 1497–2009 | 15 | 36 | 10.7 | 0.499 |  |
| [wa047](txt/northamerica/usa/wa047.txt) | WA047 | Pseudotsuga menziesii (Mirb.) Franco | 36 | 1248–1980 | 8 | 42 | 10.7 | 0.540 |  |
| [fran10](txt/europe/fran10.txt) | FRAN10 | Abies alba Mill. | 1248 | 1747–1988 | 88 | 132 | 10.7 | 0.517 |  |
| [cypr006](txt/europe/cypr006.txt) | CYPR006 | Cedrus libani A. Rich. | 24 | 1869–1981 | 3 | 0 | 10.7 | 0.477 |  |
| [nc013](txt/northamerica/usa/nc013.txt) | NC013 | Tsuga canadensis (L.) Carr. | 14 | 1784–1997 | 2 | 1 | 10.7 | 0.472 |  |
| [swit236](txt/europe/swit236.txt) | SWIT236 | Fagus sylvatica L. | 42 | 1865–1997 | 3 | 6 | 10.7 | 0.594 |  |
| [va041](txt/northamerica/usa/va041.txt) | VA041 | Quercus alba L. | 16 | 1675–1840 | 0 | 6 | 10.7 | 0.507 |  |
| [vt004](txt/northamerica/usa/vt004.txt) | VT004 | Acer saccharum Marsh. | 133 | 1865–2005 | 15 | 9 | 10.7 | 0.453 |  |
| [newz088](txt/australia/newz088.txt) | NEWZ088 | Agathis australis (D. Don) Loudon | 38 | 1269–1998 | 6 | 23 | 10.7 | 0.547 |  |
| [wv008](txt/northamerica/usa/wv008.txt) | WV008 | Tsuga canadensis (L.) Carr. | 39 | 1770–2010 | 5 | 6 | 10.7 | 0.528 |  |
| [wy048](txt/northamerica/usa/wy048.txt) | WY048 | Pinus contorta Douglas ex Loudon | 33 | 1680–2010 | 8 | 14 | 10.7 | 0.524 |  |
| [wy050](txt/northamerica/usa/wy050.txt) | WY050 | Pinus albicaulis Engelm. | 47 | 937–1998 | 15 | 18 | 10.7 | 0.517 |  |
| [cze040](txt/europe/cze040.txt) | CZE040 | Pinus sylvestris L. | 26 | 1843–2019 | 0 | 8 | 10.7 | 0.568 |  |
| [newz054](txt/australia/newz054.txt) | NEWZ054 | Nothofagus menziesii (Hook. f.) Oerst. | 16 | 1622–1979 | 0 | 8 | 10.7 | 0.511 |  |
| [me038](txt/northamerica/usa/me038.txt) | ME038 | Picea rubens Sarg. | 775 | 1649–2001 | 69 | 187 | 10.6 | 0.565 |  |
| [il028](txt/northamerica/usa/il028.txt) | IL028 | Quercus alba L. | 13 | 1792–2014 | 2 | 3 | 10.6 | 0.569 |  |
| [wa066](txt/northamerica/usa/wa066.txt) | WA066 | Tsuga mertensiana (Bong.) Carrière | 34 | 1520–1982 | 10 | 30 | 10.6 | 0.565 |  |
| [newz033](txt/australia/newz033.txt) | NEWZ033 | Nothofagus menziesii (Hook. f.) Oerst. | 18 | 1710–1980 | 1 | 6 | 10.6 | 0.449 |  |
| [chin074](txt/asia/chin074.txt) | CHIN074 | Juniperus tibetica Kom. | 144 | 128–2010 | 120 | 170 | 10.6 | 0.516 |  |
| [ausl049](txt/australia/ausl049.txt) | AUSL049 | Athrotaxis cupressoides D. Don | 85 | 1376–2004 | 32 | 49 | 10.6 | 0.524 |  |
| [id013](txt/northamerica/usa/id013.txt) | ID013 | Picea engelmannii Parry ex Engelm. | 28 | 1305–1997 | 20 | 15 | 10.6 | 0.504 |  |
| [russ207](txt/asia/russ207.txt) | RUSS207 | Picea obovata Ledeb. = Picea abies (L.) H. Karst. subsp. obovata (Ledeb.) Hultén | 19 | 1695–2007 | 2 | 11 | 10.6 | 0.539 |  |
| [japa019](txt/asia/japa019.txt) | JAPA019 | Chamaecyparis obtusa (Sieb. & Zucc.) Endl. | 66 | 1600–2001 | 20 | 33 | 10.6 | 0.505 |  |
| [alb005](txt/europe/alb005.txt) | ALB005 | Pinus pinea L. | 10 | 1862–2010 | 0 | 2 | 10.5 | 0.496 |  |
| [ca523](txt/northamerica/usa/ca523.txt) | CA523 | Pinus jeffreyi Balf. | 24 | 1388–1981 | 9 | 31 | 10.5 | 0.501 |  |
| [cana201](txt/northamerica/canada/cana201.txt) | CANA201 | Pinus banksiana Lamb. | 20 | 1875–2002 | 3 | 3 | 10.5 | 0.486 |  |
| [che475](txt/europe/che475.txt) | CHE475 | Picea abies (L.) H. Karst. | 15 | 1841–2023 | 4 | 0 | 10.5 | 0.597 |  |
| [che586](txt/europe/che586.txt) | CHE586 | Fagus sylvatica L. | 12 | 1885–2009 | 1 | 1 | 10.5 | 0.435 |  |
| [che588](txt/europe/che588.txt) | CHE588 | Fagus sylvatica L. | 20 | 1914–2012 | 1 | 1 | 10.5 | 0.478 | Segment length reduced from 50 to 48 years: the record is only 99 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [co055](txt/northamerica/usa/co055.txt) | CO055 | Pseudotsuga menziesii (Mirb.) Franco | 11 | 1328–1964 | 4 | 10 | 10.5 | 0.646 |  |
| [czec017](txt/europe/czec017.txt) | CZEC017 | Picea abies (L.) H. Karst. | 40 | 1689–2018 | 11 | 9 | 10.5 | 0.510 |  |
| [russ284](txt/asia/russ284.txt) | RUSS284 | Quercus spp. L. | 23 | 1074–1306 | 1 | 9 | 10.5 | 0.580 |  |
| [swit142w](txt/europe/swit142w.txt) | SWIT142 | Abies alba Mill. | 24 | 1874–1977 | 1 | 3 | 10.5 | 0.595 |  |
| [swit153w](txt/europe/swit153w.txt) | SWIT153 | Abies alba Mill. | 26 | 1905–1979 | 2 | 0 | 10.5 | 0.493 | Segment length reduced from 50 to 36 years: the record is only 75 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [swit341](txt/europe/swit341.txt) | SWIT341 | Fagus sylvatica L. | 19 | 1906–1989 | 1 | 1 | 10.5 | 0.549 | Segment length reduced from 50 to 42 years: the record is only 84 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [va042](txt/northamerica/usa/va042.txt) | VA042 | Pinus pungens Lamb. | 20 | 1921–2020 | 0 | 2 | 10.5 | 0.539 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [wa063](txt/northamerica/usa/wa063.txt) | WA063 | Tsuga mertensiana (Bong.) Carrière | 40 | 1488–1980 | 12 | 26 | 10.5 | 0.534 |  |
| [wa099](txt/northamerica/usa/wa099.txt) | WA099 | Tsuga mertensiana (Bong.) Carrière | 14 | 1713–1992 | 2 | 6 | 10.5 | 0.539 |  |
| [ak131](txt/northamerica/usa/ak131.txt) | AK131 | Tsuga mertensiana (Bong.) Carrière | 80 | 1454–2010 | 30 | 41 | 10.5 | 0.505 |  |
| [chin048](txt/asia/chin048.txt) | CHIN048 | Juniperus tibetica Kom. | 57 | 1080–1998 | 23 | 72 | 10.5 | 0.639 |  |
| [ca687](txt/northamerica/usa/ca687.txt) | CA687 | Quercus lobata Née | 44 | 1697–2010 | 5 | 8 | 10.5 | 0.532 |  |
| [deu412](txt/europe/deu412.txt) | DEU412 | Picea abies (L.) H. Karst. | 20 | 1799–2009 | 7 | 4 | 10.5 | 0.527 |  |
| [wi030](txt/northamerica/usa/wi030.txt) | WI030 | Quercus macrocarpa Michx. | 18 | 1734–2014 | 5 | 6 | 10.5 | 0.461 |  |
| [ausl019](txt/australia/ausl019.txt) | AUSL019 | Phyllocladus aspleniifolius (Labill.) Hook. f. | 8 | 1507–1974 | 2 | 7 | 10.5 | 0.578 |  |
| [ca603](txt/northamerica/usa/ca603.txt) | CA603 | Pinus albicaulis Engelm. | 29 | 1430–1996 | 7 | 11 | 10.5 | 0.496 |  |
| [can690](txt/northamerica/canada/can690.txt) | CAN690 | Picea glauca (Moench) Voss | 57 | 1870–2022 | 4 | 5 | 10.5 | 0.509 |  |
| [ak166](txt/northamerica/usa/ak166.txt) | AK166 | Betula papyrifera var. neoalaskana (Sarg.) Raup | 54 | 1938–2017 | 3 | 4 | 10.4 | 0.593 | Segment length reduced from 50 to 40 years: the record is only 80 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [cana203](txt/northamerica/canada/cana203.txt) | CANA203 | Pinus banksiana Lamb. | 29 | 1832–2002 | 3 | 4 | 10.4 | 0.508 |  |
| [wa144](txt/northamerica/usa/wa144.txt) | WA144 | Tsuga mertensiana (Bong.) Carrière | 18 | 1623–2011 | 5 | 9 | 10.4 | 0.460 |  |
| [cana326](txt/northamerica/canada/cana326.txt) | CANA326 | Picea glauca (Moench) Voss | 218 | 1067–2002 | 75 | 109 | 10.4 | 0.506 |  |
| [cana273](txt/northamerica/canada/cana273.txt) | CANA273 | Larix laricina (Du Roi) K. Koch | 72 | 1752–2006 | 2 | 17 | 10.4 | 0.529 |  |
| [wa134](txt/northamerica/usa/wa134.txt) | WA134 | Tsuga mertensiana (Bong.) Carrière | 14 | 1600–2006 | 4 | 13 | 10.4 | 0.488 |  |
| [brit5](txt/europe/brit5.txt) | BRIT5 | Quercus robur L. = Quercus pendunculata Ehrl. | 26 | 1025–1214 | 2 | 3 | 10.4 | 0.580 |  |
| [tn012](txt/northamerica/usa/tn012.txt) | TN012 | Castanea dentata (Marsh.) Borkh. | 22 | 1700–1924 | 3 | 2 | 10.4 | 0.567 |  |
| [ak200](txt/northamerica/usa/ak200.txt) | AK200 | Pinus contorta Douglas ex Loudon | 95 | 1490–2008 | 40 | 34 | 10.4 | 0.498 |  |
| [brit2](txt/europe/brit2.txt) | BRIT2 | Quercus petraea (Matt.) Liebl. = Quercus sessiliflora Salisb. | 70 | 1710–1974 | 1 | 20 | 10.4 | 0.541 |  |
| [newz127](txt/australia/newz127.txt) | NEWZ127 | Libocedrus bidwillii Hook. f. | 101 | 1303–2009 | 51 | 47 | 10.4 | 0.499 |  |
| [che519](txt/europe/che519.txt) | CHE519 | Picea abies (L.) H. Karst. | 10 | 1864–2010 | 1 | 2 | 10.3 | 0.522 |  |
| [che573](txt/europe/che573.txt) | CHE573 | Fagus sylvatica L. | 10 | 1855–2006 | 1 | 2 | 10.3 | 0.520 |  |
| [grc045](txt/europe/grc045.txt) | GRC045 | Pinus heldreichii Christ | 60 | 1689–2015 | 13 | 26 | 10.3 | 0.554 |  |
| [lith007](txt/europe/lith007.txt) | LITH007 | Picea abies (L.) H. Karst. | 27 | 1859–1998 | 1 | 2 | 10.3 | 0.513 |  |
| [russ108w](txt/asia/russ108w.txt) | RUSS108 | Larix gmelinii (Rupr.) Kuzen.=L.kurulensis(Maxim ex Regel)Pilg.=L.dahuricaTurcz.exTratv=Larix cajanderi Mayr | 17 | 1683–1992 | 1 | 5 | 10.3 | 0.580 |  |
| [turk004](txt/europe/turk004.txt) | TURK004 | Pinus sylvestris L. | 5 | 1717–1988 | 3 | 0 | 10.3 | 0.779 |  |
| [wa111](txt/northamerica/usa/wa111.txt) | WA111 | Abies lasiocarpa (Hook.) Nutt. | 16 | 1850–1992 | 2 | 1 | 10.3 | 0.593 |  |
| [ak049](txt/northamerica/usa/ak049.txt) | AK049 | Picea glauca (Moench) Voss | 99 | 1715–2001 | 10 | 24 | 10.3 | 0.500 |  |
| [rus381](txt/asia/rus381.txt) | RUS381 |  | 205 | 1518–1881 | 21 | 23 | 10.3 | 0.483 |  |
| [ausl002](txt/australia/ausl002.txt) | AUSL002 | Athrotaxis selaginoides D. Don | 35 | 1198–1975 | 15 | 21 | 10.3 | 0.558 |  |
| [arge005](txt/southamerica/arge005.txt) | ARGE005 | Araucaria araucana (Molina) K. Koch | 24 | 1459–1974 | 3 | 14 | 10.3 | 0.532 |  |
| [fran003](txt/europe/fran003.txt) | FRAN003 | Quercus robur L. = Quercus pendunculata Ehrl. | 25 | 1531–1979 | 11 | 17 | 10.3 | 0.560 |  |
| [pa016](txt/northamerica/usa/pa016.txt) | PA016 | Tsuga canadensis (L.) Carr. | 42 | 1896–2010 | 1 | 6 | 10.3 | 0.475 |  |
| [deu382](txt/europe/deu382.txt) | DEU382 | Picea abies (L.) H. Karst. | 20 | 1719–2010 | 5 | 6 | 10.3 | 0.501 |  |
| [wi026](txt/northamerica/usa/wi026.txt) | WI026 | Quercus macrocarpa Michx. | 25 | 1760–2014 | 4 | 7 | 10.3 | 0.508 |  |
| [deu485](txt/europe/deu485.txt) | DEU485 | Pseudotsuga menziesii (Mirb.) Franco | 47 | 1960–2016 | 4 | 4 | 10.3 | 0.640 | Segment length reduced from 50 to 28 years: the record is only 57 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [turk045](txt/europe/turk045.txt) | TURK045 | Juniperus spp. L. | 46 | -2084–-1821 | 1 | 3 | 10.3 | 0.630 |  |
| [va007](txt/northamerica/usa/va007.txt) | VA007 | Tsuga canadensis (L.) Carr. | 26 | 1742–1977 | 3 | 9 | 10.3 | 0.642 |  |
| [chil006](txt/southamerica/chil006.txt) | CHIL006 | Araucaria araucana (Molina) K. Koch | 30 | 1375–1975 | 10 | 19 | 10.2 | 0.526 |  |
| [vt006](txt/northamerica/usa/vt006.txt) | VT006 | Picea rubens Sarg. | 76 | 1734–2005 | 7 | 11 | 10.2 | 0.575 |  |
| [ny014](txt/northamerica/usa/ny014.txt) | NY014 | Pinus strobus L. | 26 | 1696–1978 | 4 | 16 | 10.2 | 0.553 |  |
| [newz066](txt/australia/newz066.txt) | NEWZ066 | Libocedrus bidwillii Hook. f. | 51 | 1431–1991 | 23 | 26 | 10.2 | 0.505 |  |
| [wa077](txt/northamerica/usa/wa077.txt) | WA077 | Abies lasiocarpa (Hook.) Nutt. | 37 | 1679–1991 | 9 | 13 | 10.2 | 0.603 |  |
| [me040](txt/northamerica/usa/me040.txt) | ME040 | Chamaecyparis thyoides (L.) B.S.P. | 75 | 1872–2014 | 3 | 9 | 10.2 | 0.555 |  |
| [newz052](txt/australia/newz052.txt) | NEWZ052 | Nothofagus solanderi (Hook f.) Oerst. | 21 | 1787–1980 | 3 | 3 | 10.2 | 0.551 |  |
| [swit257](txt/europe/swit257.txt) | SWIT257 | Larix decidua Mill. | 18 | 1777–2005 | 0 | 6 | 10.2 | 0.685 |  |
| [newz122](txt/australia/newz122.txt) | NEWZ122 | Lagarostrobos colensoi (Hook.) C.J. Quinn = Dacrydium colensoi Hook. | 69 | 1130–1969 | 26 | 43 | 10.2 | 0.492 |  |
| [nepa024](txt/asia/nepa024.txt) | NEPA024 | Picea smithiana (Wall.) Boiss. | 21 | 1671–1997 | 4 | 9 | 10.2 | 0.514 |  |
| [swit315](txt/europe/swit315.txt) | SWIT315 | Pinus cembra L. | 31 | 1745–1999 | 7 | 6 | 10.2 | 0.556 |  |
| [ca605](txt/northamerica/usa/ca605.txt) | CA605 | Pinus albicaulis Engelm. | 55 | 885–1996 | 15 | 21 | 10.1 | 0.504 |  |
| [finl067](txt/europe/finl067.txt) | FINL067 | Pinus sylvestris L. | 31 | 1472–1879 | 2 | 13 | 10.1 | 0.508 |  |
| [nc020](txt/northamerica/usa/nc020.txt) | NC020 | Quercus prinus L. | 23 | 1716–1996 | 5 | 3 | 10.1 | 0.506 |  |
| [ri002](txt/northamerica/usa/ri002.txt) | RI002 | Quercus alba L. | 38 | 1778–1999 | 8 | 16 | 10.1 | 0.538 |  |
| [chil020](txt/southamerica/chil020.txt) | CHIL020 | Nothofagus pumilio (Poepp. & Endl.) Reiche | 43 | 1728–1996 | 6 | 13 | 10.1 | 0.529 |  |
| [ca714](txt/northamerica/usa/ca714.txt) | CA714 | Sequoiadendron giganteum (Lindl.) Buchholz | 21 | -1237–1991 | 38 | 45 | 10.1 | 0.537 |  |
| [cana470](txt/northamerica/canada/cana470.txt) | CANA470 | Tsuga mertensiana (Bong.) Carrière | 32 | 1668–1997 | 10 | 15 | 10.1 | 0.518 |  |
| [wa051](txt/northamerica/usa/wa051.txt) | WA051 | Tsuga mertensiana (Bong.) Carrière | 38 | 1401–1980 | 14 | 39 | 10.1 | 0.542 |  |
| [ausl045](txt/australia/ausl045.txt) | AUSL045 | Athrotaxis selaginoides D. Don | 50 | 954–2011 | 24 | 58 | 10.1 | 0.523 |  |
| [or057](txt/northamerica/usa/or057.txt) | OR057 | Pinus ponderosa Douglas ex C. Lawson | 15 | 1442–1995 | 3 | 12 | 10.1 | 0.470 |  |
| [mt172](txt/northamerica/usa/mt172.txt) | MT172 | Pinus albicaulis Engelm. | 48 | 1560–2000 | 12 | 19 | 10.1 | 0.508 |  |
| [wy020](txt/northamerica/usa/wy020.txt) | WY020 | Picea engelmannii Parry ex Engelm. | 49 | 1421–1990 | 10 | 35 | 10.0 | 0.538 |  |
| [turk005](txt/europe/turk005.txt) | TURK005 | Cedrus libani A. Rich. | 39 | 1370–1988 | 22 | 24 | 10.0 | 0.556 |  |
| [ak134](txt/northamerica/usa/ak134.txt) | AK134 | Picea mariana (Mill.) Britton, Sterns, & Poggenb. | 27 | 1875–2010 | 1 | 2 | 10.0 | 0.445 |  |
| [arg160](txt/southamerica/arg160.txt) | ARG160 | Juglans australis Griseb. | 20 | 1930–2006 | 2 | 2 | 10.0 | 0.708 | Segment length reduced from 50 to 38 years: the record is only 77 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [aut126](txt/europe/aut126.txt) | AUT126 | Picea abies (L.) H. Karst. | 20 | 1822–2010 | 3 | 3 | 10.0 | 0.568 |  |
| [brit082](txt/europe/brit082.txt) | BRIT082 | Quercus spp. L. | 18 | 1681–1772 | 2 | 0 | 10.0 | 0.647 | Segment length reduced from 50 to 46 years: the record is only 92 years long. |
| [ca600](txt/northamerica/usa/ca600.txt) | CA600 | Pinus coulteri D.Don | 25 | 1794–1995 | 1 | 1 | 10.0 | 0.635 |  |
| [ca684](txt/northamerica/usa/ca684.txt) | CA684 | Pinus lambertiana Douglas | 27 | 1900–2009 | 3 | 3 | 10.0 | 0.540 |  |
| [cana629](txt/northamerica/canada/cana629.txt) | CANA629 | Acer saccharum Marsh. | 6 | 1895–2017 | 1 | 0 | 10.0 | 0.647 |  |
| [che447](txt/europe/che447.txt) | CHE447 | Fraxinus excelsior L. | 10 | 1911–2014 | 1 | 0 | 10.0 | 0.482 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [che518](txt/europe/che518.txt) | CHE518 | Picea abies (L.) H. Karst. | 10 | 1908–2010 | 1 | 0 | 10.0 | 0.546 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [co635](txt/northamerica/usa/co635.txt) | CO635 | Abies lasiocarpa (Hook.) Nutt. | 16 | 1709–2003 | 6 | 4 | 10.0 | 0.469 |  |
| [czec033](txt/europe/czec033.txt) | CZEC033 | Quercus robur L. = Quercus pendunculata Ehrl. | 24 | 1900–2015 | 2 | 2 | 10.0 | 0.508 |  |
| [deu471](txt/europe/deu471.txt) | DEU471 | Pseudotsuga menziesii (Mirb.) Franco | 23 | 1913–2018 | 1 | 1 | 10.0 | 0.539 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [indi009](txt/asia/indi009.txt) | INDI009 | Picea smithiana (Wall.) Boiss. | 13 | 1775–1982 | 2 | 3 | 10.0 | 0.500 |  |
| [ital047](txt/europe/ital047.txt) | ITAL047 | Pinus nigra J.F. Arnold | 54 | 1893–2013 | 1 | 2 | 10.0 | 0.497 |  |
| [mo061](txt/northamerica/usa/mo061.txt) | MO061 | Quercus coccinea Muenchh. | 27 | 1907–2002 | 0 | 2 | 10.0 | 0.490 | Segment length reduced from 50 to 48 years: the record is only 96 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [nc021](txt/northamerica/usa/nc021.txt) | NC021 | Quercus alba L. | 17 | 1733–1996 | 4 | 2 | 10.0 | 0.436 |  |
| [newz076](txt/australia/newz076.txt) | NEWZ076 | Dacrydium biforme (Hook.) Pilg. = Halocarpus biformis (Hook.) Quinn = Halocarpus biformis Hook. | 13 | 1708–1995 | 2 | 3 | 10.0 | 0.502 |  |
| [or122](txt/northamerica/usa/or122.txt) | OR122 | Pseudotsuga menziesii (Mirb.) Franco | 6 | 1587–1771 | 0 | 2 | 10.0 | 0.563 |  |
| [rus389](txt/asia/rus389.txt) | RUS389 | Pinus spp. L. | 10 | 1404–1521 | 0 | 1 | 10.0 | 0.509 | First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [russ166w](txt/asia/russ166w.txt) | RUSS166 | Larix gmelinii (Rupr.) Kuzen.=L.kurulensis(Maxim ex Regel)Pilg.=L.dahuricaTurcz.exTratv=Larix cajanderi Mayr | 22 | 1914–1990 | 0 | 2 | 10.0 | 0.543 | Segment length reduced from 50 to 38 years: the record is only 77 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [russ294](txt/asia/russ294.txt) | RUSS294 | Pinus sylvestris L. | 10 | 1856–2004 | 1 | 0 | 10.0 | 0.525 |  |
| [spai060](txt/europe/spai060.txt) | SPAI060 | Pinus sylvestris L. | 23 | 1931–2008 | 1 | 2 | 10.0 | 0.692 | Segment length reduced from 50 to 38 years: the record is only 78 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [spai078](txt/europe/spai078.txt) | SPAI078 | Pinus nigra J.F. Arnold | 10 | 1889–2010 | 1 | 2 | 10.0 | 0.556 |  |
| [swit157w](txt/europe/swit157w.txt) | SWIT157 | Picea abies (L.) H. Karst. | 28 | 1882–1979 | 1 | 3 | 10.0 | 0.549 | Segment length reduced from 50 to 48 years: the record is only 98 years long. |
| [swit208](txt/europe/swit208.txt) | SWIT208 | Quercus spp. L. | 12 | 1908–1999 | 0 | 1 | 10.0 | 0.637 | Segment length reduced from 50 to 46 years: the record is only 92 years long. \| First segment floored to 50 years, not 100: at 100 no segment could be tested. |
| [swit357](txt/europe/swit357.txt) | SWIT357 | Quercus pubescens Willd. | 15 | 1956–2006 | 1 | 2 | 10.0 | 0.635 | Segment length reduced from 50 to 24 years: the record is only 51 years long. \| First segment floored to 10 years, not 100: at 100 no segment could be tested. |
| [tha019](txt/asia/tha019.txt) | THA019 | Tectona grandis L. f. | 41 | 1855–2004 | 0 | 3 | 10.0 | 0.429 |  |
| [ut553](txt/northamerica/usa/ut553.txt) | UT553 | Populus tremuloides Michx. | 15 | 1813–2010 | 1 | 3 | 10.0 | 0.583 |  |
| [ut554](txt/northamerica/usa/ut554.txt) | UT554 | Populus tremuloides Michx. | 7 | 1828–2012 | 0 | 2 | 10.0 | 0.528 |  |
| [wa127](txt/northamerica/usa/wa127.txt) | WA127 | Abies lasiocarpa (Hook.) Nutt. | 27 | 1854–1990 | 2 | 0 | 10.0 | 0.470 |  |
| [wv021](txt/northamerica/usa/wv021.txt) | WV021 | Liriodendron tulipifera L. | 17 | 1777–1865 | 0 | 2 | 10.0 | 0.567 | Segment length reduced from 50 to 44 years: the record is only 89 years long. |
| [brit052](txt/europe/brit052.txt) | BRIT052 | Picea abies (L.) H. Karst. | 2 | 1559–1683 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [brit084](txt/europe/brit084.txt) | BRIT084 | Quercus spp. L. | 1 | 1368–1670 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [cana627](txt/northamerica/canada/cana627.txt) | CANA627 | Acer saccharum Marsh. | 2 | 1869–2017 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [che418](txt/europe/che418.txt) | CHE418 | Larix decidua Mill. | 22 | 1672–2010 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [che419](txt/europe/che419.txt) | CHE419 | Larix decidua Mill. | 26 | 1641–2010 |  |  |  |  | No segment could be tested: 'seg.length' can be at most 1/2 the number of years in 'rwl'. |
| [che420](txt/europe/che420.txt) | CHE420 | Larix decidua Mill. | 26 | 1884–2011 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [che423](txt/europe/che423.txt) | CHE423 | Larix decidua Mill. | 24 | 1860–2011 |  |  |  |  | Only 0 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [co578w](txt/northamerica/usa/co578w.txt) | CO578 | Pseudotsuga menziesii (Mirb.) Franco | 13 | 1867–1982 |  |  |  |  | No segment could be tested: 'seg.length' can be at most 1/2 the number of years in 'rwl'. |
| [deu440](txt/europe/deu440.txt) | DEU440 | Abies alba Mill. | 128 | 1915–2014 |  |  |  |  | No segment could be tested: 'seg.length' can be at most 1/2 the number of years in 'rwl'. |
| [fl015](txt/northamerica/usa/fl015.txt) | FL015 | Pinus palustris Mill. | 58 | 1908–2017 |  |  |  |  | No segment could be tested: 'seg.length' can be at most 1/2 the number of years in 'rwl'. |
| [fl016](txt/northamerica/usa/fl016.txt) | FL016 | Pinus palustris Mill. | 54 | 1925–2017 |  |  |  |  | Segment length reduced from 50 to 46 years: the record is only 93 years long. \| No segment could be tested: 'seg.length' can be at most 1/2 the number of years in 'rwl'. |
| [fl018](txt/northamerica/usa/fl018.txt) | FL018 | Pinus palustris Mill. | 32 | 1917–2017 |  |  |  |  | No segment could be tested: 'seg.length' can be at most 1/2 the number of years in 'rwl'. |
| [fran040](txt/europe/fran040.txt) | FRAN040 | Picea abies (L.) H. Karst. | 2 | 1803–1910 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [fran048](txt/europe/fran048.txt) | FRAN048 | Quercus robur L. = Quercus pendunculata Ehrl. | 128 | 1994–2012 |  |  |  |  | Segment length reduced from 50 to 8 years: the record is only 19 years long. \| The record is too short to test any segment, so there are no segment correlations or flags. |
| [germ049w](txt/europe/germ049w.txt) | GERM049 | Picea abies (L.) H. Karst. | 1 | 1851–1995 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [germ062](txt/europe/germ062.txt) | GERM062 | Picea abies (L.) H. Karst. | 1 | 1490–1803 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [germ063](txt/europe/germ063.txt) | GERM063 | Picea abies (L.) H. Karst. | 1 | 1605–1805 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [germ064](txt/europe/germ064.txt) | GERM064 | Picea abies (L.) H. Karst. | 1 | 1630–1793 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [germ065](txt/europe/germ065.txt) | GERM065 | Abies alba Mill. | 2 | 1342–1387 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [germ067](txt/europe/germ067.txt) | GERM067 | Abies alba Mill. | 2 | 1748–1802 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [germ077](txt/europe/germ077.txt) | GERM077 | Abies alba Mill. | 2 | 1629–1717 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [germ080](txt/europe/germ080.txt) | GERM080 | Abies alba Mill. | 2 | 1410–1455 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [germ085](txt/europe/germ085.txt) | GERM085 | Abies alba Mill. | 2 | 1626–1698 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [germ107](txt/europe/germ107.txt) | GERM107 | Picea abies (L.) H. Karst. | 2 | 1627–1678 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [ital039](txt/europe/ital039.txt) | ITAL039 | Fagus sylvatica L. | 36 | 1942–2002 |  |  |  |  | Segment length reduced from 50 to 30 years: the record is only 61 years long. \| No segment could be tested: 'seg.length' can be at most 1/2 the number of years in 'rwl'. |
| kyrg014 | KYRG014 | Picea schrenkiana Fisch. & C.A. Mey. |  |  |  |  |  |  | Failed: In data_files/treering/measurements/asia/kyrg014.rwl, series kok3a carries a stop marker of -9999, which declares 0.001 mm, at 1898, part way through its span of 1642-2004, while the last record of the series ends with 999, which declares 0.01 mm. A series is measured at one precision; these are not, so the file does not say which precision to read them at. Read as one series, the years on one side of the marker come out ten times the size of the years on the other -- a wrong ring width, not a missing one, which is why this is refused rather than warned about. The remedy is in the file: enter each record under its own series ID, one per precision. read.tucson.legacy() will read the file as it stands, treating the internal marker as a measurement; that is the guess this reader will not make. |
| [newz095](txt/australia/newz095.txt) | NEWZ095 | Agathis australis (D. Don) Loudon | 1 | 1280–1497 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [newz096](txt/australia/newz096.txt) | NEWZ096 | Agathis australis (D. Don) Loudon | 1 | 1791–1871 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [newz097](txt/australia/newz097.txt) | NEWZ097 | Agathis australis (D. Don) Loudon | 1 | 1545–1758 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [newz112](txt/australia/newz112.txt) | NEWZ112 | Agathis australis (D. Don) Loudon | 2 | 952–1088 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [russ168w](txt/asia/russ168w.txt) | RUSS168 | Pinus sylvestris L. | 28 | 1903–1996 |  |  |  |  | Segment length reduced from 50 to 46 years: the record is only 94 years long. \| No segment could be tested: 'seg.length' can be at most 1/2 the number of years in 'rwl'. |
| [swe347](txt/europe/swe347.txt) | SWE347 |  | 1 | 1673–1878 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [swit194](txt/europe/swit194.txt) | SWIT194 | Picea abies (L.) H. Karst. | 2 | 1713–1835 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [swit205](txt/europe/swit205.txt) | SWIT205 | Abies alba Mill. | 2 | 1830–1990 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [swit252](txt/europe/swit252.txt) | SWIT252 | Ulmus spp. L. | 2 | 1946–1991 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [swit286](txt/europe/swit286.txt) | SWIT286 | Abies alba Mill. | 2 | 1931–1994 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [swit339](txt/europe/swit339.txt) | SWIT339 | Pinus cembra L. | 2 | 1770–1996 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [swit350](txt/europe/swit350.txt) | SWIT350 | Fagus sylvatica L. | 1 | 1911–1992 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [swit353](txt/europe/swit353.txt) | SWIT353 | Carpinus betulus L. | 1 | 1934–1998 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [vt012](txt/northamerica/usa/vt012.txt) | VT012 | Tsuga canadensis (L.) Carr. | 2 | 1600–1809 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [vt014](txt/northamerica/usa/vt014.txt) | VT014 | Tsuga canadensis (L.) Carr. | 1 | 1624–1845 |  |  |  |  | Only 1 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [wa156](txt/northamerica/usa/wa156.txt) | WA156 | Pseudotsuga menziesii (Mirb.) Franco | 2 | 561–923 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [wi022](txt/northamerica/usa/wi022.txt) | WI022 | Quercus spp. L. | 2 | 1842–1934 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
| [wi043](txt/northamerica/usa/wi043.txt) | WI043 | Quercus spp. L. | 2 | 1677–1852 |  |  |  |  | Only 2 series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags. |
