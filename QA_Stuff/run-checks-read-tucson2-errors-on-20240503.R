library(dplR)
library(doFuture)
source("QA_Stuff/read_tucson2.R")
studies2check <- read.csv("QA_Stuff/studies_that_failed.csv")
# grab a problem child


checks <- rep(FALSE, nrow(studies2check))
for (ii in 1:nrow(studies2check)) {
  print(ii)
  if (!ii %in% c(324, 502)) {
    foo1 <- read.tucson2(studies2check$rwlfilename[ii], verbose = FALSE) |> rwl_to_dt()
    foo2 <- invisible(read.tucson(studies2check$rwlfilename[ii])) |> rwl_to_dt()
    checks[ii] <- all.equal(foo1[order(core, year)], foo2[order(core, year)])
  }
}

mismatch <- which(checks != '1' & checks != 'TRUE')
mismatch
checks[mismatch]

# New version misses
# 4  :  brit008 : space in ID (fixed)
# 6  :  turk033 : 73-char lines (fixed)
#    :  fran10  : headers in the middle of file with comment lines (fixed)
# 9  :  cana7rw : 4 lines of header, 4th lines has # (fixed)
# 21 :  cana015 : multiple 999 as end (fixed)
# 22 :  wa059   : strange character at end of file (fixed)
# 80 :  neth025 : End-of-line problem (fixed)
# 324:  kyrg014 : mixed precision - throw errors **********
# 464:  swit384 : space in ID (fixed)
# 465:  swit385 : space in ID (fixed)
# 466:  mong043b: both space and '-' in ID (fixed)
# 480:  az615   : core AZP0310A, year 1992; core AZP0310B, year 1957: characters in measurements (fixed)
# 502:  russ301 : has Russian character  ************
# 507:  fl010   : space in name, long core name (fixed)
# 508:  bol019  : weird number year 1866 core sot875 (fixed)
# 516:  deu317  : dmm0606m and dmm0625m, year 1799 actual 999 measurement at end of line (fixed)
# 545:  fl014   : multiple header lines (fixed)
# 561:  ita065  : multiple comment lines not starting with #, produced by Coorecorder (fixed)
# 577:  dza004  : headers more than 3 lines (fixed); 
# 580:  dza017  : 6 digit ring size (fixed)
#       cana136 : space in file name, followed by number, bunched with year (fixed)
#       mexi077 : -9999 shifted left and bunched (fixed)
#       or005   : gaps for missing value, missed it.
#       mex117  : numbers very large all bunched up
# 538-544:   che417-423: -999 used as missing ring (fixed)


# OG misses
#  14:  ct001   : only one line of header, OG missed first 2 data lines
#  18:  va024   : OG missed some numbers in 12A and 25B due to non-standard EOL
#  77:  neth031 : core ABGD544 off by 1 character, OG read as 0.22 instead of 2.22 
# 170:  swit177x: mixed -999 and 0 for zero rings; cores 638002, 638003, 638010: read -999 as zero at the end before the 999 mark 
# 377:  ausl043 : core SPR01C off by one character in years 1693, 1694; OG read as 0.1 instead of 1, and 0.052 instead of 0.52


file <- studies2check$rwlfilename[577]
file

# file <- 'data_files/treering/measurements/northamerica/canada/cana209.rwl'
# file <- 'data_files/treering/measurements/northamerica/usa/ak165.rwl'
# file <- 'data_files/treering/measurements/europe/tur090.rwl'
# file <- 'data_files/treering/measurements/southamerica/chil013.rwl'
# file <- 'data_files/treering/measurements/northamerica/mexico/mexi077.rwl'
# file <- 'data_files/treering/measurements/northamerica/canada/cana015.rwl'
# file <- 'data_files/treering/measurements/europe/czec1.rwl'
# file <- 'data_files/treering/measurements/asia/kyrg014.rwl'
file <- 'data_files/treering/measurements/asia/or005.rwl'

foo1 <- read.tucson2(file, verbose = FALSE)

foo1 <- read.tucson2(file, verbose = FALSE) |> rwl_to_dt()
foo2 <- read.tucson(file) |> rwl_to_dt()
setkey(foo1, core, year)
setkey(foo2, core, year)

foo1
foo2

merged <- merge(foo1, foo2, by = c('core', 'year'))
merged[abs(rw.x - rw.y) > 1e-4]

foo1[!foo2]
foo2[!foo1]

all.equal(foo1, foo2)

microbenchmark::microbenchmark(
  dplR::read.tucson(file),
  read.tucson2(file, verbose = FALSE),
  times = 1
)
