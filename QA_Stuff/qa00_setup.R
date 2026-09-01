# AGB -- Aug 2026
#
# Common setup for the read.tucson2 QA track. Source this first, from the repo
# root, before any of the qa* scripts:
#
#   source("QA_Stuff/qa00_setup.R")
#
# read.tucson2() used to attach data.table and dplR itself. It no longer does,
# because a function cannot attach packages as a side effect and still go into
# dplR. Inside the reader every call is now written data.table:: or dplR::, so
# the reader alone needs neither package attached.
#
# The QA scripts are a different matter. They call dplR::read.tucson() to compare
# against, and they use data.table syntax directly, so both are attached here.

library(data.table)
library(dplR)

source("QA_Stuff/read_tucson2.R")

## AGB Aug 2026: note for when read.tucson2() moves into dplR. Putting data.table
## in Imports and writing data.table:: prefixes is NOT sufficient. If nothing is
## imported into the NAMESPACE, data.table's cedta() check ("Calling Environment
## is Data Table Aware") sees a package namespace that does not import it, and
## [.data.table falls back to [.data.frame as a safeguard. Every := and .SD call
## in read.tucson2() would then break, quietly. dplR needs either
## importFrom(data.table, ...) in NAMESPACE, or .datatable.aware <- TRUE in R/.
## That works here only because cedta() treats the global environment as aware.
## See vignette("datatable-importing", package = "data.table").
