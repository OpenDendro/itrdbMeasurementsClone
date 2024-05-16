rm(list=ls())
library(dplR)
source("QA_Stuff/read_tucson2.R")
studies2check <- read.csv("QA_Stuff/studies_that_failed.csv")
# grab a problem child
<<<<<<< HEAD
<<<<<<< HEAD
foo <- read.tucson2(studies2check$rwlfilename[1])
=======
# OG read.tucson works
foo <- read.tucson(studies2check$rwlfilename[1])
# new function doesn't
foo <- read.tucson2(studies2check$rwlfilename[1])

>>>>>>> 68aa4eeb (A demo script for Hung edited)
=======
foo <- read.tucson2(studies2check$rwlfilename[1])
>>>>>>> 22e723dd (Updated the ITRDB and ran checks. Most probelmes with spp, enciding, etc fixed.)
