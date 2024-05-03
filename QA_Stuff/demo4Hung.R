rm(list=ls())
library(dplR)
source("QA_Stuff/read_tucson2.R")
studies2check <- read.csv("QA_Stuff/studies_that_failed.csv")
# grab a problem child
foo <- read.tucson2(studies2check$rwlfilename[1])
