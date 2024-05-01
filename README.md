# itrdbMeasurementsClone
A repo to keep a copy of the measurement data from the ITRDB in a R-friendly format

```
# Here is an example of grabbing a rwl from the rwls.Rdata file. 
# It shows how we can extract a rwl object and its meta info

rm(list=ls())
library(dplR)
load("Rdatafiles/rwls.Rdata")

nrow(rwls_meta)
length(rwls)

# the first study
rwls_meta[1,]
aRWL <- rwls[[1]]

rwl.report(aRWL)
summary(aRWL)
plot(aRWL)

```
