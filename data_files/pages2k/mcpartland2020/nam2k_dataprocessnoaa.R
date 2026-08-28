load("treeTSv9_NOAA.Rdata")

###subset by proxy type
trw<-treeTS[sapply(treeTS, function(x) all(x$paleoData_proxy == "TRW"))]
mxd<-treeTS[sapply(treeTS, function(x) all(x$paleoData_proxy == "MXD"))]

####subsetting by detrending 'flavor' 
NegEx<-treeTS[sapply(treeTS, function(x) all(x$paleoData_detrendingMethod == "NegEx"))]
Ssf<-treeTS[sapply(treeTS, function(x) all(x$paleoData_detrendingMethod == "SsfCrn"))]
SsfStb<-treeTS[sapply(treeTS, function(x) all(x$paleoData_detrendingMethod == "SsfCrnStb"))]
AgeDep<-treeTS[sapply(treeTS, function(x) all(x$paleoData_detrendingMethod == "AgeDependentStdCrn"))]
AgeDepStb<-treeTS[sapply(treeTS, function(x) all(x$paleoData_detrendingMethod == "AgeDependentStdCrnStb"))]
RCS<-treeTS[sapply(treeTS, function(x) all(x$paleoData_detrendingMethod == "RCS"))]


###extract metadata
authors<-as.character(lapply(treeTS, function(x) x$pub1_author))
name<-as.character(lapply(treeTS, function(x) x$dataSetName))
pagesid<-as.character(lapply(treeTS, function(x) x$paleoData_pages2kID))
proxy<-as.character(lapply(treeTS, function(x) x$paleoData_proxy))
site<-as.character(lapply(treeTS, function(x) x$geo_siteName))
location<-as.character(lapply(treeTS, function(x) x$geo_location))
elevation<-as.numeric(lapply(treeTS, function(x) x$geo_elevation))
spp<-as.character(lapply(treeTS, function(x) x$paleoData_archiveGenus))
lat<-as.numeric(lapply(treeTS, function(x) x$geo_latitude))
lon<-as.numeric(lapply(treeTS, function(x) x$geo_longitude))
seasonality<-as.character(lapply(treeTS, function(x) x$interpretation1_seasonality))
startyear<-as.numeric(lapply(treeTS, function(x) min(x$year)))
endyear<-as.numeric(lapply(treeTS, function(x) max(x$year)))
pub1_doi<-as.character(lapply(treeTS, function(x) x$pub1_DOI))
pub2_doi<-as.character(lapply(treeTS, function(x) x$pub2_DOI))
detrending_method<-as.character(lapply(treeTS, function(x) x$paleoData_detrendingMethod))

meta<-as.data.frame(cbind(name, proxy, site, location, lat, lon, elevation, spp, startyear, endyear, authors, pub1_doi, pub2_doi, detrending_method))



