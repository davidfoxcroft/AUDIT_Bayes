library(XML)
library(googleVis)
datasetname<-c("ARUK_dataset2_full_responders_cleaned_WITH_RECODES_DEIDENTIFIED_with_DEPR.csv")
dataset <- read.csv(file=datasetname, header=TRUE, stringsAsFactors=FALSE)


study.postcodes <- unlist(dataset$depr.standard_postcode[!is.na(dataset$depr.standard_postcode)])
study.postcodes.short <- substr(study.postcodes,1,nchar(study.postcodes)-3)

ll <- lapply(study.postcodes,
             function(str){
               u <- paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
               doc <-  xmlTreeParse(u, useInternal=TRUE)
               lat=xpathApply(doc,'/GeocodeResponse/result/geometry/location/lat',xmlValue)[1]
               lng=xpathApply(doc,'/GeocodeResponse/result/geometry/location/lng',xmlValue)[1]
               Sys.sleep(0.2)
               c(code = str,lat = lat, lng = lng)
             })


llshort <- lapply(study.postcodes.short,
             function(str){
               u <- paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
               doc <-  xmlTreeParse(u, useInternal=TRUE)
               lat=xpathApply(doc,'/GeocodeResponse/result/geometry/location/lat',xmlValue)[1]
               lng=xpathApply(doc,'/GeocodeResponse/result/geometry/location/lng',xmlValue)[1]
               Sys.sleep(0.2)
               c(code = str,lat = lat, lng = lng)
             })


lldata <- data.frame(do.call(rbind,ll))
lldata.short <- data.frame(do.call(rbind,llshort))

#format the latlong coordinates into the proper format lat:long
#latLongFormatted<-lapply(lldata,FUN=function(x){
#  paste(lldata$lat,lldata$lng,sep=":")
#})
latLongFormatted<-lapply(lldata.short,FUN=function(x){
  paste(lldata.short$lat,lldata.short$lng,sep=":")
})

plotData<-data.frame(latlong=latLongFormatted)
save(plotData, file="sites.Rdata")

sites<-gvisMap(plotData,locationvar="latlong.lng", options=list(mapType='normal'))
#plot(sites)
