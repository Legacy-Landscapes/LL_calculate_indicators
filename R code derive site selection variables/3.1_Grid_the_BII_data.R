#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#     Aggregate the BII data to a 0.5 degree resolution       #
#                                                             #
#                     Alke January 2020                       #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#-#-# Load libraries #-#-#
library(raster)
library(rgdal)


#-#-# Load the BII data as well as the realm data file as example raster #-#-#
#setwd("~/Documents/Legacy Landscape/Site selection analysis/")
coords <- read.csv("~/Documents/Legacy Landscape/Site selection analysis/Realm_coordinates_Lat_lon.csv")
head(coords)

## Rasterize the realm data to use as matching raster
rastercells <- coords[2:3]
head(rastercells)
baseraster <- rasterFromXYZ(rastercells)
str(baseraster)

newproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

BII <- "~/Desktop/lbii.asc"
BII <- raster(BII)
str(BII)

BIIraster <- projectRaster(BIIraster, crs=newproj)
BIIraster <- resample(BII, baseraster, method="bilinear")
plot(BIIraster)
coord <- round(coordinates(BIIraster),4)
values <- getValues(BIIraster)
BIIdata <- as.data.frame(cbind(coord,values))
head(BIIdata)

colnames(BIIdata) <- c("x","y","BII")

BIIdataLand <- merge(rastercells,BIIdata,by=c("x","y"),all.x=T)
head(BIIdataLand)
nrow(BIIdataLand)
nrow(rastercells)

write.csv(BIIdataLand,paste0("~/Documents/Legacy Landscape/Site selection analysis/","BIIdata.csv"))

