#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#       Re-gridding the PE data from 1 degree to 0.5degree      # 
#               to match with the other datasets                #
#                                                               #
#                        Alke March 2020                        #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

rm(list=ls())


#-#-# Load libraries #-#-#
library(raster)
library(lattice)


#-#-# Set file path #-#-#
datapath <- "/home/avoskamp/Legacy_landscapes/PE/"
rasterpath <- "/home/avoskamp/Legacy_landscapes/Carbon storage/"


#-#-# Set projection #-#-#
newproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


#-#-# Get the resample raster #-#-#
RasterData <- read.csv(paste0(rasterpath,"Carbon_agg.csv"))
RasterData <- RasterData[c("x","y")]
Raster <- rasterFromXYZ(RasterData)
head(Raster)


#-#-# Get the data PE data #-#-#
PEdata <- read.csv(paste0(datapath,"PE_Reptiles.csv"))
PEdata <- PEdata[c("x","y","PE")]
PEraster <- rasterFromXYZ(PEdata,crs=newproj)
plot(PEraster)


#-#-# Resample the  raster #-#-#
PEraster05 <- resample(PEraster, Raster, method="bilinear")


#-#-# Extract values to dataframe #-#-#
coords <- as.data.frame(coordinates(PEraster05))
vals <- values(PEraster05)
PE_05 <- cbind(coords,vals)
head(PE_05)
levelplot(vals~x*y,data=PE_05)

write.csv(PE_05,paste0(datapath,"PE_Reptiles_05.csv"))
