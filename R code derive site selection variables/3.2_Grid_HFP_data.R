#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#             Grid the human footprint data               #
#                    Venter et al 2016                    #            
#                                                         #
#                    Alke January 2020                    #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear memory #-#-#
rm(list=ls())


#-#-# Load libraries #-#-#
library(raster)
library(rgdal)


#-#-# Load realm coordinates dataframe as example raster #-#-#
setwd("/home/avoskamp/")
sp <- read.csv("Realm_coordinates_Lat_Lon.csv")
rastercells <- sp[2:3]
str(rastercells)
head(rastercells)
nrow(rastercells)
head(rastercells)
baseraster <- rasterFromXYZ(rastercells)
newproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 


#-#-# Load the individual HFP raster files #-#-#
## Files are processed one by one 
setwd("/home/avoskamp/Legacy_landscapes/Human_footprint_data/Maps/")
x <- "croplands2005.tif" # <--- Change here
data <- raster(x)
str(data)


#-#-# Set target projection and re-project raster file #-#-#
newproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
data <- projectRaster(data, crs=newproj)


#-#-# Resample HFP raster to example raster #-#-#
HFPraster <- resample(data, baseraster, method="bilinear")


#-#-# Extract coordinates and data from raster into dataframe #-#-#
coord <- round(coordinates(HFPraster),4)
HFP <- getValues(HFPraster)
HFPDF <- (as.data.frame(cbind(coord,HFP)))
HFPDF[is.na(HFPDF)] <- 0
colnames(HFPDF) <- c("x","y","croplands_2005") # <--- Change here
head(HFPDF)


#-#-# Save output raster and dataframe #-#-#
Outpath <- "/home/avoskamp/Legacy_landscapes/Human_footprint_data/"
save(HFPDF,file=paste0(Outpath,"croplands_2005_05_grid.Rdata"),compress="xz") #<#<#<#<#<#<#<#<#<#<#< Change here
setwd("/home/avoskamp/Legacy_landscapes/Human_footprint_data/")
write.csv(HFPDF,"croplands_2005_05_grid.csv") # <--- Change here


#-#-# Check the resulting data visually #-#-#
setwd("E:/Legacy Landscapes/Data_layers/Human_footprint/Gridded_HFP/")
data <- read.csv("croplands_2005_05_grid.csv") # <--- Change here
head(data)
library(lattice)
levelplot(croplands_2005~x*y,data=data)

#-#-# Merge to land cells to reduce size #-#-#
sp <- read.csv(paste0("E:/Legacy Landscapes/Data_layers/Species_richness/","Realm_coordinates_Lat_Lon.csv"))
sp <- sp[2:3]
str(sp)
head(sp)
nrow(sp)
combdata <- merge(sp,data,all.x=TRUE)
head(combdata)
levelplot(croplands_2005~x*y,data=combdata) # <--- Change here
write.csv(combdata,"E:/Legacy Landscapes/Data_layers/Human_footprint/Gridded_HFP/croplands_2005_05_grid_ter_only.csv")
