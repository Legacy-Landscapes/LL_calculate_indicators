#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#        Aggregate the modelled carbon storage data       #
#         and change from tCmk2 to tC per grid cell        #
#                      Legacy Landscapes                  #
#                                                         #
#                       Alke June 2020                    #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear memory #-#-#
rm(list=ls())


#-#-# Load libraries #-#-#
library(raster)
library(rgdal)
library(tiff)


#-#-# Set filepaths #-#-#
datapath <- "/home/avoskamp/Legacy_landscapes/Carbon storage/CI_data/"


#-#-# Coordinates for region extent #-#-#
#newproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
coords <- read.csv(paste0(datapath,"Realm_coordinates_Lat_Lon.csv"))[,2:3]
head(coords)
baseraster <- rasterFromXYZ(coords)


#-#-# Derive the area per cell in km2 and change to ha #-#-#
area <- area(baseraster)
coordsA <- coordinates(area)
valsA <- getValues(area)
AreaDF <- as.data.frame(cbind(coordsA,valsA))
colnames(AreaDF) <- c("x","y","area_in_km2") 
AreaDF$area_in_ha <- AreaDF$area_in_km2 * 100


#-#-# Get the carbon storage data #-#-#
setwd(datapath)
x <- "Irrecoverable_C_Biomass_2018.tif" #<#<#<#<#<#<#<#<#<#<#< Change here and calculate for baseline, vulnerable and irreplaceable
data <- raster(x)
str(data)


#-#-# Aggregate the high resolution carbon data #-#-#
resfact<- res(baseraster)[1]/res(data)[1] # set factor to aggregate by
CarbonAgg <- aggregate(data,fact=resfact, fun=mean) # aggregate
CarbonAggRes <- resample(CarbonAgg,baseraster,method="bilinear") # align raster
coordCarbonAgg <- coordinates(CarbonAggRes)
valsCarbonAgg <- getValues(CarbonAggRes)
CarbonAggDF <- as.data.frame(cbind(coordCarbonAgg,valsCarbonAgg))
colnames(CarbonAggDF) <- c("x","y","Carbon_mean_t_per_ha") 
head(CarbonAggDF)


#-#-# Merge area and carbon dataframes and calculate total carbon per cell #-#-#
CarbonDF <- merge(AreaDF,CarbonAggDF,by=c("x","y"),all.x=T)
CarbonDF$TotalCarbon <- CarbonDF$Carbon_mean_t_per_ha * CarbonDF$area_in_ha

write.csv(CarbonDF,"Irreplaceable_0.5deg_CI_agg.csv")

test <- read.csv("Baseline_Carbon_0.5deg_CI_agg.csv")
min(na.omit(test$Carbon_mean_t_per_ha))
