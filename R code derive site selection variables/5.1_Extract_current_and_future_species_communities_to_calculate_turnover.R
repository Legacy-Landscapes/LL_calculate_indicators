#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#    Calculate current and future species communities     #
#                  to derive turnover                     #
#                                                         #
#                  Alke October 2019                      #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


rm(list=ls())

## Load libraries
library(raster)
library(rgdal)
library(maptools)
library(maps)
library(latticeExtra)
library(sp)
library(base)
library(lattice)
library(snowfall)


#-#-# Baseline #-#-#

## Get baseline species files
allSpecies <- list.files("/home/avoskamp/Legacy_landscapes/Climate stability/Reptile_GAM_GBM_results_climate_no_thres/")
allSpecies <- unlist(lapply(allSpecies,function(x) paste0(strsplit(x,"_")[[1]][1],"_",strsplit(x,"_")[[1]][2])))

## Baseline communities
sfInit(parallel=TRUE, cpus=ceiling(0.6*parallel::detectCores()))
predData <- sfLapply(allSpecies, function(x){ n <- read.csv(paste0("/home/avoskamp/Legacy_landscapes/Climate stability/Reptile_GAM_GBM_results_climate_no_thres/",x,"_ensemble_dispersal.csv.xz"))                                                        
n <- subset(n,dispersal2.x=="1") ## Change dispersal here
if(nrow(n)>=1){ ## There are a couple of species where the model is so off that there are no presences projected within the current range
n <-n[c("x","y","EWEMBI_1995")]
n$species <- x 
return(n)}})
sfStop()

predData <- data.table::rbindlist(predData)
head(predData)
predData <- tidyr::spread(predData, species, EWEMBI_1995)

head(predData)
levelplot(Abeillia_abeillei~x*y,data=predData)
nrow(predData)
ncol(predData)

## Save baseline communities
outpath <- "/home/avoskamp/Legacy_landscapes/Climate stability/Species_climate_results/"
save(predData,file=paste(outpath,"Cellcommunities_EWEMBI_reptiles_global_medium_dispersal_no_thres.Rdata",sep=""),compress="xz")


#-#-# Future #-#-#

## Get baseline species files
allSpecies <- list.files("/home/avoskamp/Legacy_landscapes/Climate stability/Reptile_GAM_GBM_results_climate_no_thres/")
allSpecies <- unlist(lapply(allSpecies,function(x) paste0(strsplit(x,"_")[[1]][1],"_",strsplit(x,"_")[[1]][2])))

## Future communities
sfInit(parallel=TRUE, cpus=ceiling(0.55*parallel::detectCores()))
predData <- sfLapply(allSpecies, function(x){ n <- read.csv(paste0("/home/avoskamp/Legacy_landscapes/Climate stability/Reptile_GAM_GBM_results_climate_no_thres/",x,"_ensemble_dispersal.csv.xz"))                                                        
n <- subset(n,dispersal2.x=="1") ## Change dispersal here
if(nrow(n)>=1){ 
  n <-n[c("x","y","S60_50")] ## Change RCP here
  n$species <- x 
  return(n)}})
sfStop()  

predData <- data.table::rbindlist(predData)
head(predData)
predData <- tidyr::spread(predData, species, S60_50)

head(predData)
nrow(predData)
ncol(predData)

## Save future communities
outpath <- "/home/avoskamp/Legacy_landscapes/Climate stability/Species_climate_results/"
save(predData,file=paste(outpath,"Cellcommunities_RCP60_2050_reptiles_global_medium_dispersal_no_thres.Rdata",sep=""),compress="xz")


