#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#                                                               #
#      Extract current species data matrix and range extent     #  
#         All species included - original distributions         #
#                                                               #
#                     Alke - January 2020                       #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Load libraries #-#-#
library(snowfall)
library(raster)
library(snowfall)


## This code assumes all gridded species data for one taxa are stored in one folder


#-#-# Collate distribution data into species matrix #-#-#

#-#-# Set file paths #-#-#
filepath <- "/home/avoskamp/Legacy_landscapes/Species_data/reptiles/"
outpath <- "/home/avoskamp/Legacy_landscapes/Biodiversity_data/Species_data/"
ExtDataLocation <- "/home/avoskamp/Legacy_landscapes/Additional_files_needed/"

#-#-# Get list of files to process #-#-#
FileList <- list.files(filepath)

#-#-# Get coordinates file #-#-#
coords <- read.csv(paste0(ExtDataLocation,"Realm_coordinates_Lat_Lon.csv"))[2:3]

#-#-# Load data #-#-#
sfInit(parallel=TRUE, cpus=ceiling(0.5*parallel::detectCores()))
sfExport(list=c("filepath","FileList","coords"))
GetFiles <- sfLapply(FileList,function(x){
  data <- get(load(paste0(filepath,x)))
  coordsData <- merge(coords,data,by=c("x","y"),all.x=T)
  return(coordsData)
})
str(GetFiles)

#-#-# Merge into species matrix #-#-#
SpMatrix <- Reduce(function(...) merge(..., all=T), GetFiles)
head(SpMatrix)

#-#-# Save species matrix #-#-#
save(SpMatrix, file= paste0(outpath,"Reptiles_current_dist_all_species.RData"), compress="xz")



#---------# Derive range size per species and calculate range size rarity (endemism) #----------#


#-#-# Extract range extent and calculate range size rarity value for each species #-#-# 

#-#-# Need first derive area in km2 per grid cell to calculate area extent per species #-#-#
# ExtDataLocation <- "/home/avoskamp/Legacy_landscapes/Additional_files_needed/"
# grid <- read.csv(paste0(ExtDataLocation,"Realm_coordinates_Lat_Lon.csv"))[2:4]
# raster <- rasterFromXYZ(grid)
# cellSize <- area(raster)
# 
# gridKM <- as.data.frame(cellSize,xy=T)
# gridKM <- merge(grid,gridKM,by=c("x","y"),all.x=T)
# colnames(gridKM) <- c("x","y","Realm","km2")
# 
# write.csv(gridKM,paste0(ExtDataLocation,"Realm_coordinates_Lat_Lon_area.csv"))


#-#-# Set filepaths #-#-#
## With the current code structure this needs to be done per taxa
filepath <- "/home/avoskamp/Legacy_landscapes/Species_data/reptiles/" ##change her for taxa
outpath <- "/home/avoskamp/Legacy_landscapes/Biodiversity_data/"
ExtDataLocation <- "/home/avoskamp/Legacy_landscapes/Additional_files_needed/"


#-#-# Get coordinates and area file #-#-#
coords <- read.csv(paste0(ExtDataLocation,"Realm_coordinates_Lat_Lon_area.csv"))[c(2,3,5)]


#-#-# Get list of files to process #-#-#
FileList <- list.files(filepath)

#-#-# Extract ranges #-#-#
sfInit(parallel=TRUE, cpus=ceiling(0.5*parallel::detectCores()))
sfExport(list=c("filepath","FileList","coords"))
RangeData <- sfLapply(FileList,function(n){
  name <- paste0(strsplit(n,split=".Rdata")[[1]][1])
  species <- get(load(paste0(filepath,n)))
  colnames(species) <- c("x","y","val")
  species <- subset(species,val==1)
  NumCell <- nrow(species)
  extentData <- merge(species,coords,by=c("x","y"),all.x=T)
  extentKM2 <- sum(extentData$km2)
  data <- as.data.frame(do.call("cbind", list(name, NumCell, extentKM2)))
  colnames(data) <- c("SpName","NumCells","ExtentKm2")
  return(data)
})

#-#-# Merge files #-#-#
AllRanges <- do.call(rbind,RangeData)

#-#-# Save data file #-#-#
write.csv(AllRanges, file= paste0(outpath,"Reptiles_range_extent_all_species.csv"))
