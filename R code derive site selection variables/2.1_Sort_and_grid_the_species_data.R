#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#           Select terrestrial species included           #
#      in the analysis and format species data into       #
#        dataframes used for all further analysis         #
#                                                         #
#                   Alke January 2020                     #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#-#-# Clear memory #-#-#
rm(list=ls(all=TRUE))

## Original species data for all birds, mammals, amphibians and reptiles was downloaded from 
## IUCN, BirdLife International and GARD. All species range maps were gridded to 
## 0.5 degrees without using a threshold and saved as .tif files in one folder. Here we extract all terrestrial 
## species from the storage folder based on species lists, sort them into folders by taxa and format the 
## files for all further analysis.

#!#!# This file is not needed to replicate the analysis it used internally to process the above mentioned data which
#!#!# had already been downloaded and pre-processed for other purposes. To replicate the analysis downloading the range 
#!#!# maps for all species in the provided species list and gridding them to 0.5 degrees lat/lon will do 


#-#-# Split the species names by taxa and combine in list #-#-#

## Set working directory
setwd("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Species_data/SpeciesData_BioScen/")

## Birds
birds <- read.csv("ter_birds.csv")
head(birds)
spnames <- as.vector(birds$SCINAME)
birdnames <- lapply(spnames,function(x){
  print(x)
  two <- strsplit(x," ")
  full <- paste0(two[[1]][1],"_",two[[1]][2])
  data <- as.data.frame(cbind(full,"birds"))
  colnames(data) <- c("spname","taxa")
  return(data)
})
birdnames <- unique(do.call(rbind,birdnames))
head(birdnames)
nrow(birdnames)

## Mammals
mam <- read.csv("ter_mammals.csv")
head(mam)
spnames <- as.vector(mam$binomial)
mamnames <- lapply(spnames,function(x){
  print(x)
  two <- strsplit(x," ")
  full <- paste0(two[[1]][1],"_",two[[1]][2])
  data <- as.data.frame(cbind(full,"mammals"))
  colnames(data) <- c("spname","taxa")
  return(data)
})
mamnames <- unique(do.call(rbind,mamnames))
head(mamnames)
nrow(mamnames)

## Amphibians
amp <- read.csv("amphibians.csv")
head(amp)
spnames <- as.vector(amp$binomial)
ampnames <- lapply(spnames,function(x){
  print(x)
  two <- strsplit(x," ")
  full <- paste0(two[[1]][1],"_",two[[1]][2])
  data <- as.data.frame(cbind(full,"amphibians"))
  colnames(data) <- c("spname","taxa")
  return(data)
})
ampnames <- unique(do.call(rbind,ampnames))
head(ampnames)
nrow(ampnames)

# ## Reptiles
rep <- read.csv("gard_reptiles.csv")
head(rep)
spnames <- as.vector(rep$Binomial)
repnames <- lapply(spnames,function(x){
  print(x)
  two <- strsplit(x," ")
  full <- paste0(two[[1]][1],"_",two[[1]][2])
  data <- as.data.frame(cbind(full,"reptiles"))
  colnames(data) <- c("spname","taxa")
  return(data)
})
repnames <- unique(do.call(rbind,repnames))
head(repnames)
nrow(repnames)

AllSpecies <- do.call("rbind", list(birdnames, mamnames, ampnames, repnames))
head(AllSpecies)
nrow(AllSpecies)
write.csv(AllSpecies,"Names_Mam_Birds_Amp_Rep.csv")


#-#-# Grid and sort the .tif species files into folders by taxa (sort on cluster where data is located) #-#-# 

#-#-# Load libraries #-#-#
library("filesstrings")
library("raster")
library("snowfall")

#-#-# Get taxa list #-#-#
setwd("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Species_data/SpeciesData_BioScen/")
speciesList <- read.csv("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Species_data/SpeciesData_BioScen/Names_Mam_Birds_Amp_Rep.csv")

taxaList <- c("birds","mammals","amphibians","reptiles")

## Get realm coordinates
setwd("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Species_data/")
sp <- read.csv("Realm_coordinates_Lat_Lon.csv")
rastercells <- sp[2:3]
str(rastercells)
head(rastercells)
nrow(rastercells)
head(rastercells)

## Grid and sort files
sortSpecies <- lapply(taxaList,function(x){
  print(x)
  
  ## Get taxalist
  spList <- subset(speciesList,taxa==x)
  species <- as.vector(spList$spname)
  print(length(species))
  directory <- paste0("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Species_data/SpeciesData_BioScen/SpeciesData/")
  directoryII <- paste0("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Species_data/SpeciesData_BioScen/GARD_SpeciesData/")
  moveto <- paste0("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Species_data/SpeciesData_BioScen/",x,"/")
  
  ## Loop through species in parallel
  
  #lapply(species,function(n){
  sfInit(parallel=TRUE, cpus=ceiling(1*parallel::detectCores()))
  sfExport(list=c("species","directory","moveto","rastercells"))
  sfLibrary(raster) 
  sfLapply(species,function(n){
    spfile <- paste0(directory,n,"_0.5.tif")
    spfileII <- paste0(directoryII,n,"_0.5.tif")
    if(file.exists(paste0(directory,n,"_0.5.tif"))){
      if(!file.exists(paste0(moveto,n,".Rdata"))){
    spfile <- raster(spfile)
    #file.move(spfile, directory)
    name <- n
    print(name)
    coord <- round(coordinates(spfile),4)
    presence <- getValues(spfile)
    ObDist <- (as.data.frame(cbind(coord,presence)))
    ObDist[is.na(ObDist)] <- 0
    colnames(ObDist) <- c("x","y",name)
    rastercells <- merge(rastercells,ObDist,by=c("x","y"))
    rastercells[is.na(rastercells)] <- 0
    save(rastercells,file=paste0(moveto,name,".Rdata"),compress="xz")
    }
    }else{
      if(file.exists(paste0(directoryII,n,"_0.5.tif"))){
        if(!file.exists(paste0(moveto,n,".Rdata"))){
          spfile <- raster(spfileII)
          name <- n
          print(name)
          coord <- round(coordinates(spfile),4)
          presence <- getValues(spfile)
          ObDist <- (as.data.frame(cbind(coord,presence)))
          ObDist[is.na(ObDist)] <- 0
          colnames(ObDist) <- c("x","y",name)
          rastercells <- merge(rastercells,ObDist,by=c("x","y"))
          rastercells[is.na(rastercells)] <- 0
          save(rastercells,file=paste0(moveto,name,".Rdata"),compress="xz")
        }}}
  })
  sfStop() 
})

## faster with file.move(spfile, directory) if files are just moved - but can't be installed on cluster

#-#-# Test species #-#-# 
setwd("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Species_data/SpeciesData_BioScen/")
path <- "E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Species_data/SpeciesData_BioScen/"
pathII <- "E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Species_data/SpeciesData_BioScen/reptiles/"
speciesList <- read.csv(paste0(path,"Names_Mam_Birds_Amp_Rep.csv"))
head(speciesList)

subspecies <- as.vector(subset(speciesList,taxa=="reptiles")$spname)
str(subspecies)
SpeciesTest <- lapply(subspecies,function(x){
  print(x)
  if(!file.exists(paste0(pathII,x,".Rdata"))){return(x)}
})

MissingSp <- as.vector(do.call(rbind,SpeciesTest))

setwd("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Species_data/")
write.csv(MissingSp,"AmphibiansMissingInTiff.csv")

setwd("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Species_data/SpeciesData_BioScen/SpeciesData/")
Tiffs <- list.files()

TiffNames <- lapply(Tiffs,function(x){
  print(x)
  Name <- paste0(strsplit(x,"_")[[1]][1],"_",strsplit(x,"_")[[1]][2])
  return(Name)
})

TiffNames <- as.vector(do.call(rbind,TiffNames))
SpeciesNames <- as.vector(speciesList$spname)

Missing <- TiffNames[which(!TiffNames %in% SpeciesNames)]
write.csv(Missing,"TiffFilesNotInList.csv")
