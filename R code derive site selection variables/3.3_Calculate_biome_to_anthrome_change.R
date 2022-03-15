#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#              Process the land cover data              #
#            Change frome Biome to Anthrome             #
# Based on fraction of land cover classes derived from  #     
#           ESI CCI land cover time series              #  
#     https://zenodo.org/record/3730469#.X3RHxJozZhE    #
#                                                       #
#                   September 2020                      #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear memory #-#-#
rm(list=ls())


#-#-# Load libraries #-#-#
library("sf")
library("raster")
library("ncdf4")
library("rasterVis")
library("RColorBrewer")
library("ggplot2")


#-#-# Set the filepath and names #-#-#
LUpath <- "E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Raster_data_layers/Recent land use change/"
LUname <- "frac10_allyears.nc"
Fullname <- paste0(LUpath,LUname)

#-#-# Get raster to match data to #-#-#
setwd("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Raster_data_layers/Biological realms Holt/")
realm <- get(load("Biological_Realms_gridded_05.Rdata"))
BaseRaster <- rasterFromXYZ(realm)

## Calculate change for 10, 20, 30, 40 and 190 which characterize the shift from biome to anthrome 
## See IPBES Global Assessment and IPCC Assessment Report 4

VariableList <- as.vector(c("fracCover_10","fracCover_20","fracCover_30","fracCover_40","fracCover_190")) # Select the raster layer 


#-#-# Extract raster vals and aggregate #-#-#
GetVals <- lapply(VariableList, function(x){
  OneVarRaster <- brick(Fullname,varname=x) ## selecting one LU variable
  
    layername <- x
    print(x)
  
      BothYears <- lapply(c(1,27),function(n){ ## selecting time slice 1 (1992) and 27 (2018)
      
      print(n)
    
      OneYear <- subset(OneVarRaster,n) # subset by time slice
      #OneYearAgg <- aggregate(OneYear, 18, fun=mean, expand=F, na.rm=TRUE) ## aggregate by factor x and take mean
      OneYearRes <- resample(OneYear, BaseRaster, method="bilinear") ## resample to match raster n analysis
    
      ## Extract values into dataframe
      coords <- coordinates(OneYearRes)
      vals <- values(OneYearRes)
      Data <- as.data.frame(cbind(coords,vals))
    
      ## Format dataframe
      name <- names(OneYearRes)
      colnames(Data) <- c("x","y",name)
      return(Data)
      })
    
    ## Combine time slices into one dataframe
    CombData <- Reduce(function(...) merge(..., all=T), BothYears)
    
    ## Format dataframe
    name1992 <- paste0(layername,"_",colnames(CombData[3]))
    name2018 <- paste0(layername,"_",colnames(CombData[4]))
    
    colnames(CombData) <- c("x","y",name1992,name2018)
    return(CombData)
})

LUChangeData <- Reduce(function(...) merge(..., all=T), GetVals)
head(LUChangeData)


#-#-# Add change from biome to anthrome between the two years #-#-#
LUChangeData$change_fc_10 <- LUChangeData$fracCover_10_X2018 - LUChangeData$fracCover_10_X1992
LUChangeData$change_fc_20 <- LUChangeData$fracCover_20_X2018 - LUChangeData$fracCover_20_X1992
LUChangeData$change_fc_30 <- LUChangeData$fracCover_30_X2018 - LUChangeData$fracCover_30_X1992
LUChangeData$change_fc_40 <- LUChangeData$fracCover_40_X2018 - LUChangeData$fracCover_40_X1992
LUChangeData$change_fc_190 <- LUChangeData$fracCover_190_X2018 - LUChangeData$fracCover_190_X1992
LUChangeData$Sum_fracCover_Change <- rowSums(LUChangeData[13:17]) # Summed change accross the five classes


#-#-# Save the landcover change file #-#-#
setwd("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Raster_data_layers/Recent land use change/")
write.csv(LUChangeData,"Biome_to_anthrome.csv")
# ABdata <- read.csv("Biome_to_anthrome.csv") 
# head(ABdata)
# realm <- get(load("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Raster_data_layers/Biological realms Holt/Biological_Realms_gridded_05.Rdata"))
# head(realm)
# unique(ABdata$Sum_fracCover_Change)
# test <- merge(realm,ABdata,by=c("x","y"),all.x=TRUE)

#-#-# Data check #-#-#
test <- rasterFromXYZ(LUChangeData[c("x","y","Sum_fracCover_Change")]) #change_fc_10 #fracCover_10_X2018 #fracCover_10_X1992
test[test == 0] <- NA
plot(test)


