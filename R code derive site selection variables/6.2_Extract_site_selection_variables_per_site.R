#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#         Extracting values from the site selection       #
#               variables for each PA polygon             #            
#                                                         #
#                  Alke January 2020                      #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


rm(list=ls())


#-#-# Load libraries #-#-#
library(raster)
library(rgdal)
library(maptools)
library(maps)
library(latticeExtra)
library(sp)
library(base)
library(lattice)
library(snowfall)
library(dplyr)
library(ape)
library(picante)
library(phylobase)


#-#-#-#-#-#-# Load the data #-#-#-#-#-#-#
  

  #-#-# Set the filepaths #-#-#
  filepath <- "/home/avoskamp/Legacy_landscapes/"

  outpath <- "/home/avoskamp/Legacy_landscapes/PA_result_lists/Single_files_all_variables_with_country/"

  
  #-# PA gridded data
  PAgrid <- get(load(paste0(filepath,"PA_Data/Combined data global/Combined data global with country/Combined_data_global_IUCN_WHS_KBA_grid_country.Rdata")))

  #-# PA polygons
  PABuffers <- get(load(paste0(filepath,"PA_Data/Combined data global/Combined data global with country/Combined_data_global_IUCN_WHS_KBA_grid_country_Buffers.Rdata")))

  #-# Realms
  Realms <- get(load(paste0(filepath,"Additional_files_needed/Realms and Regions/CMEC regions & realms/","WWF_Realms_gridded_05.Rdata"))) # Read in the shapefile

  
  #---#---# Objective 1 Biodiversity #---#---#
  
    #-# Species richness data
    SpMatrixOrigBird <- get(load(paste0(filepath,"Biodiversity_data/Species_data/","Birds_current_dist_all_species.RData")))
    SpMatrixOrigBird[SpMatrixOrigBird == 0] <- NA
  
    SpMatrixOrigMammal <- get(load(paste0(filepath,"Biodiversity_data/Species_data/","Mammals_current_dist_all_species.RData")))
    SpMatrixOrigMammal[SpMatrixOrigMammal == 0] <- NA
  
    SpMatrixOrigAmphibian <- get(load(paste0(filepath,"Biodiversity_data/Species_data/","Amphibians_current_dist_all_species.RData")))
    SpMatrixOrigAmphibian[SpMatrixOrigAmphibian == 0] <- NA

    SpMatrixOrigReptile <- get(load(paste0(filepath,"Biodiversity_data/Species_data/","Reptiles_current_dist_all_species.RData")))
    SpMatrixOrigReptile[SpMatrixOrigReptile == 0] <- NA
  
  
    #-# Species range size rarity
    RarityBird <- read.csv(paste0(filepath,"Biodiversity_data/","Birds_range_extent_all_species.csv"))
    RarityBird$RSR_bird <- 1/RarityBird$ExtentKm2
  
    RarityMammal <- read.csv(paste0(filepath,"Biodiversity_data/","Mammals_range_extent_all_species.csv"))
    RarityMammal$RSR_mammal <- 1/RarityMammal$ExtentKm2
  
    RarityAmphibian <- read.csv(paste0(filepath,"Biodiversity_data/","Amphibians_range_extent_all_species.csv"))
    RarityAmphibian$RSR_amphibian <- 1/RarityAmphibian$ExtentKm2

    RarityReptile <- read.csv(paste0(filepath,"Biodiversity_data/","Reptiles_range_extent_all_species.csv"))
    RarityReptile$RSR_reptile <- 1/RarityReptile$ExtentKm2
  
    #-# Phylogenetic endemism
    PE_birds <- read.csv(paste0(filepath,"PE/","PE_Birds_05.csv"))
    PE_mammals <- read.csv(paste0(filepath,"PE/","PE_Mammals_05.csv"))
    PE_amphibians <- read.csv(paste0(filepath,"PE/","PE_Amphibians_05.csv"))
    PE_reptiles <- read.csv(paste0(filepath,"PE/","PE_Reptiles_05.csv"))
  
  
  #---#---# Objective 2 Ecosystem integrity #---#---#
  
    #-# Biodiversity Intacness Index
    BII <- read.csv(paste0(filepath,"BII/","BIIdata.csv"))
  
    #-# Human footprint
    HFP <- read.csv(paste0(filepath,"Human_footprint_data/","HFP_2009_05_grid_ter_only.csv"))
  
    #-# Biome to anthrome
    BiomeAnthrome <- read.csv(paste0(filepath,"BiomeToAnthrome/","Biome_to_anthrome.csv"))
    BiomeAnthrome <- BiomeAnthrome[c("x","y","Sum_fracCover_Change")]
  
  
  #---#---# Objective 3 Climate protection #---#---#
  
    #-# Carbon storage
    CarbonBase <- read.csv(paste0(filepath,"Carbon storage/CI_data/","Baseline_Carbon_0.5deg_CI_agg.csv"))
    CarbonBase <- CarbonBase[c("x","y","Carbon_mean_t_per_ha")] # Replaced total carbon by carbon per ha
    colnames(CarbonBase) <- c("x","y","Carbon")
  
    CarbonVulnerable <- read.csv(paste0(filepath,"Carbon storage/CI_data/","Vulnerable_Carbon_0.5deg_CI_agg.csv"))
    CarbonVulnerable <- CarbonVulnerable[c("x","y","Carbon_mean_t_per_ha")]
    colnames(CarbonVulnerable) <- c("x","y","Carbon")
  
    CarbonIrrcoverable <- read.csv(paste0(filepath,"Carbon storage/CI_data/","Irreplaceable_Carbon_0.5deg_CI_agg.csv"))
    CarbonIrrcoverable <- CarbonIrrcoverable[c("x","y","Carbon_mean_t_per_ha")]
    colnames(CarbonIrrcoverable) <- c("x","y","Carbon")
  
    
  # -> Objective 4 Size data is included in the spatial polygon data of the PAs
    
    
  #---#---# Objective 5 Land-use stability #---#---#
  
    #-# Land-use change
    LandUseChange <- read.csv(paste0(filepath,"LandUse/","landuse_change.csv"))
    LandUseChange <- subset(LandUseChange,scenario=="rcp60" & year == "2050")
    LUCbiocrop <- subset(LandUseChange,var=="biofuel_cropland")
    LUCcrop <- subset(LandUseChange,var=="cropland")
    LUCpastures <- subset(LandUseChange,var=="pastures")
  
    
  #---#---# Objective 6 Climatic stability #---#---#  
  
    #-# Turnover of species
    currentComTObird <- get(load(paste0(filepath,"Climate_stability/Species_climate_results/Final_data_matrix/","Cellcommunities_EWEMBI_birds_global_medium_dispersal_no_thres.Rdata")))
    futureComTObird <- get(load(paste0(filepath,"Climate_stability/Species_climate_results/Final_data_matrix/","Cellcommunities_RCP60_2050_birds_global_medium_dispersal_no_thres.Rdata")))

    currentComTOmammal <- get(load(paste0(filepath,"Climate_stability/Species_climate_results/Final_data_matrix/","Cellcommunities_EWEMBI_mammals_global_medium_dispersal_no_thres.Rdata")))
    futureComTOmammal <- get(load(paste0(filepath,"Climate_stability/Species_climate_results/Final_data_matrix/","Cellcommunities_RCP60_2050_mammals_global_medium_dispersal_no_thres.Rdata")))
  
    currentComTOamphibian <- get(load(paste0(filepath,"Climate_stability/Species_climate_results/Final_data_matrix/","Cellcommunities_EWEMBI_amphibians_global_medium_dispersal_no_thres.Rdata")))
    futureComTOamphibian <- get(load(paste0(filepath,"Climate_stability/Species_climate_results/Final_data_matrix/","Cellcommunities_RCP60_2050_amphibians_global_medium_dispersal_no_thres.Rdata")))
  
    currentComTOreptile <- get(load(paste0(filepath,"Climate_stability/Species_climate_results/Final_data_matrix/","Cellcommunities_EWEMBI_reptiles_global_medium_dispersal_no_thres.Rdata")))
    futureComTOreptile <- get(load(paste0(filepath,"Climate_stability/Species_climate_results/Final_data_matrix/","Cellcommunities_RCP60_2050_reptiles_global_medium_dispersal_no_thres.Rdata")))
  
  
    #-# Change in tree cover
    TreeCover <- read.csv(paste0(filepath,"TreeCoverChange/","Tree_cover_change_RCP60_2050.csv"))
  
 
#-#-#-#-#-#-# Set functions needed #-#-#-#-#-#-#    
  delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
    x.list[unlist(lapply(x.list, length) != 0)]
  } 

  
#-#-#-#-#-#-# Extract site selection values for each PA #-#-#-#-#-#-#
source("/home/avoskamp/Legacy_landscapes/Rcode/Calc_LL_Variables_country.R")  
  
# sfInit(parallel=TRUE, cpus=ceiling(0.2*parallel::detectCores()))
# sfExport(list=c("CalcPAdata","PAgrid","PABuffers","Realms","SpMatrixOrig","Rarity","HFP","SpmatrixMPD","tr","PE","currentComTO",
#                 "futureComTO","Carbon","BiomeAnthrome","TreeCover","BII","LUCbiocrop","LUCcrop","LUCpastures","delete.NULLs","outpath"))
# sfLibrary(raster);sfLibrary(rgdal);sfLibrary(maptools);sfLibrary(maps);sfLibrary(latticeExtra);sfLibrary(sp);sfLibrary(base);
# sfLibrary(lattice);sfLibrary(dplyr);sfLibrary(ape);sfLibrary(picante);sfLibrary(phylobase);

ExtractPAdata <- lapply(PAgrid[1:1352],function(PA){
  print(PA)
  cat("\n",as.character(PA$Int_Name))
  PAname <- PA$IntName

  ## Extract site ID for file name
  SP_ID <- as.character(PA$SP_ID)
  print(SP_ID)
  CalcPAdata(PA=PA,
            PAgrid=PAgrid,
            SP_ID=SP_ID,
            PABuffers=PABuffers,
            Realms=Realms,
            SpMatrixOrigBird=SpMatrixOrigBird, 
            SpMatrixOrigMammal=SpMatrixOrigMammal,
            SpMatrixOrigAmphibian=SpMatrixOrigAmphibian,
            SpMatrixOrigReptile=SpMatrixOrigReptile,
            RarityBird=RarityBird,
            RarityMammal=RarityMammal,
            RarityAmphibian=RarityAmphibian,
            RarityReptile=RarityReptile,
            HFP=HFP,
            PE_birds=PE_birds,
            PE_mammals=PE_mammals,
            PE_amphibians=PE_amphibians,
            PE_reptiles=PE_reptiles,
            currentComTObird=currentComTObird,
            futureComTObird=futureComTObird,
            currentComTOmammal=currentComTOmammal,
            futureComTOmammal=futureComTOmammal,
            currentComTOamphibian=currentComTOamphibian,
            futureComTOamphibian=futureComTOamphibian,
            currentComTOreptile=currentComTOreptile,
            futureComTOreptile=futureComTOreptile,
            TreeCover=TreeCover,
            CarbonBase=CarbonBase,
            CarbonVulnerable=CarbonVulnerable,
            CarbonIrrcoverable=CarbonIrrcoverable,
            BiomeAnthrome=BiomeAnthrome,
            BII=BII,
            LUCbiocrop=LUCbiocrop,
            LUCcrop=LUCcrop,
            LUCpastures=LUCpastures,
            delete.NULLs=delete.NULLs,
            outpath=outpath)
  
})

#sfStop()  




#-#-# Summarize PA files into one dataframe #-#-#
setwd("/home/avoskamp/Legacy_landscapes/PA_result_lists/Single_files_all_variables_with_country/")
all <- list.files()

stitch <- lapply(all,function(n){
  print(n)
  data <- get(load(n))
  return(data)
})

Final <- do.call(rbind,stitch)
save(Final,file=paste0("/home/avoskamp/Legacy_landscapes/PA_result_lists/","Final_dataset_IUCN_WHS_KBA_country.Rdata"),compress="xz")
