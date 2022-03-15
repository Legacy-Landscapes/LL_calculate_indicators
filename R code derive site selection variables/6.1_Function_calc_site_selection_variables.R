CalcPAdata <- function(PA,PAgrid,SP_ID,PABuffers,Realms,SpMatrixOrigBird,SpMatrixOrigMammal,SpMatrixOrigAmphibian,SpMatrixOrigReptile,
                       RarityBird,RarityMammal,RarityAmphibian,RarityReptile,HFP,PE_birds,PE_mammals,PE_amphibians,PE_reptiles, 
                       currentComTObird,futureComTObird,currentComTOmammal,futureComTOmammall,currentComTOamphibian,
                       futureComTOamphibian,currentComTOreptile,futureComTOreptile,TreeCover,CarbonBase=CarbonBase,CarbonVulnerable=CarbonVulnerable,
                       CarbonIrrcoverable=CarbonIrrcoverable,BiomeAnthrome,BII,LUCbiocrop,LUCcrop,LUCpastures,delete.NULLs=delete.NULLs,outpath=outpath){ 
 
      
      #-#-# Get the PA name #-#-#
      PAname <- PA$IntName
      
      
      #-#-# Check if file exists #-#-#
      if(!file.exists(paste0(outpath,PAname,".Rdata"))){
      
        
      #-#-# Get PA data and merge with indicator data #-#-#
      PACells <- PA$data
      datatest <- nrow(PACells)
      
      if(datatest > 0){
        
        #-#-# Get the Realm information #-#-#
        RealmData <- merge(PACells,Realms,by=c("x","y"), all.x=T)
        RealmNr <- unique(RealmData$RealmWWF)
        RealmNr <- na.omit(RealmNr)
        RealmNr <- RealmNr[1]
        
        
        #---#---# Objective 1 Biodiversity #---#---#
        
        #-#-# Species richness for the different taxa #-#-#
        #-# birds
        CellsSR_B <- merge(PACells,SpMatrixOrigBird,by=c("x","y"), all.x=T) ## Merge with the current species lists per cell
        CellsSR_B <- CellsSR_B %>% select_if(~sum(!is.na(.)) > 0) ## Subset to only species that occur in the PA
        
        if(ncol(CellsSR_B) >= 5){ ## If a marine/coastal PA slipped through that does not have any terrestrial species it will be filtered out here
        SR_B <- (ncol(CellsSR_B)) -4
        SR_B_Prob <- ((ncol(CellsSR_B)) -4)/(ncol(SpMatrixOrigBird)-2)}else{ # not weighted
        SR_B <- 0} 
          
        #-# mammals
        CellsSR_M <- merge(PACells,SpMatrixOrigMammal,by=c("x","y"), all.x=T) ## Merge with the current species lists per cell
        CellsSR_M <- CellsSR_M %>% select_if(~sum(!is.na(.)) > 0) ## Subset to only species that occur in the PA
        
        if(ncol(CellsSR_M) >= 5){
        SR_M <- (ncol(CellsSR_M)) -4
        SR_M_Prob <- ((ncol(CellsSR_M)) -4)/(ncol(SpMatrixOrigMammal)-2)}else{ # not weighted
        SR_M <- 0}  
        
        #-# amphibians
        CellsSR_A <- merge(PACells,SpMatrixOrigAmphibian,by=c("x","y"), all.x=T) ## Merge with the current species lists per cell
        CellsSR_A <- CellsSR_A %>% select_if(~sum(!is.na(.)) > 0) ## Subset to only species that occur in the PA
          
        if(ncol(CellsSR_A) >= 5){
        SR_A <- (ncol(CellsSR_A)) -4
        SR_A_Prob <- ((ncol(CellsSR_A)) -4)/(ncol(SpMatrixOrigAmphibian)-2)}else{ # not weighted
        SR_A <- 0} 
        
        #-# reptiles
        CellsSR_R <- merge(PACells,SpMatrixOrigReptile,by=c("x","y"), all.x=T) ## Merge with the current species lists per cell
        CellsSR_R <- CellsSR_R %>% select_if(~sum(!is.na(.)) > 0) ## Subset to only species that occur in the PA
        
        if(ncol(CellsSR_R) >= 5){
        SR_R <- (ncol(CellsSR_R)) -4
        SR_R_Prob <- ((ncol(CellsSR_R)) -4)/(ncol(SpMatrixOrigReptile)-2)}else{ # not weighted
        SR_R <- 0}  
            
        
        #-#-# Range size rarity for the different taxa #-#-#
        #-# birds
        if(ncol(CellsSR_B) >= 5){
          NamesSR_B <- as.data.frame(colnames(CellsSR_B[5:ncol(CellsSR_B)]))
          colnames(NamesSR_B) <- c("SpName")
          
          RSRdata_B <- merge(NamesSR_B,RarityBird,by=c("SpName"))
          RSR_B <- sum(RSRdata_B$RSR_bird)/nrow(NamesSR_B)}else{
          RSR_B <- NA}
        
        #-# mammals
        if(ncol(CellsSR_M) >= 5){
          NamesSR_M <- as.data.frame(colnames(CellsSR_M[5:ncol(CellsSR_M)]))
          colnames(NamesSR_M) <- c("SpName")
        
          RSRdata_M <- merge(NamesSR_M,RarityMammal,by=c("SpName"))
          RSR_M <- sum(RSRdata_M$RSR_mammal)/nrow(NamesSR_M)}else{  
          RSR_M <- NA}  
            
        #-# amphibians
        if(ncol(CellsSR_A) >= 5){
          NamesSR_A <- as.data.frame(colnames(CellsSR_A[5:ncol(CellsSR_A)]))
          colnames(NamesSR_A) <- c("SpName")
        
          RSRdata_A <- merge(NamesSR_A,RarityAmphibian,by=c("SpName"))
          RSR_A <- sum(RSRdata_A$RSR_amphibian)/nrow(NamesSR_A)}else{       
          RSR_A <- NA}   
        
        #-# reptiles
        if(ncol(CellsSR_R) >= 5){
          NamesSR_R <- as.data.frame(colnames(CellsSR_R[5:ncol(CellsSR_R)]))
          colnames(NamesSR_R) <- c("SpName")
          
          RSRdata_R <- merge(NamesSR_R,RarityReptile,by=c("SpName"))
          RSR_R <- sum(RSRdata_R$RSR_reptile)/nrow(NamesSR_R)}else{       
          RSR_R <- NA}
        
          
        #-#-# Evolutionary diversity - Phylogenetic endemism #-#-#
        #-# birds
        PEdataBirds  <- merge(PACells,PE_birds,by=c("x","y"))
        if(nrow(PEdataBirds)>=1){ 
          PE_WMean_Birds <- weighted.mean(PEdataBirds[,"PE"],PEdataBirds[,"pcArea"],na.rm=T)
          PE_Max_Birds <- max(PEdataBirds[,"PE"],na.rm=T)
        }else{
          PE_WMean_Birds <- NA
          PE_Max_Birds <- NA
        }
          
        #-# mammals
        PEdataMammals  <- merge(PACells,PE_mammals,by=c("x","y"))
        if(nrow(PEdataMammals)>=1){ 
          PE_WMean_Mammals <- weighted.mean(PEdataMammals[,"vals"],PEdataMammals[,"pcArea"],na.rm=T)
          PE_Max_Mammals <- max(PEdataMammals[,"vals"],na.rm=T)
        }else{
          PE_WMean_Mammals <- NA
          PE_Max_Mammals <- NA
        }
          
        #-# amphibians
        PEdataAmphibians  <- merge(PACells,PE_amphibians,by=c("x","y"))
        if(nrow(PEdataAmphibians)>=1){ 
          PE_WMean_Amphibians <- weighted.mean(PEdataAmphibians[,"vals"],PEdataAmphibians[,"pcArea"],na.rm=T)
          PE_Max_Amphibians <- max(PEdataAmphibians[,"vals"],na.rm=T)
        }else{
          PE_WMean_Amphibians <- NA
          PE_Max_Amphibians <- NA
        }
        
        #-# reptiles
        PEdataReptiles  <- merge(PACells,PE_reptiles,by=c("x","y"))
        if(nrow(PEdataReptiles)>=1){ 
          PE_WMean_Reptiles <- weighted.mean(PEdataReptiles[,"vals"],PEdataReptiles[,"pcArea"],na.rm=T)
          PE_Max_Reptiles <- max(PEdataReptiles[,"vals"],na.rm=T)
        }else{
          PE_WMean_Reptiles <- NA
          PE_Max_Reptiles <- NA
        }
        
        
        #---#---# Objective 2 Ecosystem integrity #---#---#
          
          #-#-# Biodiversity intactness index #-#-#
          BIIdata <- merge(PACells,BII,by=c("x","y"),all.x=T)
          BII_WMean <- weighted.mean(BIIdata[,"BII"],BIIdata[,"pcArea"],na.rm=T)
          BII_Max <- max(BIIdata[,"BII"],na.rm=T)
          
          
          #-#-# Human footprint #-#-#
          #-# within the site
          HFPdata  <- merge(PACells,HFP,by=c("x","y"),all.x=T)
          HFP_WMean <- weighted.mean(HFPdata[,"HFP_2009"],HFPdata[,"pcArea"],na.rm=T)
          HFP_Max <- max(HFPdata[,"HFP_2009"],na.rm=T)
          
          ## around the site
          PABuffer <- lapply(1:length(PABuffers), function(x){
            if(PABuffers[[x]]$IntName == PAname){return(PABuffers[[x]])}})
          PABuffer<- delete.NULLs(PABuffer)
          PABufferdata <- PABuffer[[1]]$data
          
          PABufferHFP <- merge(PABufferdata,HFP,by=c("x","y"),all.x=T)
          HFPRisk_around <- weighted.mean(PABufferHFP[,"HFP_2009"],PABufferHFP[,"pcArea"],na.rm=T)
          
          
          #-#-# Biome to anthrome #-#-#
          BioAnthData <- merge(PACells,BiomeAnthrome,by=c("x","y"),all.x=T)
          BioAnth_WMean <- weighted.mean(BioAnthData[,"Sum_fracCover_Change"],BioAnthData[,"pcArea"],na.rm=T)
          BioAnth_Max <- max(BioAnthData[,"Sum_fracCover_Change"],na.rm=T)
          
          
        #---#---# Objective 3 Climate protection #---#---#
          
          # #-#-# Carbon storage #-#-#
          BaseCarbon <- merge(PACells,CarbonBase,by=c("x","y"),all.x=T)
          BaseCarbon_WMean <- weighted.mean(BaseCarbon[,"Carbon"],BaseCarbon[,"pcArea"],na.rm=T)
          BaseCarbon_WSum <- sum(BaseCarbon[,"Carbon"],BaseCarbon[,"pcArea"],na.rm=T)
          
          VulCarbon <- merge(PACells,CarbonVulnerable,by=c("x","y"),all.x=T)
          VulCarbon_WMean <- weighted.mean(VulCarbon[,"Carbon"],VulCarbon[,"pcArea"],na.rm=T)
          VulCarbon_WSum <- sum(VulCarbon[,"Carbon"],VulCarbon[,"pcArea"],na.rm=T)
          
          IrrCarbon <- merge(PACells,CarbonIrrcoverable,by=c("x","y"),all.x=T)
          IrrCarbon_WMean <- weighted.mean(IrrCarbon[,"Carbon"],IrrCarbon[,"pcArea"],na.rm=T)
          IrrCarbon_WSum <- sum(IrrCarbon[,"Carbon"],IrrCarbon[,"pcArea"],na.rm=T)
          
          
        # -> Objective 4 Size is extracted at the end together with the basic site information
            
          
        #---#---# Objective 5 Land-use stability #---#---#
          
          #-#-# Land-use change #-#-#
          PABuffer <- lapply(1:length(PABuffers), function(x){
            if(PABuffers[[x]]$IntName== PAname){return(PABuffers[[x]])}})
          PABuffer<- delete.NULLs(PABuffer)
          PABufferdata <- PABuffer[[1]]$data
          
          PABufferBioCrop <- merge(PABufferdata,LUCbiocrop,by=c("x","y"),all.x=T)
          BioCropRisk <- weighted.mean(PABufferBioCrop[,"value"],PABufferBioCrop[,"pcArea"],na.rm=T)
          
          PABufferCrop <- merge(PABufferdata,LUCcrop,by=c("x","y"),all.x=T)
          CropRisk <- weighted.mean(PABufferCrop[,"value"],PABufferCrop[,"pcArea"],na.rm=T)
          
          PABufferPastures <- merge(PABufferdata,LUCpastures,by=c("x","y"),all.x=T)
          PastureRisk <- weighted.mean(PABufferPastures[,"value"],PABufferPastures[,"pcArea"],na.rm=T)
          
          
        #---#---# Objective 6 Climatic stability #---#---#
          
          #-#-# Climatic stability #-#-#
          #-# birds
          CellsSRTObird <- merge(PACells,currentComTObird,by=c("x","y"), all.x=T) ## Merge with the current species lists per cell
          CellsSRTObird[is.na(CellsSRTObird)] <- 0 ## Replace all NAs by 0 - if taking a mean 0 cells need to stay
          
          #-# Current mean probability of occurrence per species
          Cur_meanbird <- stack(apply(CellsSRTObird[5:ncol(CellsSRTObird)], 2, mean)) ## Mean probability of occurrence for all species
          Cur_meanbird <- tidyr::spread(Cur_meanbird, ind, values) ## Structure result into data frame
          
          #-# Current weighted mean probability of occurrence per species
          Cur_wt_meanbird <- stack(apply(CellsSRTObird[5:ncol(CellsSRTObird)], 2, weighted.mean, CellsSRTObird[,"pcArea"])) ## Weighted mean probability of occurrence for all species
          Cur_wt_meanbird <- tidyr::spread(Cur_wt_meanbird, ind, values) ## Structure result into data frame
          
          #-# Get future species list for PA
          CellsSRTOFutbird <- merge(PACells,futureComTObird,by=c("x","y"), all.x=T) ## Merge with the future species lists per cell
          CellsSRTOFutbird[is.na(CellsSRTOFutbird)] <- 0 ## Replace all NAs by 0 - if taking a mean 0 cells need to stay
          
          #-# Future mean probability of occurrence per species
          Fut_meanbird <- stack(apply(CellsSRTOFutbird[5:ncol(CellsSRTOFutbird)], 2, mean)) ## Mean probability of occurrence for all species
          Fut_meanbird <- tidyr::spread(Fut_meanbird, ind, values) ## Structure result into data frame
          
          #-# Future weighted mean probability of occurrence per species
          Fut_wt_meanbird <- stack(apply(CellsSRTOFutbird[5:ncol(CellsSRTOFutbird)], 2, weighted.mean, CellsSRTOFutbird[,"pcArea"])) ## Weighted mean probability of occurrence for all species
          Fut_wt_meanbird <- tidyr::spread(Fut_wt_meanbird, ind, values) ## Structure result into data frame
          
          #-# Calculate turnover
          TO_PAbird <- sum(abs(Fut_meanbird-Cur_meanbird))/(sum(Cur_meanbird,na.rm=T)+sum(Fut_meanbird,na.rm=T))
          TO_PA_weightbird <- sum(abs(Fut_wt_meanbird-Cur_wt_meanbird))/(sum(Cur_wt_meanbird,na.rm=T)+sum(Fut_wt_meanbird,na.rm=T))
          
          #-# mammals
          CellsSRTOmammal <- merge(PACells,currentComTOmammal,by=c("x","y"), all.x=T) ## Merge with the current species lists per cell
          CellsSRTOmammal[is.na(CellsSRTOmammal)] <- 0 ## Replace all NAs by 0 - if taking a mean 0 cells need to stay
          
          #-# Current mean probability of occurrence per species
          Cur_meanmammal <- stack(apply(CellsSRTOmammal[5:ncol(CellsSRTOmammal)], 2, mean)) ## Mean probability of occurrence for all species
          Cur_meanmammal <- tidyr::spread(Cur_meanmammal, ind, values) ## Structure result into data frame
          
          #-# Current weighted mean probability of occurrence per species
          Cur_wt_meanmammal <- stack(apply(CellsSRTOmammal[5:ncol(CellsSRTOmammal)], 2, weighted.mean, CellsSRTOmammal[,"pcArea"])) ## Weighted mean probability of occurrence for all species
          Cur_wt_meanmammal <- tidyr::spread(Cur_wt_meanmammal, ind, values) ## Structure result into data frame
          
          #-# Get future species list for PA
          CellsSRTOFutmammal <- merge(PACells,futureComTOmammal,by=c("x","y"), all.x=T) ## Merge with the future species lists per cell
          CellsSRTOFutmammal[is.na(CellsSRTOFutmammal)] <- 0 ## Replace all NAs by 0 - if taking a mean 0 cells need to stay
          
          #-# Future mean probability of occurrence per species
          Fut_meanmammal <- stack(apply(CellsSRTOFutmammal[5:ncol(CellsSRTOFutmammal)], 2, mean)) ## Mean probability of occurrence for all species
          Fut_meanmammal <- tidyr::spread(Fut_meanmammal, ind, values) ## Structure result into data frame
          
          #-# Future weighted mean probability of occurrence per species
          Fut_wt_meanmammal <- stack(apply(CellsSRTOFutmammal[5:ncol(CellsSRTOFutmammal)], 2, weighted.mean, CellsSRTOFutmammal[,"pcArea"])) ## Weighted mean probability of occurrence for all species
          Fut_wt_meanmammal <- tidyr::spread(Fut_wt_meanmammal, ind, values) ## Structure result into data frame
          
          #-# Calculate turnover
          TO_PAmammal <- sum(abs(Fut_meanmammal-Cur_meanmammal))/(sum(Cur_meanmammal,na.rm=T)+sum(Fut_meanmammal,na.rm=T))
          TO_PA_weightmammal <- sum(abs(Fut_wt_meanmammal-Cur_wt_meanmammal))/(sum(Cur_wt_meanmammal,na.rm=T)+sum(Fut_wt_meanmammal,na.rm=T))
          
          #-# amphibians
          CellsSRTOamphibian <- merge(PACells,currentComTOamphibian,by=c("x","y"), all.x=T) ## Merge with the current species lists per cell
          CellsSRTOamphibian[is.na(CellsSRTOamphibian)] <- 0 ## Replace all NAs by 0 - if taking a mean 0 cells need to stay
          
          #-# Current mean probability of occurrence per species
          Cur_meanamphibian <- stack(apply(CellsSRTOamphibian[5:ncol(CellsSRTOamphibian)], 2, mean)) ## Mean probability of occurrence for all species
          Cur_meanamphibian <- tidyr::spread(Cur_meanamphibian, ind, values) ## Structure result into data frame
          
          #-# Current weighted mean probability of occurrence per species
          Cur_wt_meanamphibian <- stack(apply(CellsSRTOamphibian[5:ncol(CellsSRTOamphibian)], 2, weighted.mean, CellsSRTOamphibian[,"pcArea"])) ## Weighted mean probability of occurrence for all species
          Cur_wt_meanamphibian <- tidyr::spread(Cur_wt_meanamphibian, ind, values) ## Structure result into data frame
          
          #-# Get future species list for PA
          CellsSRTOFutamphibian <- merge(PACells,futureComTOamphibian,by=c("x","y"), all.x=T) ## Merge with the future species lists per cell
          CellsSRTOFutamphibian[is.na(CellsSRTOFutamphibian)] <- 0 ## Replace all NAs by 0 - if taking a mean 0 cells need to stay
          
          #-# Future mean probability of occurrence per species
          Fut_meanamphibian <- stack(apply(CellsSRTOFutamphibian[5:ncol(CellsSRTOFutamphibian)], 2, mean)) ## Mean probability of occurrence for all species
          Fut_meanamphibian <- tidyr::spread(Fut_meanamphibian, ind, values) ## Structure result into data frame
          
          #-# Future weighted mean probability of occurrence per species
          Fut_wt_meanamphibian <- stack(apply(CellsSRTOFutamphibian[5:ncol(CellsSRTOFutamphibian)], 2, weighted.mean, CellsSRTOFutamphibian[,"pcArea"])) ## Weighted mean probability of occurrence for all species
          Fut_wt_meanamphibian <- tidyr::spread(Fut_wt_meanamphibian, ind, values) ## Structure result into data frame
          
          #-# Calculate turnover
          TO_PAamphibian <- sum(abs(Fut_meanamphibian-Cur_meanamphibian))/(sum(Cur_meanamphibian,na.rm=T)+sum(Fut_meanamphibian,na.rm=T))
          TO_PA_weightamphibian <- sum(abs(Fut_wt_meanamphibian-Cur_wt_meanamphibian))/(sum(Cur_wt_meanamphibian,na.rm=T)+sum(Fut_wt_meanamphibian,na.rm=T))
          
          #-# reptiles
          CellsSRTOreptile <- merge(PACells,currentComTOreptile,by=c("x","y"), all.x=T) ## Merge with the current species lists per cell
          CellsSRTOreptile[is.na(CellsSRTOreptile)] <- 0 ## Replace all NAs by 0 - if taking a mean 0 cells need to stay
          
          #-# Current mean probability of occurrence per species
          Cur_meanreptile <- stack(apply(CellsSRTOreptile[5:ncol(CellsSRTOreptile)], 2, mean)) ## Mean probability of occurrence for all species
          Cur_meanreptile <- tidyr::spread(Cur_meanreptile, ind, values) ## Structure result into data frame
          
          #-# Current weighted mean probability of occurrence per species
          Cur_wt_meanreptile <- stack(apply(CellsSRTOreptile[5:ncol(CellsSRTOreptile)], 2, weighted.mean, CellsSRTOreptile[,"pcArea"])) ## Weighted mean probability of occurrence for all species
          Cur_wt_meanreptile <- tidyr::spread(Cur_wt_meanreptile, ind, values) ## Structure result into data frame
          
          #-# Get future species list for PA
          CellsSRTOFutreptile <- merge(PACells,futureComTOreptile,by=c("x","y"), all.x=T) ## Merge with the future species lists per cell
          CellsSRTOFutreptile[is.na(CellsSRTOFutreptile)] <- 0 ## Replace all NAs by 0 - if taking a mean 0 cells need to stay
          
          #-# Future mean probability of occurrence per species
          Fut_meanreptile <- stack(apply(CellsSRTOFutreptile[5:ncol(CellsSRTOFutreptile)], 2, mean)) ## Mean probability of occurrence for all species
          Fut_meanreptile <- tidyr::spread(Fut_meanreptile, ind, values) ## Structure result into data frame
          
          #-# Future weighted mean probability of occurrence per species
          Fut_wt_meanreptile <- stack(apply(CellsSRTOFutreptile[5:ncol(CellsSRTOFutreptile)], 2, weighted.mean, CellsSRTOFutreptile[,"pcArea"])) ## Weighted mean probability of occurrence for all species
          Fut_wt_meanreptile <- tidyr::spread(Fut_wt_meanreptile, ind, values) ## Structure result into data frame
          
          #-# Calculate turnover
          TO_PAreptile <- sum(abs(Fut_meanreptile-Cur_meanreptile))/(sum(Cur_meanreptile,na.rm=T)+sum(Fut_meanreptile,na.rm=T))
          TO_PA_weightreptile <- sum(abs(Fut_wt_meanreptile-Cur_wt_meanreptile))/(sum(Cur_wt_meanreptile,na.rm=T)+sum(Fut_wt_meanreptile,na.rm=T))
          
          
          #-#-# Tree cover change #-#-#
          TreeCoverData <- merge(PACells,TreeCover,by=c("x","y"),all.x=T)
          TreeCover_WMean <- weighted.mean(TreeCoverData[,"TreeCoverChange"],TreeCoverData[,"pcArea"],na.rm=T)
          TreeCover_Max <- max(TreeCoverData[,"TreeCoverChange"],na.rm=T)
          
          
          #-#-# PA info #-#-#
          Int_Name <- as.character(PA$IntName)
          Area_Rep <- as.character(PA$REP_AREA)
          Area_GIS <- as.character(PA$GIS_AREA) ## Values for the size objective
          PA_type <- as.character(PA$PA_type)
          PA_DEF <- as.character(PA$PA_DEF)
          IUCN_CAT <- as.character(PA$IUCN_CAT)
          ISO3 <- as.character(PA$ISO3)
          COUNTRY <- as.character(PA$COUNTRY)
          
          #-# Shorten name if necessary
          NameParts <- strsplit(PAname,split="/") ## Take second part of PA name if they are split
          if((length(NameParts[[1]])) > 1){
            PAname <- NameParts[[1]][2]
          }
          
          
          #-#-# Return all site specific data #-#-#
          dataList <- list(SP_ID=SP_ID,Int_Name=Int_Name,Area_Rep=Area_Rep,Area_GIS=Area_GIS,PA_type=PA_type,PA_DEF=PA_DEF,IUCN_CAT=IUCN_CAT,RealmNr=RealmNr,
                           ISO3=ISO3,COUNTRY=COUNTRY,SR_B=SR_B,SR_M=SR_M,SR_A=SR_A,SR_R=SR_R,RSR_B=RSR_B,RSR_M=RSR_M,RSR_A=RSR_A,RSR_R=RSR_R,PE_WMean_B=PE_WMean_Birds,
                           PE_Max_B=PE_Max_Birds,PE_WMean_M=PE_WMean_Mammals,PE_Max_M=PE_Max_Mammals,PE_WMean_A=PE_WMean_Amphibians,PE_Max_A=PE_Max_Amphibians,
                           PE_WMean_R=PE_WMean_Reptiles,PE_Max_R=PE_Max_Reptiles,TO_PAbird=TO_PAbird,TO_PA_weightbird=TO_PA_weightbird,
                           TO_PAmammal=TO_PAmammal,TO_PA_weightmammal=TO_PA_weightmammal,TO_PAamphibian=TO_PAamphibian,
                           TO_PA_weightamphibian=TO_PA_weightamphibian,TO_PAreptile=TO_PAreptile,TO_PA_weightreptile=TO_PA_weightreptile,
                           HFP_WMean=HFP_WMean,HFP_Max=HFP_Max,HFPRisk_around=HFPRisk_around,BaseCarbon_WMean=BaseCarbon_WMean,BaseCarbon_WSum=BaseCarbon_WSum,
                           VulCarbon_WMean=VulCarbon_WMean,VulCarbon_WSum=VulCarbon_WSum,IrrCarbon_WMean=IrrCarbon_WMean,
                           IrrCarbon_WSum=IrrCarbon_WSum,BII_WMean=BII_WMean,BII_Max=BII_Max,BioAnth_WMean=BioAnth_WMean,BioAnth_Max=BioAnth_Max,
                           TreeCover_WMean=TreeCover_WMean,TreeCover_Max=TreeCover_Max,BioCropRisk=BioCropRisk,CropRisk=CropRisk,PastureRisk=PastureRisk)
          Data <- do.call(cbind,dataList)
          Data <- as.data.frame(Data)
          
          save(Data,file=paste0(outpath,PAname,".Rdata"),compress="xz")
      }
    }
}


