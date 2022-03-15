#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#        Calculate PE values for each grid cell globally        # 
#                Using the "phylospatial" scripts               #
#                    adapted from Dan Rosauer                   #
#                 https://github.com/DanRosauer                 #
#                        Legacy Landscapes                      #
#                          February 2020                        #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
  
rm(list=ls())

#-#-# Load libraries #-#-#
library(phylobase)
library(ape)


#-#-# Set file path #-#-#
distpath <- "/home/avoskamp/Legacy_landscapes/PE/Distributions/"


#-#-# Load the tree #-#-#
setwd("/home/avoskamp/Legacy_landscapes/PE/Tree/Dimensions_Tetrapod_Data/")
load("tetrapod_phylogeny_list_incl_reptilia.Rdata") 


#-#-# Subset thr phylogeny to the right taxa #-#-#
Ctree <- taxa_phy_list[[4]] # Change here for taxa [[1]] birds, [[2]] mammals, [[3]] amphibians, [[4]] all
class(Ctree)


#-#-# Remove negative edge.length #-#-#
Ctree$edge.length[Ctree$edge.length<0]<-0


#-#-# Change to phylo4 tree #-#-#
if (class(Ctree) == "phylo") {Ctree <- phylo4(Ctree)}


#-#-# Get the distribution data #-#-#
# Even though the distribution data is not fully matched with the phylogeny, we need this data set
# to extract species community per cell - species with no tip label match will be removed later
SpM <- get(load(paste0(distpath,"Reptile_distributions_tetrapod.Rdata"))) # Change to right taxa
str(SpM)
nrow(SpM)
ncol(SpM)

## Remove empty cells to speed analysis up
SpM$SR <- rowSums(SpM[3:ncol(SpM)])
SpM <- subset(SpM,SR>0)
SpM <- SpM[1:(ncol(SpM)-1)]

SpM$site <- 1:(nrow(SpM)) ## Add cell/site ID
coordID <- SpM[c("x","y","site")] ## Keep coordinates with matching ID for later
SpM <- cbind(coordID[3],SpM[4:(ncol(SpM)-1)]) ## Bind ID and species data ## SpM remove last row "site" and first three rows "x", "y", "cell" 

SpeciesM <- colnames(SpM[2:ncol(SpM)]) 


# #-# Match species names in tree with spatial data if there are mismatches #-#-#
## Use with matched taxonomy lists to rename tree tips
# remap <- read.csv(paste0("E:/PD analysis/South America Models/","Remap SA species.csv"),stringsAsFactors = FALSE)
# 
# ## Remap tree names to match the spatial data
# labels <- as.character(tipLabels(Ctree)) ## Takes forever...
# for (i in 1:length(labels)) {
#   print(i)
#   remap.row <- remap[remap$Tree_name==labels[i],]
#   if (nrow(remap.row)==1) {tipLabels(Ctree)[i] <- remap.row$Spatial_name}
# }

## Ensure that the tree tips match the spatial names
spatial_names <- as.character(unique(SpeciesM))
labels <- as.character(tipLabels(Ctree))
on_tree     <- intersect(spatial_names,labels)
not_on_tree <- setdiff(spatial_names,labels)


#-#-# Subset the tree to species existing in the matrix
subtree <- subset(Ctree,tips.include=SpeciesM)

## Change community matrix to same order as the tree
SpM <- SpM[c("site",subtree@label)]

#SpMTest <- SpM[1:10,]

gc()
result <- calc_PE(subtree,SpM[,-1],"presence") ## Call calc_PE, omitting the site name column from the matrix
output <- merge(coordID,result,by="site") ## Add on the lat and long columns
gc()

#-#-# Add SR to the resultfile #-#-#
SpM$SR <- rowSums(SpM[2:ncol(SpM)])
SpM <- SpM[c("site","SR")]

final <- merge(output,SpM,by="site")
head(final)
setwd("/home/avoskamp/Legacy_landscapes/PE/")
write.csv(final,"PE_Reptiles_new.csv")


#-#-# Check mammal patterns #-#-#
## Load the libraries
library(fields)
library(colorRamps)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(gridExtra)

setwd("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Raster_data_layers/PE/")
mam <- read.csv("PE_Mammals.csv")
head(mam)
min(mam$PE)
max(mam$PE)
hist(mam$PE)

mam$Col <- 0
mam$Col[mam$PE >= 0 & mam$PE < 1] <- 1
mam$Col[mam$PE >= 1 & mam$PE < 2] <- 2
mam$Col[mam$PE >= 2 & mam$PE < 3] <- 3
mam$Col[mam$PE >= 3 & mam$PE < 4] <- 4
mam$Col[mam$PE >= 4 & mam$PE < 5] <- 5
mam$Col[mam$PE >= 5 & mam$PE < 10] <- 6
mam$Col[mam$PE >= 10 & mam$PE < 20] <- 7
mam$Col[mam$PE >= 20 & mam$PE < 30] <- 8
mam$Col[mam$PE >= 30 & mam$PE < 45] <- 9
mam$Col[mam$PE >= 45 & mam$PE < 60] <- 10
mam$Col[mam$PE >= 60 & mam$PE < 70] <- 11

colPal <- rev(brewer.pal(11,"RdYlGn"))

## Rearrange the edge coordinates to fit country boarder map
mamC1 <- subset(mam,x > -170) ## Subset to x coords higher than -170 these are fine
mamC2 <- subset(mam,x <= -170) ## Subset to x coords lower than -170 these need to move
mamC2$x <- abs(mamC2$x) + 10 ## Make coords positive to shift to other side and add 10 degree to attach end of map (179.5 center)  
x <- rev(mamC2$x) ## Reorder the x coords because they need to be mirrored to be attached on the other side
mamC2$x <- x ## Replace with the reordered x coords
mamC3 <- rbind(mamC1,mamC2) ##Merge the two dataframes back together


# Plotting the map for current SR clipped for 50
PE <- ggplot(data=mamC3, aes(y=y, x=x)) +
  geom_raster(aes(fill = Col), stat = "identity", position = "identity", hjust = 0, vjust = 0, interpolate = FALSE,)+
  scale_fill_gradientn("PE\nmammals",colours=colPal,limits=c(0,11))+ # Insert colour and set range
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  theme(legend.position = "none")+ # Positioning the legend   
  theme(legend.text=element_text(size=12))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='#FFFFFF',colour="white"))+ # Remove the background
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ #Remove the grid
  labs(x="", y="Mammal_PE", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  theme(axis.title = element_text(size = 28))+
  ggtitle("")+ 
  theme(plot.title = element_text(size = 25, face = "bold"))

print(PE)

setwd("E:/Legacy Landscapes/Legacy Landscape Site selection/Site selection data/Raster_data_layers/PE/")
ggsave("Mammal PE RG.tiff",PE,width=24, height=12, unit="in", dpi=600, bg="transparent")                
