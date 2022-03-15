#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#      Grid the site polygons and calculate percentage      #  
#               overlap with raster grid cells              #
#                  Adapted from David Baker                 #
#                     Alke January 2020                     #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear memory #-#-#
rm(list=ls(all=TRUE))


#-#-# Load libraries #-#-#
packList <- c("maptools","raster","ggplot2","rgdal","snowfall")
lapply(packList,function(x) require(x,character.only = TRUE))



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##                                                               ##
##  Grid each site onto small grid then upscale to 0.5 degree    ##
##                                                               ##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# The site shapefile contains the downloaded shapefiles from protected planet (IUCN and WHS sites) and from
# BirdLife International (KBAs) before gridding the shapefile, the data sets have been pre-processed
# in QGIS version 3.16.3. The area (GIS_Area) has been calculated using the field calculator, overlapping 
# sites have been removed manually (as described in the supplementary material), and the country the site
# is located in has been added using TM_WORLD_BORDERS-3.0 (https://thematicmapping.org) and the join 
# attributes by location tool.


#-#-# Load the site shapefile (Combined shp containing IUCN PAs, WHS sites and KBAs) #-#-#
setwd("/home/avoskamp/Legacy_landscapes/PA_Data/Combined data global/Combined data global with country")

## Check the data structure 
getinfo.shape("Combined_data_global_IUCN_WHS_KBA_countries.shp")

## Read shapefile in
PA.map <- readShapeSpatial("Combined_data_global_IUCN_WHS_KBA_countries.shp")
head(PA.map@data)
IUCNPoly <- PA.map


#-#-# Set coordinates for region extent #-#-#
# baseline.csv is a file containing grid cell coordinates for a global 0.5 degree grid
coords <- read.csv("/home/avoskamp/Legacy_landscapes/PA_Data/baseline.csv")[,2:3]
xmin <- min(coords$x)
xmax <- max(coords$x)
ymin <- min(coords$y) 
ymax <- max(coords$y) 

## Check the coordinates
head(coords)
testx <- unique(coords$x)
length(testx)
testy <- unique(coords$y)
length(testy)


#-#-# Define base raster to grid polygons #-#-#
r.grid.l <- raster(nrows=286, ncols=719, xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax,crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
x.coord <- sort(c(unique(as.data.frame(coordinates(r.grid.l))[,1]),c(xmin,xmax)))
y.coord <- sort(c(unique(as.data.frame(coordinates(r.grid.l))[,2]),c(ymin,ymax)))



#-#-# Remove empty lists function #-#-#
delete.NULLs  <-  function(x.list){   # delete null/empty entries in a list
  x.list[unlist(lapply(x.list, length) != 0)]
}


#-#-# Loop to grid all site polygons #-#-#
sfInit(parallel=TRUE, cpus=ceiling(0.6*parallel::detectCores()))
sfExport(list=c("IUCNPoly","coords","r.grid.l","x.coord","y.coord"))
sfLibrary(maptools); sfLibrary(raster); sfLibrary(rgdal)
gridIUCN <- lapply(1:length(slot(IUCNPoly, "polygons")),function(s){

  ## Print progress (if not running in parallel)
  cat(s, " of 1352","\n")
  
  ## Subset DF to one polygon
  IUCNSPDF <- IUCNPoly[s,]
  name <- IUCNSPDF@data$NatName
  print(name)
      
      ## Extract info about extent and # rows and columns in order to create a small grid for rasterizing
      xmin.pa <- x.coord[findInterval(extent(IUCNSPDF)@xmin,x.coord)-1]
      xmax.pa <- x.coord[findInterval(extent(IUCNSPDF)@xmax,x.coord)+1]
      ymin.pa <- y.coord[findInterval(extent(IUCNSPDF)@ymin,y.coord)-1]
      ymax.pa <- y.coord[findInterval(extent(IUCNSPDF)@ymax,y.coord)+1]
      ncol <- length(x.coord[x.coord >= xmin.pa & x.coord <= xmax.pa])-1
      nrow <- length(y.coord[y.coord >= ymin.pa & y.coord <= ymax.pa])-1
      
      ## Create 0.44 x 0.440002 degree raster layer grid but with extent limited to site extent
      r.grid.l <- raster(nrows=nrow, ncols=ncol, xmn=xmin.pa, xmx=xmax.pa, ymn=ymin.pa, ymx=ymax.pa,crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
      vals <- 1:ncell(r.grid.l) # create vector of numbers
      r.grid.l <- setValues(r.grid.l, vals) # fill grid squares with numerical value to create label
      r.grid.s <- disaggregate(r.grid.l, fact=c(100,100),method="") # disaggregate so that each 2.5' cell has number associating it to a 1 degree cell
      
      ## Rasterize polygon to small grid
      IUCNGrid <- rasterize(IUCNSPDF, r.grid.s, background=0, silent=TRUE, getCover=FALSE)
      IUCNGrid <- aggregate(IUCNGrid, 100, sum, progress='text')
      
      ## Extract coordinates and overlap stats
      IUCNGridOut <- as.data.frame(cbind(pa.coord = coordinates(IUCNGrid), pa.overlap = getValues(IUCNGrid)))
      IUCNGridOut <- IUCNGridOut[IUCNGridOut$pa.overlap > 0,]
      
      ## Calculate statistics (percentage overlap) 
      IUCNGridOut$pcArea <- round(IUCNGridOut$pa.overlap/sum(IUCNGridOut$pa.overlap),3)
      
      ## Save results
      return(list(SP_ID=as.character(IUCNSPDF@data$SP_ID),
                  WDPA_ID=as.character(IUCNSPDF@data$WDPAID),   
                  WDPA_PID=as.character(IUCNSPDF@data$WDPA_PID),  
                  PA_DEF=as.character(IUCNSPDF@data$PA_DEF), 
                  IntName=as.character(IUCNSPDF@data$IntName), 
                  Nat_Name=as.character(IUCNSPDF@data$NatName), 
                  IUCN_CAT=as.character(IUCNSPDF@data$IUCN_CAT), 
                  REP_AREA=as.character(IUCNSPDF@data$REP_AREA), 
                  GIS_AREA=as.character(IUCNSPDF@data$GIS_AREA),
                  ISO3 = as.character(IUCNSPDF@data$ISO3),
                  COUNTRY = as.character(IUCNSPDF@data$NAME),
                  PA_type=as.character(IUCNSPDF@data$PA_type),
                  PCoverlap=IUCNGridOut$pcArea,
                  data=IUCNGridOut))
                  
})


#-#-# Remove empty lists from output (if there are any) #-#-#
gridIUCNnoNull <- delete.NULLs(gridIUCN)
length(gridIUCNnoNull)
  

#-#-# Save the gridded sites file #-#-#  
save(gridIUCNnoNull,file="Combined_data_global_IUCN_WHS_KBA_grid_country.Rdata",compress="xz")
head(gridIUCNnoNull)
str(gridIUCNnoNull)
IUCNtest <- gridIUCNnoNull[[1]]


