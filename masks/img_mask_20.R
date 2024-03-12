##########################################################
###### convert shapefiles to raster for masks        ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)

dirD <- "/media/hkropp/research/Kolyma_Data/training/Kolyma/v2/u_net_20v2"
# names of all shapefiles
img <- list.files(paste0(dirD, "/img"),pattern=".tif")
imgXML <- grepl(".aux.xml",img)
img <- img[imgXML == FALSE ] 

water <- list.files(paste0(dirD, "/shapefiles/water"),pattern=".shp")
waterXML <- grepl(".xml",water)
water <- water[waterXML == FALSE ] 

shrubs <- list.files(paste0(dirD, "/shapefiles/shrub"),pattern=".shp")
shrubsXML <- grepl(".xml",shrubs)
shrubs <- shrubs[shrubsXML == FALSE ] 

trees <- list.files(paste0(dirD, "/shapefiles/taiga"),pattern=".shp")
treesXML <- grepl(".xml",trees)
trees <- trees[treesXML == FALSE ] 


# extract numbers and naming info
imgNumber <- as.numeric(gsub("\\D","", img))
waterNumber <- as.numeric(gsub("\\D","", water))
waterName <- gsub(".shp", ".tif",water)
shrubNumber <- as.numeric(gsub("\\D","", shrubs))
shrubName <- gsub(".shp", ".tif",shrubs)
treesNumber <- as.numeric(gsub("\\D","", trees))
treesName <- gsub(".shp", ".tif", trees)



# read in all images
imgL <- list()
for(i in 1:length(img)){
  imgL[[i]] <- rast(paste0(dirD,"/img/",img[i]))
  
}
#### Water ----
# read in shapefiles
waterL <- list()
for(i in 1:length(water)){
  waterL[[i]] <- vect(paste0(dirD, "/shapefiles/water/",water[i]))
}  

# pull out the corresponding image and convert shapefile to raster
imgPos <- numeric()
waterR <- list()
for(i in 1:length(water)){
  imgPos <- which(imgNumber == waterNumber[i])
  waterR[[i]] <- rasterize(waterL[[i]], imgL[[imgPos]], background=0)
}

for(i in 1:length(water)){
  writeRaster(waterR[[i]], paste0("/media/hkropp/research/Kolyma_Data/training/Kolyma/v2/u_net_20v2/masks_img/water/", waterName[i] ))
}

#### Shrubs ---
# read in shapefiles
shrubL <- list()
for(i in 1:length(shrubs)){
  shrubL[[i]] <- vect(paste0(dirD, "/shapefiles/shrub/",shrubs[i]))
}  

# pull out the corresponding image and convert shapefile to raster
imgPos <- numeric()
shrubR <- list()
for(i in 1:length(shrubs)){
  imgPos <- which(imgNumber == shrubNumber[i])
  shrubR[[i]] <- rasterize(shrubL[[i]], imgL[[imgPos]], background=0)
}

for(i in 1:length(shrubs)){
  writeRaster(shrubR[[i]], paste0("/media/hkropp/research/Kolyma_Data/training/Kolyma/v2/u_net_20v2/masks_img/shrub/", shrubName[i] ))
}

#### Taiga ---
# read in shapefiles
treesL <- list()
for(i in 1:length(trees)){
  treesL[[i]] <- vect(paste0(dirD, "/shapefiles/taiga/",trees[i]))
}  

# pull out the corresponding image and convert shapefile to raster
imgPos <- numeric()
treesR <- list()
for(i in 1:length(trees)){
  imgPos <- which(imgNumber == treesNumber[i])
  treesR[[i]] <- rasterize(treesL[[i]], imgL[[imgPos]], background=0)
}

for(i in 1:length(trees)){
  writeRaster(treesR[[i]], paste0("/media/hkropp/research/Kolyma_Data/training/Kolyma/v2/u_net_20v2/masks_img/taiga/", treesName[i] ))
}

