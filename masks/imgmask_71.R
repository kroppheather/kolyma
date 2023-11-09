##########################################################
###### convert shapefiles to raster for masks        ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)

# first 50 masks were created by Adam

dirD <- "/media/hkropp/research/Kolyma_Data/training/Kolyma/u_net71e/training"

img <- list.files(paste0(dirD, "/img"),pattern=".tif")
imgXML <- grepl(".aux.xml",img)
img <- img[imgXML == FALSE ] 

water <- list.files(paste0(dirD, "/shapefiles/water"),pattern=".shp")
waterXML <- grepl(".xml",water)
water <- water[waterXML == FALSE ] 
imgL <- list()

shrubs <- list.files(paste0(dirD, "/shapefiles/shrub"),pattern=".shp")
shrubsXML <- grepl(".xml",shrubs)
shrubs <- shrubs[shrubsXML == FALSE ] 

trees <- list.files(paste0(dirD, "/shapefiles/tree"),pattern=".shp")
treesXML <- grepl(".xml",trees)
trees <- trees[treesXML == FALSE ] 

lowDensity <- list.files(paste0(dirD, "/shapefiles/low"),pattern=".shp")
lowDensityXML <- grepl(".xml",lowDensity)
lowDensity <- lowDensity[lowDensityXML == FALSE ] 

imgNumber <- as.numeric(gsub("\\D","", img))
waterNumber <- as.numeric(gsub("\\D","", water))
waterName <- gsub(".shp", ".tif",water)
shrubNumber <- as.numeric(gsub("\\D","", shrubs))
shrubName <- gsub(".shp", ".tif",shrubs)
treesNumber <- as.numeric(gsub("\\D","", trees))
treesName <- gsub(".shp", ".tif", trees)
lowDensityNumber <- as.numeric(gsub("\\D","", lowDensity))
lowDensityName <- gsub(".shp", ".tif", lowDensity)

imgL <- list()
for(i in 1:length(img)){
  imgL[[i]] <- rast(paste0(dirD,"/img/",img[i]))

}



waterL <- list()
for(i in 1:length(water)){
  waterL[[i]] <- vect(paste0(dirD, "/shapefiles/water/",water[i]))
}  

imgPos <- numeric()
waterR <- list()
imgPos <- which(imgNumber == waterNumber[1])
for(i in 1:length(water)){
  imgPos <- which(imgNumber == waterNumber[i])
  waterR[[i]] <- rasterize(waterL[[i]], imgL[[imgPos]], background=0)
}


for(i in 1:length(water)){
  writeRaster(waterR[[i]], paste0("/media/hkropp/research/Kolyma_Data/training/Kolyma/u_net71e/training/masks_img/water/", waterName[i] ))
}

shrubL <- list()
for(i in 1:length(shrubs)){
  shrubL[[i]] <- vect(paste0(dirD, "/shapefiles/shrub/",shrubs[i]))
}  

imgPos <- numeric()
shrubR <- list()
imgPos <- which(imgNumber == shrubNumber[10])
for(i in 1:length(shrubs)){
  imgPos <- which(imgNumber == shrubNumber[i])
  shrubR[[i]] <- rasterize(shrubL[[i]], imgL[[imgPos]], background=0)
}

plot(shrubL[[2]])
plot(shrubR[[2]], add=TRUE, alpha=0.5)

for(i in 1:length(shrubs)){
  writeRaster(shrubR[[i]], paste0("/media/hkropp/research/Kolyma_Data/training/Kolyma/u_net71e/training/masks_img/shrub/", shrubName[i] ))
}


treesL <- list()
for(i in 1:length(trees)){
  treesL[[i]] <- vect(paste0(dirD, "/shapefiles/tree/",trees[i]))
}  

imgPos <- numeric()
treesR <- list()

for(i in 1:length(trees)){
  imgPos <- which(imgNumber == treesNumber[i])
  treesR[[i]] <- rasterize(treesL[[i]], imgL[[imgPos]], background=0)
}

plot(treesL[[2]])
plot(treesR[[2]], add=TRUE, alpha=0.5)

for(i in 1:length(trees)){
  writeRaster(treesR[[i]], paste0("/media/hkropp/research/Kolyma_Data/training/Kolyma/u_net71e/training/masks_img/tree/", treesName[i] ))
}



lowDensityL <- list()
for(i in 1:length(lowDensity)){
  lowDensityL[[i]] <- vect(paste0(dirD, "/shapefiles/low/",lowDensity[i]))
}  

imgPos <- numeric()
lowDensityR <- list()

for(i in 1:length(lowDensity)){
  imgPos <- which(imgNumber == lowDensityNumber[i])
  lowDensityR[[i]] <- rasterize(lowDensityL[[i]], imgL[[imgPos]], background=0)
}

for(i in 1:length(lowDensity)){
  writeRaster(lowDensityR[[i]], paste0("/media/hkropp/research/Kolyma_Data/training/Kolyma/u_net71e/training/masks_img/low/", lowDensityName[i] ))
}

plot(l)