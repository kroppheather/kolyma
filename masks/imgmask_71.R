##########################################################
###### convert shapefiles to raster for masks        ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)

dirD <- "/media/studentuser/STORE N GO/training"

img <- list.files(paste0(dirD, "/img"),pattern=".tif")
imgXML <- grepl(".aux.xml",img)
img <- img[imgXML == FALSE ] 

water <- list.files(paste0(dirD, "/masks/water"),pattern=".shp")
waterXML <- grepl(".xml",water)
water <- water[waterXML == FALSE ] 

shrubs <- list.files(paste0(dirD, "/masks/shrubs"),pattern=".shp")
shrubsXML <- grepl(".xml",shrubs)
shrubs <- shrubs[shrubsXML == FALSE ] 

trees <- list.files(paste0(dirD, "/masks/trees"),pattern=".shp")
treesXML <- grepl(".xml",trees)
trees <- trees[treesXML == FALSE ] 

lowDensity <- list.files(paste0(dirD, "/masks/low_density"),pattern=".shp")
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

plot(imgL[[306]], col=grey(1:100/100))

waterL <- list()
for(i in 1:length(water)){
  waterL[[i]] <- vect(paste0(dirD, "/masks/water/",water[i]))
}  

imgPos <- numeric()
waterR <- list()
imgPos <- which(imgNumber == waterNumber[10])
for(i in 1:length(water)){
  imgPos <- which(imgNumber == waterNumber[i])
  waterR[[i]] <- rast(waterL[[i]], imgL[[imgPos]], background=0)
}

for(i in 1:length(water)){
  writeRaster(waterR[[i]], paste0("/media/studentuser/STORE N GO/training/masks_img/water_img/", waterName[i] ))
}

shrubL <- list()
for(i in 1:length(shrubs)){
  shrubL[[i]] <- vect(paste0(dirD, "/masks/shrubs/",shrubs[i]))
}  

imgPos <- numeric()
shrubR <- list()
imgPos <- which(imgNumber == shrubNumber[10])
for(i in 1:length(shrubs)){
  imgPos <- which(imgNumber == shrubNumber[i])
  shrubR[[i]] <- rast(shrubL[[i]], imgL[[imgPos]], background=0)
}

plot(shrubL[[2]])
plot(shrubR[[2]], add=TRUE, alpha=0.5)

for(i in 1:length(shrubs)){
  writeRaster(shrubR[[i]], paste0("/media/studentuser/STORE N GO/training/masks_img/shrubs_img/", shrubName[i] ))
}


treesL <- list()
for(i in 1:length(trees)){
  treesL[[i]] <- vect(paste0(dirD, "/masks/trees/",trees[i]))
}  

imgPos <- numeric()
treesR <- list()

for(i in 1:length(trees)){
  imgPos <- which(imgNumber == treesNumber[i])
  treesR[[i]] <- rast(treesL[[i]], imgL[[imgPos]], background=0)
}

plot(treesL[[2]])
plot(treesR[[2]], add=TRUE, alpha=0.5)

for(i in 1:length(trees)){
  writeRaster(treesR[[i]], paste0("/media/studentuser/STORE N GO/training/masks_img/trees_img/", treesName[i] ))
}



lowDensityL <- list()
for(i in 1:length(lowDensity)){
  lowDensityL[[i]] <- vect(paste0(dirD, "/masks/low_density/",lowDensity[i]))
}  

imgPos <- numeric()
lowDensityR <- list()

for(i in 1:length(lowDensity)){
  imgPos <- which(imgNumber == lowDensityNumber[i])
  lowDensityR[[i]] <- rast(lowDensityL[[i]], imgL[[imgPos]], background=0)
}

for(i in 1:length(lowDensity)){
  writeRaster(lowDensityR[[i]], paste0("/media/studentuser/STORE N GO/training/masks_img/low_density_img/", lowDensityName[i] ))
}

plot(l)