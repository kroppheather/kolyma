##########################################################
###### convert shapefiles to raster for masks        ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)

dirD <- "/media/studentuser/Seagate Portable Drive/training"

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

poly <- list.files(paste0(dirD, "/masks/polygonal"),pattern=".shp")
polyXML <- grepl(".xml",poly)
poly <- poly[polyXML == FALSE ]

imgNumber <- as.numeric(gsub("\\D","", img))
waterNumber <- as.numeric(gsub("\\D","", water))
waterName <- gsub(".shp", ".tif",water)
shrubNumber <- as.numeric(gsub("\\D","", shrubs))
shrubName <- gsub(".shp", ".tif",shrubs)
polyNumber <- as.numeric(gsub("\\D","", poly))
polyName <- gsub(".shp", ".tif", poly)
treesNumber <- as.numeric(gsub("\\D","", trees))
treesName <- gsub(".shp", ".tif", trees)

imgL <- list()
for(i in 1:length(img)){
  imgL[[i]] <- rast(paste0(dirD,"/img/",img[i]))

}

plot(imgL[[3]], col=grey(1:100/100))

waterL <- list()
for(i in 1:length(water)){
  waterL[[i]] <- vect(paste0(dirD, "/masks/water/",water[i]))
}  

imgPos <- numeric()
waterR <- list()
imgPos <- which(imgNumber == waterNumber[10])
for(i in 1:length(water)){
  imgPos <- which(imgNumber == waterNumber[i])
  waterR[[i]] <- rasterize(waterL[[i]], imgL[[imgPos]], background=0)
}

for(i in 1:length(water)){
  writeRaster(waterR[[i]], paste0("/media/studentuser/Seagate Portable Drive/training/masks_img/water_img/", waterName[i] ))
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
  shrubR[[i]] <- rasterize(shrubL[[i]], imgL[[imgPos]], background=0)
}

plot(shrubL[[2]])
plot(shrubR[[2]], add=TRUE, alpha=0.5)

for(i in 1:length(shrubs)){
  writeRaster(shrubR[[i]], paste0("/media/studentuser/Seagate Portable Drive/training/masks_img/shrubs_img/", shrubName[i] ))
}




polyL <- list()
for(i in 1:length(poly)){
  polyL[[i]] <- vect(paste0(dirD, "/masks/polygonal/",poly[i]))
}  

imgPos <- numeric()
polyR <- list()

for(i in 1:length(poly)){
  imgPos <- which(imgNumber == polyNumber[i])
  polyR[[i]] <- rasterize(polyL[[i]], imgL[[imgPos]], background=0)
}

plot(polyL[[2]])
plot(polyR[[2]], alpha=0.5)

for(i in 1:length(poly)){
  writeRaster(polyR[[i]], paste0("/media/studentuser/Seagate Portable Drive/training/masks_img/polygonal_img/", polyName[i] ))
}


treesL <- list()
for(i in 1:length(trees)){
  treesL[[i]] <- vect(paste0(dirD, "/masks/trees/",trees[i]))
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
  writeRaster(treesR[[i]], paste0("/media/studentuser/Seagate Portable Drive/training/masks_img/trees_img/", treesName[i] ))
}