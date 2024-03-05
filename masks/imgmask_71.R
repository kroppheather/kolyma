##########################################################
###### convert shapefiles to raster for masks        ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)



dirD <- "/media/hkropp/research/Kolyma_Data/training/Kolyma/v2/u_net_71v2"

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

taiga <- list.files(paste0(dirD, "/shapefiles/taiga"),pattern=".shp")
taigaXML <- grepl(".xml",taiga)
taiga <- taiga[taigaXML == FALSE ] 


imgNumber <- as.numeric(gsub("\\D","", img))
waterNumber <- as.numeric(gsub("\\D","", water))
waterName <- gsub(".shp", ".tif",water)
shrubNumber <- as.numeric(gsub("\\D","", shrubs))
shrubName <- gsub(".shp", ".tif",shrubs)
taigaNumber <- as.numeric(gsub("\\D","", taiga))
taigaName <- gsub(".shp", ".tif", taiga)

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
  writeRaster(waterR[[i]], paste0("/media/hkropp/research/Kolyma_Data/training/Kolyma/v2/u_net_71v2/masks_img/water/", waterName[i] ))
}

plot(waterR[[150]])

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
  writeRaster(shrubR[[i]], paste0("/media/hkropp/research/Kolyma_Data/training/Kolyma/v2/u_net_71v2/masks_img/shrub/", shrubName[i] ))
}


taigaL <- list()
for(i in 1:length(taiga)){
  taigaL[[i]] <- vect(paste0(dirD, "/shapefiles/taiga/",taiga[i]))
}  
taiga[[1]]
imgPos <- numeric()
taigaR <- list()

for(i in 1:length(taiga)){
  imgPos <- which(imgNumber == taigaNumber[i])
  taigaR[[i]] <- rasterize(taigaL[[i]], imgL[[imgPos]], background=0)
}

plot(taigaL[[2]])
plot(taigaR[[2]], add=TRUE, alpha=0.5)

for(i in 1:length(taiga)){
  writeRaster(taigaR[[i]], paste0("/media/hkropp/research/Kolyma_Data/training/Kolyma/v2/u_net_71v2/masks_img/taiga/", taigaName[i] ))
}


