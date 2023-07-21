library(terra)
library(dplyr)

dirD <- '/media/studentuser/Seagate Portable Drive/training/'

img <- list.files(paste0(dirD, "/img"),pattern=".tif")
imgXML <- grepl(".aux.xml",img)
img <- img[imgXML == FALSE ] 

trees <- list.files(paste0(dirD, "/masks/trees"),pattern=".shp")
treesXML <- grepl(".xml",trees)
trees <- trees[treesXML == FALSE ] 

lowDensity <- list.files(paste0(dirD, "/masks/low_density"),pattern=".shp")
lowDensityXML <- grepl(".xml",lowDensity)
lowDensity <- lowDensity[lowDensityXML == FALSE ] 

imgNumber <- as.numeric(gsub("\\D","", img))

treesNumber <- as.numeric(gsub("\\D","", trees))
treesName <- gsub(".shp", ".tif", trees)
lowDensityNumber <- as.numeric(gsub("\\D","", lowDensity))
lowDensityName <- gsub(".shp", ".tif", lowDensity)

imgL <- list()
for(i in 1:length(img)){
  imgL[[i]] <- rast(paste0(dirD,"/img/",img[i]))
  
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

lowDensityL <- list()
for(i in 1:length(lowDensity)){
  lowDensityL[[i]] <- vect(paste0(dirD, "/masks/low_density/",lowDensity[i]))
}  

imgPos <- numeric()
lowDensityR <- list()

for(i in 1:length(lowDensity)){
  imgPos <- which(imgNumber == lowDensityNumber[i])
  lowDensityR[[i]] <- rasterize(lowDensityL[[i]], imgL[[imgPos]], background=0)
}

imgPos <- numeric()
forestR <- treesR
forestNum <- treesNumber
forestName <- NA

for(i in 1:length(trees)){
  imgPos <- which(imgNumber == treesNumber[i])
  forestName[i] <- paste0('forest_',imgNumber[imgPos],'.tif')
}

dups = 0
for(i in 1:length(lowDensity)){
  imgPos <- which(imgNumber == lowDensityNumber[i])
  treePos = which(treesNumber == imgNumber[imgPos])
  print(i)
  print(imgPos)
  print(treePos)
  if (length(treePos) == 0)
  {
    forestR[[i + length(trees) - dups]] <- lowDensityR[[i]]
    forestName[i + length(trees) - dups] <- paste0('forest_',imgNumber[imgPos],'.tif')
    forestNum[i + length(trees) - dups] <- imgPos 
  }
  else {
    curr = treesR[[treePos]] + lowDensityR[[i]]
    forestR[[which(forestNum == imgNumber[imgPos])]] = ifel(curr >= 1,1,0)
    dups = dups + 1
  }
}

for(i in 1:length(forestR)){
  writeRaster(forestR[[i]], paste0("/media/studentuser/Seagate Portable Drive/training/masks_img/forest_img/", forestName[i] ))
}
