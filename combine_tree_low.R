dirD <- '/media/studentuser/Seagate Portable Drive/training/'

trees <- list.files(paste0(dirD, "/masks/trees"),pattern=".shp")
treesXML <- grepl(".xml",trees)
trees <- trees[treesXML == FALSE ] 

lowDensity <- list.files(paste0(dirD, "/masks/low_density"),pattern=".shp")
lowDensityXML <- grepl(".xml",lowDensity)
lowDensity <- lowDensity[lowDensityXML == FALSE ] 

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