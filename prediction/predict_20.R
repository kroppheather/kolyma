library(terra)
library(dplyr)


# Image merge ---------

dirP <- "/media/hkropp/research/Kolyma_Data/predictions/2020"

Nimg <- 12144


tiles <- list()
for(i in 1:Nimg){
  tiles[[i]] <- rast(paste0("/media/hkropp/research/Kolyma_Data/img_tiles/2020/tiles_256/img_",i,".tif"))
  
}

plot(tiles[[1000]]$Blue)

treeImg <- list()

for(i in 1:Nimg){
  treeImg[[i]] <- rast(paste0(dirP,"/tree/tree_predict_",i,".tif"))
  
  
}

treeAll <- do.call(merge, treeImg)

test <- vrt(list.files(paste0(dirP,"/tree"), full.names=TRUE))

waterImg <- list()

for(i in 1:Nimg){
  waterImg[[i]] <- rast(paste0(dirP,"/water/water_predict_",i,".tif"))
  
  
}

waterAll <- do.call(merge, waterImg)

shrubImg <- list()

for(i in 1:Nimg){
  shrubImg[[i]] <- rast(paste0(dirP,"/shrub/shrub_predict_",i,".tif"))
  
  
}

shrubAll <- do.call(merge, shrubImg)

lowDImg <- list()

for(i in 1:Nimg){
  lowDImg[[i]] <- rast(paste0(dirP,"/low/lowD_predict_",i,".tif"))
  
  
}

lowDAll <- do.call(merge, lowDImg)
plot(lowDAll)
plot(shrubAll)
plot(treeAll)

# Make final map cover -------------

# remove noise below set threshold

#v2 increase shrub threshold
treeMap <- ifel(treeAll <= 0.1, 0, treeAll)
waterMap <- ifel(waterAll <= 0.3, 0, waterAll)
shrubMap <- ifel(shrubAll <= 0.3, 0, shrubAll)
lowDMap <- ifel(lowDAll <= 0.1, 0, lowDAll)


# binary map of above

treeMapB <- ifel(treeAll <= 0.1, 0, 1)
waterMapB <- ifel(waterAll <= 0.3, 0, 1)
shrubMapB <- ifel(shrubAll <= 0.3, 0, 1)
lowDMapB <- ifel(lowDAll <= 0.1, 0, 1)




# need to filter so only one class for each pixel
# take the highest probability

coverStack <- c(treeMap, waterMap, shrubMap, lowDMap)

classR <- which.max(coverStack)

plot(classR)

#now need to make a rule for determining if the class has a high enough threshold
# need to muliply by binary so turns to zero if too low

treeCalc <- ifel(classR ==1,1,0)
treeClass <- treeMapB*treeCalc
plot(treeClass)

waterCalc <- ifel(classR ==2,1,0)
waterClass <- waterMapB*waterCalc


shrubCalc <- ifel(classR ==3,1,0)
shrubClass <- shrubMapB*shrubCalc


lowDCalc <- ifel(classR ==4,1,0)
lowDClass <- lowDMapB*lowDCalc

plot(treeClass)
plot(waterClass)
plot(shrubClass)
plot(lowDClass)


waterClass2 <- waterClass*2
shrubClass2 <- shrubClass*3
lowDClass2 <- lowDClass*4

# other will be zero, trees =1, water =2, shrub =3, low =4
finalClass <- treeClass+waterClass2+shrubClass2+lowDClass2

plot(finalClass)




writeRaster(finalClass, "/media/hkropp/research/Kolyma_Data/predictions/maps/class2020_v2.tif", filetype="GTiff" )
