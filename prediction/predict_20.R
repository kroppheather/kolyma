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
testW <- vrt(list.files(paste0(dirP,"/water"), full.names=TRUE))
plot(testW)
waterAll <- do.call(merge, waterImg)
waterTest <- waterAll[1:1000,1:1000,drop=FALSE]
plot(treeAll)
plot(tiles[[204]]$Blue)
plot(waterImg[[204]])
plot(tiles[[1000]]$Blue)
plot(waterImg[[1000]])

test <- merge(tiles[[204]],tiles[[205]],tiles[[206]],tiles[[207]],tiles[[208]],
  tiles[[1000]], tiles[[1001]],tiles[[1002]],tiles[[1003]],tiles[[1004]], tiles[[3000]])
plot(test$Blue)
testW <- merge(waterImg[[204]],waterImg[[205]],waterImg[[206]],waterImg[[207]],waterImg[[208]],
               waterImg[[1000]], waterImg[[1001]],waterImg[[1002]],waterImg[[1003]],waterImg[[1004]], waterImg[[3000]])

plot(testW)

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


# Make final map cover -------------

# remove noise below set threshold

threshold <- 0.1

treeMap <- ifel(treeAll <= threshold, 0, treeAll)
waterMap <- ifel(waterAll <= threshold, 0, waterAll)
shrubMap <- ifel(shrubAll <= threshold, 0, shrubAll)
lowDMap <- ifel(lowDAll <= threshold, 0, lowDAll)


# binary map of above

treeMapB <- ifel(treeAll <= threshold, 0, 1)
waterMapB <- ifel(waterAll <= threshold, 0, 1)
shrubMapB <- ifel(shrubAll <= threshold, 0, 1)
lowDMapB <- ifel(lowDAll <= threshold, 0, 1)




# need to filter so only one class for each pixel
# take the highest probability

coverStack <- c(treeMap, waterMap, shrubMap, lowDMap)

classR <- which.max(coverStack)

plot(coverR)

#now need to make a rule for determining if the class has a high enough threshold
# need to muliply by binary so turns to zero if too low

treeCalc <- ifel(classR ==1,1,0)
treeClass <- treeMapB*treeCalc


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

# other will be zero, trees =1, buildings =2, pavement =3
finalClass <- treeClass+waterClass2+shrubClass2+lowDClass2

plot(finalClass)




writeRaster(finalClass, paste0(dirP, "/finalmodel.tif"), filetype="GTiff" )

