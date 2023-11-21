library(terra)
library(dplyr)


# Image merge ---------

dirP <- "/media/hkropp/research/Kolyma_Data/predictions/1971"

Nimg <- 3036


tiles <- list()
for(i in 1:Nimg){
  tiles[[i]] <- rast(paste0("/media/hkropp/research/Kolyma_Data/img_tiles/1971e/img_256/img",i,".tif"))
  
}


treeImg <- list()

for(i in 1:Nimg){
  treeImg[[i]] <- rast(paste0(dirP,"/tree/tree_predict_",i,".tif"))
  
  
}

treeAll <- do.call(merge, treeImg)



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
plot(waterAll)



# Image merge second tile ---------

dirP2 <- "/media/hkropp/research/Kolyma_Data/predictions/1971_2"

Nimg <- 3036



treeImg2 <- list()

for(i in 1:Nimg){
  treeImg2[[i]] <- rast(paste0(dirP2,"/tree/tree_predict_",i,".tif"))
  
  
}

treeAll2 <- do.call(merge, treeImg2)



waterImg2 <- list()

for(i in 1:Nimg){
  waterImg2[[i]] <- rast(paste0(dirP2,"/water/water_predict_",i,".tif"))
  
  
}

waterAll2 <- do.call(merge, waterImg2)

shrubImg2 <- list()

for(i in 1:Nimg){
  shrubImg2[[i]] <- rast(paste0(dirP2,"/shrub/shrub_predict_",i,".tif"))
  
  
}

shrubAll2 <- do.call(merge, shrubImg2)

lowDImg2 <- list()

for(i in 1:Nimg){
  lowDImg2[[i]] <- rast(paste0(dirP2,"/low/lowD_predict_",i,".tif"))
  
  
}

lowDAll2 <- do.call(merge, lowDImg2)
plot(lowDAll2)
plot(shrubAll2)
plot(treeAll2)
plot(waterAll2)

# Image merge third tile ---------

dirP3 <- "/media/hkropp/research/Kolyma_Data/predictions/1971_3"

Nimg <- 3036




treeImg3 <- list()

for(i in 1:Nimg){
  treeImg3[[i]] <- rast(paste0(dirP3,"/tree/tree_predict_",i,".tif"))
  
  
}

treeAll3 <- do.call(merge, treeImg3)



waterImg3 <- list()

for(i in 1:Nimg){
  waterImg3[[i]] <- rast(paste0(dirP3,"/water/water_predict_",i,".tif"))
  
  
}

waterAll3 <- do.call(merge, waterImg3)

shrubImg3 <- list()

for(i in 1:Nimg){
  shrubImg3[[i]] <- rast(paste0(dirP3,"/shrub/shrub_predict_",i,".tif"))
  
  
}

shrubAll3 <- do.call(merge, shrubImg3)

lowDImg3 <- list()

for(i in 1:Nimg){
  lowDImg3[[i]] <- rast(paste0(dirP3,"/low/lowD_predict_",i,".tif"))
  
  
}

lowDAll3 <- do.call(merge, lowDImg3)
plot(lowDAll3)
plot(shrubAll3)
plot(treeAll3)
plot(waterAll3)
# Merge tile offsets ------

# resample to original
low2rs <- resample(lowDAll2, lowDAll)
low3rs <- resample(lowDAll3, lowDAll)

water2rs <- resample(waterAll2, waterAll)
water3rs <- resample(waterAll3, waterAll)

tree2rs <- resample(treeAll2, treeAll)
tree3rs <- resample(treeAll3, treeAll)

shrub2rs <- resample(shrubAll2, shrubAll)
shrub3rs <- resample(shrubAll3, shrubAll)

waterStack <- c(waterAll, water2rs, water3rs)
waterLayer <- max(waterStack, na.rm=TRUE)
plot(waterLayer)

shrubStack <- c(shrubAll, shrub2rs, shrub3rs)
shrubLayer <- max(shrubStack, na.rm=TRUE)
plot(shrubLayer)

treeStack <- c(treeAll, tree2rs, tree3rs)
treeLayer <- max(treeStack, na.rm=TRUE)
plot(treeLayer)

lowStack <- c(lowDAll, low2rs, low3rs)
lowLayer <- max(lowStack, na.rm=TRUE)
plot(lowLayer)
# Make final map cover -------------

# remove noise below set threshold

#v3
treeMap <- ifel(treeLayer <= 0.1, 0, treeLayer)
waterMap <- ifel(waterLayer <= 0.6, 0, waterLayer)
shrubMap <- ifel(shrubLayer <= 0.3, 0, shrubLayer)
lowDMap <- ifel(lowLayer <= 0.1, 0, lowLayer)


# binary map of above

treeMapB <- ifel(treeLayer <= 0.1, 0, 1)
waterMapB <- ifel(waterLayer <= 0.6, 0, 1)
shrubMapB <- ifel(shrubLayer <= 0.3, 0, 1)
lowDMapB <- ifel(lowLayer <= 0.1, 0, 1)




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




writeRaster(finalClass, "/media/hkropp/research/Kolyma_Data/predictions/maps/class1971_v3.tif", filetype="GTiff" )
