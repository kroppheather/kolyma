library(terra)
library(dplyr)


# Image merge first tile ---------

dirP <- "/media/hkropp/research/Kolyma_Data/predictions/1971_strat_k5"

Nimg <- 3036



treeImg <- list()

for(i in 1:Nimg){
  treeImg[[i]] <- rast(paste0(dirP,"/tree/tree_predict_",i,".tif"))
  
  
}

for(i in 1:Nimg){
  treeImg[[i]][1:3,] <- 0
  treeImg[[i]][,1:3] <- 0
  treeImg[[i]][254:256,] <- 0
  treeImg[[i]][,254:256] <- 0
}

treeAll <- do.call(merge, treeImg)

writeRaster(treeAll, "/media/hkropp/cold/k5_71_strat/treeAll_1.tif")

waterImg <- list()

for(i in 1:Nimg){
  waterImg[[i]] <- rast(paste0(dirP,"/water/water_predict_",i,".tif"))
  
  
}

for(i in 1:Nimg){
  waterImg[[i]][1:3,] <- 0
  waterImg[[i]][,1:3] <- 0
  waterImg[[i]][254:256,] <- 0
  waterImg[[i]][,254:256] <- 0
  
}


waterAll <- do.call(merge, waterImg)

writeRaster(waterAll, "/media/hkropp/cold/k5_71_strat/waterAll_1.tif")

shrubImg <- list()

for(i in 1:Nimg){
  shrubImg[[i]] <- rast(paste0(dirP,"/shrub/shrub_predict_",i,".tif"))
  
  
}

for(i in 1:Nimg){
  shrubImg[[i]][1:3,] <- 0
  shrubImg[[i]][,1:3] <- 0
  shrubImg[[i]][254:256,] <- 0
  shrubImg[[i]][,254:256] <- 0
  
}

shrubAll <- do.call(merge, shrubImg)

writeRaster(shrubAll, "/media/hkropp/cold/k5_71_strat/shrubAll_1.tif")

lowDImg <- list()

for(i in 1:Nimg){
  lowDImg[[i]] <- rast(paste0(dirP,"/low/lowD_predict_",i,".tif"))
  
  
}

for(i in 1:Nimg){
  lowDImg[[i]][1:3,] <- 0
  lowDImg[[i]][,1:3] <- 0
  lowDImg[[i]][254:256,] <- 0
  lowDImg[[i]][,254:256] <- 0
  
  
}


lowDAll <- do.call(merge, lowDImg)

writeRaster(lowDAll, "/media/hkropp/cold/k5_71_strat/lowDAll_1.tif")

# Image merge second tile ---------

dirP2 <- "/media/hkropp/research/Kolyma_Data/predictions/1971_strat_k5_2"

Nimg <- 3036




treeImg2 <- list()

for(i in 1:Nimg){
  treeImg2[[i]] <- rast(paste0(dirP2,"/tree/tree_predict_",i,".tif"))
  
  
}

for(i in 1:Nimg){
  treeImg2[[i]][1:3,] <- 0
  treeImg2[[i]][,1:3] <- 0
  treeImg2[[i]][254:256,] <- 0
  treeImg2[[i]][,254:256] <- 0
}

treeAll2 <- do.call(merge, treeImg2)

writeRaster(treeAll2, "/media/hkropp/cold/k5_71_strat/treeAll_2.tif")

waterImg2 <- list()

for(i in 1:Nimg){
  waterImg2[[i]] <- rast(paste0(dirP2,"/water/water_predict_",i,".tif"))
  
  
}

for(i in 1:Nimg){
  waterImg2[[i]][1:3,] <- 0
  waterImg2[[i]][,1:3] <- 0
  waterImg2[[i]][254:256,] <- 0
  waterImg2[[i]][,254:256] <- 0
  
}

waterAll2 <- do.call(merge, waterImg2)

writeRaster(waterAll2, "/media/hkropp/cold/k5_71_strat/waterAll_2.tif")

shrubImg2 <- list()

for(i in 1:Nimg){
  shrubImg2[[i]] <- rast(paste0(dirP2,"/shrub/shrub_predict_",i,".tif"))
  
  
}

for(i in 1:Nimg){
  shrubImg2[[i]][1:3,] <- 0
  shrubImg2[[i]][,1:3] <- 0
  shrubImg2[[i]][254:256,] <- 0
  shrubImg2[[i]][,254:256] <- 0
  
}

shrubAll2 <- do.call(merge, shrubImg2)

writeRaster(shrubAll2, "/media/hkropp/cold/k5_71_strat/shrubAll_2.tif")

lowDImg2 <- list()

for(i in 1:Nimg){
  lowDImg2[[i]] <- rast(paste0(dirP2,"/low/lowD_predict_",i,".tif"))
  
  
}

for(i in 1:Nimg){
  lowDImg2[[i]][1:3,] <- 0
  lowDImg2[[i]][,1:3] <- 0
  lowDImg2[[i]][254:256,] <- 0
  lowDImg2[[i]][,254:256] <- 0
  
  
}


lowDAll2 <- do.call(merge, lowDImg2)

writeRaster(lowDAll2, "/media/hkropp/cold/k5_71_strat/lowDAll_2.tif")


# Image merge third tile ---------

dirP3 <- "/media/hkropp/research/Kolyma_Data/predictions/1971_strat_k5_3"

Nimg <- 3036




treeImg3 <- list()

for(i in 1:Nimg){
  treeImg3[[i]] <- rast(paste0(dirP3,"/tree/tree_predict_",i,".tif"))
  
  
}

for(i in 1:Nimg){
  treeImg3[[i]][1:3,] <- 0
  treeImg3[[i]][,1:3] <- 0
  treeImg3[[i]][254:256,] <- 0
  treeImg3[[i]][,254:256] <- 0
}


treeAll3 <- do.call(merge, treeImg3)

writeRaster(treeAll3, "/media/hkropp/cold/k5_71_strat/treeAll_3.tif")

waterImg3 <- list()

for(i in 1:Nimg){
  waterImg3[[i]] <- rast(paste0(dirP3,"/water/water_predict_",i,".tif"))
  
  
}

for(i in 1:Nimg){
  waterImg3[[i]][1:3,] <- 0
  waterImg3[[i]][,1:3] <- 0
  waterImg3[[i]][254:256,] <- 0
  waterImg3[[i]][,254:256] <- 0
  
}

waterAll3 <- do.call(merge, waterImg3)

writeRaster(waterAll3, "/media/hkropp/cold/k5_71_strat/waterAll_3.tif")

shrubImg3 <- list()

for(i in 1:Nimg){
  shrubImg3[[i]] <- rast(paste0(dirP3,"/shrub/shrub_predict_",i,".tif"))
  
  
}

for(i in 1:Nimg){
  shrubImg3[[i]][1:3,] <- 0
  shrubImg3[[i]][,1:3] <- 0
  shrubImg3[[i]][254:256,] <- 0
  shrubImg3[[i]][,254:256] <- 0
  
}

shrubAll3 <- do.call(merge, shrubImg3)

writeRaster(shrubAll3, "/media/hkropp/cold/k5_71_strat/shrubAll_3.tif")

lowDImg3 <- list()

for(i in 1:Nimg){
  lowDImg3[[i]] <- rast(paste0(dirP3,"/low/lowD_predict_",i,".tif"))
  
  
}

for(i in 1:Nimg){
  lowDImg3[[i]][1:3,] <- 0
  lowDImg3[[i]][,1:3] <- 0
  lowDImg3[[i]][254:256,] <- 0
  lowDImg3[[i]][,254:256] <- 0
  
  
}

lowDAll3 <- do.call(merge, lowDImg3)

writeRaster(lowDAll3, "/media/hkropp/cold/k5_71_strat/lowDAll_3.tif")


# Merge tile offsets ------

# resample to original

lowAll <- rast("/media/hkropp/cold/k5_71_strat/lowDAll_1.tif")
lowAll2 <- rast("/media/hkropp/cold/k5_71_strat/lowDAll_2.tif")
lowAll3 <- rast("/media/hkropp/cold/k5_71_strat/lowDAll_3.tif")

low2rs <- resample(lowAll2, lowAll)
low3rs <- resample(lowAll3, lowAll)
plot(lowAll)
plot(lowAll2)
plot(lowAll3)
lowStack <- c(lowAll, low2rs)
lowLayer <- max(lowStack, na.rm=TRUE)
plot(lowLayer)

writeRaster(lowLayer, "/media/hkropp/cold/k5_71_strat/lowLayer.tif")


waterAll <- rast("/media/hkropp/cold/k5_71_strat/waterAll_1.tif")
waterAll2 <- rast("/media/hkropp/cold/k5_71_strat/waterAll_2.tif")
waterAll3 <- rast("/media/hkropp/cold/k5_71_strat/waterAll_3.tif")

water2rs <- resample(waterAll2, waterAll)
water3rs <- resample(waterAll3, waterAll)
plot(waterAll)
plot(water2rs, add=TRUE)
plot(water3rs, add=TRUE, alpha=0.5)


waterStack <- c(waterAll, water2rs)
waterLayer <- max(waterStack, na.rm=TRUE)
plot(waterLayer)

writeRaster(waterLayer, "/media/hkropp/cold/k5_71_strat/waterLayer.tif")


treeAll <- rast("/media/hkropp/cold/k5_71_strat/treeAll_1.tif")
treeAll2 <- rast("/media/hkropp/cold/k5_71_strat/treeAll_2.tif")
treeAll3 <- rast("/media/hkropp/cold/k5_71_strat/treeAll_3.tif")

tree2rs <- resample(treeAll2, treeAll)
tree3rs <- resample(treeAll3, treeAll)

treeStack <- c(treeAll, tree2rs, tree3rs)
treeLayer <- max(treeStack, na.rm=TRUE)
plot(treeLayer)

writeRaster(treeLayer, "/media/hkropp/cold/k5_71_strat/treeLayer.tif")

shrubAll <- rast("/media/hkropp/cold/k5_71_strat/shrubAll_1.tif")
shrubAll2 <- rast("/media/hkropp/cold/k5_71_strat/shrubAll_2.tif")
shrubAll3 <- rast("/media/hkropp/cold/k5_71_strat/shrubAll_3.tif")

shrub2rs <- resample(shrubAll2, shrubAll)
shrub3rs <- resample(shrubAll3, shrubAll)
plot(shrubAll)
plot(shrub2rs)
plot(shrub3rs)
plot(shrubAll)


shrubStack <- c(shrubAll, shrub2rs)
shrubLayer <- max(shrubStack, na.rm=TRUE)
plot(shrubLayer)


writeRaster(shrubLayer, "/media/hkropp/cold/k5_71_strat/shrubLayer.tif")



# Make final map cover -------------
shrubLayer <- rast("/media/hkropp/cold/k5_71_strat/shrubLayer.tif")
treeLayer <- rast("/media/hkropp/cold/k5_71_strat/treeLayer.tif")
lowDLayer <- rast("/media/hkropp/cold/k5_71_strat/lowLayer.tif")
waterLayer <- rast("/media/hkropp/cold/k5_71_strat/waterLayer.tif")
plot(shrubLayer)
plot(treeLayer)
plot(lowDLayer)
plot(waterLayer)

# remove noise below set threshold

#v1 shrub and tiaga threshold at 0.4
#v2 increased threshold to 0.6 for both
treeMap <- ifel(treeLayer <= 0.08, 0, treeLayer)
waterMap <- ifel(waterLayer <= 0.08, 0, waterLayer)
shrubMap <- ifel(shrubLayer <= 0.65, 0, shrubLayer)
lowDMap <- ifel(lowDLayer <= 0.3, 0, lowDLayer)


plot(treeMap)
plot(waterMap)
plot(shrubMap)
plot(lowDMap)

# binary map of above

treeMapB <- ifel(treeLayer <= 0.08, 0, 1)
waterMapB <- ifel(waterLayer <= 0.08, 0, 1)
shrubMapB <- ifel(shrubLayer <= 0.65, 0, 1)
lowDMapB <- ifel(lowDLayer <= 0.3, 0, 1)




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




writeRaster(finalClass, "/media/hkropp/research/Kolyma_Data/predictions/maps/class1971_k5_strat_v3.tif", filetype="GTiff" )
