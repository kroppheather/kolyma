library(terra)
library(dplyr)


# Image merge ---------

dirP <- "/media/hkropp/research/Kolyma_Data/predictions/v2/2020_k5_1"

Nimg <- 12144



treeImg <- list()

for(i in 1:Nimg){
  treeImg[[i]] <- rast(paste0(dirP,"/taiga/taiga_predict_",i,".tif"))
  
  
}

treeAll <- do.call(merge, treeImg)
plot(treeAll)
writeRaster(treeAll, "/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/taigaAll_1.tif")

waterImg <- list()

for(i in 1:Nimg){
  waterImg[[i]] <- rast(paste0(dirP,"/water/water_predict_",i,".tif"))
  
  
}

waterAll <- do.call(merge, waterImg)
plot(waterAll)
writeRaster(waterAll, "/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/waterAll_1.tif")

shrubImg <- list()

for(i in 1:Nimg){
  shrubImg[[i]] <- rast(paste0(dirP,"/shrub/shrub_predict_",i,".tif"))
  
  
}

shrubAll <- do.call(merge, shrubImg)
plot(shrubAll)
writeRaster(shrubAll, "/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/shrubAll_1.tif")



# Image merge tile 2 ---------

dirP2 <- "/media/hkropp/research/Kolyma_Data/predictions/v2/2020_k5_2"

Nimg <- 12144



treeImg2 <- list()

for(i in 1:Nimg){
  treeImg2[[i]] <- rast(paste0(dirP2,"/taiga/taiga_predict_",i,".tif"))
  
  
}

treeAll2 <- do.call(merge, treeImg2)
plot(treeAll2)
writeRaster(treeAll2, "/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/taigaAll_2.tif")



waterImg2 <- list()

for(i in 1:Nimg){
  waterImg2[[i]] <- rast(paste0(dirP2,"/water/water_predict_",i,".tif"))
  
  
}

waterAll2 <- do.call(merge, waterImg2)
plot(waterAll2)
writeRaster(waterAll2, "/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/waterAll_2.tif")


shrubImg2 <- list()

for(i in 1:Nimg){
  shrubImg2[[i]] <- rast(paste0(dirP2,"/shrub/shrub_predict_",i,".tif"))
  
  
}

shrubAll2 <- do.call(merge, shrubImg2)
plot(shrubAll2)
writeRaster(shrubAll2, "/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/shrubAll_2.tif")


# Image merge tile 3 ---------

dirP3 <- "/media/hkropp/research/Kolyma_Data/predictions/v2/2020_k5_3"

Nimg <- 12144


treeImg3 <- list()

for(i in 1:Nimg){
  treeImg3[[i]] <- rast(paste0(dirP3,"/taiga/taiga_predict_",i,".tif"))
  
  
}

treeAll3 <- do.call(merge, treeImg3)
plot(treeAll3)
writeRaster(treeAll3, "/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/taigaAll_3.tif")



waterImg3 <- list()

for(i in 1:Nimg){
  waterImg3[[i]] <- rast(paste0(dirP3,"/water/water_predict_",i,".tif"))
  
  
}

waterAll3 <- do.call(merge, waterImg3)
plot(waterAll3)
writeRaster(waterAll3, "/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/waterAll_3.tif")



shrubImg3 <- list()

for(i in 1:Nimg){
  shrubImg3[[i]] <- rast(paste0(dirP3,"/shrub/shrub_predict_",i,".tif"))
  
  
}

shrubAll3 <- do.call(merge, shrubImg3)
plot(shrubAll3)
writeRaster(shrubAll3, "/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/shrubAll_3.tif")


##################################
##################################
# read in data part 2


# Merge tile offsets ------


waterAll <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/waterAll_1.tif")
waterAll2 <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/waterAll_2.tif")
waterAll3 <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/waterAll_3.tif")

water2rs <- resample(waterAll2, waterAll)
water3rs <- resample(waterAll3, waterAll)

waterStack <- c(waterAll, water2rs, water3rs)
waterLayer <- mean(waterStack, na.rm=TRUE)
plot(waterLayer)

writeRaster(waterLayer, "/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/waterLayer.tif")



treeAll <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/taigaAll_1.tif")
treeAll2 <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/taigaAll_2.tif")
treeAll3 <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/taigaAll_3.tif")

tree2rs <- resample(treeAll2, treeAll)
tree3rs <- resample(treeAll3, treeAll)

treeStack <- c(treeAll, tree2rs, tree3rs)
treeLayer <- mean(treeStack, na.rm=TRUE)
plot(treeLayer)

writeRaster(treeLayer, "/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/taigaLayer.tif")

shrubAll <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/shrubAll_1.tif")
shrubAll2 <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/shrubAll_2.tif")
shrubAll3 <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/shrubAll_3.tif")

shrub2rs <- resample(shrubAll2, shrubAll)
shrub3rs <- resample(shrubAll3, shrubAll)


shrubStack <- c(shrubAll, shrub2rs, shrub3rs)
shrubLayer <- mean(shrubStack, na.rm=TRUE)
plot(shrubLayer)


writeRaster(shrubLayer, "/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/shrubLayer.tif")





# Make final map cover -------------

shrubLayer <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/shrubLayer.tif")
treeLayer <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/taigaLayer.tif")
waterLayer <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/class_save/2020_k5/waterLayer.tif")
plot(waterLayer)
plot(shrubLayer)
plot(treeLayer)
# remove noise below set threshold

# version 
treeMap <- ifel(treeLayer <= 0.3, 0, treeLayer)
waterMap <- ifel(waterLayer <= 0.95, 0, waterLayer)
shrubMap <- ifel(shrubLayer <= 0.2, 0, shrubLayer)


# binary map of above

treeMapB <- ifel(treeLayer <= 0.3, 0, 1)
waterMapB <- ifel(waterLayer <= 0.95, 0, 1)
shrubMapB <- ifel(shrubLayer <= 0.2, 0, 1)





# need to filter so only one class for each pixel
# take the highest probability

coverStack <- c(treeMap, waterMap, shrubMap)

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



plot(treeClass)
plot(waterClass)
plot(shrubClass)



waterClass2 <- waterClass*2
shrubClass2 <- shrubClass*3


# other will be zero, trees =1, water =2, shrub =3, low =4
finalClass <- treeClass+waterClass2+shrubClass2

plot(finalClass)




writeRaster(finalClass, "/media/hkropp/research/Kolyma_Data/predictions/v2/maps/class2020_k5_v3.tif", filetype="GTiff" )
