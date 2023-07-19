library(rast)
library(sf)
library(mapview)


# Image merge ---------

dirP <- "/media/studentuser/Seagate Portable Drive/predicitons_71/all"

Nimg <- 1740


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
  lowDImg[[i]] <- rast(paste0(dirP,"/lowD/lowD_predict_",i,".tif"))
  
  
}

lowDAll <- do.call(merge, lowDImg)


# get maximum prob
treeLayer <- calc(treeAll, function(x){max(x, na.rm=TRUE)})
plot(treeLayer)

waterLayer <- calc(waterAll, function(x){max(x, na.rm=TRUE)})
plot(waterLayer)

shrubLayer <- calc(shrubAll, function(x){max(x, na.rm=TRUE)})
plot(shrubLayer)

lowDLayer <- calc(lowDAll, function(x){max(x, na.rm=TRUE)})
plot(lowDLayer)


# Make final map cover -------------

# remove noise below set threshold

threshold <- 0.2

treeMap <- ifel(treeLayer <= threshold, 0, x)
waterMap <- ifel(waterLayer <= threshold, 0, x)
shrubMap <- ifel(shrubLayer <= threshold, 0, x)
lowDMap <- ifel(lowDLayer <= threshold, 0, x)


# binary map of above

treeMapB <- ifel(treeLayer <= threshold, 0, 1)
waterMapB <- ifel(waterLayer <= threshold, 0, 1)
shrubMapB <- ifel(shrubLayer <= threshold, 0, 1)
lowDMapB <- ifel(lowDLayer <= threshold, 0, 1)

binaryStack <- c(treeMapB,waterMapB,shrubMapB,lowDMapB)


# need to filter so only one class for each pixel
# take the highest probability

coverStack <- c(treeMap, waterMapB, shrubMapB, lowDMapB)

which.max2 <- function(x){
  max_idx <- which.max(x)   # Get the max
  ifel(length(max_idx)== 0,return(NA),return(max_idx))
}

classR <- calc(coverStack, which.max2)

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
finalClass <- treeClass+buildClass2+paveClass2

plot(finalClass)




writerast(finalClass, "/media/studentuser/Seagate Portable Drive/predicitons_71/all/finalClass.tif", format="GTiff" )