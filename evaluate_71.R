library(terra)
library(ggplot2)
library(dplyr)


# directory 
dir <-"/media/studentuser/PATRIOT/training/"
dirP <- "/media/studentuser/PATRIOT/predictions_71/valid"
dirM <- "/media/studentuser/PATRIOT/training/masks_img"
# images 13 and 175 are skipped...training images = 1-159, validation 160 - 200

# number of validation images
nValid <- 39

# predictions
treePredict <- list()
for(i in 161:174){
  treePredict[[i-160]] <- rast(paste0(dirP,"/tree/tree_predict_",i,".tif"))
}
for(i in 176:200){
  treePredict[[i-161]] <- rast(paste0(dirP,"/tree/tree_predict_",i,".tif"))
}

polyPredict <- list()
for(i in 161:174){
  polyPredict[[i-160]] <- rast(paste0(dirP,"/poly/poly_predict_",i,".tif"))
}
for(i in 176:200){
  polyPredict[[i-161]] <- rast(paste0(dirP,"/poly/poly_predict_",i,".tif"))
}

waterPredict <- list()
for(i in 161:174){
  waterPredict[[i-160]] <- rast(paste0(dirP,"/water/water_predict_",i,".tif"))
}
for(i in 176:200){
  waterPredict[[i-161]] <- rast(paste0(dirP,"/water/water_predict_",i,".tif"))
}

shrubPredict <- list()
for(i in 161:174){
  shrubPredict[[i-160]] <- rast(paste0(dirP,"/shrub/shrub_predict_",i,".tif"))
}
for(i in 176:200){
  shrubPredict[[i-161]] <- rast(paste0(dirP,"/shrub/shrub_predict_",i,".tif"))
}

lowDPredict <- list()
for(i in 161:174){
  lowDPredict[[i-160]] <- rast(paste0(dirP,"/lowD/lowD_predict_",i,".tif"))
}
for(i in 176:200){
  lowDPredict[[i-161]] <- rast(paste0(dirP,"/lowD/lowD_predict_",i,".tif"))
}

# images
img50 <- list()

for(i in 161:174){
  img50[[i-160]] <- rast(paste0(dir,"/img/img_",i,".tif"))
}
for(i in 176:200){
  img50[[i-161]] <- rast(paste0(dir,"/img/img_",i,".tif"))
}

# masks

treeMask <- list()
for(i in 161:174){
  path <- paste0(dirM,"/trees_img/tree_",i,".tif")
  if (file.exists(path)) {
    treeMask[[i-160]] <- rast(path)
  }
  else {
    emptyRaster <- img50[[i -160]]
    values(emptyRaster) = 0
    treeMask[[i - 160]] <- emptyRaster
  }
}
for(i in 176:200){
  path <- paste0(dirM,"/trees_img/tree_",i,".tif")
  if (file.exists(path)) {
    treeMask[[i-161]] <- rast(path)
  }
  else {
    emptyRaster <- img50[[i -161]]
    values(emptyRaster) = 0
    treeMask[[i - 161]] <- emptyRaster
  }
}

polyMask <- list()
for(i in 161:174){
  path <- paste0(dirM,"/polygonal_img/polygonal_",i,".tif")
  if (file.exists(path)) {
    polyMask[[i-160]] <- rast(path)
  }
  else {
    emptyRaster <- img50[[i -160]]
    values(emptyRaster) = 0
    polyMask[[i - 160]] <- emptyRaster
  }
}
for(i in 176:200){
  path <- paste0(dirM,"/polygonal_img/polygonal_",i,".tif")
  if (file.exists(path)) {
    polyMask[[i-161]] <- rast(path)
  }
  else {
    emptyRaster <- img50[[i -161]]
    values(emptyRaster) = 0
    polyMask[[i - 161]] <- emptyRaster
  }
}

waterMask <- list()
for(i in 161:174){
  path <- paste0(dirM,"/water_img/water_",i,".tif")
  if (file.exists(path)) {
    waterMask[[i-160]] <- rast(path)
  }
  else {
    emptyRaster <- img50[[i -160]]
    values(emptyRaster) = 0
    waterMask[[i - 160]] <- emptyRaster
  }
}
for(i in 176:200){
  path <- paste0(dirM,"/water_img/water_",i,".tif")
  if (file.exists(path)) {
    waterMask[[i-161]] <- rast(path)
  }
  else {
    emptyRaster <- img50[[i -161]]
    values(emptyRaster) = 0
    waterMask[[i - 161]] <- emptyRaster
  }
}

shrubMask <- list()
for(i in 161:174){
  path <- paste0(dirM,"/shrubs_img/shrub_",i,".tif")
  if (file.exists(path)) {
    shrubMask[[i-160]] <- rast(path)
  }
  else {
    emptyRaster <- img50[[i -160]]
    values(emptyRaster) = 0
    shrubMask[[i - 160]] <- emptyRaster
  }
}
for(i in 176:200){
  path <- paste0(dirM,"/shrubs_img/shrub_",i,".tif")
  if (file.exists(path)) {
    shrubMask[[i-161]] <- rast(path)
  }
  else {
    emptyRaster <- img50[[i -161]]
    values(emptyRaster) = 0
    shrubMask[[i - 161]] <- emptyRaster
  }
}

lowDMask <- list()
for(i in 161:174){
  path <- paste0(dirM,"/low_density_img/low_",i,".tif")
  if (file.exists(path)) {
    lowDMask[[i-160]] <- rast(path)
  }
  else {
    emptyRaster <- img50[[i -160]]
    values(emptyRaster) = 0
    lowDMask[[i - 160]] <- emptyRaster
  }
}
for(i in 176:200){
  path <- paste0(dirM,"/low_density_img/low_",i,".tif")
  if (file.exists(path)) {
    lowDMask[[i-161]] <- rast(path)
  }
  else {
    emptyRaster <- img50[[i -161]]
    emptyRaster <-  0
    lowDMask[[i - 161]] <- emptyRaster
  }
}

sampStack <- list()

for(i in 1:nValid){
  sampStack[[i]] <- c(treePredict[[i]],
                          polyPredict[[i]],
                          waterPredict[[i]],
                          shrubPredict[[i]],
                          lowDPredict[[i]],
                          treeMask[[i]],
                          polyMask[[i]],
                          waterMask[[i]],
                          shrubMask[[i]],
                          lowDMask[[i]],
                          img50[[i]]
  )
}

# check influence of threshold decision with IOU and accuracy

# make class under incremental thresholds
treeThresh <- list()
polyThresh <- list()
waterThresh <- list()
shrubThresh <- list()
lowDThresh <- list()


for(i in 1:nValid){
  treeThresh[[i]] <- c(ifel(treePredict[[i]] <= 0.1, 0, 1),
                            ifel(treePredict[[i]] <= 0.2, 0, 1),
                            ifel(treePredict[[i]] <= 0.3, 0, 1),
                            ifel(treePredict[[i]] <= 0.4, 0, 1),
                            ifel(treePredict[[i]] <= 0.5, 0, 1),
                            ifel(treePredict[[i]] <= 0.6, 0, 1),
                            ifel(treePredict[[i]] <= 0.7, 0, 1),
                            ifel(treePredict[[i]] <= 0.8, 0, 1),
                            ifel(treePredict[[i]] <= 0.9, 0, 1))
  
  polyThresh[[i]] <- c(ifel(polyPredict[[i]] <= 0.1, 0, 1),
                            ifel(polyPredict[[i]] <= 0.2, 0, 1),
                            ifel(polyPredict[[i]] <= 0.3, 0, 1),
                            ifel(polyPredict[[i]] <= 0.4, 0, 1),
                            ifel(polyPredict[[i]] <= 0.5, 0, 1),
                            ifel(polyPredict[[i]] <= 0.6, 0, 1),
                            ifel(polyPredict[[i]] <= 0.7, 0, 1),
                            ifel(polyPredict[[i]] <= 0.8, 0, 1),
                            ifel(polyPredict[[i]] <= 0.9, 0, 1))

  waterThresh[[i]] <- c(ifel(waterPredict[[i]] <= 0.1, 0, 1),
                            ifel(waterPredict[[i]] <= 0.2, 0, 1),
                            ifel(waterPredict[[i]] <= 0.3, 0, 1),
                            ifel(waterPredict[[i]] <= 0.4, 0, 1),
                            ifel(waterPredict[[i]] <= 0.5, 0, 1),
                            ifel(waterPredict[[i]] <= 0.6, 0, 1),
                            ifel(waterPredict[[i]] <= 0.7, 0, 1),
                            ifel(waterPredict[[i]] <= 0.8, 0, 1),
                            ifel(waterPredict[[i]] <= 0.9, 0, 1))

  shrubThresh[[i]] <- c(ifel(shrubPredict[[i]] <= 0.1, 0, 1),
                            ifel(shrubPredict[[i]] <= 0.2, 0, 1),
                            ifel(shrubPredict[[i]] <= 0.3, 0, 1),
                            ifel(shrubPredict[[i]] <= 0.4, 0, 1),
                            ifel(shrubPredict[[i]] <= 0.5, 0, 1),
                            ifel(shrubPredict[[i]] <= 0.6, 0, 1),
                            ifel(shrubPredict[[i]] <= 0.7, 0, 1),
                            ifel(shrubPredict[[i]] <= 0.8, 0, 1),
                            ifel(shrubPredict[[i]] <= 0.9, 0, 1))

  lowDThresh[[i]] <- c(ifel(lowDPredict[[i]] <= 0.1, 0, 1),
                            ifel(lowDPredict[[i]] <= 0.2, 0, 1),
                            ifel(lowDPredict[[i]] <= 0.3, 0, 1),
                            ifel(lowDPredict[[i]] <= 0.4, 0, 1),
                            ifel(lowDPredict[[i]] <= 0.5, 0, 1),
                            ifel(lowDPredict[[i]] <= 0.6, 0, 1),
                            ifel(lowDPredict[[i]] <= 0.7, 0, 1),
                            ifel(lowDPredict[[i]] <= 0.8, 0, 1),
                            ifel(lowDPredict[[i]] <= 0.9, 0, 1))
}

#calculate IOU and accuracy for each
treeTot <- list()
polyTot <- list()
waterTot <- list()
shrubTot <- list()
lowDTot <- list()

for(i in 1:nValid){
  
  treeTot[[i]] <- treeThresh[[i]]+treeMask[[i]]
  polyTot[[i]] <- polyThresh[[i]]+polyMask[[i]]
  waterTot[[i]] <- waterThresh[[i]]+waterMask[[i]]
  shrubTot[[i]] <- shrubThresh[[i]]+shrubMask[[i]]
  lowDTot[[i]] <- lowDThresh[[i]]+lowDMask[[i]]
}

plot(lowDMask[[2]])
plot(lowDTot[[2]][[2]])


treeAssessDF <- list()
polyAssessDF <- list()
waterAssessDF <- list()
shrubAssessDF <- list()
lowDAssessDF <- list()
for(i in 1:nValid){
  treeAssessDF[[i]] <- freq(treeTot[[i]])
  polyAssessDF[[i]] <- freq(polyTot[[i]])
  waterAssessDF[[i]] <- freq(waterTot[[i]])
  shrubAssessDF[[i]] <- freq(shrubTot[[i]])
  lowDAssessDF[[i]] <- freq(lowDTot[[i]])
}  

treeAssess <- list()
polyAssess <- list()
waterAssess <- list()
shrubAssess <- list()
lowDAssess <- list()

for(i in 1:nValid){
  splitTreeAssess = split(treeAssessDF[[i]], f = treeAssessDF[[i]]$layer)
  treeAssess[[i]] = splitTreeAssess

  splitPolyAssess = split(polyAssessDF[[i]], f = polyAssessDF[[i]]$layer)
  polyAssess[[i]] = splitPolyAssess

  splitWaterAssess = split(waterAssessDF[[i]], f = waterAssessDF[[i]]$layer)
  waterAssess[[i]] = splitWaterAssess

  splitShrubAssess = split(shrubAssessDF[[i]], f = shrubAssessDF[[i]]$layer)
  shrubAssess[[i]] = splitShrubAssess

  splitLowDAssess = split(lowDAssessDF[[i]], f = lowDAssessDF[[i]]$layer)
  lowDAssess[[i]] = splitLowDAssess
}

#calculate IOU using pixels as area (1 pixel)
# 1+ 2 are union and 2 are intersection

threshSeq <- seq(0.1,0.9,by=0.1)


treeIOU <- list()
treeDF <- data.frame()

polyIOU <- list()
polyDF <- data.frame()

waterIOU <- list()
waterDF <- data.frame()

shrubIOU <- list()
shrubDF <- data.frame()

lowDIOU <- list()
lowDDF <- data.frame()

for(i in 1:nValid){
  if(nrow(treeAssess[[i]][[1]]) == 3){
    treeDF <- data.frame(thresh=threshSeq[1],
                          IOU = treeAssess[[i]][[1]][3,2]/(treeAssess[[i]][[1]][2,2]+treeAssess[[i]][[1]][3,2]),
                          imgN = i)
  }else{ treeDF <- data.frame(thresh=threshSeq[1],
                               IOU = 0,
                               imgN = i)}
  for(j in 2:9){
    if(nrow(treeAssess[[i]][[j]]) == 3){  
      treeDF <- rbind(treeDF,data.frame(thresh=threshSeq[j],
                                          IOU = treeAssess[[i]][[j]][3,2]/(treeAssess[[i]][[j]][2,2]+treeAssess[[i]][[j]][3,2]),
                                          imgN=i))
    }else{ treeDF <- rbind(treeDF,data.frame(thresh=threshSeq[j],
                                               IOU = 0,
                                               imgN=i))}
  }
  treeIOU[[i]] <- treeDF
}


for(i in 1:nValid){
  if(nrow(polyAssess[[i]][[1]]) == 3){
    polyDF <- data.frame(thresh=threshSeq[1],
                         IOU = polyAssess[[i]][[1]][3,2]/(polyAssess[[i]][[1]][2,2]+polyAssess[[i]][[1]][3,2]),
                         imgN = i)
  }else{ polyDF <- data.frame(thresh=threshSeq[1],
                              IOU = 0,
                              imgN = i)}
  for(j in 2:9){
    if(nrow(polyAssess[[i]][[j]]) == 3){  
      polyDF <- rbind(polyDF,data.frame(thresh=threshSeq[j],
                                        IOU = polyAssess[[i]][[j]][3,2]/(polyAssess[[i]][[j]][2,2]+polyAssess[[i]][[j]][3,2]),
                                        imgN=i))
    }else{ polyDF <- rbind(polyDF,data.frame(thresh=threshSeq[j],
                                             IOU = 0,
                                             imgN=i))}
  }
  polyIOU[[i]] <- polyDF
}

for(i in 1:nValid){
  if(nrow(waterAssess[[i]][[1]]) == 3){
    waterDF <- data.frame(thresh=threshSeq[1],
                         IOU = waterAssess[[i]][[1]][3,2]/(waterAssess[[i]][[1]][2,2]+waterAssess[[i]][[1]][3,2]),
                         imgN = i)
  }else{ waterDF <- data.frame(thresh=threshSeq[1],
                              IOU = 0,
                              imgN = i)}
  for(j in 2:9){
    if(nrow(waterAssess[[i]][[j]]) == 3){  
      waterDF <- rbind(waterDF,data.frame(thresh=threshSeq[j],
                                        IOU = waterAssess[[i]][[j]][3,2]/(waterAssess[[i]][[j]][2,2]+waterAssess[[i]][[j]][3,2]),
                                        imgN=i))
    }else{ waterDF <- rbind(waterDF,data.frame(thresh=threshSeq[j],
                                             IOU = 0,
                                             imgN=i))}
  }
  waterIOU[[i]] <- waterDF
}

for(i in 1:nValid){
  if(nrow(shrubAssess[[i]][[1]]) == 3){
    shrubDF <- data.frame(thresh=threshSeq[1],
                         IOU = shrubAssess[[i]][[1]][3,2]/(shrubAssess[[i]][[1]][2,2]+shrubAssess[[i]][[1]][3,2]),
                         imgN = i)
  }else{ shrubDF <- data.frame(thresh=threshSeq[1],
                              IOU = 0,
                              imgN = i)}
  for(j in 2:9){
    if(nrow(shrubAssess[[i]][[j]]) == 3){  
      shrubDF <- rbind(shrubDF,data.frame(thresh=threshSeq[j],
                                        IOU = shrubAssess[[i]][[j]][3,2]/(shrubAssess[[i]][[j]][2,2]+shrubAssess[[i]][[j]][3,2]),
                                        imgN=i))
    }else{ shrubDF <- rbind(shrubDF,data.frame(thresh=threshSeq[j],
                                             IOU = 0,
                                             imgN=i))}
  }
  shrubIOU[[i]] <- shrubDF
}

for(i in 1:nValid){
  if(nrow(lowDAssess[[i]][[1]]) == 3){
    lowDDF <- data.frame(thresh=threshSeq[1],
                         IOU = lowDAssess[[i]][[1]][3,2]/(lowDAssess[[i]][[1]][2,2]+lowDAssess[[i]][[1]][3,2]),
                         imgN = i)
  }else{ lowDDF <- data.frame(thresh=threshSeq[1],
                              IOU = 0,
                              imgN = i)}
  for(j in 2:9){
    if(nrow(lowDAssess[[i]][[j]]) == 3){  
      lowDDF <- rbind(lowDDF,data.frame(thresh=threshSeq[j],
                                        IOU = lowDAssess[[i]][[j]][3,2]/(lowDAssess[[i]][[j]][2,2]+lowDAssess[[i]][[j]][3,2]),
                                        imgN=i))
    }else{ lowDDF <- rbind(lowDDF,data.frame(thresh=threshSeq[j],
                                             IOU = 0,
                                             imgN=i))}
  }
  lowDIOU[[i]] <- lowDDF
}

IOUtree <- do.call("rbind", treeIOU)
IOUpoly <- do.call("rbind", polyIOU)
IOUwater <- do.call("rbind", waterIOU)
IOUshrub <- do.call("rbind", shrubIOU)
IOUlowD <- do.call("rbind", lowDIOU)

#calculate IOU across all images
treeTotDF <- list()
tempDFt <- data.frame()
for(i in 1:nValid){
  tempDFt <- data.frame(treeAssess[[i]][[1]])
  tempDFt$thresh <- rep(0.1,nrow(tempDFt))
  for(j in 2:9){
    tempDFt <- rbind(tempDFt,data.frame(treeAssess[[i]][[j]],
                                        thresh=rep(j/10,nrow(data.frame(treeAssess[[i]][[j]])))))
  }  
  treeTotDF[[i]] <- tempDFt
}  


polyTotDF <- list()
tempDFp <- data.frame()
for(i in 1:nValid){
  tempDFp <- data.frame(polyAssess[[i]][[1]])
  tempDFp$thresh <- rep(0.1,nrow(tempDFp))
  for(j in 2:9){
    tempDFp <- rbind(tempDFp,data.frame(polyAssess[[i]][[j]],
                                        thresh=rep(j/10,nrow(data.frame(polyAssess[[i]][[j]])))))
  }  
  polyTotDF[[i]] <- tempDFp
}  

waterTotDF <- list()
tempDFw <- data.frame()
for(i in 1:nValid){
  tempDFw <- data.frame(waterAssess[[i]][[1]])
  tempDFw$thresh <- rep(0.1,nrow(tempDFw))
  for(j in 2:9){
    tempDFw <- rbind(tempDFw,data.frame(waterAssess[[i]][[j]],
                                        thresh=rep(j/10,nrow(data.frame(waterAssess[[i]][[j]])))))
  }  
  waterTotDF[[i]] <- tempDFw
} 

shrubTotDF <- list()
tempDFs <- data.frame()
for(i in 1:nValid){
  tempDFs <- data.frame(shrubAssess[[i]][[1]])
  tempDFs$thresh <- rep(0.1,nrow(tempDFs))
  for(j in 2:9){
    tempDFs <- rbind(tempDFs,data.frame(shrubAssess[[i]][[j]],
                                        thresh=rep(j/10,nrow(data.frame(shrubAssess[[i]][[j]])))))
  }  
  shrubTotDF[[i]] <- tempDFs
}  

lowDTotDF <- list()
tempDFl <- data.frame()
for(i in 1:nValid){
  tempDFl <- data.frame(lowDAssess[[i]][[1]])
  tempDFl$thresh <- rep(0.1,nrow(tempDFl))
  for(j in 2:9){
    tempDFl <- rbind(tempDFl,data.frame(lowDAssess[[i]][[j]],
                                        thresh=rep(j/10,nrow(data.frame(lowDAssess[[i]][[j]])))))
  }  
  lowDTotDF[[i]] <- tempDFl
}  

treeSumDF <- do.call("rbind",treeTotDF)
polySumDF <- do.call("rbind",polyTotDF)
waterSumDF <- do.call("rbind",waterTotDF)
shrubSumDF <- do.call("rbind",shrubTotDF)
lowDSumDF <- do.call("rbind",lowDTotDF)

IOUcalc <- function(x,y){
  sum(x[y == 2])/ (sum(x[y == 2])+ sum(x[y == 1]))
}

allIOUtree <- treeSumDF %>%
  group_by(thresh) %>%
  summarize(IOU=IOUcalc(count,value))

allIOUpoly <- polySumDF %>%
  group_by(thresh) %>%
  summarize(IOU=IOUcalc(count,value))

allIOUwater <- waterSumDF %>%
  group_by(thresh) %>%
  summarize(IOU=IOUcalc(count,value))

allIOUshrub <- shrubSumDF %>%
  group_by(thresh) %>%
  summarize(IOU=IOUcalc(count,value))

allIOUlowD <- lowDSumDF %>%
  group_by(thresh) %>%
  summarize(IOU=IOUcalc(count,value))

allIOUtree$type <- rep("tree", nrow(allIOUtree))
allIOUpoly$type <- rep("polygonal tundra", nrow(allIOUpoly))
allIOUwater$type <- rep("water", nrow(allIOUwater))
allIOUshrub$type <- rep("shrub", nrow(allIOUshrub))
allIOUlowD$type <- rep("low density forest", nrow(allIOUlowD))

IOU50 <- rbind(allIOUtree,allIOUpoly,allIOUwater, allIOUshrub, allIOUlowD)

#plot IOU over the different thresholds

ggplot(data=IOUtree, aes(x=thresh,y=IOU,color=imgN))+
  geom_point()+
  geom_path()

ggplot(data=IOUpoly, aes(x=thresh,y=IOU,color=imgN))+
  geom_point()+
  geom_path()

ggplot(data=IOUwater, aes(x=thresh,y=IOU,color=imgN))+
  geom_point()+
  geom_path()

ggplot(data=IOUshrub, aes(x=thresh,y=IOU,color=imgN))+
  geom_point()+
  geom_path()

ggplot(data=IOUlowD, aes(x=thresh,y=IOU,color=imgN))+
  geom_point()+
  geom_path()

ggplot(data=IOU50, aes(x=thresh,y=IOU,color=type))+
  geom_point()+
  geom_path()
#calculate threshold with highest IOU