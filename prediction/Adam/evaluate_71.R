library(terra)
library(ggplot2)
library(dplyr)


# directory 
dir <-"/media/studentuser/Seagate Portable Drive/training/"
dirV <- "/media/studentuser/Seagate Portable Drive/predictions_71/kernalmodel/valid"
dirM <- "/media/studentuser/Seagate Portable Drive/training/masks_img"
# images 13 and 175 are skipped

# number of validation images
nValid <- 40
nTotTraining <- 400
nTraining <- nTotTraining - nValid

# predictions
treePredict <- list()
for(i in nTraining + 1:nValid){
  treePredict[[i-nTraining]] <- rast(paste0(dirV,"/tree/tree_predict_",i,".tif"))
}

waterPredict <- list()
for(i in nTraining + 1:nValid){
  waterPredict[[i-nTraining]] <- rast(paste0(dirV,"/water/water_predict_",i,".tif"))
}

shrubPredict <- list()
for(i in nTraining + 1:nValid){
  shrubPredict[[i-nTraining]] <- rast(paste0(dirV,"/shrub/shrub_predict_",i,".tif"))
}

lowDPredict <- list()
for(i in nTraining + 1:nValid){
  lowDPredict[[i-nTraining]] <- rast(paste0(dirV,"/lowD/lowD_predict_",i,".tif"))
}

# images
imgV <- list()

for(i in nTraining + 1:nValid){
  imgV[[i-nTraining]]  <- rast(paste0(dir,"/img/img_",i,".tif"))
}

# masks

treeMask <- list()
for(i in nTraining + 1:nValid){
  path <- paste0(dirM,"/trees_img/tree_",i,".tif")
  if (file.exists(path)) {
    treeMask[[i-nTraining]] <- rast(path)
  }
  else {
    emptyRaster <- imgV[[i -nTraining]]
    values(emptyRaster) = 0
    treeMask[[i - nTraining]] <- emptyRaster
  }
}

waterMask <- list()
for(i in nTraining + 1:nValid){
  path <- paste0(dirM,"/water_img/water_",i,".tif")
  if (file.exists(path)) {
    waterMask[[i - nTraining]] <- rast(path)
  }
  else {
    emptyRaster <- imgV[[i - nTraining]]
    values(emptyRaster) = 0
    waterMask[[i - nTraining]] <- emptyRaster
  }
}

shrubMask <- list()
for(i in nTraining + 1:nValid){
  path <- paste0(dirM,"/shrubs_img/shrub_",i,".tif")
  if (file.exists(path)) {
    shrubMask[[i - nTraining]] <- rast(path)
  }
  else {
    emptyRaster <- imgV[[i - nTraining]]
    values(emptyRaster) = 0
    shrubMask[[i - nTraining]] <- emptyRaster
  }
}

lowDMask <- list()
for(i in nTraining + 1:nValid){
  path <- paste0(dirM,"/low_density_img/low_",i,".tif")
  if (file.exists(path)) {
    lowDMask[[i - nTraining]] <- rast(path)
  }
  else {
    emptyRaster <- imgV[[i - nTraining]]
    values(emptyRaster) = 0
    lowDMask[[i - nTraining]] <- emptyRaster
  }
}

sampStack <- list()

for(i in 1:nValid){
  sampStack[[i]] <- c(treePredict[[i]],
                          waterPredict[[i]],
                          shrubPredict[[i]],
                          lowDPredict[[i]],
                          treeMask[[i]],
                          waterMask[[i]],
                          shrubMask[[i]],
                          lowDMask[[i]],
                          imgV[[i]]
  )
}

# check influence of threshold decision with IOU and accuracy

# make class under incremental thresholds
treeThresh <- list()
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
waterTot <- list()
shrubTot <- list()
lowDTot <- list()

for(i in 1:nValid){
  
  treeTot[[i]] <- treeThresh[[i]]+treeMask[[i]]
  waterTot[[i]] <- waterThresh[[i]]+waterMask[[i]]
  shrubTot[[i]] <- shrubThresh[[i]]+shrubMask[[i]]
  lowDTot[[i]] <- lowDThresh[[i]]+lowDMask[[i]]
}

plot(lowDMask[[6]])
plot(lowDTot[[6]][[2]])
plot(treeMask[[6]])
plot(waterMask[[6]])

plot(treeTot[[6]][[2]])
plot(imgV[[6]], col=grey(1:100/100))

treeAssessDF <- list()
waterAssessDF <- list()
shrubAssessDF <- list()
lowDAssessDF <- list()
for(i in 1:nValid){
  treeAssessDF[[i]] <- freq(treeTot[[i]])
  waterAssessDF[[i]] <- freq(waterTot[[i]])
  shrubAssessDF[[i]] <- freq(shrubTot[[i]])
  lowDAssessDF[[i]] <- freq(lowDTot[[i]])
}  

treeAssess <- list()
waterAssess <- list()
shrubAssess <- list()
lowDAssess <- list()

for(i in 1:nValid){
  splitTreeAssess = split(treeAssessDF[[i]], f = treeAssessDF[[i]]$layer)
  treeAssess[[i]] = splitTreeAssess

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
waterSumDF <- do.call("rbind",waterTotDF)
shrubSumDF <- do.call("rbind",shrubTotDF)
lowDSumDF <- do.call("rbind",lowDTotDF)

IOUcalc <- function(x,y){
  sum(x[y == 2])/ (sum(x[y == 2])+ sum(x[y == 1]))
}

allIOUtree <- treeSumDF %>%
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
allIOUwater$type <- rep("water", nrow(allIOUwater))
allIOUshrub$type <- rep("shrub", nrow(allIOUshrub))
allIOUlowD$type <- rep("low density forest", nrow(allIOUlowD))

IOU50 <- rbind(allIOUtree,allIOUwater, allIOUshrub, allIOUlowD)

#plot IOU over the different thresholds

ggplot(data=IOUtree, aes(x=thresh,y=IOU,color=imgN))+
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

