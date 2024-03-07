##########################################################
###### organize georef images and prepare for tiling ####
###### runs r v4.3.0                                 #####
##########################################################
# matches extent since the 2020 imagery could only be obtained for a smaller extent

library(terra)
library(dplyr)


imgDir <- "K:/Environmental_Studies/hkropp/Private/siberia_wv/1971/clip_07_16_71.tif"

extent <- vect("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/extent_match/extent_bound.shp")

# small extent that matches available 2020 data
ext_img <- rast(imgDir)
img_c <- crop(ext_img, extent)
img_final <-rast("K:/Environmental_Studies/hkropp/Private/siberia_wv/1971/ext_07_16_71.tif")
plot(img_final, col = grey(1:100/100))

##### use training images in extent set up by Koplik project ----
# read in training from larger extent and pull out samples that were in the extent

imgO <- list()
for(i in 1:400){
  imgO[[i]] <- rast(paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/img/img_",i,".tif"))
}
# identify images that are within the smaller extent
imgRel <- logical()
for(i in 1:400){
  imgRel[i] <- relate(imgO[[i]],extent,"within")
}
imgPull <- which(imgRel == TRUE)
imgKeepDF <- data.frame(oldID = imgPull, newID = seq(1, length(imgPull)))

imgS <- list()
for(i in 1:length(imgPull)){
  imgS[[i]] <- imgO[[imgPull[i]]]
  
}

# save images
for(i in 1:length(imgPull)){
  writeRaster(imgS[[i]], paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/training/img/img_",i,".tif"))
}

# save raster shapefiles that were already created
# water
wfiles <- list.files("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks/water/", pattern=".shp")
wXML <-  grepl(".xml",wfiles)
wfiles <- wfiles[wXML == FALSE]
wfilesN <- as.numeric(gsub("\\D","", wfiles))
wpresent <- imgPull %in% wfilesN

w_mask <- list()
for(i in 1:length(imgPull)){
  if(wpresent[i] == TRUE){
    w_mask <- vect(paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks/water/water_",
                             imgPull[i],".shp"))
    writeVector(w_mask, 
                paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/shapefiles/water/water_",i,".shp"))
  }
}

# shrub
sfiles <- list.files("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks/shrubs", pattern=".shp")
sXML <- grepl(".xml",sfiles)
sfiles <- sfiles[sXML == FALSE]
sfilesN <- as.numeric(gsub("\\D","", sfiles))
spresent <- imgPull %in% sfilesN

s_mask <- list()
for(i in 1:length(imgPull)){
  if(spresent[i] == TRUE){
    s_mask <- vect(paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks/shrubs/shrub_",
                          imgPull[i],".shp"))
    writeVector(s_mask, 
                paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/shapefiles/shrub/shrub_",i,".shp"))
  }
}

# tree
tfiles <- list.files("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks/trees/", pattern=".shp")
tXML <- grepl(".xml",tfiles)
tfiles <- tfiles[sXML == FALSE]
tfilesN <- as.numeric(gsub("\\D","", tfiles))
tpresent <- imgPull %in% tfilesN

t_mask <- list()
for(i in 1:length(imgPull)){
  if(tpresent[i] == TRUE){
    t_mask <- vect(paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks/trees/tree_",
                          imgPull[i],".shp"))
    writeVector(t_mask, 
                paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/shapefiles/tree/tree_",i,".shp"))
  }
}

# low
lfiles <- list.files("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks/low_density/", pattern=".shp")
lfilesN <- as.numeric(gsub("\\D","", lfiles))
lpresent <- imgPull %in% lfilesN

l_mask <- list()
for(i in 1:length(imgPull)){
  if(lpresent[i] == TRUE){
    l_mask <- vect(paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks/low_density/low_",
                          imgPull[i],".shp"))
    writeVector(l_mask, 
                paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/training/masks_img/low/low_",i,".shp"))
  }
}

write.table(imgKeepDF,"K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/orig_71_imgN.csv", 
            sep=",", row.names=FALSE)

########## set up new training image samples in extent -------

# sample rows and cols try to avoid na buffer
set.seed(34)
rowsi <- sample(1:(nrow(img_final)-255), 200)
set.seed(87)
colsi <- sample(1:(ncol(img_final)-255), 200)

# subset raster
training.samples <- list()
training.check <- numeric()
for(i in 1:200){
  training.samples[[i]] <-  img_final[rowsi[i]:(rowsi[i]+255), colsi[i]:(colsi[i]+255), drop=FALSE]
  # create a check to remove any training samples with NAs
  training.check[i] <- ifelse(is.na(mean(values(training.samples[[i]],mat=FALSE))) == TRUE,0,1)
}

training.valid <- which(training.check == 1)
samples.use <- list()
#first sample looks odd...
#first sample training valid range from 1 to 220
for(i in 1:150){
  samples.use[[i]] <- training.samples[[training.valid[i]]]
}
plot(samples.use[[150]], col=gray(1:100/100))

for(i in 1:150){
  writeRaster(samples.use[[i]], paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/training/img/img_",i+50,".tif"))
} 


tileI <- makeTiles(img_final, c(256,256), "K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/img_256/img.tif")





imgDir <- "/media/hkropp/research/Kolyma_Data/imagery_71/1971/clip_07_16_71.tif"

extent <- vect("/media/hkropp/research/Kolyma_Data/imagery_71/extent_match/extent_bound.shp")

# small extent that matches available 2020 data
ext_img <- rast(imgDir)
img_c <- crop(ext_img, extent)
plot(img_c, col=grey(1:100/100))
dim(img_c)
imgO2 <- img_c[25:16851,25:11728, drop=FALSE]
plot(imgO2, col=grey(1:100/100))

tileI <- makeTiles(imgO2, c(256,256), "/media/hkropp/research/Kolyma_Data/img_tiles/1971e/img_256_2/img.tif")
imgO3 <- img_c[75:16851,75:11728, drop=FALSE]
plot(imgO3, col=grey(1:100/100))

tileI <- makeTiles(imgO3, c(256,256), "/media/hkropp/research/Kolyma_Data/img_tiles/1971e/img_256_3/img.tif")



### tree and low density stratified sampling
tree_stratpts <- vect("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/tree_1971_e/tree_strat_pts.shp")
low_stratpts <- vect("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/tree_1971_e/low_strat_pts.shp")
plot(tree_stratpts, add=TRUE, col="red", cex=2)
plot(low_stratpts, add=TRUE, col="red", cex=2)


xycelll <- cells(img_c, low_stratpts)

xycellt <- cells(img_c, tree_stratpts)

xycell <- rbind(xycelll, xycellt[1:20,])

rowi <- numeric()
coli <- numeric()
#training.check <- numeric()
for(i in 1:40){
  rowi[i] <- rowFromCell(img_c, xycell[i,2])
  coli[i] <- colFromCell(img_c, xycell[i,2])
}

training.samples <- list()
training.check <- numeric()
for(i in 1:40){  
  if(rowi[i] <= nrow(img_c)-255 & coli[i] <= ncol(img_c)-255){
    training.samples[[i]] <-  img_c[rowi[i]:(rowi[i]+255), coli[i]:(coli[i]+255), drop=FALSE]
    training.check[i] <- ifelse(is.na(mean(values(training.samples[[i]],mat=FALSE))) == TRUE,0,1)
  }else{
    training.samples[[i]] <- NA
    training.check[i] <- 0
  }
}
training.valid <- which(training.check == 1)

length(training.valid)

for(i in 1:40){
  writeRaster(training.samples[[i]], paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/training/img/img_",i+200,".tif"))
} 


### tree and low density stratified sampling
add_stratpts <- vect("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/tree_1971_e/add_tree_pts.shp")

xycell <- cells(img_c, add_stratpts)


rowi <- numeric()
coli <- numeric()
#training.check <- numeric()
for(i in 1:10){
  rowi[i] <- rowFromCell(img_c, xycell[i,2])
  coli[i] <- colFromCell(img_c, xycell[i,2])
}

training.samples <- list()
training.check <- numeric()
for(i in 1:10){  
  if(rowi[i] <= nrow(img_c)-255 & coli[i] <= ncol(img_c)-255){
    training.samples[[i]] <-  img_c[rowi[i]:(rowi[i]+255), coli[i]:(coli[i]+255), drop=FALSE]
    training.check[i] <- ifelse(is.na(mean(values(training.samples[[i]],mat=FALSE))) == TRUE,0,1)
  }else{
    training.samples[[i]] <- NA
    training.check[i] <- 0
  }
}
training.valid <- which(training.check == 1)

length(training.valid)

for(i in 1:10){
  writeRaster(training.samples[[i]], paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/training/img/img_",i+240,".tif"))
} 


### additional stratified sampling to increase coverage across the study extent
stratpts <- vect("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/strat71_2/strat_2.shp")

plot(stratpts, add=TRUE, col="red", cex=2)



xycell <- cells(img_c, stratpts)



rowi <- numeric()
coli <- numeric()
#training.check <- numeric()
for(i in 1:32){
  rowi[i] <- rowFromCell(img_c, xycell[i,2])
  coli[i] <- colFromCell(img_c, xycell[i,2])
}

training.samples <- list()
training.check <- numeric()
for(i in 1:32){  
  if(rowi[i] <= nrow(img_c)-255 & coli[i] <= ncol(img_c)-255){
    training.samples[[i]] <-  img_c[rowi[i]:(rowi[i]+255), coli[i]:(coli[i]+255), drop=FALSE]
    training.check[i] <- ifelse(is.na(mean(values(training.samples[[i]],mat=FALSE))) == TRUE,0,1)
  }else{
    training.samples[[i]] <- NA
    training.check[i] <- 0
  }
}
training.valid <- which(training.check == 1)

length(training.valid)

for(i in 1:32){
  writeRaster(training.samples[[i]], paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net_71v2/img/img_",i+250,".tif"))
} 
