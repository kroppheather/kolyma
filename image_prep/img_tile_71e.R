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
img_final <- mask(img_c, extent, filename="K:/Environmental_Studies/hkropp/Private/siberia_wv/1971/ext_07_16_71.tif")
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

# save raster masks that were already created
# water
wfiles <- list.files("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks_img/water_img/", pattern=".tif")
wfilesN <- as.numeric(gsub("\\D","", wfiles))
wpresent <- imgPull %in% wfilesN

w_mask <- list()
for(i in 1:length(imgPull)){
  if(wpresent[i] == TRUE){
    w_mask <- rast(paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks_img/water_img/water_",
                             imgPull[i],".tif"))
    writeRaster(w_mask, 
                paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/training/masks_img/water/water_",i,".tif"))
  }
}

# shrub
sfiles <- list.files("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks_img/shrubs_img/", pattern=".tif")
sfilesN <- as.numeric(gsub("\\D","", sfiles))
spresent <- imgPull %in% sfilesN

s_mask <- list()
for(i in 1:length(imgPull)){
  if(spresent[i] == TRUE){
    s_mask <- rast(paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks_img/shrubs_img/shrub_",
                          imgPull[i],".tif"))
    writeRaster(s_mask, 
                paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/training/masks_img/shrub/shrub_",i,".tif"))
  }
}

# tree
tfiles <- list.files("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks_img/trees_img/", pattern=".tif")
tfilesN <- as.numeric(gsub("\\D","", tfiles))
tpresent <- imgPull %in% tfilesN

t_mask <- list()
for(i in 1:length(imgPull)){
  if(tpresent[i] == TRUE){
    t_mask <- rast(paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks_img/trees_img/tree_",
                          imgPull[i],".tif"))
    writeRaster(t_mask, 
                paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/training/masks_img/tree/tree_",i,".tif"))
  }
}

# low
lfiles <- list.files("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks_img/low_density_img/", pattern=".tif")
lfilesN <- as.numeric(gsub("\\D","", lfiles))
lpresent <- imgPull %in% lfilesN

l_mask <- list()
for(i in 1:length(imgPull)){
  if(lpresent[i] == TRUE){
    l_mask <- rast(paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71/training/masks_img/low_density_img/low_",
                          imgPull[i],".tif"))
    writeRaster(l_mask, 
                paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net71e/training/masks_img/low/low_",i,".tif"))
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
