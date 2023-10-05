##########################################################
###### organize georef images and prepare for tiling ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)
library(mapview)


imgDir <- "/media/studentuser/STORE N GO/clip_07_16_71.tif"

extent <- vect("/media/studentuser/STORE N GO/small_extent.shp")

# small extent to focus on for start
ext_mask <- rast("/media/studentuser/STORE N GO/clip_07_16_71.tif")
plot(ext_mask, col = grey(1:100/100))
img_final <- crop(ext_mask, extent)
plot(img_final, col = grey(1:100/100))
# sample rows and cols try to avoid na buffer
set.seed(15)
rowsi <- sample(1:(nrow(img_final)-255), 600)
colsi <- sample(1:(ncol(img_final)-255), 600)

# subset raster
training.samples <- list()
training.check <- numeric()
for(i in 1:600){
  training.samples[[i]] <-  img_final[rowsi[i]:(rowsi[i]+255), colsi[i]:(colsi[i]+255), drop=FALSE]
  # create a check to remove any training samples with NAs
  training.check[i] <- ifelse(is.na(mean(values(training.samples[[i]],mat=FALSE))) == TRUE,0,1)
}

training.valid <- which(training.check == 1)
samples.use <- list()
#first sample looks odd...
#first sample training valid range from 1 to 220
for(i in 1:429){
  samples.use[[i]] <- training.samples[[training.valid[i]]]
}
plot(samples.use[[200]], col=gray(1:100/100))

for(i in 201:400){
  writeRaster(samples.use[[i]], paste0("/media/studentuser/STORE N GO/training/img/img_",i,".tif"))
} 


tileI <- makeTiles(ext_mask, c(256,256), "/media/hkropp/research/Kolyma_Data/u_net71/tiles_256/img.tif", na.rm=TRUE)