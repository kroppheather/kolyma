##########################################################
###### organize georef images and prepare for tiling ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)
library(mapview)
library(mapedit)
library(stars)


imgDir <- "/media/hkropp/research/Kolyma_Data/georeference"

img71 <- rast(paste0(imgDir, "/mosaic/m_07_16_71.tif"))
dem <- rast("/media/hkropp/research/Kolyma_Data/dem/dem_mos_UTM.tif")
smoke <- vect("/media/hkropp/research/Kolyma_Data/smoke_extent/smoke_extent.shp")
extent <- vect("/media/hkropp/research/Kolyma_Data/m_extent/m_extent.shp")
img_mask2 <-rast(paste0(imgDir, "/mosaic/lowland_07_16_71.tif"))

ext_rast <- project(extent, img71)
ext_rast2 <- rasterize(ext_rast, img71)
ext_mask <- mask(img71, extent )
img_mask <- mask(ext_mask, smoke, inverse=TRUE, filename=paste0(imgDir, "/mosaic/ext_mask_07_16_71.tif"))
demExt <- ifel(dem > 60, 1, NA)
img_mask2 <- mask(img_mask, demExt, inverse=TRUE, filename=paste0(imgDir, "/mosaic/lowland_07_16_71.tif"))
plot(img_mask2, col=grey(1:100/100))
plot(dem)

plot(demExt)

# sample rows and cols try to avoid na buffer
set.seed(15)
rowsi <- sample(5000:(nrow(img_mask2)-10000), 400)
colsi <- sample(5000:(ncol(img_mask2)-10000), 400)

# subset raster
training.samples <- list()
training.check <- numeric()
for(i in 1:400){
 training.samples[[i]] <-  img_mask2[rowsi[i]:(rowsi[i]+255), colsi[i]:(colsi[i]+255), drop=FALSE]
 # create a check to remove any training samples with NAs
 training.check[i] <- ifelse(is.na(mean(values(training.samples[[i]],mat=FALSE))) == TRUE,0,1)
}

training.valid <- which(training.check == 1)

for(i in 1:200){
  writeRaster(training.samples[[i]], paste0("/media/hkropp/research/Kolyma_Data/u_net71/training/img/img_",i,".tif"))
} 


tileI <- makeTiles(img_mask2, c(256,256), "/media/hkropp/research/Kolyma_Data/u_net71/tiles_256/img.tif", na.rm=TRUE)
