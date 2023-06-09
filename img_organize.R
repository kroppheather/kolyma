##########################################################
###### organize georef images and prepare for tiling ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)



imgDir <- "/media/hkropp/research/Kolyma_Data/georef_img"
extDir <- "/media/hkropp/research/Kolyma_Data/extent_LAEA"

imgList <- c("A006_c.tif","a006_d.tif",
             "A007_ct3.tif","A007_d.tif",
             "A008_c.tif","A008_d.tif",
             "A009_c.tif","A009_d.tif",
             "A010_b.tif", "A010_c.tif","A010_d.tif")

extList <- c("A006_c.shp","A006_d.shp",
             "A007_c.shp","A007_d.shp",
             "A008_c.shp","A008_d.shp",
             "A009_c.shp","A009_d.shp",
             "A010_b.shp", "A010_c.shp","A010_d_fix.shp")
# read in raster images.
imgR <- list()
for(i in 1:length(imgList)){
  imgR[[i]] <- rast(paste0(imgDir,"/", imgList[i]))
  
}
plot(imgR[[1]], col=grey(1:100/100))

# read in polygons that give the extent of usuable space in an image

polyC <- list()
for(i in 1:length(extList)){
  polyC[[i]] <- vect(paste0(extDir, "/", extList[i]))
}

imgRrp <- list()

for(i in 1:length(extList)){
  imgRrp[[i]] <- project(imgR[[i]], crs(polyC[[i]]), method="bilinear")
}

# mask raster images to keep only the extent to use
imgC <- list()
for(i in 1:length(extList)){
  imgC[[i]] <- mask(imgRrp[[i]], polyC[[i]])
  
}
plot(imgC[[11]], col=grey(1:100/100))
for(i in 1:length(extList)){
  writeRaster(imgC[[i]], paste0("/media/hkropp/research/Kolyma_Data/trim_imgs/",imgList[i]  ))
}


# empty raster to resample
# top coordinate

