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


ext_rast <- project(extent, img71)
ext_rast2 <- rasterize(ext_rast, img71)
ext_mask <- mask(img71, extent )
img_mask <- mask(ext_mask, smoke, inverse=TRUE, filename=paste0(imgDir, "/mosaic/ext_mask_07_16_71.tif"))
demExt <- ifel(dem > 100, 1, NA)
img_mask2 <- mask(img_mask, demExt, inverse=TRUE, filename=paste0(imgDir, "/mosaic/lowland_07_16_71.tif"))

plot(dem)
plot(demExt)
filename <- paste0(tempfile(), "_.tif")
tileI <- makeTiles(img_mask, c(256,256), "/media/hkropp/research/Kolyma_Data/u_net71/tiles_256/img71.tif", na.rm=TRUE)
# empty raster to resample
# top coordinate

