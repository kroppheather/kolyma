##########################################################
###### organize georef images and prepare for tiling ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)
library(mapview)
library(mapedit)
library(stars)
install.packages("leafpop")
install.packages("mapview")
install.packages("mapedit")
install.packages("stars")

imgDir <- "/media/hkropp/research/Kolyma_Data/georeference"

img71 <- rast(paste0(imgDir, "/mosaic/m_07_16_71.tif"))
dem <- rast("/media/hkropp/research/Kolyma_Data/dem/dem_mos_UTM.tif")
smoke <- vect("/media/hkropp/research/Kolyma_Data/smoke_extent/smoke_extent.shp")
extent <- vect("/media/hkropp/research/Kolyma_Data/m_extent/m_extent.shp")
writeRaster()
plot(img71, col=grey(1:100/100))
plot(dem, col=grey(1:100/100))
demProj <- project(dem, img71)
writeRaster(demProj, "/media/hkropp/research/Kolyma_Data/dem/dem_mos_UTM.tif")

ext_rast <- project(extent, img71)
ext_rast2 <- rasterize(ext_rast, img71)
ext_mask <- mask(img71, ext_rast )

# empty raster to resample
# top coordinate

