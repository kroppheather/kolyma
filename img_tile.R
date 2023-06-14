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
dem <- rast("/media/hkropp/research/Kolyma_Data/dem/dem_mos.tif")
smoke <- vect("/media/hkropp/research/Kolyma_Data/smoke_extent/smoke_extent.shp")
extent <- vect("/media/hkropp/research/Kolyma_Data/m_extent/m_extent.shp")

plot(img71, col=grey(1:100/100))
plot(dem, col=grey(1:100/100))
demProj <- project(dem, img71)

smoke_mask <- img

# empty raster to resample
# top coordinate

