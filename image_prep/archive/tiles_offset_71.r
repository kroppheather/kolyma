
library(terra)
library(dplyr)



imgDir <- "/media/hkropp/research/Kolyma_Data"

extent <- vect("/media/hkropp/research/Kolyma_Data/small_extent/small_extent.shp")

# small extent to focus on for start
ext_mask <- rast("/media/hkropp/research/Kolyma_Data/clip_07_16_71.tif")
plot(ext_mask, col = grey(1:100/100))
img_final <- crop(ext_mask, extent)
plot(img_final, col = grey(1:100/100))



rowsi <- nrow(img_final)
colsi <- ncol(img_final)

ext_tile2 <- img_final[25:rowsi,25:colsi, drop=FALSE]
ext_tile3 <- img_final[50:rowsi,50:colsi, drop=FALSE]

tileI2 <- makeTiles(ext_tile2, c(256,256), "/media/hkropp/research/Kolyma_Data/img_tiles/1971/tiles_256_2/img.tif", na.rm=TRUE)
tileI3 <- makeTiles(ext_tile3, c(256,256), "/media/hkropp/research/Kolyma_Data/img_tiles/1971/tiles_256_3/img.tif", na.rm=TRUE)
