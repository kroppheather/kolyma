library(terra)
library(dplyr)
library(mapview)
library(mapedit)
library(stars)



imgDir <- "/media/hkropp/research/Kolyma_Data/georeference"

img71 <- rast(paste0(imgDir, "/mosaic/m_07_16_71.tif"))
ncol(img71)
nrow(img71)
startPoint <- c(44231,50000)
tileExamp <- img71[44231:(44231+255), 50000:(50000+255), drop=FALSE]
ncol(tileExamp)
nrow(tileExamp)
plot(tileExamp, col=grey(1:100/100))
testMap <- st_as_stars(tileExamp)

help("mapview")
#note mapview reprojected here
test <- drawFeatures(mapview(testMap, col.regions=grey(1:100/100), maxpixels=ncell(tileExamp)))
plot(test)

#convert back to terra
testT <- as(test,"Spatial")
testv <- vect(testT)
plot(testv)
# mapedit will create in wgs84. Convert back to utm
testP <- project(testv, crs(tileExamp))
testR <- rasterize(testP, tileExamp, background=0)
plot(testR)
plot(tileExamp)
plot(testR, add=TRUE, alpha=0.5)
