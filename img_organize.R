##########################################################
###### organize georef images and prepare for tiling ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)



imgDir <- "/media/hkropp/research/Kolyma_Data/georef_img"
extDir <- "/media/hkropp/research/Kolyma_Data/polygon_laea"

imgList <- c("A006_c.tif","a006_d.tif",
             "A007_ct3.tif","A007_d.tif",
             "A008_c.tif","A008_d.tif",
             "A009_c.tif","A009_d.tif",
             "A010_b.tif", "A010_c.tif","A010_d.tif")

imgR <- list()
for(i in 1:length(imgList)){
  imgR[[i]] <- rast(paste0(imgDir,"/", imgList[i]))
  
}

plot(imgR[[1]], col=grey(1:100/100))
imgR[[1]]@ptr$res

polyC <- vect(paste0(extDir, "/A006_c.shp"))
crs(polyC)
