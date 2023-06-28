##########################################################
###### convert shapefiles to raster for masks        ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)

dirD <- "/media/hkropp/research/Kolyma_Data/training_test"


poly <- list.files(paste0(dirD, "/"),pattern=".shp")