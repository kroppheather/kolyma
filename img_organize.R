##########################################################
###### organize georef images and prepare for tiling ####
###### runs r v4.3.0                                 #####
##########################################################


install.packages(c("terra"))
install.packages("rgdal")
library(terra)
library(dplyr)

imgDir <- "/media/hkropp/research/Kolyma_Data/georef_img"