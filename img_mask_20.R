##########################################################
###### convert shapefiles to raster for masks        ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)

dirD <- "/media/studentuser/Seagate Portable Drive/training"

img <- list.files(paste0(dirD, "/img"),pattern=".tif")
imgXML <- grepl(".aux.xml",img)
img <- img[imgXML == FALSE ] 

water <- list.files(paste0(dirD, "/masks/water"),pattern=".shp")
waterXML <- grepl(".xml",water)
water <- water[waterXML == FALSE ] 

shrubs <- list.files(paste0(dirD, "/masks/shrubs"),pattern=".shp")
shrubsXML <- grepl(".xml",shrubs)
shrubs <- shrubs[shrubsXML == FALSE ] 

trees <- list.files(paste0(dirD, "/masks/trees"),pattern=".shp")
treesXML <- grepl(".xml",trees)
trees <- trees[treesXML == FALSE ] 

poly <- list.files(paste0(dirD, "/masks/polygonal"),pattern=".shp")
polyXML <- grepl(".xml",poly)
poly <- poly[polyXML == FALSE ]
