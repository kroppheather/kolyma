##########################################################
###### convert shapefiles to raster for masks        ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)

dirD <- "/media/hkropp/research/Kolyma_Data/training/Kolyma/u_net20/shapefiles"

img <- list.files(paste0(dirD, "/img"),pattern=".tif")
imgXML <- grepl(".aux.xml",img)
img <- img[imgXML == FALSE ] 

water <- list.files(paste0(dirD, "/water"),pattern=".shp")
waterXML <- grepl(".xml",water)
water <- water[waterXML == FALSE ] 

shrubs <- list.files(paste0(dirD, "/shrub"),pattern=".shp")
shrubsXML <- grepl(".xml",shrubs)
shrubs <- shrubs[shrubsXML == FALSE ] 

trees <- list.files(paste0(dirD, "/tree"),pattern=".shp")
treesXML <- grepl(".xml",trees)
trees <- trees[treesXML == FALSE ] 

low <- list.files(paste0(dirD, "/low"),pattern=".shp")
lowXML <- grepl(".xml",low)
low <- low[lowXML == FALSE ] 


