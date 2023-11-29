library(terra)
library(dplyr)

###### read in data ----
dirDat <- "/media/hkropp/research/Kolyma_Data/predictions/maps"
dirFig <- "/media/hkropp/research/Kolyma_Data/AGU_results"

class71 <- rast(paste0(dirDat, "/class1971_strat.tif"))
class20 <- rast(paste0(dirDat, "/class2020_v3.tif"))
bound <- vect("/media/hkropp/research/Kolyma_Data/img_tiles/bound_71/na_bound_71e.shp")

###### organize data ----

# set the same boundary for both

class71m <- mask(class71, bound)
class20m <- mask(class20, bound)

# resample
class71r <- resample(class71m, class20m, method="near")

#classify all woody cover

woody71 <- ifel(class71r == 1 | class71r == 3 | class71r == 4, 1, 0)
shrub71 <- ifel(class71r == 3, 1, 0)
taiga71 <- ifel(class71r == 1 | class71r == 4, 1, 0)


woody20 <- ifel(class20m == 1 | class20m == 3 | class20m == 4, 1, 0)
shrub20 <- ifel(class20m == 3, 1, 0)
taiga20 <- ifel(class20m == 1 | class20m == 4, 1, 0)
plot(taiga20)



