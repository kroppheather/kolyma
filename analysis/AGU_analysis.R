library(terra)
library(dplyr)

###### read in data ----

comp <- 2
dirDat <- c("/media/hkropp/research/Kolyma_Data/predictions/maps",
            "c:/Users/hkropp/Documents/kolyma/maps_backup")
dirFig <- c("/media/hkropp/research/Kolyma_Data/AGU_results",
            "c:/Users/hkropp/Documents/kolyma/figs")
boundF <- c("/media/hkropp/research/Kolyma_Data/img_tiles/bound_71/na_bound_71e.shp",
            "c:/Users/hkropp/Documents/kolyma/bound/bound_71/na_bound_71e.shp")

class71 <- rast(paste0(dirDat[comp], "/class1971_strat_v4.tif"))
class20 <- rast(paste0(dirDat[comp], "/class2020_v3.tif"))
bound <- vect(boundF[comp])

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
plot(taiga71)


woody20 <- ifel(class20m == 1 | class20m == 3 | class20m == 4, 1, 0)
shrub20 <- ifel(class20m == 3, 1, 0)
taiga20 <- ifel(class20m == 1 | class20m == 4, 1, 0)
plot(taiga20)
plot(woody20)


# classify water
water71 <- ifel(class71r == 2, 1, 0)
water20 <- ifel(class20m == 2, 1, 0)
plot(water71)
plot(water20)
