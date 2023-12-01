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
plot(woody71)


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


# look at change in cover
changeF <- function(x, y){
  ifelse(x == 0 & y == 0, 0, # no cover either year
         ifelse(x == 1 & y == 0, 1, # loss of cover
          ifelse(x == 0 & y == 1, 2, # gain of cover
                 ifelse(x == 1 & y == 1, 3, NA)))) # stable cover
}

woodyC <- c(woody71, woody20)

woodyChange <- lapp(woodyC, fun=changeF)
plot(woodyChange)

# look at water related changes
waterC <- c(water71, water20)
waterChange <- lapp(waterC, fun=changeF)
plot(waterChange)

# x = water change and y = woody 20
hydroChange <- function(x, y){
  ifelse(x == 1 & y == 2 , 1, # gain of woody in water loss
       ifelse(x == 2 & y== 1,2, 0)) #loss of woody to water gain
}

hydroc <- c(waterChange, woodyChange)
shrubHydro <- lapp(hydroc, hydroChange)
# 1 = gain in woody cover due to water loss
# 2 = loss of woody cover to gain of water
plot(shrubHydro)

# vectorize water

