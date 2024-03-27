library(terra)
library(dplyr)
library(sf)
library(caret)


###### read in data ----
dirData <- "G:/My Drive/research/projects/Kolyma/final"

class71 <- rast(paste0(dirData, "/class1971_6.tif"))
class20 <- rast(paste0(dirData, "/class2020_strat_v2.tif"))
bound <- vect(paste0(dirData,"/bound/na_bound_71e.shp"))

valid71 <- st_read(paste0(dirData,"/valid/valid_class1971_6.shp"))
valid20 <- st_read(paste0(dirData,"/valid/valid_class2020_strat_2.shp"))

dem
###### read in data ----


plot(class71)
plot(class20)

# set the same boundary for both

class71m <- mask(class71, bound)
class20m <- mask(class20, bound)


# resample
class71r <- resample(class71m, class20m, method="near")
plot(class71r)
plot(class20m)


#break into individual classes

shrub71 <- ifel(class71r == 3, 1, 0)
taiga71 <- ifel(class71r == 1 , 1, 0)
water71 <- ifel(class71r == 2, 1, 0)

shrub20 <- ifel(class20m == 3, 1, 0)
taiga20 <- ifel(class20m == 1, 1, 0)
water20 <- ifel(class20m == 2, 1, 0)


woody71 <- ifel(class71r == 1 | class71r == 3 , 1, 0)

woody20 <- ifel(class20m == 1 | class20m == 3 , 1, 0)


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


shrubC <- c(shrub71, shrub20)

shrubChange <- lapp(shrubC, fun=changeF)
plot(shrubChange)

# look at water related changes
waterC <- c(water71, water20)
waterChange <- lapp(waterC, fun=changeF)
plot(waterChange)

taigaC <- c(taiga71, taiga20)
taigaChange <- lapp(taigaC, fun=changeF)
plot(taigaChange)


# x = water change and y = woody 20
hydroChange <- function(x, y){
  ifelse(x == 1 & y == 2 , 1, # gain of woody in water loss
         ifelse(x == 2 & y== 1, 2,0)) #loss of woody to water gain
  
}



hydroc <- c(waterChange, woodyChange)
shrubHydro <- lapp(hydroc, hydroChange)
# 1 = gain in woody cover due to water loss
# 2 = loss of woody cover to gain of water
# 3 = loss of water but no woody change
plot(shrubHydro)


