library(terra)
library(dplyr)
library(rnaturalearth)
library(sf)

###### read in data ----

comp <- 2
dirDat <- c("/media/hkropp/research/Kolyma_Data/predictions/maps",
            "c:/Users/hkropp/Documents/kolyma/maps_backup")
dirFig <- c("/media/hkropp/research/Kolyma_Data/AGU_results",
            "E:/Google Drive/research/conferences/AGU23/figs")
boundF <- c("/media/hkropp/research/Kolyma_Data/img_tiles/bound_71/na_bound_71e.shp",
            "c:/Users/hkropp/Documents/kolyma/bound/bound_71/na_bound_71e.shp")

class71 <- rast(paste0(dirDat[comp], "/class1971_strat_v4.tif"))
class20 <- rast(paste0(dirDat[comp], "/class2020_v5.tif"))
bound <- vect(boundF[comp])
plot(class20)

img71 <- rast("K:/Environmental_Studies/hkropp/Private/siberia_wv/1971/ext_07_16_71.tif")
###### organize data ----

# set the same boundary for both

class71m <- mask(class71, bound)
class20m <- mask(class20, bound)

# resample
class71r <- resample(class71m, class20m, method="near")
plot(class71r)
plot(class20m)
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
water20N <- ifel(water20 == 0, NA, 1)
plot(water20N)
waterdist <- distance(water20N)
waterdistM <- mask(waterdist, bound)
plot(waterdistM)
waterdistW <- ifel(waterdistM == 0, NA, waterdistM)
plot(waterdistW)

water71N <- ifel(water71 == 0, NA, 1)
plot(water71N)
waterdist71 <- distance(water71N)
waterdistM71 <- mask(waterdist71, bound)


classZ <- matrix(c(0,0, NA,
                   0, 50, 1,
                   50,100,2,
                   100,5000,3), byrow=TRUE, ncol=3)
waterZones <- classify(waterdistM, classZ, include.lowest=FALSE)
waterZones71 <- classify(waterdistM71, classZ, include.lowest=FALSE)
plot(waterZones)
plot(waterZones71)

watZ71 <- zonal(woody71, waterZones71, fun="sum", na.rm=TRUE)
zone71F <- freq(waterZones71)
zone71F$woodyPix <- watZ71$max.1
zone71F$percW <- (zone71F$woodyPix /zone71F$count)*100


watZ20 <- zonal(woody20, waterZones, fun="sum", na.rm=TRUE)
zone20F <- freq(waterZones)
zone20F$woodyPix <- watZ20$max.1
zone20F$percW <- (zone20F$woodyPix /zone20F$count)*100


######## Figure variables ----
# plotting colors:

#land cover
landPal <- c("#ECECDD","#000000","#0336A3","#9CC20E",  "#117835")


#################### AGU Land cover ----

##### Figure 1. Landcover comparison ----




# plot dim
wd1 <- 7
wd2 <- 2
hd1 <- 9

png(paste0(dirFig[comp], "/fig_1971_cover.png"), width=10, height=10, units="in", res=300)
layout(matrix(seq(1,2),ncol=2), width=lcm(c(wd1,wd2)*2.54),height=lcm(c(hd1)*2.54))

par(mai=c(0,0,0,0))
plot(class71m, breaks=c(-0.5,0.5,1.5,2.5,3.5,4.5),
     col=landPal, legend=FALSE, mar=NA, axes=FALSE,
     maxcell=ncell(class71m))
arrows(588700,7626300, 589700, 7626300, code=0, lwd=2)
arrows(588700,7626000, 588700, 7626300, code=0, lwd=2)
arrows(589700,7626000, 589700, 7626300, code=0, lwd=2)
text(588700,7625500, "0", cex=1.5)
text(588700,7625000, "km", cex=1.5)
text(589700,7625500, "1", cex=1.5)

par(mai=c(0,0,0,0))
plot(c(0,10), c(0,10), axes =FALSE, type="n", xlab = " ",
     ylab=" ")
legend(0,9, c("other", "tree", "water", "shrub", "taiga"), 
       fill=landPal, bty="n", cex=2)

dev.off()




png(paste0(dirFig[comp], "/fig_2020_cover.png"), width=10, height=10, units="in", res=300)
layout(matrix(seq(1,2),ncol=2), width=lcm(c(wd1,wd2)*2.54),height=lcm(c(hd1)*2.54))

par(mai=c(0,0,0,0))
plot(class20m, breaks=c(-0.5,0.5,1.5,2.5,3.5,4.5),
     col=landPal, legend=FALSE, mar=NA, axes=FALSE,
   maxcell=ncell(class20m))
arrows(588700,7626300, 589700, 7626300, code=0, lwd=2)
arrows(588700,7626000, 588700, 7626300, code=0, lwd=2)
arrows(589700,7626000, 589700, 7626300, code=0, lwd=2)
text(588700,7625500, "0", cex=1.5)
text(588700,7625000, "km", cex=1.5)
text(589700,7625500, "1", cex=1.5)

par(mai=c(0,0,0,0))
plot(c(0,10), c(0,10), axes =FALSE, type="n", xlab = " ",
     ylab=" ")
legend(0,9, c("other", "tree", "water", "shrub", "taiga"), 
       fill=landPal, bty="n", cex=2)

dev.off()