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

dem <- rast("E:/Kolyma/dem/dem_mos.tif")

img71 <- rast("E:/Kolyma/1971/ext_07_16_71.tif")
img20 <- rast("E:/Kolyma/wv/wv8b_07_20.tif")

# comparision to change data:
#boreal_greenness_median_percent_change_2000to2019_p500.tif

###### figure director ----
dirSave <- "G:/My Drive/research/projects/Kolyma/manuscript/figures"
###### change maps and analysis ----


plot(class71)
plot(class20)

# calculate frequency & percent occurance
freq71 <- freq(class71)
freq20 <- freq(class20)

freq71$perc <- (freq71$count/sum(freq71$count))*100
freq20$perc <- (freq20$count/sum(freq20$count))*100
# set the same boundary for both

class71m <- mask(class71, bound)
class20m <- mask(class20, bound)

img71m <- mask(img71, bound)
img20m <- mask(img20, bound)


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

#shrub distance from water
water20N <- ifel(water20 == 0, NA, 1)
water71N <- ifel(water71 == 0, NA, 1)

waterdist <- distance(water20N)
waterdistM <- mask(waterdist, bound)
plot(waterdistM)

waterdist71 <- distance(water71N)
waterdistM71 <- mask(waterdist71, bound)

plot(waterdistM71)

classZ <- matrix(c(0,0, NA,
               0, 50, 1,
               50,100,2,
               100,150,3,
               150,50000,4), byrow=TRUE, ncol=3)

waterZones <- classify(waterdistM, classZ, include.lowest=FALSE)
waterZones71 <- classify(waterdistM71, classZ, include.lowest=FALSE)
plot(waterZones)
plot(waterZones71)


watZ71 <- zonal(woody71, waterZones71, fun="sum", na.rm=TRUE)
zone71F <- freq(waterZones71)
zone71F$woodyPix <- watZ71$mean.1
zone71F$percW <- (zone71F$woodyPix /zone71F$count)*100


watZ20 <- zonal(woody20, waterZones, fun="sum", na.rm=TRUE)
zone20F <- freq(waterZones)
zone20F$woodyPix <- watZ20$mean.1
zone20F$percW <- (zone20F$woodyPix /zone20F$count)*100



############ Figure variables -----
colsClass <- c("white", "#FFB1AE", "#0A4BD1","#38AD11")


############ Figure 2: land cover maps and images -----


# plot dim
wd <- 4
hd1 <- 5
hd2 <- 2
# arrow line width for scale bar
awd <- 2
# text size for scale bar
sce <- 2
#axis size for area plot
cap <- 2.5
# axis label size for area plot
lax <- 2
#border for bars
borderi <- c("black",NA,NA,NA)
#size for area text label
tcx <- 1.2
#panel label line
llc <- -1.75
#panel label size
pcx <- 2
# x type label
capl <- 1.5


png(paste0(dirSave, "/fig_2_cover_panel.png"), width=14, height=14, units="in", res=300)
layout(matrix(seq(1,6),ncol=2), width=lcm(rep(wd*2.54,3)),height=lcm(c(hd1,hd1,hd2)*2.54))
# 1971 imagery
par(mai=c(0.01,0.01,0.01,0.01))
plot(img71m, col=grey(1:100/100),axes=FALSE, mar=NA, legend=FALSE,
     maxcell=ncell(img71m))

arrows(588400,7625800, 589400, 7625800, code=0, lwd=awd)
arrows(588400,7625300, 588400, 7625800, code=0, lwd=awd)
arrows(589400,7625300, 589400, 7625800, code=0, lwd=awd)
text(588400,7624800, "0", cex=sce)
text(588900,7624000, "km", cex=sce)
text(589400,7624800, "1", cex=sce)

mtext("a", side=3, at=589000,  line=llc, cex=pcx)

# 1971 land cover class
par(mai=c(0.01,0.01,0.01,0.01))

plot(class71m, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(class71m))

mtext("c", side=3, at=589000,  line=llc, cex=pcx)

par(mai=c(0.1,0.1,0.1,0.1))

plot(c(0,1),c(0,1), xlim=c(0.5,4.5),ylim=c(0,60),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
for(i in 1:4){
  polygon(c(i-0.25,i-0.25,i+0.25,i+0.25),
          c(0,freq71$perc[i],freq71$perc[i],0),
          col=colsClass[i], border=borderi[i])
  
}
mtext("e", side=3, at=0.8,  line=llc, cex=pcx)

axis(1, seq(1,4),labels=c("","","",""), 
     cex.axis=cap, lwd.ticks=1.5, lwd=2)
axis(2, seq(0,60, by=20), las=2, cex.axis= cap , lwd.ticks=1.5, lwd=2)
mtext("Land cover type", side=1, line=5, cex=lax )
mtext("Percent Area (%)", side=2, line=4, cex=lax )

mtext(c("Other","Taiga","Water","Shrub"), at=seq(1,4),side=1.5, cex=capl,
      line=1)
text(seq(1,4), freq71$perc+5, paste0(round(freq71$perc,1)), cex=1.5)

# 2020 image
par(mai=c(0.01,0.01,0.01,0.01))
plotRGB(img20m, r=3, g=2, b= 1, stretch="lin", axes=FALSE, mar=NA, legend=FALSE,
        maxcell=ncell(img20m))
mtext("b", side=3, at=589000,  line=llc, cex=pcx)

# 2020 land cover class
par(mai=c(0.01,0.01,0.01,0.01))

plot(class20m, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(class20m))

mtext("d", side=3, at=589000,  line=llc, cex=pcx)

par(mai=c(0.1,0.1,0.1,0.1))

plot(c(0,1),c(0,1), xlim=c(0.5,4.5),ylim=c(0,60),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
for(i in 1:4){
  polygon(c(i-0.25,i-0.25,i+0.25,i+0.25),
          c(0,freq20$perc[i],freq20$perc[i],0),
          col=colsClass[i], border=borderi[i])
  
}
mtext("f", side=3, at=0.8,  line=llc, cex=pcx)

text(seq(1,4), freq20$perc+5, paste0(round(freq20$perc,1)), cex=1.5)

axis(1, seq(1,4),labels=c("","","",""), 
     cex.axis=cap, lwd.ticks=1.5, lwd=2)
axis(2, seq(0,60, by=20), las=2, cex.axis= cap , lwd.ticks=1.5, lwd=2)
mtext("Land cover type", side=1, line=5, cex=lax )
mtext(c("Other","Taiga","Water","Shrub"), at=seq(1,4),side=1.5, cex=capl,
      line=1)
dev.off()
