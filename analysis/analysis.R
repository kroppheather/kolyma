library(terra)
library(dplyr)
library(sf)
library(caret)
library(exactextractr)

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

aoi <- vect("E:/Kolyma/aoi/aoi.shp")

# Distance calculation from ArcGIS (after terra update broke the function)
distW20w <- rast("E:/Kolyma/distance/dist_20N.tif")
distW71w <- rast("E:/Kolyma/distance/dist_water71N.tif")
plot(distW20w)
# comparision to change data:
greenComp <- rast("E:/Kolyma/BorealForest_Greenness_Trends_2023/BorealForest_Greenness_Trends_2023/data/boreal_greenness_median_percent_change_2000to2019_p500.tif")

plot(greenComp)
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

distW20 <- mask(distW20w, bound)
distW71 <- mask(distW71w, bound)

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
plot(waterChange)

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

water20N <- ifel(water20 == 0, NA, 1)
water71N <- ifel(water71 == 0, NA, 1)
#writeRaster(water20N, "E:/Kolyma/distance/water20N.tif")
#writeRaster(water71N, "E:/Kolyma/distance/water71N.tif")

# woody cover near rivers

classZ <- matrix(c(0,0, NA,
                   0, 50, 1,
                   50,100,2,
                   100,5000,3), byrow=TRUE, ncol=3)

waterZones20 <- classify(distW20, classZ, include.lowest=FALSE)
waterZones71 <- classify(distW71, classZ, include.lowest=FALSE)
plot(waterZones20)
plot(waterZones71)


taigZ71 <- zonal(taiga71, waterZones71, fun="sum", na.rm=TRUE)
zone71F <- freq(waterZones71)
zone71F$taigaPix <- taigZ71$mean
zone71F$percTaig <- (zone71F$taigaPix /zone71F$count)*100
shrubz71 <- zonal(shrub71, waterZones71, fun="sum", na.rm=TRUE)
zone71F$shrubPix <- shrubz71$mean
zone71F$percshrub <- (zone71F$shrubPix /zone71F$count)*100

taigZ20 <- zonal(taiga20, waterZones20, fun="sum", na.rm=TRUE)
zone20F <- freq(waterZones20)
zone20F$taigaPix <- taigZ20$mean
zone20F$percTaig <- (zone20F$taigaPix /zone20F$count)*100
shrubz20 <- zonal(shrub20, waterZones20, fun="sum", na.rm=TRUE)
zone20F$shrubPix <- shrubz20$mean
zone20F$percshrub <- (zone20F$shrubPix /zone20F$count)*100

# comparision to trends
greenCompP <- project(greenComp, crs(class71m))
plot(greenCompP)
res(greenCompP)
greenCompC <- crop(greenCompP, class71m)
plot(greenCompC)


classV71 <- as.polygons(class71, aggregate=TRUE, na.rm=TRUE)
shrubV71 <- subset(classV71, classV71$mean == 3)
plot(shrubV71, col="green", border=NA)
shrubV71s <- st_as_sf(shrubV71)
plot(shrubV71s)


covS71 <- exact_extract(greenCompC, shrubV71s, include_cell=TRUE)
covS71d <- data.frame(covS71[[1]])
comp71J <- values(greenCompC, mat=FALSE, dataframe=TRUE)
comp71J$cell <- 1:nrow(comp71J)
covs71A <- left_join(comp71J, covS71d, by="cell")
covs71A$percCov <- ifelse(is.na(covs71A$coverage_fraction),0,
                          covs71A$coverage_fraction*100)
perc71comp <- greenCompC
plot(perc71comp)
values(perc71comp) <- covs71A$percCov
plot(perc71comp)

classV20 <- as.polygons(class20, aggregate=TRUE, na.rm=TRUE)
shrubV20 <- subset(classV20, classV20$mean == 3)
shrubV20s <- st_as_sf(shrubV20)


covS20 <- exact_extract(greenCompC, shrubV20s, include_cell=TRUE)
covS20d <- data.frame(covS20[[1]])
comp20J <- values(greenCompC, mat=FALSE, dataframe=TRUE)
comp20J$cell <- 1:nrow(comp20J)
covs20A <- left_join(comp20J, covS20d, by="cell")
covs20A$percCov <- ifelse(is.na(covs20A$coverage_fraction),0,
                          covs20A$coverage_fraction*100)
perc20comp <- greenCompC
plot(perc20comp)
values(perc20comp) <- covs20A$percCov
plot(perc20comp)

changePercShrub <- perc20comp - perc71comp
plot(changePercShrub)
plot(greenCompC)
names(changePercShrub) <- "percShrubC"
greenStack <- c(greenCompC, changePercShrub)

greenCompDF <- values(greenStack, dataframe=TRUE)
plot(greenCompDF$percShrubC, greenCompDF$boreal_greenness_median_percent_change_2000to2019_p500)

par(mfrow=c(1,3))
plot(greenCompC)
plot(changePercShrub)
plot(shrubChange, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsChange)

# look at landscape characteristics distribution w shrub/other

dempP <- project(dem, crs(class71m))
demC <- crop(dempP, class71m)
plot(demC)

plot(class71r)
plot(class20m)

shrubTChange <- function(x, y){
  ifelse(x == 0 & y == 0 , 1, # always other
         ifelse(x == 3 & y== 3, 2, # stable shrub
                ifelse(x == 0 & y == 3,3,#new shrub other
                       ifelse(x == 1 & y == 3,3,#new shrub taiga
                        ifelse(x == 2 & y == 3,3, 0))))) # new shrub from water
  
}


classC <- c(class71r, class20m)
shrubChangeC <- lapp(classC, fun=shrubTChange)
plot(shrubChangeC)


############ Figure variables -----
colsClass <- c("#ECECDD", "#117835" , "#0336A3","#9CC20E")
colsChange <- c("white", "#D15230", "#4393c3", "grey30")
colsHydroChange <- c("white", "#4393c3","#D15230")



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





############ Figure 3: change maps ------


# plot dim
wd <- 4
hd1 <- 5
hd2 <- 0.5
# arrow line width for scale bar
awd <- 2
# text size for scale bar
sce <- 2
#panel label line
llc <- 0
#panel label size
pcx <- 2
# map label line
llcm <- 2
# map label size
mcx <- 2



png(paste0(dirSave, "/fig_3_change_panel.png"), width=14, height=7, units="in", res=300)
layout(matrix(seq(1,6),ncol=3, byrow=TRUE), width=lcm(rep(wd*2.54,1)),height=lcm(c(hd1,hd2)*2.54))
# 1971 imagery
par(mai=c(0.01,0.01,0.01,0.01))
plot(waterChange, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsChange,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(waterChange))

arrows(588400,7625800, 589400, 7625800, code=0, lwd=awd)
arrows(588400,7625300, 588400, 7625800, code=0, lwd=awd)
arrows(589400,7625300, 589400, 7625800, code=0, lwd=awd)
text(588400,7624800, "0", cex=sce)
text(588900,7624000, "km", cex=sce)
text(589400,7624800, "1", cex=sce)
mtext("a", side=3, at=589000,  line=llc, cex=pcx)
mtext("Water", side=3,   line=llcm, cex=mcx)

par(mai=c(0.01,0.01,0.01,0.01))

plot(shrubChange, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsChange,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(shrubChange))

mtext("b", side=3, at=589000,  line=llc, cex=pcx)
mtext("Shrub", side=3,   line=llcm, cex=mcx)


par(mai=c(0.01,0.01,0.01,0.01))

plot(taigaChange, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsChange,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(taigaChange))

mtext("c", side=3, at=589000,  line=llc, cex=pcx)
mtext("Taiga", side=3,   line=llcm, cex=mcx)


par(mai=c(0.01,0.01,0.01,0.01))
plot(c(0,10),c(0,10), type="n", axes=FALSE, xlab=" ", ylab= " ")

legend("center", c("no cover", "loss", "gain","stable"),
       fill=colsChange, bty="n", horiz=TRUE, cex=1.4)

par(mai=c(0.01,0.01,0.01,0.01))
plot(c(0,10),c(0,10), type="n", axes=FALSE, xlab=" ", ylab= " ")
legend("center", c("no cover", "loss", "gain","stable"),
       fill=colsChange, bty="n", horiz=TRUE, cex=1.4)

par(mai=c(0.01,0.01,0.01,0.01))
plot(c(0,10),c(0,10), type="n", axes=FALSE, xlab=" ", ylab= " ")
legend("center", c("no cover", "loss", "gain","stable"),
       fill=colsChange, bty="n", horiz=TRUE, cex=1.4)

dev.off()


########### Figure 4: hydro change ------



plot(aoi[1,])
#prep aoi
aoiWL <- aoi[3,]
aoiWG <- aoi[2,]

img71WL <- crop(img71m, aoiWL)
img20WL <- crop(img20m, aoiWL)
img71WG <- crop(img71m, aoiWG)
img20WG <- crop(img20m, aoiWG)

plot(img71WG, col=grey(1:100/100),axes=FALSE, mar=NA, legend=FALSE,
     maxcell=ncell(img71m))
plotRGB(img20WG, r=3, g=2, b= 1, stretch="lin", axes=FALSE, mar=NA, legend=FALSE,
        maxcell=ncell(img20m))

# plot dim
wd <- 4
hd1 <- 5
hd2 <- 1
# arrow line width for scale bar
awd <- 2
# text size for scale bar
sce <- 1.5

#panel label line
llc <- -1.5
#panel label size
pcx <- 2
# x type label
capl <- 1.5

png(paste0(dirSave, "/fig_4a_hydro_map.png"), width=6, height=8, units="in", res=300)
layout(matrix(seq(1,2),ncol=1), width=lcm(rep(wd*2.54,3)),height=lcm(c(hd1, hd2)*2.54))

par(mai=c(0,0,0,0))
# 1 = gain in woody cover due to water loss
# 2 = loss of woody cover to gain of water
# 3 = loss of water but no woody change
plot(shrubHydro, breaks=c(-0.5,0.5,1.5,2.5),col=colsHydroChange,
     legend=FALSE,  axes=FALSE, mar=NA) #maxcell=ncell(shrubHydro))
polys(aoiWL,  col=NA, border="black")
polys(aoiWG,  col=NA, border="black")
text(594600,7621900, "AOI: b,c", cex=0.66) 
text(591031,7621100, "AOI: d,e", cex=0.66)
arrows(588400,7625800, 589400, 7625800, code=0, lwd=awd)
arrows(588400,7625300, 588400, 7625800, code=0, lwd=awd)
arrows(589400,7625300, 589400, 7625800, code=0, lwd=awd)
text(588400,7624800, "0", cex=sce)
text(588900,7624000, "km", cex=sce)
text(589400,7624800, "1", cex=sce)
mtext("a", side=3, at=589000,  line=llc, cex=pcx)


par(mai=c(0.01,0.01,0.01,0.01))
plot(c(0,10),c(0,10), type="n", axes=FALSE, xlab=" ", ylab= " ")
legend("center", c("other", "+ woody & - water", "- woody & + water"),
       fill=colsHydroChange, bty="n")
dev.off()


# part b


# plot dim
wd <- 4
hd1 <- 4
# arrow line width for scale bar
awd <- 2
# text size for scale bar
sce <- 1.5

#panel label line
llc <- -2.5
#panel label size
pcx <- 3
# x type label
capl <- 1.5


png(paste0(dirSave, "/fig_4b_change examples.png"), width=14, height=14, units="in", res=300)
layout(matrix(seq(1,4),ncol=2, byrow=TRUE), width=lcm(rep(wd*2.54,3)),height=lcm(c(hd1,hd1)*2.54))
# 1971 imagery
par(mai=c(0.01,0.01,0.01,0.01))

plot(img71WL, col=grey(1:100/100),axes=FALSE, mar=NA, legend=FALSE,
     maxcell=ncell(img71WL))

arrows(594800,7622250, 595000, 7622250, code=0, lwd=awd, col="white")
arrows(594800,7622210, 594800, 7622250, code=0, lwd=awd, col="white")
arrows(595000,7622210,  595000, 7622250, code=0, lwd=awd, col="white")
text(594800,7622180, "0", cex=sce, col="white")
text(594840,7622180, "m", cex=sce, col="white")
text(595000,7622180, "200", cex=sce, col="white")
mtext("b", side=3, at=594830,  line=llc, cex=pcx, col="white")

par(mai=c(0.01,0.01,0.01,0.01))

plotRGB(img20WL, r=3, g=2, b= 1, stretch="lin", axes=FALSE, mar=NA, legend=FALSE,
        maxcell=ncell(img20m))
mtext("c", side=3, at=594830,  line=llc, cex=pcx, col="white")
par(mai=c(0.01,0.01,0.01,0.01))

plot(img71WG, col=grey(1:100/100),axes=FALSE, mar=NA, legend=FALSE,
     maxcell=ncell(img71WL))

arrows(591175,7620775, 591225, 7620775, code=0, lwd=awd, col="white")
arrows(591175,7620765, 591175, 7620775, code=0, lwd=awd, col="white")
arrows(591225,7620765,  591225, 7620775, code=0, lwd=awd, col="white")
text(591175,7620755, "0", cex=sce, col="white")
text(591185,7620755, "m", cex=sce, col="white")
text(591225,7620755, "50", cex=sce, col="white")

mtext("d", side=3, at=591050,  line=llc, cex=pcx, col="white")
par(mai=c(0.01,0.01,0.01,0.01))

plotRGB(img20WG, r=3, g=2, b= 1, stretch="lin", axes=FALSE, mar=NA, legend=FALSE,
        maxcell=ncell(img20m))
mtext("e", side=3, at=591050,  line=llc, cex=pcx, col="white")
dev.off()




########### Figure 5: shrub and taiga coverage near water -----

wd1 <- 8
hd1 <- 8
x71 <- c(1,4,7)
x20 <- c(2,5,8)

png(paste0(dirSave, "/prox_water.png"), width=10, height=10, units="in", res=300)
layout(matrix(seq(1),ncol=1), width=lcm(c(wd1)*2.54),height=lcm(c(hd1)*2.54))


plot(c(0.5,3.5), c(0, 60), ylim=c(0,61), xlim=c(0,9),
     type="n", axes=FALSE, yaxs="i", xaxs="i",
     xlab = " ", ylab= " ")

for(i in 1:3){
  polygon(c(x71[i]-0.25,x71[i]-0.25,x71[i]+0.25,x71[i]+0.25),
          c(0, zone71F$percTaig[i+1],zone71F$percTaig[i+1],0),
          col=colsClass[2], border=NA)
}

for(i in 1:3){
  polygon(c(x71[i]-0.25,x71[i]-0.25,x71[i]+0.25,x71[i]+0.25),
          c(zone71F$percTaig[i+1], zone71F$percTaig[i+1]+zone71F$percshrub[i+1],
            zone71F$percTaig[i+1]+zone71F$percshrub[i+1],zone71F$percTaig[i+1]),
          col=colsClass[4], border=NA)
}

for(i in 1:3){
  polygon(c(x20[i]-0.25,x20[i]-0.25,x20[i]+0.25,x20[i]+0.25),
          c(0, zone20F$percTaig[i+1],zone20F$percTaig[i+1],0),
          col=colsClass[2], border=NA)
}

for(i in 1:3){
  polygon(c(x20[i]-0.25,x20[i]-0.25,x20[i]+0.25,x20[i]+0.25),
          c(zone20F$percTaig[i+1], zone20F$percTaig[i+1]+zone20F$percshrub[i+1],
            zone20F$percTaig[i+1]+zone20F$percshrub[i+1],zone20F$percTaig[i+1]),
          col=colsClass[4], border=NA)
}

text(x20, zone20F$percTaig[2:4]-2, paste(round(zone20F$percTaig[2:4])), 
     cex=1.5, col="white")
text(x71,  zone71F$percTaig[2:4]-2, paste(round(zone71F$percTaig[2:4])),
     cex=1.5, col="white")
text(x71,  zone71F$percTaig[2:4]+ zone71F$percshrub[2:4]-2, 
                    paste(round(zone71F$percshrub[2:4])), cex=1.5, col="white")
text(x20,  zone20F$percTaig[2:4]+ zone20F$percshrub[2:4]-2, 
     paste(round(zone20F$percshrub[2:4])), cex=1.5, col="white")

axis(1, c(-1,1,2,4,5,7,8,11), c("","1971","2020","1971","2020","1971","2020",""), 
     cex.axis=1.25)
axis(2, seq(0,60, by=10), las=2, cex.axis=1.25) 
mtext( "Percent area with cover (%)",side=2, line=3, cex=2)
mtext("0-50 m", side=1, at=1.5,line=2.75, cex=1.75)
mtext("50-100 m", side=1, at=4.5,line=2.75, cex=1.75)
mtext(">100 m", side=1, at=7.5,line=2.75, cex=1.75)

mtext("Proximity to surface water",side=1, line=4.5, cex=2)
legend("topright", c("taiga", "shrub"),
       fill=c(colsClass[2],colsClass[4]), bty="n", cex=1.75)

dev.off()



########### Figure Supplement: examples of changes -----
# taiga loss
aoiTL <- aoi[1,]
# shrub gain
aoiSG <- aoi[4,]
# taiga gain
aoiTG <- aoi[5,]
# shrub loss
aoiSL <- aoi[6,]



img71TL <- crop(img71m, aoiTL)
img20TL <- crop(img20m, aoiTL)
lc71TL <- crop(class71m, aoiTL)
lc20TL <- crop(class20m, aoiTL)

img71SG <- crop(img71m, aoiSG)
img20SG <- crop(img20m, aoiSG)
lc71SG <- crop(class71m, aoiSG)
lc20SG <- crop(class20m, aoiSG)


img71TG <- crop(img71m, aoiTG)
img20TG <- crop(img20m, aoiTG)
lc71TG <- crop(class71m, aoiTG)
lc20TG <- crop(class20m, aoiTG)

img71SL <- crop(img71m, aoiSL)
img20SL <- crop(img20m, aoiSL)
lc71SL <- crop(class71m, aoiSL)
lc20SL <- crop(class20m, aoiSL)


# plot dim
wd <- 4
hd1 <- 4
# arrow line width for scale bar
awd <- 2
# text size for scale bar
sce <- 3

#panel label line
llc <- -3.5
#panel label size
pcx <- 3
# x type label
capl <- 1.5
# scale bar color
scb <- "white"
# panel label color
pcc <- "white"


png(paste0(dirSave, "/fig_supplement_change examples.png"), width=20, height=20, units="in", res=300)
layout(matrix(seq(1,16),ncol=4, byrow=TRUE), 
       width=lcm(rep(wd*2.54,4)),height=lcm(c(hd1,hd1, hd1,hd1)*2.54))

### Taiga loss
# 1971 imagery
par(mai=c(0.01,0.01,0.01,0.01))

plot(img71TL, col=grey(1:100/100),axes=FALSE, mar=NA, legend=FALSE,
     maxcell=ncell(img71TL))
mtext("a", side=3, at=597660,  line=llc, cex=pcx, col=pcc)
arrows(597660,7622155, 597680, 7622155, code=0, lwd=awd, col=scb)
arrows(597660,7622151, 597660, 7622155, code=0, lwd=awd, col=scb)
arrows(597680,7622151, 597680, 7622155, code=0, lwd=awd, col=scb)
text(597660,7622149, "0", cex=sce, col=scb)
text(597665,7622149, "m", cex=sce, col=scb)
text(597680,7622149, "20", cex=sce, col=scb)

# 1971 class
plot(lc71TL, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(lc71TL))
mtext("b", side=3, at=597660,  line=llc, cex=pcx, col=pcc)
# 2020 imagery
par(mai=c(0.01,0.01,0.01,0.01))
plotRGB(img20TL, r=3, g=2, b= 1, stretch="lin", axes=FALSE, mar=NA, legend=FALSE,
        maxcell=ncell(img20TL))
mtext("c", side=3, at=597660,  line=llc, cex=pcx, col=pcc)
# 2020 class
par(mai=c(0.01,0.01,0.01,0.01))
plot(lc20TL, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(lc20TL))
mtext("d", side=3, at=597660,  line=llc, cex=pcx, col=pcc)
### taiga gain
# 1971 imagery
par(mai=c(0.01,0.01,0.01,0.01))

plot(img71TG, col=grey(1:100/100),axes=FALSE, mar=NA, legend=FALSE,
     maxcell=ncell(img71TG))
mtext("e", side=3, at=592200,  line=llc, cex=pcx, col=pcc)

arrows(592200,7621425, 592275, 7621425, code=0, lwd=awd, col=scb)
arrows(592200,7621415, 592200, 7621425, code=0, lwd=awd, col=scb)
arrows(592275,7621415, 592275, 7621425, code=0, lwd=awd, col=scb)
text(592200,7621401, "0", cex=sce, col=scb)
text(592220,7621401, "m", cex=sce, col=scb)
text(592275,7621401, "75", cex=sce, col=scb)
# 1971 class
plot(lc71TG, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(lc71TG))
mtext("f", side=3, at=592200,  line=llc, cex=pcx, col=pcc)
# 2020 imagery
par(mai=c(0.01,0.01,0.01,0.01))
plotRGB(img20TG, r=3, g=2, b= 1, stretch="lin", axes=FALSE, mar=NA, legend=FALSE,
        maxcell=ncell(img20TG))
mtext("g", side=3, at=592200,  line=llc, cex=pcx, col=pcc)
# 2020 class
par(mai=c(0.01,0.01,0.01,0.01))
plot(lc20TG, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(lc20TG))

mtext("h", side=3, at=592200,  line=llc, cex=pcx, col=pcc)

### shrub loss
# 1971 imagery
par(mai=c(0.01,0.01,0.01,0.01))

plot(img71SL, col=grey(1:100/100),axes=FALSE, mar=NA, legend=FALSE,
     maxcell=ncell(img71SL))

arrows(593700,7615750, 593750, 7615750, code=0, lwd=awd, col="white")
arrows(593700,7615730, 593700, 7615750, code=0, lwd=awd, col="white")
arrows(593750,7615730, 593750, 7615750, code=0, lwd=awd, col="white")
text(593700,7615720, "0", cex=sce, col="white")
text(593720,7615720, "m", cex=sce, col="white")
text(593750,7615720, "50", cex=sce, col="white")

mtext("i", side=3, at=593660,  line=llc, cex=pcx, col=pcc)
# 1971 class
plot(lc71SL, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(lc71SL))
mtext("j", side=3, at=593660,  line=llc, cex=pcx, col=pcc)
# 2020 imagery
par(mai=c(0.01,0.01,0.01,0.01))
plotRGB(img20SL, r=3, g=2, b= 1, stretch="lin", axes=FALSE, mar=NA, legend=FALSE,
        maxcell=ncell(img20SL))
mtext("k", side=3, at=593660,  line=llc, cex=pcx, col=pcc)
# 2020 class
par(mai=c(0.01,0.01,0.01,0.01))
plot(lc20SL, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(lc20SL))

mtext("l", side=3, at=593660,  line=llc, cex=pcx, col=pcc)
### shrub gain
# 1971 imagery
par(mai=c(0.01,0.01,0.01,0.01))

plot(img71SG, col=grey(1:100/100),axes=FALSE, mar=NA, legend=FALSE,
     maxcell=ncell(img71SG))
mtext("m", side=3, at=593800,  line=llc, cex=pcx, col=pcc)

arrows(593810,7614910, 593860, 7614910, code=0, lwd=awd, col=scb)
arrows(593810,7614900, 593810, 7614910, code=0, lwd=awd, col=scb)
arrows(593860,7614900, 593860, 7614910, code=0, lwd=awd, col=scb)
text(593810,7614890, "0", cex=sce, col=scb)
text(593825,7614890, "m", cex=sce, col=scb)
text(593860,7614890, "50", cex=sce, col=scb)

# 1971 class
plot(lc71SG, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(lc71SL))
mtext("n", side=3, at=593800,  line=llc, cex=pcx, col=pcc)
# 2020 imagery
par(mai=c(0.01,0.01,0.01,0.01))
plotRGB(img20SG, r=3, g=2, b= 1, stretch="lin", axes=FALSE, mar=NA, legend=FALSE,
        maxcell=ncell(img20SG))
mtext("o", side=3, at=593800,  line=llc, cex=pcx, col=pcc)
# 2020 class
par(mai=c(0.01,0.01,0.01,0.01))
plot(lc20SG, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE, mar=NA, maxcell=ncell(lc20SG))
mtext("p", side=3, at=593800,  line=llc, cex=pcx, col=pcc)
dev.off()

