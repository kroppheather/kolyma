##########################################################
###### organize georef images and prepare for tiling ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)



img <- rast("K:/Environmental_Studies/hkropp/Private/Projects/Siberia/wv8b_07_20.tif")


plotRGB(img, stretch="lin", r=3,g=2,b=1)


set.seed(15)
rowsi <- sample(1:(nrow(img)-255), 400)
set.seed(43)
colsi <- sample(1:(ncol(img)-255), 400)

# subset raster
training.samples <- list()
training.check <- numeric()
for(i in 1:400){
  training.samples[[i]] <-  img[rowsi[i]:(rowsi[i]+255), colsi[i]:(colsi[i]+255), drop=FALSE]
  # create a check to remove any training samples with NAs
  training.check[i] <- ifelse(sum(values(training.samples[[i]],mat=FALSE)) == 0,0,1)
}

plotRGB(training.samples[[4]],  r=3,g=2,b=1)
values(training.samples[[4]])

training.valid <- which(training.check == 1)
samples.use <- list()
#first sample looks odd
for(i in 1:200){
  samples.use[[i]] <- training.samples[[training.valid[i]]]
}
plot(samples.use[[4]], col=gray(1:100/100))


for(i in 1:200){
  writeRaster(samples.use[[i]], paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net20/training/img/img_",i,".tif"))
} 

test <- rast("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net20/training/img/img_1.tif")
plotRGB(test, r=3,g=2,b=1, stretch="lin")


nrow(img)-255
ncol(img)-255

tiley <- seq(1,nrow(img)-255, by=255)
tilex <- seq(1,ncol(img)-255, by=255)

tilepairs <- data.frame(cols=rep(tilex, times=length(tiley)),
                        rows=rep(tiley, each=length(tilex)))

for(i in 1:nrow(tilepairs)){
 writeRaster(img[tilepairs$rows[i]:(tilepairs$rows[i]+255), tilepairs$cols[i]:(tilepairs$cols[i]+255), drop=FALSE],
              paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net20/img_256/img_",i,".tif"))
              
  
}


tileI <- makeTiles(img, c(256,256), "K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/u_net20/img_256/img.tif")



# add additional stratified sampling for the low density area



img <- rast("/media/hkropp/research/Kolyma_Data/imagery_20/wv8b_07_20.tif")
stratE <- vect("/media/hkropp/research/Kolyma_Data/low_density_e/low_density_ex.shp")

imgLE <- crop(img,stratE)


set.seed(25)
rowsi <- sample(1:(nrow(imgLE )-255), 25)
set.seed(4)
colsi <- sample(1:(ncol(imgLE )-255), 25)
training.samples <- list()
training.check <- numeric()
for(i in 1:25){
  training.samples[[i]] <-  imgLE[rowsi[i]:(rowsi[i]+255), colsi[i]:(colsi[i]+255), drop=FALSE]
  # create a check to remove any training samples with NAs
  training.check[i] <- ifelse(sum(values(training.samples[[i]],mat=FALSE)) == 0,0,1)
}


training.valid <- which(training.check == 1)



for(i in 1:25){
  writeRaster(training.samples[[i]], paste0("/media/hkropp/research/Kolyma_Data/training/Kolyma/u_net20/training/img_strat/img_",i+200,".tif"))
} 



# set up tile offset
img <- rast("/media/hkropp/research/Kolyma_Data/imagery_20/wv8b_07_20.tif")
dim(img)
imgO1 <- img[25:33723, 25:23595, drop=FALSE]
plot(imgO1)
tileI <- makeTiles(imgO1, c(256,256), "/media/hkropp/research/Kolyma_Data/img_tiles/2020/tiles_256_2/img_.tif")
imgO2 <- img[100:33723, 100:23595, drop=FALSE]
plot(imgO2)
tileI2 <- makeTiles(imgO2, c(256,256), "/media/hkropp/research/Kolyma_Data/img_tiles/2020/tiles_256_3/img_.tif")


# add additional stratified sampling for the low density area




stratE <- vect("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/strat_20/strat_points.shp")

xycell <- cells(img, stratE)


rowi <- numeric()
coli <- numeric()
#training.check <- numeric()
for(i in 1:20){
  rowi[i] <- rowFromCell(img, xycell[i,2])
  coli[i] <- colFromCell(img, xycell[i,2])
}

training.samples <- list()
training.check <- numeric()
for(i in 1:20){  
  if(rowi[i] <= nrow(img)-255 & coli[i] <= ncol(img)-255){
    training.samples[[i]] <-  img[rowi[i]:(rowi[i]+255), coli[i]:(coli[i]+255), drop=FALSE]
    training.check[i] <- ifelse(is.na(mean(values(training.samples[[i]],mat=FALSE))) == TRUE,0,1)
  }else{
    training.samples[[i]] <- NA
    training.check[i] <- 0
  }
}
training.valid <- which(training.check == 1)

plotRGB(training.samples[[2]], stretch="hist")
length(training.valid)

for(i in 1:20){
  writeRaster(training.samples[[i]], paste0("K:/Environmental_Studies/hkropp/Private/siberia_wv/Kolyma/strat_20/img/img_",i+225,".tif"))
} 
