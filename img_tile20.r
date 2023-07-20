##########################################################
###### organize georef images and prepare for tiling ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)



img <- rast("K:/Environmental_Studies/hkropp/Private/Projects/Siberia/MSJuly19_20.tif")


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
  training.check[i] <- ifelse(is.na(mean(values(training.samples[[i]],mat=FALSE))) == TRUE,0,1)
}

plotRGB(training.samples[[4]],  r=3,g=2,b=1)
values(training.samples[[4]])

training.valid <- which(training.check == 1)
samples.use <- list()
#first sample looks odd
for(i in 1:220){
  samples.use[[i]] <- training.samples[[training.valid[i]]]
}
plot(samples.use[[3]], col=gray(1:100/100))
