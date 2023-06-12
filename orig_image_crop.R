library(terra)

dir <- "e:/Google Drive/research/projects/Kolyma/orig"

files <- list.files(dir)
img <- list()
for(i in 1:length(files)){
  img[[i]] <- rast(paste0(dir,"/",files[i]))
}
ext(img[[2]])
plot(img[[2]], col=gray(1:100/100))
plot(img[[4]], col=gray(1:100/100))
plot(img[[6]], col=gray(1:100/100))
imgsC <- list()


imgC <- list()

for(i in 1:length(files)){
  imgC[[i]] <- crop(img[[i]], ext(.25, 25.75, 1.1, 15.9))
}

par(mfrow=c(2,1))
plot(imgC[[4]], col=gray(1:100/100))
plot(imgC[[2]], col=gray(1:100/100))


plot(imgC[[6]], col=gray(1:100/100))
plot(imgC[[4]], col=gray(1:100/100))

par(mfrow=c(2,1))
