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
  crs(imgC[[i]]) <- NA
}



for(i in 1:length(files)){
  writeRaster(imgC[[i]], paste0("e:/Google Drive/research/projects/Kolyma/georef_files/",files[i]))
}

plot(imgC[[1]], col=gray(1:100/100))
str(img[[1]]@ptr)
help("writeRaster")

crs(imgC[[1]]) <- NA

writeRaster(imgC[[1]], paste0("e:/Google Drive/research/projects/Kolyma/georef_files/",files[1]))

crs(imgC[[2]])
