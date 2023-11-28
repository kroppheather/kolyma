library(dplyr)
library(caret)
library(ggplot2)
library(terra)

# other will be zero, trees =1, water =2, shrub =3, low =4
classP <- rast("/media/hkropp/research/Kolyma_Data/predictions/maps/class2020_k5_v3.tif") 
pointsA <- vect("/media/hkropp/research/Kolyma_Data/valid/valid_20/valid_20.shp")

plot(classP)
plot(pointsA, add=TRUE)

p_extract <- extract(classP, pointsA, method="simple")

tabA <- values(pointsA)
tabA$classID <- ifelse(tabA$class == "o", 0,
                ifelse(tabA$class == "t", 1,
                ifelse(tabA$class == "w", 2,
                ifelse(tabA$class == "s", 3,
                ifelse(tabA$class == "l", 4,NA)))))

tabA$predID <- p_extract[,2]

predict_pts <- na.omit(tabA)

conf <- confusionMatrix(as.factor(predict_pts$predID), as.factor(predict_pts$classID))


conf$table

overallAccuracy <- conf$overall[1]



other_PA <- conf$table[1,1]/sum(conf$table[,1])
tree_PA <-  conf$table[2,2]/sum(conf$table[,2])
water_PA <-  conf$table[3,3]/sum(conf$table[,3])
shrub_PA <-  conf$table[4,4]/sum(conf$table[,4])
lowD_PA <-  conf$table[5,5]/sum(conf$table[,5])

other_UA <- conf$table[1,1]/sum(conf$table[1,])
tree_UA <-  conf$table[2,2]/sum(conf$table[2,])
water_UA <-  conf$table[3,3]/sum(conf$table[3,])
shrub_UA <-  conf$table[4,4]/sum(conf$table[4,])
lowD_UA <-  conf$table[5,5]/sum(conf$table[5,])



labels <- c("Other", "Tree", "Water", "Shrub", "Low Density Forest", "Other", "Tree", "Water", "Shrub", "Low Density Forest")
data <- as.numeric(c(other_UA, tree_UA, water_UA, shrub_UA, lowD_UA, other_PA, tree_PA, water_PA, shrub_PA, lowD_PA))
type <- c("User Accuracy", "User Accuracy", "User Accuracy", "User Accuracy", "User Accuracy", "Producer Accuracy", "Producer Accuracy", "Producer Accuracy", "Producer Accuracy", "Producer Accuracy")
acc <- tibble(labels, data,type)
acc$percent <- round(acc$data*100,2)

ggplot(acc, aes(fill=type, y=percent, x=labels)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#A3CB8F","#C7A26B")) +theme_bw() + 
  theme(text = element_text(family = "comforta"), 
        legend.title = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12.5)) + labs(x = "Classification", y = "Accuracy (%)") 


