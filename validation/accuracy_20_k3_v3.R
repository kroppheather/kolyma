library(dplyr)
library(caret)
library(ggplot2)
library(terra)

# other will be zero, trees =1, water =2, shrub =3, low =4
classP <- rast("/media/hkropp/research/Kolyma_Data/predictions/v2/maps/class2020_k3_v3.tif") 
pointsA1 <- vect("/media/hkropp/research/Kolyma_Data/valid/v2/valid_all/valid_20.shp")
pointsA2 <- vect("/media/hkropp/research/Kolyma_Data/valid/v2/valid_all/valid_71.shp")


plot(classP)
plot(pointsA1, add=TRUE)

p_extract1 <- extract(classP, pointsA1, method="simple")

tabA1 <- values(pointsA1)
tabA1$classID <- ifelse(tabA1$class == "o", 0,
                ifelse(tabA1$class == "ti", 1,
                ifelse(tabA1$class == "w", 2,
                ifelse(tabA1$class == "s", 3,NA))))

tabA1$predID <- p_extract1[,2]


p_extract2 <- extract(classP, pointsA2, method="simple")

tabA2 <- values(pointsA2)
tabA2$classID <- ifelse(tabA2$Class20 == "o", 0,
                        ifelse(tabA2$Class20 == "ti", 1,
                               ifelse(tabA2$Class20 == "w", 2,
                                      ifelse(tabA2$Class20 == "s", 3,NA))))

tabA2$predID <- p_extract2[,2]

tabA <- data.frame(classID = c(tabA1$classID, tabA2$classID),
                   predID= c(tabA1$predID, tabA2$predID))

predict_pts <- na.omit(tabA)

conf <- confusionMatrix(as.factor(predict_pts$predID), as.factor(predict_pts$classID))


conf$table

overallAccuracy <- conf$overall[1]



other_PA <- conf$table[1,1]/sum(conf$table[,1])
tree_PA <-  conf$table[2,2]/sum(conf$table[,2])
water_PA <-  conf$table[3,3]/sum(conf$table[,3])
shrub_PA <-  conf$table[4,4]/sum(conf$table[,4])


other_UA <- conf$table[1,1]/sum(conf$table[1,])
tree_UA <-  conf$table[2,2]/sum(conf$table[2,])
water_UA <-  conf$table[3,3]/sum(conf$table[3,])
shrub_UA <-  conf$table[4,4]/sum(conf$table[4,])




labels <- c("Other", "Taiga", "Water", "Shrub",  "Other", "Taiga", "Water", "Shrub")
data <- as.numeric(c(other_UA, tree_UA, water_UA, shrub_UA, other_PA, tree_PA, water_PA, shrub_PA))
type <- c("User Accuracy", "User Accuracy",  "User Accuracy", "User Accuracy", "Producer Accuracy", "Producer Accuracy",  "Producer Accuracy", "Producer Accuracy")
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

acc

conf$table
overallAccuracy
