library(dplyr)
library(caret)
library(ggplot2)

dirA <- "/media/studentuser/Seagate Portable Drive/predictions_71/kernalmodel/accuracy"
valid_pts <- read.csv(paste0(dirA, '/valid_pts.csv'))
predict_pts <- read.csv(paste0(dirA, '/predict_pts.csv'))

predict_pts_noNA <- na.omit(predict_pts)

predict_pts_noNA$actual <- ifelse(predict_pts_noNA$Class == 'OT', 0,
                                ifelse(predict_pts_noNA$Class == 'TR', 1,
                                     ifelse(predict_pts_noNA$Class == 'WT', 2,
                                              ifelse(predict_pts_noNA$Class == 'SH', 3, 4)
                                     )
                                )
                            )

predict_pts_noNA$prediction <- predict_pts_noNA$RASTERVALU

conf <- confusionMatrix(as.factor(predict_pts_noNA$prediction), as.factor(predict_pts_noNA$actual))


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


ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#A3CB8F") +
  labs(x = "Reference",y = "Prediction") +
  scale_y_discrete(labels=c("Other","Tree","Water","Shrub","Low Density Forest")) +
  scale_x_discrete(labels=c("Low Density Forest","Shrub","Water","Tree","Other"))+theme_bw() + 
  theme(text = element_text(family = "comforta"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 12.5))


