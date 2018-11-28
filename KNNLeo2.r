
install.packages("class")
library(class)


install.packages("ggplot2")
library(ggplot2)

setwd("C:/Documents")

RawData <- read.csv("BreastCancerData.csv")

names(RawData) <- c("ID","Diagnosis","Mean Radius","Mean Texture","Mean Perimeter","Mean Area","Mean Smoothness","Mean Compactness","Mean Concavity","Mean Concave Points","Mean Symmetry","Mean Fractal Dimension","Radius SE","Texture SE","Perimeter SE","Area SE","Smoothness SE","Compactness SE","Concavity SE","Concave Points SE","Symmetry SE","Fractal Dimension SE","Worst Radius","Worst Texture","Worst Perimeter","Worst Area","Worst Smoothness","Worst Compactness","Worst Concavity","Worst Concave Points","Worst Symmetry","Worst Fractal Dimension")

Data_NoID <- RawData[,-1]

Data_NoResults <- Data_NoID[,-1]

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }

Data_Normalised <- as.data.frame(lapply(Data_NoResults, FeatureScaling))

Data_Training <- Data_Normalised[1:425, ] 

Data_Test <- Data_Normalised[426:568, ]

K_Value <- floor(sqrt(length(Data_Training[,1])))  

Data_Predictions <- knn(Data_Training,Data_Test,Data_NoID[1:425,1], k=K_Value)

Data_Reference <- Data_NoID[426:568,1]

table(Data_Predictions,Data_Reference)

#---------------------------------------------------------------------------------------------

big_errors <- c()

kValues <- c()

for(i in c(1:100)) {
  
  next_error <-  table(knn(Data_Training,Data_Test,Data_NoID[1:425,1],k=i),Data_Reference)[1,2] +
                table(knn(Data_Training,Data_Test,Data_NoID[1:425,1],k=i),Data_Reference)[2,1]
  
  big_errors <- c(big_errors, next_error)
  
  kValues <- c(kValues, i)
}

big_errors_df <- data.frame(kValues,big_errors)

names(big_errors_df) <- c("k-Value","Error Value")

ggplot(big_errors_df, aes(x = kValues, y = big_errors)) +
  geom_point() +
  geom_smooth(method = "loess",colour = "blue", size = 1) + 
  ggtitle("Error vs k-Value for Breast Cancer Data") +
  xlab("k-Values") +
  ylab("Error") +
  theme(axis.text.x=element_text(angle=-45, vjust=0.5)) +
  theme(axis.text.y=element_text(angle=-45, hjust=-0.1, vjust=-0.5)) +
  scale_colour_manual(values = c("red","blue"))
