
#Generating Waveform Datasets.
  
library(mlbench)
library(caret)

setwd("C:\\Users\\Sharin\\CodeShop\\R\\Assign3\\")
data <- mlbench.waveform(1000)
write.csv(data, "Ass3.csv", row.names=TRUE)
Data <- read.csv("Ass3.csv",header = TRUE)

install.packages('e1071', dependencies=TRUE)
lda_model=train(classes ~ .,data=Data,method ="lda") ####


# Initially got Error : Error in train.default(x, y, weights = w, ...) : wrong model type for regression
# Prognosis?  Factor?? easy way change to names!!

# Next Error : Error in requireNamespaceQuietStop("e1071") : package e1071 is required
# Prognosis? install.packages('e1071', dependencies=TRUE)

table(Data$classes)
#classes <- as.factor(classes) 

inTrain <- createDataPartition(y=Data$classes, p=0.7, list=FALSE)
S_Data <- Data[inTrain,]
Train <- createDataPartition(y=S_Data$classes, p=0.7, list=FALSE)

training <- S_Data[Train,]
validation <- S_Data[-Train,]
testing <- Data[-inTrain,]

dim(training)
dim(validation)
dim(testing)

lda_model=train(training$classes ~ .,data=training,method ="lda")
#dput



t_actual <-Data$classes[-inTrain]


prediction=predict(lda_model,newdata = testing,type="class")
cerror=mean(t_actual!=prediction)) 

qda_model = train(classes~.,data=training,method ="qda")
