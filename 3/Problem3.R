set.seed(2)
library(e1071)

#Changinig the value of C 
CostList=c(1,2, 4, 8, 16, 32, 64, 128)
Erorr= c()

PreviousAccuracy=as.numeric(0)

#finding the cross validation error for different values of C
for(i in CostList)
{
SVMModel=svm(spambase.$V58~.,data=spambase.,type="C-classification",cross = 5,cost = i)
Erorr=c(Erorr,100-SVMModel$tot.accuracy)
if(PreviousAccuracy<SVMModel$tot.accuracy)
  {
  PreviousAccuracy=SVMModel$tot.accuracy
  FiveFoldAccuracies=SVMModel$accuracies
  BestCost=i
  }
}

#Graph b/w error and C.
PreviousAccuracy
plot(x = CostList,y = Erorr,xlab ="C Value",ylab = "Error(%)",type = "b")
FiveFoldAccuracies
BestCost
