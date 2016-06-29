set.seed(2)

library(mlbench)
library(mixtools)

#********************************Generating the datasets******************************#

TrainingData=mlbench.waveform(1800)
TestData=mlbench.waveform(600)
ValidationData=mlbench.waveform(600)

CovE=matrix(cov(TrainingData$x),nrow = 21,ncol =21)
Covariance=solve(CovE)

#******************************Training Data for model of each label******************#

TrainingDataw1=subset(TrainingData$x,TrainingData$classes=="1")
TrainingDataw2=subset(TrainingData$x,TrainingData$classes=="2")
TrainingDataw3=subset(TrainingData$x,TrainingData$classes=="3")

#********************K-Mean clustering for training data of each label****************#

Kmeanw1=kmeans(TrainingDataw1,centers = 3)
Kmeanw2=kmeans(TrainingDataw2,centers = 3)
Kmeanw3=kmeans(TrainingDataw3,centers = 3)

#**************Centers and lambdas of cluster made by above algorithm for each label**************#


Ceters1=Kmeanw1$centers
c11=Ceters1[1,]
c12=Ceters1[2,]
c13=Ceters1[3,]
Lambads1=Kmeanw1$size
Lambads1=Lambads1/sum(Lambads1)

Ceters2=Kmeanw2$centers
c21=Ceters2[1,]
c22=Ceters2[2,]
c23=Ceters2[3,]
Lambads2=Kmeanw2$size
Lambads2=Lambads2/sum(Lambads2)

Ceters3=Kmeanw3$centers
c31=Ceters3[1,]
c32=Ceters3[2,]
c33=Ceters3[3,]
Lambads3=Kmeanw3$size
Lambads3=Lambads3/sum(Lambads3)

Dummy1=list(as.numeric(Lambads1[1]),as.numeric(Lambads1[2]),as.numeric(Lambads1[3]),
            as.numeric(Lambads2[1]),as.numeric(Lambads2[2]),as.numeric(Lambads2[3]),
            as.numeric(Lambads3[1]),as.numeric(Lambads3[2]),as.numeric(Lambads3[3]))
Dummy2=list(c11,c12,c13,c21,c22,c23,c31,c32,c33)

#***************Function for predicting the label baesd on the above model***********#

Liklihood<-function(x, mean, covarianceInverse){
  #Covariance matrix is same and priors are also same so lets remove constants
  gx= -1* as.numeric(x-mean) %*% covarianceInverse %*% as.numeric(t(x-mean))
  return(gx)
}

"PredictedClass"=function(X,Dummy2)
{

  p11=Liklihood(X,Dummy2[[1]],Covariance)#*as.numeric(Dummy1[1])
  p12=Liklihood(X,Dummy2[[2]],Covariance)#*as.numeric(Dummy1[2])
  p13=Liklihood(X,Dummy2[[3]],Covariance)#*as.numeric(Dummy1[3])
  probA=p11+p12+p13
  
  p21=Liklihood(X,Dummy2[[4]],Covariance)#*as.numeric(Dummy1[4])
  p22=Liklihood(X,Dummy2[[5]],Covariance)#*as.numeric(Dummy1[5])
  p23=Liklihood(X,Dummy2[[6]],Covariance)#*as.numeric(Dummy1[6])
  probB=p21+p22+p23
 
  p31=Liklihood(X,Dummy2[[7]],Covariance)#*as.numeric(Dummy1[7])
  p32=Liklihood(X,Dummy2[[8]],Covariance)#*as.numeric(Dummy1[8])
  p33=Liklihood(X,Dummy2[[9]],Covariance)#*as.numeric(Dummy1[9])
  
  probC=p31+p32+p33
 
  if(probA>probB & probA>probC){
    return(1)
  }else if(probB>probA & probB>probC){
    return(2)
  }else{
    return(3)
  }
  
}

#***************************Predicting the labels*************************************#

pridictedlabel=0
variationVector = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

for(k in variationVector)
  {
  
  missclasification = 0 
Temp=list(Dummy2[[1]] - k*as.numeric(diag(CovE)), 
     Dummy2[[2]] - k*as.numeric(diag(CovE)), 
     Dummy2[[3]] - k*as.numeric(diag(CovE)), 
     Dummy2[[4]] - k*as.numeric(diag(CovE)), 
     Dummy2[[5]] - k*as.numeric(diag(CovE)), 
     Dummy2[[6]] - k*as.numeric(diag(CovE)), 
     Dummy2[[7]] - k*as.numeric(diag(CovE)), 
     Dummy2[[8]] - k*as.numeric(diag(CovE)), 
     Dummy2[[9]] - k*as.numeric(diag(CovE)))

  for(i in 1:10)
  {
    TrainingData$x[i]
    Temp 
    pridictedlabel=PredictedClass(TrainingData$x[i],Temp)
    print(pridictedlabel)
    if(pridictedlabel != TrainingData$classes[i])
    {
    missclasification=missclasification+1
    }
  }
print(paste("Misclassification: ", missclasification))
}

#***********************************LDA and QDA Error**********************************#

LDA=lda(TrainingData$classes~.,data=TrainingData)
predict=predict(object=LDA,TestData)
Error=mean(predict$class!=TestData$classes)
print(paste("Misclassification In LDA: ", Error))

QDA= qda(TrainingData$classes~.,data=TrainingData)
predict=predict(object=QDA,TestData)
Error=mean(predict$class!=TestData$classes)
print(paste("Misclassification In QDA: ", Error))

#*****************************************************************************************************#