#############--------Question 1-----------#############

# Install and load required packages for decision trees and forests
library(randomForest)
library(caret)

# Common Initialization Code
#--------------------------------------------------------


setwd("C:\\Users\\Sharin\\CodeShop\\R\\Assign2\\")

Data <- read.csv("F:\\Workspace\\IIT\\Sem2\\CS771 - Machine Learning\\Ass2\\letter-recognition.data",header = TRUE)
set.seed(415)
names(Data)
folds <- createFolds(Data$lettr, k=5)
str(folds)
t_error=numeric(0)
sample_size = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)

#FUNCTIONS
#--------------------------------------------------------
#Function - Validation Error 
getXError <- function(noTree,size){
  
  set.seed(415)  
  error<-numeric(0)
  for(i in 1:5)
  {
    x_test_s = folds[[i]]
    x_train_s =setdiff(c(1:20000), x_test_s)
    x_test = Data[x_test_s,]
    x_train = Data[x_train_s,]
    
    actual  = Data$lettr[x_test_s]  
    x_fit <- randomForest(lettr ~ . ,data=x_train, ntree=noTree,sampsize = nrow(x_train)*size,replace = FALSE)    
    predictions=predict(x_fit,newdata = x_test,type="class")
    error=rbind(error,mean(actual!=predictions)) 
    
  }
  return(mean(error))
}

#Function - Binary Search
binSearch <- function(h_error,low,high,ssize){
  mid <- floor((low+high)/2)
  m_error <- getXError(mid,ssize)
  
  int = abs(h_error - m_error)
  
  if((abs(low-mid)<=1)||(abs(mid-high)<=1))
  {
    return(high)      
  }
  print("Low Value")
  print(low)
  print("Mid Value")
  print(mid)
  print("High Value")
  print(high)
  print("Mid Error")
  print(m_error)
  
  
  if(int<0.0015){
    high =mid
    return (binSearch(h_error,low,high,ssize))
  }
  
  else 
  {
    low = mid
    return (binSearch(h_error,low,high,ssize))
  }
  
}
# ACTUAL CODE STARTS HERE
#--------------------------------------------------------

arr_opt_pt<-array(0,dim=8)

arr_x<-array(0,dim=8)

for(k in 1:8){
  s_size = sample_size[k]
  tresh = getXError(500,s_size)
  arr_opt_pt[k]=binSearch(tresh,2,500,s_size)
  print("*_*_*_*_*_*_*_*_*_*_*_*_*_  k =") 
  print(k)
  arr_x[k]<- getXError(arr_opt_pt[k],s_size)
  
  
  error_plot=array(0,dim=10)
  for(i in 1:9)
  { 
    print("=*=*=*=*=*=*=*=*=*=*=*=*=* i =") 
    print(i)
    if(i<9){
      error_plot[i]=getXError(2^i,s_size) 
    }
    else{
      error_plot[i]=getXError(500,s_size) 
    }
    
    #temp <- error4[i/5] #print(i)  #print(temp)
  }
  #print(error_plot)
  fileName = paste("error-plot-", k, ".jpg")
  png(filename = fileName, width = 600, height = 700)
  plot(error_plot*100,type = "b",xlab = "No. of Trees  (Scale - 2^x)",ylab = "CV Error(%)", main = "Error Plot vs No. of Trees")
  dev.off()
  
}
# PART D
#--------------------------------------------------------
png(filename = "question4.jpg", width = 600, height = 700)
plot(x=sample_size, y=arr_x*100,type = "b",xlab = "Sampling Size (%)",ylab = "CV Error(%)", main="Error Off vs Sample Size")
dev.off()