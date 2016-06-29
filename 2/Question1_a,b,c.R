#############--------Question 1-----------#############

# Install and load required packages for decision trees and forests
library(randomForest)
library(caret)

# Common Initialization Code
#--------------------------------------------------------

setwd("C:\\Users\\Sharin\\CodeShop\\R\\Assign4\\")

Data <- read.csv("spambase.data",header = TRUE)
set.seed(415)
names(Data)
folds <- createFolds(Data$lettr, k=5)
str(folds)
t_error=numeric(0)

#Functions
#--------------------------------------------------------
#Function - Return Validation Error 
getXError <- function(noTree){
  
  set.seed(2)  
  error<-numeric(0)
  for(i in 1:5)
  {
    x_test_s = folds[[i]]
    x_train_s =setdiff(c(1:20000), x_test_s)
    x_test = Data[x_test_s,]
    x_train = Data[x_train_s,]
    
    actual  = Data$lettr[x_test_s]  
    x_fit <- randomForest(lettr ~ . ,data=x_train, ntree=noTree,replace = TRUE)    
    predictions=predict(x_fit,newdata = x_test,type="class")
    
    error2=mean(actual!=predictions)
    error=rbind(error,error2) 
    
  }
  return(mean(error))
}

#Function - Binary Search
binSearch <- function(h_error,low,high){
  mid <- floor((low+high)/2)
  m_error <- getXError(mid)
  
  int = abs(h_error - m_error)
  
  if((abs(low-mid)<=1)||(abs(mid-high)<=1))
  {
    return(high)      
  }
  
  if(int<0.0015){
    high =mid
    return (binSearch(h_error,low,high))
  }
  
  else 
  {
    low = mid
    return (binSearch(h_error,low,high))
  }
  
}

# PART A
#--------------------------------------------------------

Lowerror=getXError(500) # 0.03500119

print(Lowerror)

no_of_tree=binSearch(Lowerror,2,500)#testing recursion
print(no_of_tree)

error_plot=array(0,dim=100)
for(i in seq(5,500,5))
{ 
  error_plot[i/5]=getXError(i)  
  #temp <- error4[i/5] #print(i)  #print(temp)
}
#print(error_plot)
plot(error_plot*100,type = "p",xlab = "No. of Trees  (Scale -5 )",ylab = "CV Error(%)")
#----------------------------------------------------------------------
# PART B
#----------------------------------------------------------------------

set.seed(415)
rf=randomForest(lettr~.,data=Data,ntree = 93,replace = TRUE)
print(r)

#OOB - 3.72
#----------------------------------------------------------------------
# PART C
#----------------------------------------------------------------------

m=1
total_error<-array(0,dim=4)
for(k in 1:4)
{
 set.seed(415)  
 cerror<-numeric(0)
 print("Count")
 print(m)
 for(i in 1:5)
 {
   cx_test_s = folds[[i]]
   cx_train_s =setdiff(c(1:20000), cx_test_s)
   cx_test = Data[cx_test_s,]
   cx_train = Data[cx_train_s,]
   
   cactual  = Data$lettr[cx_test_s]
   cx_fit <- randomForest(lettr ~ . ,data=cx_train, ntree=116,replace = TRUE,mtry = m)
   
   prediction=predict(cx_fit,newdata = cx_test,type="class")
   cerror=rbind(cerror,mean(cactual!=prediction)) 
   
 }  
 print(cerror)
  total_error[k]=mean(cerror)
  m=m*2
}
print(total_error*100)
# PART D