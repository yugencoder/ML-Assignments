# Clustering

train1 <- subset(train$x , train$classes == "1")
train2 <- subset(train$x , train$classes == "2")
train3 <- subset(train$x , train$classes == "3")

# mean of class
m1 <- kmeans(train1 , centers =3)
m2 <- kmeans(train2 , centers =3)
m3 <- kmeans(train3 , centers =3)

#center of clusters

#class 1
center_m11 <- m1$centers[1,]
center_m12 <- m1$centers[2,]
center_m13 <- m1$centers[3,]
l1 <- m1$size
l1 <- l1/sum(l1)

#class 2
center_m21 <- m2$centers[1,]
center_m22 <- m2$centers[2,]
center_m23 <- m2$centers[3,]
l2 <- m2$size
l2 <- l2/sum(l2)

#class 3
center_m31 <- m3$centers[1,]
center_m32 <- m3$centers[2,]
center_m33 <- m3$centers[3,]
l3 <- m3$size
l3 <- l3/sum(l3)

list1=list(as.numeric(l1[1]),as.numeric(l1[2]),as.numeric(l1[3]),
            as.numeric(l2[1]),as.numeric(l2[2]),as.numeric(l2[3]),
            as.numeric(l3[1]),as.numeric(l3[2]),as.numeric(l3[3]))

list2=list(center_m11,center_m12,center_m13,center_m21,center_m22,center_m23,center_m31,center_m32,center_m33)


#likelihood function

likelihood<-function(x,mean,cov_inv)
{
  gx= -1* as.numeric(x-mean) %*% cov_inv %*% as.numeric(t(x-mean))
  return(gx)
}


"Pred_Class"=function(X,list2)
{
  p11<-likelihood(X,list2[[1]],Cov_matrix)
  p12<-likelihood(X,list2[[2]],Cov_matrix)
  p13<-likelihood(X,list2[[3]],Cov_matrix)
  prob_A=p11+p12+p13
  
  p21=likelihood(X,list2[[4]],Cov_matrix)
  p22=likelihood(X,list2[[5]],Cov_matrix)
  p23=likelihood(X,list2[[6]],Cov_matrix)
  prob_B=p21+p22+p23
  
  p31=likelihood(X,list2[[7]],Cov_matrix)
  p32=likelihood(X,list2[[8]],Cov_matrix)
  p33=likelihood(X,list2[[9]],Cov_matrix)
  
  prob_C=p31+p32+p33
  
  if(prob_A>prob_B && prob_A>prob_C){
    return(1)
  }else if(prob_B>prob_A && prob_B>prob_C){
    return(2)
  }else{
    return(3)
  }
}

plabel=0
vv = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

ml=c()

for(i in vv)
{
  i =0.3
  temp=list(list2[[1]] - i*as.numeric(diag(Cov_matrix)), 
            list2[[2]] - i*as.numeric(diag(Cov_matrix)), 
            list2[[3]] - i*as.numeric(diag(Cov_matrix)), 
            list2[[4]] - i*as.numeric(diag(Cov_matrix)), 
            list2[[5]] - i*as.numeric(diag(Cov_matrix)), 
            list2[[6]] - i*as.numeric(diag(Cov_matrix)), 
            list2[[7]] - i*as.numeric(diag(Cov_matrix)), 
            list2[[8]] - i*as.numeric(diag(Cov_matrix)), 
            list2[[9]] - i*as.numeric(diag(Cov_matrix)))
  mc = 0 
  for(j in 1:nrow(train$x))
  {
    plabel=Pred_Class(train$x[j],temp)
    if(plabel != train$classes[j])
    {
      mc<-mc+1
    }
  }
  mc_error <- mc/nrow(train$x)
  i
  ml = c(ml, mc_error)
}

plot(x = vv, y = ml, xlab = "Mean Variation", ylab="Misclassification Error", type = "b")


LDA<-lda(train$classes~.,data=train)
predict<-predict(object=LDA,test)
e<-mean(predict$class!=test$classes)
e


QDA<-qda(train$classes~.,data=train)
predict<-predict(object=QDA,test)
e<-mean(predict$class!=test$classes)
e
