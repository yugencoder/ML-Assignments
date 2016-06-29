#
# Author: Sharin 
# Assignemnt: Decision Tree
#
#--------------------------------------------------------------------------#

library(rpart)
library(rpart.plot)

setwd("C:\\Users\\Sharin\\CodeShop\\R")

csvdata <- read.csv("C:\\Users\\Sharin\\CodeShop\\R\\pima_data.csv",header = FALSE,na.string="?")
names(csvdata) <- c("npreg", "glu", "bp", "bmi", "ped", "age", "type")
na.omit(csvdata)

#-------------------------------


g_fit  <- rpart(type ~ ., method="class", data=csvdata, control = list(xval =5),parms = list(split = "gini"))
# xval = 5 - number of cross validations
printcp(g_fit) #displaying cp table
prp(g_fit)

fn=sprintf("C:\\Users\\Sharin\\CodeShop\\R\\Q2_1plot.jpeg")
dev.copy(jpeg,filename=fn,width = 1000, height = 800)
dev.off () 

i_fit  <- rpart(type ~ ., method="class", data=csvdata,control=rpart.control(xval =5),parms = list(split = "information"))
prp(i_fit)
printcp(i_fit) #displaying cp table

fn=sprintf("C:\\Users\\Sharin\\CodeShop\\R\\Q2_2plot.jpeg")
dev.copy(jpeg,filename=fn,width = 1000, height = 800)
dev.off ()
