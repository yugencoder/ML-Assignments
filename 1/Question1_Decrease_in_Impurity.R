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

fit  <- rpart(type ~ ., method="class", data=csvdata, control = list(xval =5))
# xval = 5 - number of cross validations

printcp(fit) #displaying cp table
plotcp(fit) #plot cross-validation results

# Optimal cp value found to be 0.01 from plotcp
# Hence putting treshold cp = 0.01
fit  <- rpart(type ~ ., method="class", data=csvdata, control = list(xval = 5,cp = 0.01))

#Plotting the decision tree and saving it as an Image File
fn=sprintf("C:\\Users\\Sharin\\CodeShop\\R\\Q1.1_plot.jpeg")
prp(fit)
dev.copy(jpeg,filename=fn,width = 1000, height = 800)
dev.off ()


