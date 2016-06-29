#traing data ---70%
#testing data ---15%
#validation data --15%

set.seed(20)

train <- mlbench.waveform(1400)
test<- mlbench.waveform(300)
vald <-mlbench.waveform(300)

Cov_matrix=matrix(cov(train$x),nrow = 21,ncol =21)
Cov_inv=solve(Cov_matrix)
