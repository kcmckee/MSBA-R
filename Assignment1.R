rm(list=ls())

##################### 
#### QUESTION 1 #### 
##################### 

needed  <-  c("FNN")  #class contains the knn() function
installIfAbsentAndLoad(needed)
needed <- c('class')
installIfAbsentAndLoad(needed)

homeprice=read.table("HomePrices.txt",header=TRUE,sep="\t")
homeprice=data.frame(homeprices)
#head(homepricesdf)
mean_medv<-mean(homepricesdf$medv)
mean_medv_ans<-mean((homepricesdf$medv-mean_medv)^2)
print((mean_medv_ans))
n <- nrow(homeprice)
print(var(homeprice$medv)*((n-1)/n))
scalehp <- homeprice
scalehp[1:12] <- scale(homeprice[1:12])
head(scalehp)

set.seed(5072)
n <- nrow(scalehp)
trainprop <- 0.75
validateprop <- 0.15
train  <-  sample(n, trainprop * n)

validate  <-  sample(setdiff(1:n, train), validateprop * n) 
test <- setdiff(setdiff(1:n, train), validate)

trainset <- scalehp[train,]
validateset <- scalehp[validate,]
testset <- scalehp[test,]
head(trainset,1)
head(validateset,1)
head(testset,1)

train.x <- trainset[-13]
train.y <- trainset$medv
validate.x <- validateset[-13]
validate.y <- validateset$medv
test.x <- testset[-13]
test.y <- testset$medv

numreps <- 10
validate.mse <- 0
train.mse <- 0
for(k in 1:numreps) {
  knn.pred <- knn.reg(train.x, validate.x,  train.y, k = -2*k+21)
  validate.mse[k] <- sum(((knn.pred$pred - validate.y)^2)) / as.integer(validateprop * n)
  print(validate.mse)
  
  
  knn.pred <- knn.reg(train.x, train.x,  train.y, k = -2*k+21)
  train.mse[k] <- sum(((train.y - knn.pred$pred)^2)) / as.integer(trainprop * n)   
  print(train.mse)
}

print(paste("Minimum validate set mean square error occurred at k =", (which.min(validate.mse)*-2)+21))
print(paste("Minimum validate set mean square error was ", validate.mse[which.min(validate.mse)]))
print(paste("Minimum train set mean square error occurred at k =", (which.min(train.mse)*-2)+21))
print(paste("Minimum train set mean square error was ", train.mse[which.min(train.mse)]))

plot(NULL, NULL, type='n', xlim=c(19,1), ylim=c(0,max(c(validate.mse, train.mse))), xlab='Increasing Flexibility (Decreasing k)', ylab='Error Rates', main='Mean Squared Errors as a Function of \n Flexibility for KNN Classification')
# plot X axis is in reverse to show increasing flexibility
lines(seq(1, 19,2), validate.mse[length(validate.mse):1], type='b', col=2, pch=16)
lines(seq(1,19,2), train.mse[length(train.mse):1], type='b', col=1, pch=16)
legend("topright", legend = c("Validation MSE", "Train MSE"), col=c(2, 1), cex=.75, pch=16)

knn.pred <- knn.reg(train.x, test.x, train.y, k = (which.min(validate.mse)*-2)+21)
test.mse <-  mean(((test.y - knn.pred$pred)^2))
print(test.mse)

#####################
#### QUESTION 2 ####
#####################

loandata <- read.table('LoanData.csv', header=TRUE, sep = ',')

pred = 'Yes'
yes_error <-mean(loandata$loan.repaid != pred)
print(yes_error)

scaleld <- loandata
scaleld[1:7] <- scale(loandata[1:7])
head(scaleld)

set.seed(5072)

n <- nrow(scaleld)
trainprop <- 0.75
validateprop <- 0.15

train  <-  sample(n, trainprop * n)
validate <-  sample(setdiff(1:n, train), validateprop * n) 
test <- setdiff(setdiff(1:n, train), validate)

trainset <- scaleld[train,]
validateset <- scaleld[validate,]
testset <- scaleld[test,]
head(trainset,1)
head(validateset,1)
head(testset,1)

train.x <- trainset[-8]
train.y <- trainset$loan.repaid
validate.x <- validateset[-8]
validate.y <- validateset$loan.repaid
test.x <- testset[-8]
test.y <- testset$loan.repaid

numreps <- 10
validate.errors <- rep(0, numreps)
train.errors <- rep(0, numreps)
for(k in 1:numreps) {
  knn.pred <- knn(train.x, validate.x,  train.y, k = 2*k-1)
  validate.errors[k] <- mean(validate.y != knn.pred)
  
  knn.pred <- knn(train.x, train.x,  train.y, k = 2*k-1)
  train.errors[k] <- mean(train.y != knn.pred)    
}

print(paste("Minimum validate set error rate occurred at k =", (which.min(validate.errors)*2)-1))
print(paste("Minimum validate error rate was ", validate.errors[which.min(validate.errors)]))

print(paste("Minimum train set error rate occurred at k =", (which.min(train.errors)*2)-1))
print(paste("Minimum train set error rate ", train.errors[which.min(train.errors)]))

plot(NULL, NULL, type='n', xlim=c(19,1), ylim=c(0,max(c(validate.errors, train.errors))), xlab='Increasing Flexibility (Decreasing k)', ylab='Error Rates', main='Error Rates as a Function of \n Flexibility for KNN Classification')
# plot X axis is in reverse to show increasing flexibility
lines(rev(seq(1,19,2)), validate.errors[length(validate.errors):1], type='b', col=2, pch=16)
lines(rev(seq(1,19,2)), train.errors[length(train.errors):1], type='b', col=1, pch=16)
legend("topleft", legend = c("Validation Error Rate", "Train Error Rate"), col=c(2, 1), cex=.75, pch=16)

knn.pred <- knn(train.x, test.x,  train.y, k = ((which.min(validate.errors)*2)-1))
test.errors <-  mean(test.y != knn.pred)
print(test.errors)

#####################         
#### QUESTION 3 ####
#####################

set.seed(5072)

homeprice <- read.table('HomePrices.txt', header=TRUE, sep = '\t')
Mean <- mean(homeprice$medv)
n <- nrow(homeprice)
MSE <- sum((Mean - homeprice$medv)^2) / n
print(MSE)
print(var(homeprice$medv)*((n-1)/n))
scalehp <- homeprice
scalehp[1:12] <- scale(homeprice[1:12])
head(scalehp)

numreps <- 50
validatelist <- rep(0, numreps)
testlist <- rep(0,numreps)
for(i in 1:numreps) {
  n <- nrow(scalehp)
  trainprop <- 0.75
  validateprop <- 0.15
  train  <-  sample(n, trainprop * n)
  validate  <-  sample(setdiff(1:n, train), validateprop * n) 
  test <- setdiff(setdiff(1:n, train), validate)
  trainset <- scalehp[train,]
  validateset <- scalehp[validate,]
  testset <- scalehp[test,]
  train.x <- trainset[-13]
  train.y <- trainset$medv
  validate.x <- validateset[-13]
  validate.y <- validateset$medv
  test.x <- testset[-13]
  test.y <- testset$medv
  
  reps <- 10
  validate.mse <- 0
  for(k in 1:reps) {
    knn.pred <- knn.reg(train.x, validate.x,  train.y, k = -2*k+21)
    validate.mse[k] <- mean(((knn.pred$pred - validate.y)^2))
  }
  validatelist[i] <- validate.mse[which.min(validate.mse)]
  knn.pred <- knn.reg(train.x, test.x,  train.y, k = (which.min(validate.mse)*-2)+21)
  test.mse <- mean(((test.y - knn.pred$pred)^2))
  testlist[i] <- test.mse
}

validatemean <- mean(validatelist)
validatesd <- sd(validatelist)

testmean <- mean(testlist)
testsd <- sd(testlist)

print(paste("Mean validate MSE is", validatemean))
print(paste("Standard Deviation validate MSE is", validatesd))
print(paste("Mean test MSE is", testmean))
print(paste("Standard Deviation test MSE is", testsd))

plot(NULL, NULL, type='n', xlim=c(1,numreps), ylim=c(0,max(c(validate.mse, test.mse)*1.8)), xlab='Replication', ylab='MSEs', main='Test and Best Validation MSEs \n for Many Partitionings of the Data')
# plot X axis is in reverse to show increasing flexibility
lines(x=c(numreps:1), validatelist[length(validatelist):1], type='b', col=2, pch=16)
abline(validatemean,0, col='red' , lty=2)
lines(x=c(numreps:1), testlist[length(testlist):1], type='b', col=1, pch=16)
abline(testmean,0, lty=2)
legend("topright", legend = c("Validation MSEs", "Validation MSE mean","Test MSEs","Test MSE mean"), col=c(2,2,1,1), lty=c(1,2,1,2), cex=.75, pch=c(16,NA,16,NA))

# Based on my results in Question 3, we were not unlucky in Question 1.
# Question 1's validate MSE was ~11, while Question 3's was ~18. We calculated the standard error of the Validate as 7, so Question 1 was within one Standard Deviation of when we took 50 different samples. 
# This leads me to believe that we were not unlucky with our sample in Q1, rather, our sample was similar enough to when we took 50 different samples.

#####################
#### QUESTION 4 ####
#####################

set.seed(5072)

applications <- read.table('applications.train.csv', header=T, sep = ',')
head(applications)

numreps <- 50
validatelist <- rep(0, numreps)
testlist <- rep(0,numreps)
for(i in 1:numreps) {
  n <- nrow(applications)
  trainprop <- 0.75
  validateprop <- 0.15
  train  <-  sample(n, trainprop * n)
  validate  <-  sample(setdiff(1:n, train), validateprop * n) 
  test <- setdiff(setdiff(1:n, train), validate)
  trainset <- applications[train,]
  validateset <- applications[validate,]
  testset <- applications[test,]
  train.x <- trainset[c(3,4,10,11)]
  train.y <- trainset$Applications
  validate.x <- validateset[c(3,4,10,11)]
  validate.y <- validateset$Applications
  test.x <- testset[c(3,4,10,11)]
  test.y <- testset$Applications
  
  reps <- 30
  validate.mse <- 0
  for(k in 1:reps) {
    knn.pred <- knn.reg(train.x, validate.x,  train.y, k = k)
    validate.mse[k] <- mean(((knn.pred$pred - validate.y)^2))
  }
  validatelist[i] <- validate.mse[which.min(validate.mse)]
  knn.pred <- knn.reg(train.x, test.x,  train.y, k = (which.min(validate.mse)))
  test.mse <- mean(((test.y - knn.pred$pred)^2))
  testlist[i] <- test.mse
  
}

print(paste("Minimum validate MSE occurred at k =", (which.min(validate.mse))))
print(paste("Minimum validate MSE was ", validate.mse[which.min(validate.mse)]))

print(paste("Minimum train MSE occurred at k =", (which.min(train.mse))))
print(paste("Minimum train MSE was", train.mse[which.min(train.mse)]))

knn.pred <- knn.reg(train.x, test.x, train.y, k = (which.min(validate.mse)))
test.mse <-  mean(((test.y - knn.pred$pred)^2))
print(test.mse)

