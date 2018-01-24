rm(list=ls()) 
require(glmnet)

#Investigating in 2 dimensions

#Generating a set of data with low correlation
set.seed(123)
n <- 40
average <- 0
std <- 5
y <- rnorm(n, average, std)
x <- rnorm(n, average, std)

#Low dimensional setting, n = 40, p = 2
lm.ld <- lm(y ~ x)
summary(lm.ld)
plot(x, y, pch = 16)
abline(lm.ld)

#n = 2, p = 2, the boundary where lease squares fails
lm.bd <- lm(y[1:2] ~ x[1:2])
summary(lm.bd)
plot(x[1:2], y[1:2], pch = 16)
abline(lm.bd)

#Investigating in dimensions higher than 2

#Creating a table of 40 observations and 20 features (V2 ~ V20 + y-intercept)
my_table <- data.frame(matrix(rnorm(800, 0, 100), ncol = 20))

#Set partitioning for validation purpose
train.set <- my_table[c(1:20),]
test.set <- my_table[c(21:40),]
train_mses <- c()
test_mses <- c()
rsqs <- c()

#In each iteration, there is one additional feature being added into the linear model.
for (i in 20:2) {
  lm.temp <- lm(X1 ~ ., data = train.set[,c(1, i:20)])
  rsqs[21-i] <- summary(lm.temp)$r.squared   #R-suqared
  train_mses[21-i] <- mean(lm.temp$residuals^2)                #Training MSE
  test_mses[21-i] <- mean((predict(lm.temp, test.set[,c(1, i:20)]) - test.set$X1)^2)    #Test MSE
}
summary(lm.temp)
plot(rsqs, pch = 16, type = 'b', xlab = 'Number of p', ylab = 'R-Squared')
plot(train_mses, pch = 16, type = 'b', xlab = 'Number of p', ylab = 'Training MSE')
plot(test_mses, pch = 16, type = 'b', xlab = 'Number of p', ylab = 'Log(Test MSE)')

#Regression when p > n

table_hd_2 <- data.frame(matrix(rnorm(10000, 0, 1), ncol = 1000))
lm.hd <- lm(X1 ~ ., data = table_hd_2)
summary(lm.hd)




