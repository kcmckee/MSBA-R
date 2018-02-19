rm(list=ls())

#########################################
#### Part 1: Support Vector Machines ####
#########################################

#a. create a train and test set of indices

library(ISLR)
set.seed(5082) 
n = dim(OJ)[1] 
train_inds <- sample(1:n,800) 
test_inds <-(1:n)[-train_inds]

#b. fit a support vector classifier to the training data using a cost of 0.01 to predict purchase with the other variables

library("e1071")
#OJ$STORE = as.factor(OJ$STORE)
oj_train <- OJ[train_inds,]
oj_test <- OJ[-train_inds,]
#oj_train_scale<-oj_train[,c(-1,-14)]
#oj_test_scale<-oj_test[,c(-1,-14)]
#oj_train_scale<-scale(oj_train_scale)
#oj_test_scale<-scale(oj_test_scale)
#oj_train_scaled_complete<-cbind(oj_train[,c(1,14)],oj_train_scale)
#oj_test_scaled_complete<-cbind(oj_test[,c(1,14)],oj_test_scale)
svm1<- svm(Purchase~.,data=oj_train,kernel='linear',cost=0.01) 
summary(svm1)
# Description: We can see the model chooses 446 of the 800 available observations as support points.
# Selecting this many points means the model has a wide margin, likely because the cost is relatively low 

#c. compute and display the training and test error rates 

svm1_train_rate<-predict(svm1,newdata=oj_train)
svm1_train_rate_table<-table(obs=oj_train$Purchase,pred=svm1_train_rate)
overall_train_rate<-(svm1_train_rate_table[1,1]+svm1_train_rate_table[2,2])/(sum(svm1_train_rate_table))
svm1_train_rate_table
cat("The overall training error rate:",1-overall_train_rate)

svm1_test_rate<-predict(svm1,newdata = oj_test)
svm1_test_rate_table<-table(obs=oj_test$Purchase,pred=svm1_test_rate)
overall_test_rate<-(svm1_test_rate_table[1,1]+svm1_test_rate_table[2,2])/(sum(svm1_test_rate_table))
svm1_test_rate_table
cat("The overall test error rate:",1-overall_test_rate)

#d.Use the tune() function to select an optimal cost. Consider values in the range 0.01 to 10. 

svm1_tune<-tune(svm,Purchase~.,data=oj_train, ranges=list(cost=c(.01,.02,.05,.1,.2,.5,1,2,5,10)),kernel="linear") 
summary(svm1_tune)

#e. Compute and display the training and test error rates using this new value for cost. 
#train
svm1_best_train_pred<-predict(svm1_tune$best.model,newdata=oj_train) 
svm1_best_train_table<-table(obs=oj_train$Purchase,pred=svm1_best_train_pred)
svm1_overall_best_train<-(svm1_best_train_table[1,1]+svm1_best_train_table[2,2])/(sum(svm1_best_train_table))
svm1_best_train_table
cat("The overall best training error rate:",1-svm1_overall_best_train)

#test
svm1_best_test_pred<-predict(svm1_tune$best.model,newdata=oj_test) 
svm1_best_test_table<-table(obs=oj_test$Purchase,pred=svm1_best_test_pred)
svm1_overall_best_test<-(svm1_best_test_table[1,1]+svm1_best_test_table[2,2])/(sum(svm1_best_test_table))
svm1_best_test_table
cat("The overall best training error rate:",1-svm1_overall_best_test)

#f.Repeat parts (b) through (e) using a support vector machine with a radial kernel. Use the default value for gamma. 

svm2_tune = tune(svm , Purchase~., data=oj_train , ranges=list( cost=c(.01,.02,.05,.1,.2,.5,1,2,5,10), kernel='radial'))
summary(svm2_tune)

#description of results
#it appears the radial kernel performed worse than the linear kernel when the cost was low, and similarly well with higher costs. 
#In this case, we would likely choose the linear kernel since it is simpler to understand and less complex computationally. 

#g. Repeat parts (b) through (e) using a support vector machine with a polynomial kernel. Set degree =2. 

svm3_tune = tune(svm , Purchase~., data=oj_train , ranges=list( cost=c(.01,.02,.05,.1,.2,.5,1,2,5,10), kernel='polynomial',degree=2))
summary(svm3_tune)

#it appears the radial kernel performs slightly worse than both the linear and radial kernel at most ranges

#h. Overall, which approach seemed to give the best results on this data?

#The linear and radial kernels performed best, with a slight nod going to the linear kernel. Choosing the linear kernel
#probably makes the most sense, as it is simpler than the radial.



#########################################################
#### Part 2: Polynomial and Step Function Regression ####
#########################################################

#a. work through the chapter 7 lab in ISL

#b. Set the seed to 5082. Using the Wage dataset from the ISLR package, perform polynomial regression to predict wage using 
#age with polynomial degrees from 1 to 10, and use 10-fold cross-validation to select the optimal degree d from these 10 choices  
#(recall cv.glm() function (bottom of page 192). 

set.seed(5082)
library(ISLR)
library(boot)

cv_error<-rep(0,10)
for (i in 1:10){
  glm_poly_fit<-glm(wage~poly(age,i),data=Wage) 
  cv_error[i]<-cv.glm(Wage,glm_poly_fit, K=10)$delta[1]
}

#c. Plot the errors from the cross validation. What degree was chosen? 
plot(2:10, cv_error[-1], xlab = "Polynomial Degrees", ylab = "Test MSE", type = "l")
points(which.min(cv_error), cv_error[which.min(cv_error)], col = "red", cex = 2, pch = 16) # the 9th degree polynomial is the lowest 

#d. Plot the original data and the polynomial fit using the optimal value of d. 
#To plot the fit, break the range of age into 100 partitions, use the model to predict these points and plot the points 
#against the predictions

agelims<-range(Wage$age)
age_grid<-seq(from=agelims[1], to=agelims[2], length.out = 100)
pred_poly<-predict(glm_poly_fit,newdata=list(age=age_grid))
par(mfrow=c(1,1),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))

plot(Wage$age ,Wage$wage ,xlim=agelims ,cex=.5,col="black ") 
title("Degree 9 Polynomial",outer=T) 
lines(age_grid,pred_poly,lwd=2,col="red")

#e. fit a step function to predict wage using age (recall the cut function)
# To do this, set the seed to 5082, then investigate step functions using steps from 1 to 12.
#Use 10-fold cross validation to choose the optimal number of steps. 

set.seed(5082)
cv_error <- rep(NA, 10)
for (i in 2:10) {
  Wage$age.cut <- cut(Wage$age, i)
  fit <- glm(wage ~ age.cut, data = Wage)
  cv_error[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
}

#f. plot the cv errors and report the optimal amount 
plot(2:10, cv_error[-1], xlab = "Number of Intervals i.e. cuts+1", ylab = "Test MSE", type = "l")
min_error <- which.min(cv_error)
points(which.min(cv_error), cv_error[which.min(cv_error)], col = "red", cex = 2, pch = 20) 
#7 cuts and 8 intervals are optimal 


#g. Create a model using this optimal number of cuts and plot this model's fitted values as a function of the Wage$age data.

plot(wage ~ age, data = Wage, col = "black", main="Step Function with Number of Cuts Determined by CV (7)")
agelims <- range(Wage$age)
age.grid <- seq(from = agelims[1], to = agelims[2])
fit <- glm(wage ~ cut(age, 8), data = Wage)
preds <- predict(fit, data.frame(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)


#########################################################
#### Part 3:    Regression and Smoothing Splines     ####
#########################################################

#a. Finish working through the chapter 7 lab in ISLR and create test values for the rest of this part

rm(list=ls())
library(splines, MASS)

dis_range <- range(Boston$dis)
dis_grid <- seq(from=dis_range[1], to=dis_range[2], length.out=100)

#b. Plot the polynomial fits for polynomial degrees from 1 to 10, and report the associated residual sums of squares
#for the training error in a table (include a legend).

set.seed(5082)
dis_range<- range(Boston$dis)
dis_grid<- seq(from= dis_range[1], to=dis_range[2],length.out= 100) 

data<- cbind(dis=Boston$dis, nox=Boston$nox)
plot(data, main="Polynomial fit with Various Degrees of Freedom")

fit_poly<-c()
for(i in 1:10){
  fit_poly[[i]]<-lm(nox~poly(dis, i), data=Boston)
}

pred<-c()
for (i in 1:10){
  pred[[i]]<-predict(fit_poly[[i]], newdata = list(dis= dis_grid), se=T)
}

colors<-c("black", "red","blue","darkblue","purple","pink","orange","skyblue","darkgrey","green")
for (i in 1:10){
  lines(dis_grid, pred[[i]]$fit, col=colors[i])
  }

legend('topright', legend=c("Degree 1", "Degree 2","Degree 3","Degree 4","Degree 5","Degree 6","Degree 7","Degree 8","Degree 9","Degree 10"),
       col=c("black", "red","blue","darkblue","purple","pink","orange","skyblue","darkgrey","green"), lty=1, cex=1)

#display the rss table
rss_table<-anova(fit_poly[[1]],fit_poly[[2]],fit_poly[[3]],fit_poly[[4]],fit_poly[[5]],fit_poly[[6]],fit_poly[[7]],fit_poly[[8]],fit_poly[[9]],
                 fit_poly[[10]])
rss_table

#C. Set the seed to 5082, then perform cross-validation (recall cv.glm) to select the optimal degree (from 1 to 10) for the polynomial, 
#determine the degree with the minimum c.v. error and plot the original data points and the fit resulting from the optimal degree. 

#computing the cross validation error

set.seed(5082)
cv_error<-rep(0,10)
for (i in 1:10) {
  poly_fit <- glm(nox ~ poly(dis,i), data = Boston)
  cv_error[i] <- cv.glm(Boston, poly_fit, K = 10)$delta[1]
  cv_error
}

#finding the lowest cv_error
#plot(1:10, cv_error, xlab = "Degree", ylab = "Test MSE", type = "l")
#points(which.min(cv_error), cv_error[which.min(cv_error)], col = "red", cex = 2, pch = 16)
which.min(cv_error)

#plotting the polynomial with the lowest cv_error 
plot(data, main="Polynomial fit Min CV Error Degree (3 degrees)")
lines(dis_grid,pred[[3]]$fit,col="red")

#d. Use the bs() function to fit a regression spline to predict nox using dis--request 4 degrees of freedom. Report the output of the fit 
#using summary(). Answer the following: How were the knots chosen? Where was the knot placed? Plot the resulting fit.

reg_spline_fit <- lm(Boston$nox ~ bs(dis, df = 4), data = Boston)
reg_spline_pred<- predict(reg_spline_fit, newdata=list(dis=dis_grid),se=T)
summary(reg_spline_fit)
# the knots were chosen by cross validation

attr(bs(Boston$dis,df=4),"knots")
#the knots are placed at a quantile, in this case 50%

#plot the resulting fit 
plot(data, main="Regression Spline with df=4")
lines(dis_grid,reg_spline_pred$fit,col="red")

#e.Now fit a regression spline for a range of degrees of freedom from 3 to 10, 
#plot the resulting fits and report the associated residual sum of squares in a table.

set.seed(5082)

reg_spline_fit<-c()
for(i in 3:10){
  reg_spline_fit[[i]]<-lm(nox~bs(dis, df=i), data=Boston)
}

reg_spline_pred<-c()
for (i in 3:10){
  reg_spline_pred[[i]]<-predict(reg_spline_fit[[i]], newdata = list(dis= dis_grid), se=T)
}

plot(data, main="Regression Spline with Various Degrees of Freedom")
colors<-c("black", "red","blue","green","purple","pink","yellow")
for (i in 1:10){
  lines(dis_grid, reg_spline_pred[[i]]$fit, col=colors[i])
}

#f. Set the seed to 5082, then perform 10-fold cross-validation in order to select the best degrees of 
#freedom (from 3 to 10) for a regression spline on this data. Plot your results, including your best degrees of freedom in the chart title

set.seed(5082)
cv_error<-rep(NA,8)
for (i in 3:10) {
  reg_spline_fit <- glm(nox ~ bs(dis,df=i), data = Boston)
  cv_error[i] <- cv.glm(Boston, reg_spline_fit, K = 10)$delta[1]
  cv_error
}

#finding the lowest cv_error
which.min(cv_error)

#plotting the polynomial with the lowest cv_error 
reg_spline_fit <- lm(Boston$nox ~ bs(dis, df = 10), data = Boston)
reg_spline_pred<- predict(reg_spline_fit, newdata=list(dis=dis_grid),se=T)
plot(data, main="Regression Spline with Best D.F. (10) chosen with C.V.")
lines(dis_grid,reg_spline_pred$fit,col="red",lwd=3)

#g. Set the seed to 5082, then perform 10-fold cross-validation in order to select the best lambda ??? for a smoothing spline on this data.
#You are still interested in nox as a function of dis. Plot your results, including your best ??? in the chart title.

set.seed(5082)
smooth_spline_fit<-smooth.spline(jitter(Boston$dis), Boston$nox, cv=TRUE)
smooth_spline_pred<-predict(smooth_spline_fit, newdata= list(dis=dis_grid))
best_lambda<-smooth_spline_fit$lambda 
print(paste("The best lambda is", best_lambda))

plot(data, main = "Smoothing spline with best \nlambda (6.9e-05) chosen with c.v.")
lines(smooth_spline_fit, col= "red", lwd=2)

