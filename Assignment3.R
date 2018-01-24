rm(list=ls())
require(ISLR)
require(class)
require(MASS)
require(boot)

#####################
#### QUESTION 1  ####
#####################
#a) Set the random seed to 5072.
set.seed(5072)

#b) Use the full data set to perform a logistic regression with Direction
#as the response and the five lag variables plus Volume as predictors.
#Use the summary function to print the results.
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family=binomial)
summary(glm.fit)

#On a single comment line, identify predictors that are statistically significant, if any.
### The only statistically significant predictor is the postively predicted Lag2 with a p value of 0.0296.

#c) Create and display a confusion matrix, assume that Down is the null hypothesis.
glm.probs<-predict(glm.fit,type="response")
contrasts(Weekly$Direction)
glm.pred<-rep("Down", nrow(Weekly))
glm.pred[glm.probs>.5]<-"Up"
(mytable<-table(Weekly$Direction,glm.pred))

#d) From the confusion matrix, compute the following performance statistics:
# The overall fraction of correct predictions.
(glm.success<-(mytable["Up","Up"]+mytable["Down","Down"])/sum(mytable))
#The overall error rate
(mytable["Up","Down"]+mytable["Down","Up"])/sum(mytable)
#Type 1 and Type 2 error rates
#Type 1 Error Rate aka the:
mytable["Down","Up"]/sum(mytable["Down",])
#Type 2: Error rate
mytable["Up","Down"]/sum(mytable["Up",])
#The Power of the model
mytable["Up","Up"]/sum(mytable["Up",])
#The Precision of the model
mytable["Up","Up"]/sum(mytable[,"Up"])

#e) Fit a logistic regression model using a training data period from 1990 to 2008, with Lag2 as the only predictor.
train<-Weekly$Year<2009
Weekly.test<-Weekly[!train,]        
Direction.test<-Weekly$Direction[!train]  
glm.fit2<-glm(Direction~Lag2,data=Weekly,family=binomial,subset=train)
summary(glm.fit2)

#f) For the held out data (that is, the data from 2009 and 2010), use the model to create & display a confusion matrix
glm.probs2<-predict(glm.fit2, Weekly.test, type="response")
glm.pred2<-rep("Down",nrow(Weekly.test))  
glm.pred2[glm.probs2>.5]<-"Up"
(mytable<-table(Weekly.test$Direction,glm.pred2)) 
#use the matrix to compute the same five performance statistics for this new prediction set.
# The overall fraction of correct predictions.
(glm2.success<-(mytable["Up","Up"]+mytable["Down","Down"])/sum(mytable))
#The overall error rate
(mytable["Up","Down"]+mytable["Down","Up"])/sum(mytable)
#Type 1 and Type 2 error rates
#Type 1:
mytable["Down","Up"]/sum(mytable["Down",])
#Type 2:
mytable["Up","Down"]/sum(mytable["Up",])
#The Power of the model
mytable["Up","Up"]/sum(mytable["Up",])
#The Precision of the model
mytable["Up","Up"]/sum(mytable[,"Up"])

#g) Repeat e) and f) using LDA.
library(MASS)
lda.fit<-lda(Direction~Lag2,data=Weekly,subset=train)
lda.fit
lda.pred<-predict(lda.fit, Weekly.test)
(mytable<-table(Weekly.test$Direction,lda.pred$class))
# The overall fraction of correct predictions.
(lda.success<-(mytable["Up","Up"]+mytable["Down","Down"])/sum(mytable))
#The overall error rate
(mytable["Up","Down"]+mytable["Down","Up"])/sum(mytable)
#Type 1 and Type 2 error rates
#Type 1:
mytable["Down","Up"]/sum(mytable["Down",])
#Type 2:
mytable["Up","Down"]/sum(mytable["Up",])
#The Power of the model
mytable["Up","Up"]/sum(mytable["Up",])
#The Precision of the model
mytable["Up","Up"]/sum(mytable[,"Up"])

#h) Repeat e) and f) using QDA.
qda.fit<-qda(Direction~Lag2,data=Weekly,subset=train)
qda.fit
qda.pred<-predict(qda.fit,Weekly.test)
(mytable<-table(Weekly.test$Direction,qda.pred$class))
# The overall fraction of correct predictions.
qda.success<-(mytable["Up","Up"]+mytable["Down","Down"])/sum(mytable)
qda.success
#The overall error rate
(mytable["Up","Down"]+mytable["Down","Up"])/sum(mytable)
#Type 1 and Type 2 error rates
#Type 1:
mytable["Down","Up"]/sum(mytable["Down",])
#Type 2:
mytable["Up","Down"]/sum(mytable["Up",])
#The Power of the model
mytable["Up","Up"]/sum(mytable["Up",])
#The Precision of the model
mytable["Up","Up"]/sum(mytable[,"Up"])

#i) Repeat e) and f) using KNN with k=1 (scaling unnecessary, only 1 predictor)
train.X<-matrix(c(Weekly$Lag2)[train],length((Weekly$Lag2)[train]),1) 
test.X<-matrix(c(Weekly$Lag2)[!train],length((Weekly$Lag2)[!train]),1) 
train.Direction<-Weekly$Direction[train]
#Fit a knn model with k=1
knn.pred<-knn(train.X,test.X,train.Direction,k=1)
(mytable<-table(Weekly.test$Direction,knn.pred))
# The overall fraction of correct predictions.
knn1.success<-(mytable["Up","Up"]+mytable["Down","Down"])/sum(mytable)
knn1.success
#The overall error rate
(mytable["Up","Down"]+mytable["Down","Up"])/sum(mytable)
#Type 1 and Type 2 error rates
#Type 1:
mytable["Down","Up"]/sum(mytable["Down",])
#Type 2:
mytable["Up","Down"]/sum(mytable["Up",])
#The Power of the model
mytable["Up","Up"]/sum(mytable["Up",])
#The Precision of the model
mytable["Up","Up"]/sum(mytable[,"Up"])

#j) Repeat e) and f) using KNN with k = 5.
knn.pred2<-knn(train.X,test.X,train.Direction,k=5)
(mytable<-table(Weekly.test$Direction,knn.pred2))
# The overall fraction of correct predictions.
knn5.success<-(mytable["Up","Up"]+mytable["Down","Down"])/sum(mytable)
knn5.success
#The overall error rate
(mytable["Up","Down"]+mytable["Down","Up"])/sum(mytable)
#Type 1 and Type 2 error rates
#Type 1:
mytable["Down","Up"]/sum(mytable["Down",])
#Type 2:
mytable["Up","Down"]/sum(mytable["Up",])
#The Power of the model
mytable["Up","Up"]/sum(mytable["Up",])
#The Precision of the model
mytable["Up","Up"]/sum(mytable[,"Up"])

#k) Which of these methods appears to provide the best results on this data?
rates <- rbind(glm2.success, lda.success, qda.success, knn1.success, knn5.success)
rates[order(-rbind(glm2.success, lda.success, qda.success, knn1.success, knn5.success)[,1]),]
## Judging by success rate, there is a tie for best method between logistic regression and LDA,
##both with success rates of 0.625

#####################
#### QUESTION 2  ####
#####################
rm(list=ls())
#a) Set the random seed to 5072.
set.seed(5072)

#b. Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its median, and a 0 if mpg contains a value below its median.
# You can compute the median using the median() function.
mpg01 <- as.numeric(Auto$mpg > median(Auto$mpg))
 #Note you may find it helpful to use the data.frame() function to create a single data set containing both mpg01 and the other Auto variables.
Auto01 <- data.frame(Auto, mpg01)

#c. Split the data into a training set and a test set using the sample() function.
#The training set should be approximately 80% of the total number of rows.
trainindices<-sample(1:nrow(Auto01),.8*nrow(Auto01))
Auto.train<-Auto01[trainindices,]
Auto.test<-Auto01[-trainindices,]

#d. Perform logistic regression on the training data in order to predict mpg01
#using the variables cylinders, displacement and weight
glm.fit<-glm(mpg01~cylinders+displacement+weight,data=Auto01,family=binomial,subset=trainindices)
summary(glm.fit)

#e. For the test set, use the model just created to construct and display a confusion matrix
#in the format outlined in Question 1 (assuming below-median mpg to be the null hypothesis),
#and from this table, compute the same five performance statistics for this new set of predictions as was requested in Question 1.
glm.probs<-predict(glm.fit, Auto.test, type="response")
glm.pred<-rep("Below",nrow(Auto.test))  
glm.pred[glm.probs>.5]<-"Above"
predicted <- glm.pred
predicted<-factor(predicted,ordered=T,levels=c('Below','Above'))
actuals<-factor(Auto.test$mpg01,ordered=T,levels=c(0,1))
levels(actuals)[levels(actuals)==0] <- "Below"
levels(actuals)[levels(actuals)==1] <- "Above"
(mytable<-table(actuals, predicted) )
# The overall fraction of correct predictions.
glm.success<-(mytable["Above","Above"]+mytable["Below","Below"])/sum(mytable)
glm.success
#The overall error rate
(mytable["Above","Below"]+mytable["Below","Above"])/sum(mytable)
#Type 1 and Type 2 error rates
#Type 1:
mytable["Below","Above"]/sum(mytable["Below",])
#Type 2:
mytable["Above","Below"]/sum(mytable["Above",])
#The Power of the model
mytable["Above","Above"]/sum(mytable["Above",])
#The Precision of the model
mytable["Above","Above"]/sum(mytable[,"Above"])

#f. Repeat d) and e) using LDA.
(lda.fit<-lda(mpg01~cylinders+displacement+weight,data=Auto01,subset=trainindices))
lda.pred<-predict(lda.fit, Auto.test)
predicted <- lda.pred$class
predicted<-factor(predicted,ordered=T,levels=c('0','1'))
levels(predicted)[levels(predicted)==0] <- "Below"
levels(predicted)[levels(predicted)==1] <- "Above"
(mytable<-table(actuals, predicted) )
# The overall fraction of correct predictions.
(lda.success<-(mytable["Above","Above"]+mytable["Below","Below"])/sum(mytable))
#The overall error rate
(mytable["Above","Below"]+mytable["Below","Above"])/sum(mytable)
#Type 1 and Type 2 error rates
#Type 1:
mytable["Below","Above"]/sum(mytable["Below",])
#Type 2:
mytable["Above","Below"]/sum(mytable["Above",])
#The Power of the model
mytable["Above","Above"]/sum(mytable["Above",])
#The Precision of the model
mytable["Above","Above"]/sum(mytable[,"Above"])
#g. Repeat d) and e) using QDA.
qda.fit<-qda(mpg01~cylinders+displacement+weight,data=Auto01,subset=trainindices)
qda.fit
qda.pred<-predict(qda.fit, Auto.test)
predicted <- qda.pred$class
predicted<-factor(predicted,ordered=T,levels=c('0','1'))
levels(predicted)[levels(predicted)==0] <- "Below"
levels(predicted)[levels(predicted)==1] <- "Above"
(mytable<-table(actuals, predicted) )
# The overall fraction of correct predictions.
(qda.success<-(mytable["Above","Above"]+mytable["Below","Below"])/sum(mytable))
#The overall error rate
(mytable["Above","Below"]+mytable["Below","Above"])/sum(mytable)
#Type 1 and Type 2 error rates
#Type 1:
mytable["Below","Above"]/sum(mytable["Below",])
#Type 2:
mytable["Above","Below"]/sum(mytable["Above",])
#The Power of the model
mytable["Above","Above"]/sum(mytable["Above",])
#The Precision of the model
mytable["Above","Above"]/sum(mytable[,"Above"])

#h. Repeat d) and e) using KNN with k = 1.
scaledX<-apply(Auto[c(2,3,5)],2,scale)
train.X<-scaledX[trainindices,]
test.X<-scaledX[-trainindices,]
train.Y<-mpg01[trainindices]
#Fit a knn model with k=1
knn.pred<-knn(train.X,test.X,train.Y,k=1)
predicted<-factor(knn.pred,ordered=T,levels=c('0','1'))
levels(predicted)[levels(predicted)==0] <- "Below"
levels(predicted)[levels(predicted)==1] <- "Above"
(mytable<-table(actuals, predicted) )
# The overall fraction of correct predictions.
(knn1.success<-(mytable["Above","Above"]+mytable["Below","Below"])/sum(mytable))
#The overall error rate
(knn.error.rate<-(mytable["Above","Below"]+mytable["Below","Above"])/sum(mytable))
#Type 1 and Type 2 error rates
#Type 1:
(knn.type1<-mytable["Below","Above"]/sum(mytable["Below",]))
#Type 2:
(knn.type2<-mytable["Above","Below"]/sum(mytable["Above",]))
#The Power of the model
(knn.power<-mytable["Above","Above"]/sum(mytable["Above",]))
#The Precision of the model
(knn.precision<-mytable["Above","Above"]/sum(mytable[,"Above"]))
(myKNNstats<-c(1, knn1.success, knn.error.rate, knn.type1, knn.type2, knn.power, knn.precision))

#i. Repeat d) and e) using KNN, with various values of k. Choose the model that performs best.
knn.stats = data.frame(k=numeric(20),successrate=numeric(20),errorrate=numeric(20),type1=numeric(20),type2=numeric(20),power=numeric(20),precision=numeric(20))
knn.stats[1,]<-myKNNstats
for (k in 2:20)
{
  knn.pred<-knn(train.X,test.X,train.Y,k=k)
  knn.pred
  predicted<-factor(knn.pred,ordered=T,levels=c('0','1'))
  levels(predicted)[levels(predicted)==0] <- "Below"
  levels(predicted)[levels(predicted)==1] <- "Above"
  mytable<-table(actuals, predicted) 
  knn.success<-(mytable["Above","Above"]+mytable["Below","Below"])/sum(mytable)
  knn.error.rate<-(mytable["Above","Below"]+mytable["Below","Above"])/sum(mytable)
  knn.type1<-mytable["Below","Above"]/sum(mytable["Below",])
  knn.type2<-mytable["Above","Below"]/sum(mytable["Above",])
  knn.power<-mytable["Above","Above"]/sum(mytable["Above",])
  knn.precision<-mytable["Above","Above"]/sum(mytable[,"Above"])
  knn.stats[k,]<-c(k, knn.success, knn.error.rate, knn.type1, knn.type2, knn.power, knn.precision)
}
## The success rates for k's 1 through 12 (in decreasing order of success):
(knn.stats<- knn.stats[order(-knn.stats[,2]),])
(best.knn.k<-knn.stats[1,1])
(best.knn.success <- knn.stats[1,2])

#####################
#### QUESTION 3  ####
#####################
rm(list=ls())
set.seed (5072)
#a) Using the Boston data set in the MASS package, create training and test sets in the ratio of 80/20
#then fit classification models in order to predict whether a given suburb has a crime rate above or below the median.
#Explore logistic regression, LDA, and KNN models using nox, rad and dis as predictors.
#Evaluate these models using confusion matrices as above and describe your findings.
#Create binary variable for crime rate

crim01 <- as.numeric(Boston$crim > median(Boston$crim))
Boston01 <- data.frame(Boston, crim01)
#reate training and test sets
trainindices<-sample(1:nrow(Boston01),.8*nrow(Boston01))
Boston.train<-Boston01[trainindices,]
Boston.test<-Boston01[-trainindices,]
actuals<-Boston.test$crim01
## Logistic Regression
glm.fit<-glm(crim01~nox+rad+dis,data=Boston01,family=binomial,subset=trainindices)
glm.probs<-predict(glm.fit, Boston.test, type="response")
glm.pred<-rep(0,nrow(Boston.test))  
glm.pred[glm.probs>.5]<-1
predicted <- glm.pred
(glm.table<-table(actuals, predicted)) 
## LDA
lda.fit<-lda(crim01~nox+rad+dis,data=Boston01,subset=trainindices)
lda.fit
lda.pred<-predict(lda.fit, Boston.test)
predicted <- lda.pred$class
(lda.table<-table(actuals, predicted) )
## KNN
scaledX<-apply(Boston[c("nox","rad","dis")],2,scale)
train.X<-scaledX[trainindices,]
test.X<-scaledX[-trainindices,]
train.Y<-crim01[trainindices]

knum <- seq(1,99)
successrate=numeric()
for (i in knum) {
  knn.pred<-knn(train.X,test.X,train.Y,k=i)
  mytable<-table(actuals,knn.pred)
  #print(mytable)
  successrate<-c(successrate,(mytable["0","0"]+mytable["1","1"])/sum(mytable))
}
max<-which.max(successrate)
max
# KNN model performs best when k=1


#Fit a knn model with k=1
knn.pred<-knn(train.X,test.X,train.Y,k=1)
(knn1.table<-table(actuals, knn.pred) )

## Evaluation 
table_list <- list(glm.table, lda.table, knn1.table)
stats = matrix(0,3,6)
col_names<-c("Success Rate", "Error Rate", "Type1", "Type2", "Power", "Precision")
row_names<-c("glm.fit", "lda.fit", "knn1.fit")
colnames(stats) = col_names
rownames(stats) = row_names
n = 1
for (mytable in table_list)
{
  success<-(mytable[1,1]+mytable[2,2])/sum(mytable)  # The overall fraction of correct predictions
  error_rate<-(mytable[1,2]+mytable[2,1])/sum(mytable)  # The overall error rate
  type1<-(mytable[2,1]/sum(mytable[2,]))  #Type I Error Rate
  type2<-(mytable[1,2]/sum(mytable[1,]))  #Type II Error Rate
  power<-1-type2  #Power
  precision<-(mytable[2,2]/sum(mytable[,2]))  #Precision
  stats[n,]<-c(success, error_rate, type1, type2, power, precision)
  n<-n+1
} 
## Based on success rate, the best method is KNN with a k of 1, with a success rate of ~.97.
## All of the stats, in descending order by success rate:
stats[order(-stats[,1]),]

#####################
#### QUESTION 4  ####
#####################
rm(list=ls())
#In this question, we will perform cross-validation on a simulated data set. Do the following:
#a) Generate a simulated data set as follows:
set.seed(5072)
x=rnorm(100)
y = x - 2 * x^2 + rnorm(100)

#b) Create a data frame containing these x and y variables in named columns X and Y
frame <- data.frame(X = x, Y = y)

#c) Create a scatterplot of X against Y.
plot(frame$X,frame$Y)

#d) Set the random seed to 123, then compute the LOOCV errors (using the cv.glm() function) that result from fitting
#the following four models using least squares:
set.seed(123)
cv.error<-c()
for (i in 1:4){
  glm.fit<-glm(Y~poly(X,i),data=frame)
  cv.error[i]<-cv.glm(frame,glm.fit)$delta[1]
}
cv.error
plot(cv.error,type="b",xlab='degree',ylab='LOOCV MSE',main ='Leave-One-Out Cross Validation', xaxp  = c(0, 4, 4))

#e) Repeat d) using random seed 456 and report your LOOCV errors as above. Are your results the same as what you got in d)? Why?
set.seed(456)
cv.error<-c()
for (i in 1:4){
  glm.fit<-glm(Y~poly(X,i),data=frame)
  cv.error[i]<-cv.glm(frame,glm.fit)$delta[1]
}
cv.error
plot(cv.error,type="b",xlab='degree',ylab='LOOCV MSE',main ='Leave-One-Out Cross Validation', ylim = c(0,6), xaxp  = c(0, 4, 4))
## The results are exactly the same. This is because the LOOCV approach isn't random.
## vs, the validation set approach, which that uses different random splits.
## The LOOCV approach carries out every possible split of 1, n-1, so there's no need for any randomization.

#f) Which of the models in (c) had the smallest LOOCV error? Is this what you expected? Explain your answer.

## The quadratic model had the smallest LOOCV error. 
## These results make sense because the scatterplot of X vs. Y appears parabolic

#g) Comment on the statistical significance of the coefficient estimates (citing p-values)
#that results from fitting each of the models in d) using least squares. 
#Do these results agree with the conclusions drawn based on the cross-validation results?

MSE.list<-c()

for (i in 1:4){
  glm.fit<-glm(Y~poly(X,i),data=frame)
  MSE.list[i]<-mean(glm.fit$residual^2)
  print(coef(summary(glm.fit)))
  print("-----------------------------------------------------------")
}

## We see in all four models above that only the first two coefficients (x and x^2) are significant based on their p-values.
## The p values for the first two coefficients consistently hover in the range of e-06 and e-33
## In contrast, when x^3 and x^4 appear in the third and fourth models, their p values are in the e-01 range.
## These results are in line with cross-validation results suggesting a quadratic model would best fit the data.

## Additionally, we can see in the following graph that LOOCV predictions for MSE's at degrees 1 through 4 were very close to the actual MSE's.

plot(MSE.list,type="b",xlab='degree',ylab='MSE',main ='Least Squares Models and LOOCV', ylim = c(0,6), xaxp  = c(0, 4, 4))
points(cv.error,col ="blue")
lines(cv.error,col ="blue")
legend(x = "topright", legend=c("Least Squares Models", "LOOCV"), col=c("black", "blue"), lwd=1, lty=1)