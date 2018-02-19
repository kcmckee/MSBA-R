rm(list=ls())

##################### 
####  QUESTION 1 ####
#####################

set.seed(5072)
x<-c(rnorm(100,0,1))
eps<-c(rnorm(100,0,0.5))
y<- -1+0.5*x+eps
length(y)
#the values of B0 and B1 are -1 and 0.5 respectively 
plot(x,y,pch=20)
#x&y have a strong positive correlation (as x increases, y also increases), the degree of linearity is high, and the variability is relatively minor
reg.lm_1<-lm(y~x)
reg.summary<-summary(reg.lm_1)
beta0_hat<-coef(reg.summary)[1,1]
beta1_hat<-coef(reg.summary)[2,1]
cat("beta0_hat for the linear regression is", beta0_hat)
cat("beta1_hat for the linear regression is", beta1_hat)
abline(reg.lm_1)
y_true<- -1+0.5*x
abline(lm(y_true~x),col='red')
legend('topleft', legend=c("Population Regression","Least Squares Regression"),lty=c(1,1),lwd=c(2.5,2.5),col=c('black','red'))
x_squared<-x^2
reg.poly<-lm(y~x+x_squared)
anova(reg.lm_1,reg.poly)
print("The anova table shows the polynomial has a smaller sum of squares, which indicates that it explains more of the variation in the data.")

x<-rnorm(100,0,1)
eps<- rnorm(100,0,0.1^(1/2))
y<- -1+0.5*x+eps #beta0 = -1, beta1= 0.5
length(y)
plot(x,y,pch=20)
#x&y have a very strong positive relationshiop, as x increases, so does y. the degree of linearity is high and variability is minor. 
reg.lm_2<-lm(y~x)
reg.summary<-summary(reg.lm_2)
beta0_hat<-coef(reg.summary)[1,1]
beta1_hat<-coef(reg.summary)[2,1]
cat("beta0_hat for the linear regression is", beta0_hat)
cat("beta1_hat for the linear regression is", beta1_hat)
abline(reg.lm_2)
y_true<- -1+0.5*x
abline(lm(y_true~x),col='red')
legend('topleft', legend=c("Population Regression","Least Squares Regression"),lty=c(1,1),lwd=c(2.5,2.5),col=c('black','red'))
print("Examining the plot shows reducing the variability of epsilon improves the fit of the least squares regression line")

x<-rnorm(100,0,1)
eps<-rnorm(100,0,0.5^(1/2))
y<- -1+0.5*x+eps #beta0=-1, beta1=0.5
length(y)
plot(x,y,pch=20)
#X&y have a moderately strong positive relationship, the degree of linearity is moderately high, and the variability is moderate
reg.lm_3<-lm(y~x)
reg.summary<-summary(reg.lm_3)
beta0_hat<-coef(reg.summary)[1,1]
beta1_hat<-coef(reg.summary)[2,1]
cat("beta0_hat for the linear regression is", beta0_hat)
cat("beta1_hat for the linear regression is", beta1_hat)
abline(reg.lm_3)
y_true<- -1+0.5*x
abline(lm(y_true~x),col='red')
legend('topleft', legend=c("Population Regression","Least Squares Regression"),lty=c(1,1),lwd=c(2.5,2.5),col=c('black','red'))
print("With increased epsilon variance increases and the fit of the regression line becomes less accurate relative to the data")
print("The fit of the regression line is most accurate with a variance of 0.1")

confint(reg.lm_1,level=0.95)
confint(reg.lm_2,level=0.95)
confint(reg.lm_3,level=0.95)

print("the widths of the confidence intervals vary due to the changing epsilon values: reduced variance levels lead to tighter intervals.")

##################### 
####  QUESTION 2 ####
#####################
set.seed(5072)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
print('beta0=2,beta1=2, and beta2=0.3')
variable_mtx<-matrix(c(x1,x2,y),nrow=100,ncol=3)
colnames(variable_mtx)<-c('x1','x2','y')
cor(variable_mtx)
pairs(variable_mtx)
print('x1 and y, x2 and y both have a weak linear relationship while x1 and x2 show a strong linear relationship')

#multi
lm.fit.both<-lm(y~x1+x2)
summary(lm.fit.both)
print('both the intercept and x1_hat are statistically significant, but x2_hat is statiscally insignificant due to its high p value ')
print('by using alpha=0.05, we can reject the null hypothesis that x1=0, but we cannot reject the null hypotheis for x2 because its p value is too high approach')

#Just x1
lm.fit.justx1<-lm(y~x1)
summary(lm.fit.justx1)
print('we can reject the null hypothesis that x1=0 at a significance level of 0.01 because it is greater than the p-value of justx1 according to our summary')

#just x2
lm.fit.justx2<-(y~x2)
summary(lm.fit.justx2)
print('we can reject the null hypothesis that x2=0 at a significance level of 0.01 because it is greater than the p-value')
print("this does not necessarily contradict our data because we have run the regression with x2 alone vs the multivariable approach. Also, a failure to reject the null does not mean something isnt true.")

#adding new data points 
x1=c(x1,0.1)
x2=c(x2,0.8)
y=c(y,6)

lm.fit.both_new<-lm(y~x1+x2) 
lm.fit.justx1_new<-lm(y~x1)
lm.fit.justx2_new<-lm(y~x2)

par(mfrow=c(2,2))
plot(lm.fit.both)
summary(lm.fit.both)
plot(lm.fit.both_new)
par(mfrow=c(2,2))
plot(lm.fit.justx1)
plot(lm.fit.justx1_new)
par(mfrow=c(2,2))
plot(lm.fit.justx2)
plot(lm.fit.justx2_new)
print("based on the outputs of the plots, it is clear this new data point reduces all 3 model accuracies and acts as a high leverage point in .both and .justx1")

##################### 
####  QUESTION 3 ####
#####################

#a. 
set.seed(5072)
my_data<-Boston
predictors_table<-data.frame()

for (i in 2:14) {
  
  lm.fit_ith<-lm(Boston$crim~Boston[,i])
  row_ith<-c(summary(lm.fit_ith)$fstatistic[1],anova(lm.fit_ith)$'Pr(>F)'[1], coef(summary(lm.fit_ith))[1,1],coef(summary(lm.fit_ith))[1,2])
  predictors_table<-rbind(predictors_table,row_ith)
            
}
colnames(predictors_table)<-c('F-Statistic','P-Value','Y-Intercept','Beta1-hat')
rownames(predictors_table)<-c('zn','indus','chas','nox','rm','age','dis','rad','tax','ptratio','black','lstat','medv')

predictors_table

#b.
predictors_table[predictors_table$'P-Value'<0.05,]

#c
par(mfrow=c(3,4))
par(mar=c(2,2,2,2))
predictor_vec<-rownames(predictors_table[predictors_table$`P-Value`<0.05,])
for (i in predictor_vec){
  
  plot(Boston[i][,1],Boston$crim, main=1)
  abline(lm(Boston$crim~Boston[i][,1],col='red'))
}
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))

#d.
lm.fit_multiple<- lm(Boston$crim~Boston$zn+Boston$indus+Boston$chas+Boston$nox+Boston$rm+Boston$age+Boston$dis+Boston$rad+Boston$tax+Boston$ptratio+Boston$black+Boston$lstat+Boston$medv)

#e.
model_coef_info<-coef(summary(lm.fit_multiple))
model_coef_info[model_coef_info[,4]<0.05,]

#f
plot(predictors_table$`Beta1-hat`,model_coef_info[-1,1],pch=16,xlab='Predictors used in univariate models',ylab='Predictors used in multivariable models')
#which approch produces the most accurate reflection of the population parameters?
for(i in 2:14){
  x=anova(lm.fit_multiple,lm(Boston$crim~Boston[,i]))
  print(x$RSS)
}

#g.
poly_table<-data.frame()
for (i in 2:14){
  
  x<-my_data[,i]
  x_sq<-x^2
  x_cub<-x^3
  lm.fit.simp_ith<-lm(my_data$crim~x)
  lm.fit.poly_ith<-lm(my_data$crim~x+x_sq+x_cub)
  row_ith<-data.frame()
  row_ith[1,1]<-names(my_data[i])
  row_ith[1,2]<-anova(lm.fit.simp_ith,lm.fit.poly_ith$r[2])
  row_ith[1,3]<anova(lm.fit.simp_ith,lm.fit.poly_ith)$'Pr(>F)'[2]
  poly_table<-rbind(poly_table,row_ith)
  
}
colnames(poly_table)<-c('predictor','fstat','pvalueofFstat')
poly_table<-na.omit(poly_table[order(-poly_table[,2]),])
#the following table contains the predictors with p values indicating there is a difference between the two models at significace
#level of alpha=0.05
poly_table$pvalueofFstat<0.05





