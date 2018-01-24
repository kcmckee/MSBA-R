require(class)
require(glmnet)
require(MASS)
require(boot)

######################
#Attrition Prediction#
######################
rm(list=ls())

#Read file
table_hr <- read.csv('WA_Fn-UseC_-HR-Employee-Attrition.csv', sep = ',')
colnames(table_hr)[1] <- 'Age'
table_hr_left <- table_hr[table_hr$Attrition == 'Yes',]

#Exploratory Analysis: Job Dimention

par(mfrow = c(2,2))

barplot(c(length(table_hr_left$JobRole[table_hr_left$JobRole == 'Sales Executive']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Laboratory Technician']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Sales Representative']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Research Scientist']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Research Director']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Human Resources']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Healthcare Representative']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Manufacturing Director']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Manager'])),
     names.arg = c('Sales Exe','Lab Technician','Sales Rep',
                     'Research Sci','Research Dir','H& R',
                     'Hth Rep','Manuf Dir','Manager'),
       main = 'Former Employee Job Roles')

barplot(c(length(table_hr_left$Department[table_hr_left$Department == 'Human Resources']), 
          length(table_hr_left$Department[table_hr_left$Department == 'Research & Development']),
          length(table_hr_left$Department[table_hr_left$Department == 'Sales'])), 
        names.arg = c('Human Resources', 'R & D', 'Sales'), 
        main = 'Former Employee Departments',
        ylab = 'Frequency')

barplot(c(length(table_hr_left$JobLevel[table_hr_left$JobLevel == 1]), 
          length(table_hr_left$JobLevel[table_hr_left$JobLevel == 2]),
          length(table_hr_left$JobLevel[table_hr_left$JobLevel == 3]),
          length(table_hr_left$JobLevel[table_hr_left$JobLevel == 4]),
          length(table_hr_left$JobLevel[table_hr_left$JobLevel == 5])), 
        names.arg = c(1,2,3,4,5), 
        main = 'Former Employee Job Levels',
        ylab = 'Frequency')

barplot(c(length(table_hr_left$OverTime[table_hr_left$OverTime == 'Yes']), 
          length(table_hr_left$OverTime[table_hr_left$OverTime == 'No'])), 
        names.arg = c('Yes', 'No'), 
        main = 'Former Employee Overtime Status',
        ylab = 'Frequency')

par(mfrow = c(1,1))

#Exploratory Analysis: Personal Dimention

par(mfrow = c(2,2))

hist(table_hr_left$MonthlyIncome, breaks = 50, xlab = 'Montyly Income', 
     main = 'Former Employee Monthly Income Distribution')

hist(table_hr_left$Age, breaks = 50, xlab = 'Age', main = 'Age')

barplot(c(length(table_hr_left$BusinessTravel[table_hr_left$BusinessTravel == 'Travel_Frequently']), 
          length(table_hr_left$BusinessTravel[table_hr_left$BusinessTravel == 'Travel_Rarely']),
          length(table_hr_left$BusinessTravel[table_hr_left$BusinessTravel == 'Non-Travel'])), 
        names.arg = c('Frequently', 'Rarely', 'None'), 
        main = 'Former Employee Business Travel Status',
        ylab = 'Frequency')

barplot(c(length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 1]), 
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 2]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 3]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 4])), 
        names.arg = c(1,2,3,4), 
        main = 'Former Employee Job Involvement',
        ylab = 'Frequency')

par(mfrow = c(1,1))

#Exploratory Analysis: Career Development Dimention

par(mfrow = c(2,2))

barplot(c(length(table_hr_left$EducationField[table_hr_left$EducationField == 'Human Resources']), 
          length(table_hr_left$EducationField[table_hr_left$EducationField == 'Life Science']),
          length(table_hr_left$EducationField[table_hr_left$EducationField == 'Marketing']),
          length(table_hr_left$EducationField[table_hr_left$EducationField == 'Mecidal']),
          length(table_hr_left$EducationField[table_hr_left$EducationField == 'Other']),
          length(table_hr_left$EducationField[table_hr_left$EducationField == 'Technical Degree'])), 
        names.arg = c('H & R', 'Life Sci', 'Marketing', 'Mecidal', 'Other', 'Tech.Degree'), 
        main = 'Former Employee Education Fields',
        ylab = 'Frequency')

barplot(c(length(table_hr_left$JobSatisfaction[table_hr_left$JobSatisfaction == 1]), 
          length(table_hr_left$JobSatisfaction[table_hr_left$JobSatisfaction == 2]),
          length(table_hr_left$JobSatisfaction[table_hr_left$JobSatisfaction == 3]),
          length(table_hr_left$JobSatisfaction[table_hr_left$JobSatisfaction == 4])), 
        names.arg = c(1,2,3,4), 
        main = 'Former Employee Job Satisfaction',
        ylab = 'Frequency')

barplot(c(length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 0]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 1]), 
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 2]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 3]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 4]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 5]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 6]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 7]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 8]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 9])), 
        names.arg = c(0,1,2,3,4,5,6,7,8,9), 
        main = 'Former Employee"s Number of Companies Worked',
        ylab = 'Frequency')

hist(table_hr_left$YearsAtCompany, breaks = 100, main = 'Former Employee"s Years With Company', xlab = NULL)

par(mfrow = c(1,1))

#Feature re-engineering
table_hr <- table_hr[, -c(9,22,27)]    #Taking out 3 features that have only one value
X <- model.matrix(Attrition ~ ., data = table_hr)[,-1]     #Spliting factor type features with multiple levels into dummy variables
y <- table_hr$Attrition
table_hr <- data.frame(X, y)    #Taking out the intercept column
colnames(X) <- colnames(table_hr)[-length(colnames(table_hr))]    #Fixing column names for matrix X

#Set partitioning
set.seed(1693)
n <- nrow(X)
trainprop <- 0.9
testprop <- 1 - trainprop
train <- sample(1:n, n * trainprop)
test <- setdiff(1:n, train)
train.x <- X[train,]
test.x <- X[test,]
train.y <- y[train]
test.y <- y[test]
numfolds <- 10
fold.indices <- cut(1:n, breaks = numfolds, labels=FALSE)      #For later mannual CV usage

#Creating a table to store confution matrix for each model
result_mtx <- matrix(ncol = 6)
colnames(result_mtx) <- c('Correct_Rate', 
                          'Error_Rate', 
                          'Type-I_Error_Rate', 
                          'Type-II_Error_Rate',
                          'Power',
                          'Precision')

######################################
#Lasso Regression (Feature Selection)#
######################################

grid <- 10 ^ seq(10, -2, length = 100)
Ls.Reg <- glmnet(train.x, train.y, alpha = 1, lambda = grid, family = 'binomial')
#plot(Ls.Reg)

#Finding the best lambda by using cross validation
set.seed(1693)
cv.out <- cv.glmnet(train.x, train.y, alpha=1, family="binomial")
#plot(cv.out)
bestlam <- cv.out$lambda.min
Ls.pred <- predict(Ls.Reg, s = bestlam, newx = test.x, type="class")

#Creating confusion matrix for Lasso
cf_mtx_lasso <- table(test.y, Ls.pred)
(cf_mtx <- cf_mtx_lasso)
row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))
result_mtx <- rbind(result_mtx, row_temp)

#Fitting model over the entire dataset and extract the best subset
out <- glmnet(X, y, alpha = 1, lambda = grid, family = 'binomial')
Ls.coefs <- predict(out, type = 'coefficients', s = bestlam)[1:46,]
Ls.coefs
Ls.coefs[Ls.coefs!=0][-1]
coef_names <- names(Ls.coefs[Ls.coefs!=0][-1])

#Subsetting data set by extracted coefficient names
table_hr_new <- data.frame(table_hr[coef_names], table_hr$y)
colnames(table_hr_new)[28] <- 'Attrition'

#####################
#Logistic Regression#
#####################

correct_rate <- c()
error_rate <- c()
type_I <- c()
type_II <- c()
model_power <- c()
precision <- c()

for (i in 1:numfolds) {
  test.indices <- which(fold.indices == i)        
  test.data <- table_hr_new[test.indices, ]
  train.data <- table_hr_new[-test.indices, ]               #Set partitioning
  log.fit = glm(Attrition ~ ., data = train.data, family = binomial)             #Fitting model on training set
  log.pred <- rep('No', nrow(test.data))
  log.probs <- predict(log.fit, test.data, type = 'response')
  log.pred[log.probs > 0.5] <- 'Yes'
  #mse[i] <- mean(test.data$Attrition != log.pred)
  cf_mtx <- table(test.data$Attrition, log.pred)
  correct_rate[i] <- (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)
  error_rate[i] <- (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)
  type_I[i] <- cf_mtx[1,2]/sum(cf_mtx[1,])
  type_II[i] <- cf_mtx[2,1]/sum(cf_mtx[2,])
  model_power[i] <- cf_mtx[2,2]/sum(cf_mtx[2,])
  precision[i] <- cf_mtx[2,2]/sum(cf_mtx[,2])
}

row_temp <- c(mean(correct_rate),
              mean(error_rate),
              mean(type_I),
              mean(type_II),
              mean(model_power),
              mean(precision))
result_mtx <- rbind(result_mtx, row_temp)

###########
#LDA Model#
###########

correct_rate <- c()
error_rate <- c()
type_I <- c()
type_II <- c()
model_power <- c()
precision <- c()

for (i in 1:numfolds) {
  test.indices <- which(fold.indices == i)        
  test.data <- table_hr_new[test.indices, ]
  train.data <- table_hr_new[-test.indices, ]                  #Set partitioning
  lda.fit <- lda(Attrition ~ ., data = train.data, family = binomial)             #Fitting model on training set
  lda.pred <- predict(lda.fit, test.data)$class
  #mse[i] <- mean(test.data$Attrition != lda.pred)
  cf_mtx <- table(test.data$Attrition, lda.pred)
  correct_rate[i] <- (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)
  error_rate[i] <- (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)
  type_I[i] <- cf_mtx[1,2]/sum(cf_mtx[1,])
  type_II[i] <- cf_mtx[2,1]/sum(cf_mtx[2,])
  model_power[i] <- cf_mtx[2,2]/sum(cf_mtx[2,])
  precision[i] <- cf_mtx[2,2]/sum(cf_mtx[,2])
}

row_temp <- c(mean(correct_rate),
              mean(error_rate),
              mean(type_I),
              mean(type_II),
              mean(model_power),
              mean(precision))
result_mtx <- rbind(result_mtx, row_temp)

###########
#QDA Model#
###########

correct_rate <- c()
error_rate <- c()
type_I <- c()
type_II <- c()
model_power <- c()
precision <- c()

for (i in 1:numfolds) {
  test.indices <- which(fold.indices == i)        
  test.data <- table_hr_new[test.indices, -14]
  train.data <- table_hr_new[-test.indices, -14]                  #Set partitioning
  qda.fit <- qda(Attrition ~ ., data = train.data, family = binomial)             #Fitting model on training set
  qda.pred <- predict(qda.fit, test.data)$class
  #mse[i] <- mean(test.data$Attrition != qda.pred)
  cf_mtx <- table(test.data$Attrition, qda.pred)
  correct_rate[i] <- (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)
  error_rate[i] <- (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)
  type_I[i] <- cf_mtx[1,2]/sum(cf_mtx[1,])
  type_II[i] <- cf_mtx[2,1]/sum(cf_mtx[2,])
  model_power[i] <- cf_mtx[2,2]/sum(cf_mtx[2,])
  precision[i] <- cf_mtx[2,2]/sum(cf_mtx[,2])
}

row_temp <- c(mean(correct_rate),
              mean(error_rate),
              mean(type_I),
              mean(type_II),
              mean(model_power),
              mean(precision))
result_mtx <- rbind(result_mtx, row_temp)

#####
#KNN#
#####

#Scaling data for KNN
table_hr_new_scaled <- as.matrix(scale(table_hr_new[-28], center = T))
attrition_vector <- table_hr_new$Attrition

#Fiding optimal K for KNN classifier

correct_rate <- c()
error_rate <- c()
type_I <- c()
type_II <- c()
model_power <- c()
precision <- c()

for (i in 1:50) {
  
  cor_r_temp <- c()
  er_r_temp <- c()
  tpI_temp <- c()
  tpII_temp <- c()
  mp_temp <- c()
  prec_temp <- c()
  
  for (j in 1:numfolds) {
    test.indices <- which(fold.indices == j)
    test.data <- table_hr_new_scaled[test.indices, ]
    train.data <- table_hr_new_scaled[-test.indices, ]
    knn.pred.k <- knn(train.data, test.data, attrition_vector[-test.indices], k = i)
    cf_mtx <- table(y[test.indices], knn.pred.k)
    cor_r_temp[i] <- (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)
    er_r_temp[i] <- (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)
    tpI_temp[i] <- cf_mtx[1,2]/sum(cf_mtx[1,])
    tpII_temp[i] <- cf_mtx[2,1]/sum(cf_mtx[2,])
    mp_temp[i] <- cf_mtx[2,2]/sum(cf_mtx[2,])
    prec_temp[i] <- cf_mtx[2,2]/sum(cf_mtx[,2])
  }
  
  correct_rate[i] <-mean(cor_r_temp)
  error_rate[i] <- mean(er_r_temp)
  type_I[i] <- mean(tpI_temp)
  type_II[i] <- mean(tpII_temp)
  model_power[i] <- mean(mp_temp)
  precision[i] <- mean(prec_temp)
  
}

print(paste('The minimum Type-II error rate:', type_II[which.min(type_II)], 'occur at K =', which.min(type_II)))

#Running a KNN with k=1 and calculate the confusion matrix by using 10-folds cross validation
correct_rate <- c()
error_rate <- c()
type_I <- c()
type_II <- c()
model_power <- c()
precision <- c()

for (i in 1:numfolds) {
  test.indices <- which(fold.indices == i)
  test.data <- table_hr_new_scaled[test.indices, ]
  train.data <- table_hr_new_scaled[-test.indices, ]
  knn.pred.k <- knn(train.data, test.data, attrition_vector[-test.indices], k = 1)
  cf_mtx <- table(y[test.indices], knn.pred.k)
  correct_rate[i] <- (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)
  error_rate[i] <- (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)
  type_I[i] <- cf_mtx[1,2]/sum(cf_mtx[1,])
  type_II[i] <- cf_mtx[2,1]/sum(cf_mtx[2,])
  model_power[i] <- cf_mtx[2,2]/sum(cf_mtx[2,])
  precision[i] <- cf_mtx[2,2]/sum(cf_mtx[,2])
}

row_temp <- c(mean(correct_rate),
              mean(error_rate),
              mean(type_I),
              mean(type_II),
              mean(model_power),
              mean(precision))
result_mtx <- rbind(result_mtx, row_temp)
result_mtx <- result_mtx[-1,]
rownames(result_mtx) <- c('Lasso','Logistic','LDA', 'QDA', 'KNN')
result_mtx

############Tunning Model That Has Lowest Type-II Error###############
#####################
#Logistic Regression#
#####################
tuning_parameter <- seq(0.2, 0.8, 0.1)

for (j in 1:length(tuning_parameter)) {
  correct_rate <- c()
  error_rate <- c()
  type_I <- c()
  type_II <- c()
  model_power <- c()
  precision <- c()
  
  for (i in 1:numfolds) {
    test.indices <- which(fold.indices == i)        
    test.data <- table_hr_new[test.indices, ]
    train.data <- table_hr_new[-test.indices, ]               #Set partitioning
    log.fit = glm(Attrition ~ ., data = train.data, family = binomial)             #Fitting model on training set
    log.pred <- rep('No', nrow(test.data))
    log.probs <- predict(log.fit, test.data, type = 'response')
    log.pred[log.probs > tuning_parameter[j]] <- 'Yes'
    cf_mtx <- table(test.data$Attrition, log.pred)
    correct_rate[i] <- (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)
    error_rate[i] <- (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)
    type_I[i] <- cf_mtx[1,2]/sum(cf_mtx[1,])
    type_II[i] <- cf_mtx[2,1]/sum(cf_mtx[2,])
    model_power[i] <- cf_mtx[2,2]/sum(cf_mtx[2,])
    precision[i] <- cf_mtx[2,2]/sum(cf_mtx[,2])
  }
  
  cf_mtx_logistic <- matrix(c(correct_rate[which.min(type_II)],
                              error_rate[which.min(type_II)],
                              type_I[which.min(type_II)],
                              type_II[which.min(type_II)],
                              model_power[which.min(type_II)],
                              precision[which.min(type_II)]), ncol = 6, byrow = T, dimnames = list(c('Logistic Regression'),
                                                                                                   c('Overall_Correct',
                                                                                                     'Overall_Error',
                                                                                                     'Type_I',
                                                                                                     'Type_II',
                                                                                                     'Power',
                                                                                                     'Precision')))
  print(round(cf_mtx_logistic, 4))
}

#Overall Correct Rate: The total percentage of correct prediction.
#Overall Incorrect Rate: The total percentage of incorrect prediction.
#Type-I Error Rate: The percentage of false-positive prediction, which is the employees that would not leave but identified as leaving.
#Type-II Error Rate: The percentage of false-negative prediction, which is the employees that would leave but identified as not-leaving.
#Power: Percentage of employess whom has left that are correctly identified by the model.
#Precision: Percentage of employees that are correctly predicted as leaving.