#remove objects in your workspace
rm(list = ls())

autompg <- read.csv("~/Desktop/teaching 2022 fall/Math 540&440 statistical learning/yang/datasets/autompg.csv")

Dat <- autompg

## Include the functions required for data partitioning
source("~/Desktop/teaching 2022 fall/Math 540&440 statistical learning/yang/R files/myfunctions.R")

##########################################
## Create training validation test data ##
##########################################
RNGkind (sample.kind = "Rounding") 
set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(Dat, 0.7) ## creating 60:30:10 partition
training.data <- p2$data.train
test.data <- p2$data.test


################################################################################
############# K-fold cross validation using  ######################
################################################################################

k = 5 # number of folds
# use the train function in package caret for cross validation and loocv 
library(caret)
RNGkind (sample.kind = "Rounding") 
set.seed(O)
# specify the sampling method as 5-folds cross validation
train_control <- trainControl(method = "cv", 
                              number = 5) 
# Fit K-fold CV model  
mlr_kcv <- train(mpg ~ ., data = training.data,  
                 method = "lm", trControl = train_control) 
print(mlr_kcv)
mlr_kcv$finalModel

yhat_test = predict(mlr_kcv,test.data)
RMSE_test = sqrt(mean((test.data$mpg - yhat_test)^2))
RMSE_test


# the model predicted by linear regression function lm 
mlr = lm(mpg ~., data = training.data)
mlr$coefficients


###################################################################
############# leave one out cross validation ######################
###################################################################


RNGkind (sample.kind = "Rounding") 
set.seed(0)
# specify the sampling method as leave one out cross validation
train_control <- trainControl(method = "LOOCV") 

# Fit LOOCV model  
mlr_loocv <- train(mpg ~ ., data = training.data,  
                 method = "lm", trControl = train_control) 
print(mlr_loocv)
mlr_loocv$finalModel

yhat_test = predict(mlr_loocv,test.data)
RMSE_test = sqrt(mean((test.data$mpg - yhat_test)^2))
RMSE_test


