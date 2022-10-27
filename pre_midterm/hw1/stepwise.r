rm(list = ls())

autompg <- read.csv("~/Desktop/teaching 2022 fall/Math 540&440 statistical learning/yang/datasets/autompg.csv")
Dat <- autompg

## Include the functions required for data partitioning
source("~/Desktop/teaching 2022 fall/Math 540&440 statistical learning/yang/R files/myfunctions.R")

set.seed(123) ## set seed so that you get same partition each time
p2 <- partition.2(Dat, 0.7) ## creating 70:30 partition
training.data <- p2$data.train
test.data <- p2$data.test

#######################
## Create full model ##
#######################
mlr_full <- lm(mpg ~ ., data = training.data)
summary(mlr_full)
vif(mlr_full)
cor(training.data)

# prediction on test data
yhat.full = predict(mlr_full, newdata=data.frame(test.data))
# RMSE for test data
error.test.full <- yhat.full - test.data$mpg
rmse.test.full <- sqrt(mean(error.test.full^2))
rmse.test.full

###########################
### Stepwise selection ####
###########################

# backward selection by default
summary(mlr_full <- lm(mpg ~ ., data = training.data))
step.model <- step(mlr_full)
summary(step.model)

# prediction on test data
yhat = predict(step.model, newdata=data.frame(test.data))
# RMSE for test data
error.test <- yhat - test.data$mpg
rmse.test <- sqrt(mean(error.test^2))
rmse.test

# forward selection
summary(mlr_null <- lm(mpg ~ 1, data = training.data))
step.model.fwd <- step(mlr_null, scope = ~ cyl + disp + hp + wt + acc + year, direction = "forward")
summary(step.model.fwd)

###########################
### Stepwise selection ####
### w Cross Validation ####
###########################

library(caret)

## K-fold Cross Validation
# value of K equal to 5 
set.seed(0)
train_control <- trainControl(method = "cv", 
                              number = 5) 

# Fit K-fold CV model  
step_kcv <- train(mpg ~ ., data = training.data,  
                 method = "lmStepAIC", trControl = train_control) 
print(step_kcv)
step_kcv$finalModel

# prediction on test data
yhat.kcv = predict(step_kcv$finalModel, newdata=data.frame(test.data))
# RMSE for test data
error.test.kcv <- yhat.kcv - test.data$mpg
rmse.test.kcv <- sqrt(mean(error.test.kcv^2))
rmse.test.kcv