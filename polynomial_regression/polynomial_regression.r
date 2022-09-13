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
set.seed(123) ## set seed so that you get same partition each time
p3 <- partition.3(Dat, 0.6, 0.3) ## creating 60:30:10 partition
training.data <- p3$data.train
validation.data <- p3$data.val
test.data <- p3$data.test

####################################
## Simple linear regression model ##
####################################
## Fit SLR model on training data
slr.train <- lm(mpg ~ hp, data = training.data)



# prediction on validation data
yhat = predict(slr.train, newdata=validation.data)
# RMSE for validation data
error.validation <- yhat - validation.data$mpg
rmse.validation <- sqrt(mean(error.validation^2))
rmse.validation


###################################################################
## Polynomial regression model of degree 2 using poly() function ##
###################################################################

## Fit polynomial model on training data 
q <- 2
poly.train <- lm(mpg ~ poly(hp, degree = q), data = training.data)

# prediction on validation data
yhat = predict(poly.train, newdata=data.frame(validation.data))
# RMSE for validation data
error.validation <- yhat - validation.data$mpg
rmse.validation <- sqrt(mean(error.validation^2))
rmse.validation


##################################################################
## Polynomial regression model of degree 3 using poly() function ##
###################################################################

## Fit polynomial model on training data 
q <- 3
poly.train <- lm(mpg ~ poly(hp, degree = q), data = training.data)

# prediction on validation data
yhat = predict(poly.train, newdata=data.frame(validation.data))
# RMSE for validation data
error.validation <- yhat - validation.data$mpg
rmse.validation <- sqrt(mean(error.validation^2))
rmse.validation


#########################################################################
## Polynomial regression model of degrees 1 to 10 using poly() function ##
#########################################################################

## Fit polynomial model on training data 
rmse.validation = numeric(10)
for(q in 1:10)
{
  poly.train <- lm(mpg ~ poly(hp, degree = q), data = training.data)
  # prediction on validation data
  yhat = predict(poly.train, newdata=data.frame(validation.data))
  # RMSE for validation data
  error.validation <- yhat - validation.data$mpg
  rmse.validation[q] <- sqrt(mean(error.validation^2))
}
rmse.validation


#########################################################################
###### compute RMSE for the test data test ##############################
###### to evaluate model performance with the chosen degree #############
#########################################################################

q = which.min(rmse.validation)
q
Dat1 = rbind(training.data, validation.data)
poly.train <- lm(mpg ~ poly(hp, degree = q), data = Dat1)

# prediction on test data
yhat = predict(poly.train, newdata=data.frame(test.data))
# RMSE for test data
error.test <- yhat - test.data$mpg
rmse.test <- sqrt(mean(error.test^2))
rmse.test









