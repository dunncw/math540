#remove objects in your workspace
rm(list = ls())

autompg <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/polynomial_regression/data/autompg.csv")
Dat <- autompg

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/polynomial_regression/myfunctions copy.r")

##########################################
# Create training validation test data
RNGkind (sample.kind = "Rounding") 
set.seed(123) ## set seed so that you get same partition each time
p3 <- partition.3(Dat, 0.6, 0.3) ## creating 60:30:10 partition
training.data <- p3$data.train # training data
validation.data <- p3$data.val # validation data
test.data <- p3$data.test # test data

####################################
# Simple linear regression model
# Fit SLR model on training data
slr.train <- lm(mpg ~ hp, data = training.data)



# prediction on validation data
yhat = predict(slr.train, newdata=validation.data)
# RMSE for validation data
error.validation <- yhat - validation.data$mpg # error on validation data
rmse.validation <- sqrt(mean(error.validation^2)) # RMSE on validation data
rmse.validation # print RMSE on validation data


###################################################################
# Polynomial regression model of degree 2 using poly() function

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
#Polynomial regression model of degree 3 using poly() function

## Fit polynomial model on training data 
q <- 3
poly.train <- lm(mpg ~ poly(hp, degree = q), data = training.data) # fit polynomial model on training data
#poly(hp, degree = q) ploynomial function of degree q

# prediction on validation data
yhat = predict(poly.train, newdata=data.frame(validation.data))
# RMSE for validation data
error.validation <- yhat - validation.data$mpg
rmse.validation <- sqrt(mean(error.validation^2))
rmse.validation


#########################################################################
#Polynomial regression model of degrees 1 to 10 using poly() function

## Fit polynomial model on training data 
rmse.validation = numeric(10)
rmse.training = numeric(10)
for(q in 1:10)
{
  poly.train <- lm(mpg ~ poly(hp, degree = q), data = training.data)
  #RMSE for training data
  rmse.training[q] = sqrt(mean(poly.train$residuals^2))
  # prediction on validation data
  yhat = predict(poly.train, newdata=data.frame(validation.data))
  # RMSE for validation data
  error.validation <- yhat - validation.data$mpg
  rmse.validation[q] <- sqrt(mean(error.validation^2))
}
rmse.training
rmse.validation

# plot rmse against degree of polynomial
plot(x = 1:10,rmse.validation,type = "b", col = "blue", xlab = "degree of polynomial regression model",ylab = "RMSE", ylim = c(4,5.6) )
points(x = 1:10,rmse.training, type = "b", col = "red")


#########################################################################
#compute RMSE for the test data test to evaluate model performance with the chosen degree

q = 2
Dat1 = rbind(training.data, validation.data)
poly.train <- lm(mpg ~ poly(hp, degree = q), data = Dat1)

# prediction on test data
yhat = predict(poly.train, newdata=data.frame(test.data))
# RMSE for test data
error.test <- yhat - test.data$mpg
rmse.test <- sqrt(mean(error.test^2))
rmse.test



#########################################################################
#K-fold cross validation for polynomial regression of degress 2
#we use cv to find the average RMSE for the validation data
k = 5 # number of folds
q = 2 
Dat1 = rbind(training.data, validation.data) # combine the training and validation datasets
# use the train function in package caret for cross validation and loocv 
library(caret)
RNGkind (sample.kind = "Rounding") 
set.seed(0)
# specify the sampling method as 5-folds cross validation
train_control <- trainControl(method = "cv", 
                              number = k) 
# Fit K-fold CV model  
f <- bquote(mpg ~ poly(hp, .(q))) # polynomial regression model of degree q
ploy_kcv <- train(as.formula(f), data = Dat1,  
                 method = "glm", trControl = train_control) 
print(ploy_kcv)

#below we comapre 5 fold cv model and poly regression model with no cross validation
ploy_kcv$finalModel
ploy <- lm(mpg~poly(hp,q), Dat1)

yhat_test = predict(ploy_kcv,test.data)
RMSE_test = sqrt(mean((test.data$mpg - yhat_test)^2))
RMSE_test

#########################################################################
#K-fold cross validation for polynomial regression of degresses 1 to 10
k = 5 # number of folds
q = 1:10
Dat1 = rbind(training.data, validation.data) # combine the training and validation datasets
# use the train function in package caret for cross validation and loocv 
library(caret)
RNGkind (sample.kind = "Rounding") 
set.seed(0)
rmse_kcv = numeric(10)
for(q in 1:10)
  {
    # specify the sampling method as 5-folds cross validation
    train_control <- trainControl(method = "cv", 
                              number = k) 
    # Fit K-fold CV model  
    f <- bquote(mpg ~ poly(hp, .(q)))
    ploy_kcv <- train(as.formula(f), data = Dat1,  
                  method = "glm", trControl = train_control) 
    rmse_kcv[q] = ploy_kcv$results$RMSE

}

rmse_kcv
# plot rmse against degree of polynomial
plot(x = 1:10, rmse_kcv, type = "b", col = "blue", xlab = "degree of polynomial regression model" )











