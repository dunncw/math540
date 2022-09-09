autompg <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/SLR_MLR_regression/data/autompg.csv") # read in data
Dat <- autompg # rename data

## Scatter plot ##
par(mfrow = c(2, 3)) # set up 2x3 plot
plot(Dat$cyl, Dat$mpg) # cyl vs mpg
plot(Dat$disp, Dat$mpg) # disp vs mpg
plot(Dat$hp, Dat$mpg) # hp vs mpg
plot(Dat$wt, Dat$mpg) # wt vs mpg
plot(Dat$acc, Dat$mpg) # acc vs mpg
plot(Dat$year, Dat$mpg) # year vs mpg

#########################################
## Create training and test data ##
#########################################

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/SLR_MLR_regression/myfunctions copy.r")


RNGkind (sample.kind = "Rounding") # set seed
set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(Dat, 0.7) ## creating 70:30 partition
training.data <- p2$data.train ## training data
test.data <- p2$data.test ## test data

####################################
## Simple linear regression model ##
####################################
## Fit SLR model
slr <- lm(mpg ~ hp, data = training.data) # fit SLR model

## Inference on model parameters ##
summary(slr) # summary of SLR model
confint(slr, level = 0.95) # 95% confidence intervals for model parameters

## Forecasting mean response for a given value of x=160
newDat <- data.frame(hp = 160) # create new data frame
predict(slr, newdata = newDat) # predict mpg for hp = 160


######################################
## Multiple linear regression model ##
######################################

## Fit MLR model
mlr <- lm(mpg~., data = training.data) # fit MLR model

## Inference on model parameters ##
# we want to see small p-values for each of the predictors
summary(mlr) # summary of MLR model

## 95% Confidence interval for model parameters ##
confint(mlr, level = 0.95) # 95% confidence intervals for model parameters




## Check multicollinearity
library(car) # load car package
# we want our VIF values to be less than 10 for each predictor and ideally less than 5 for each predictor 
# the we will systematicly remove the predictor with the highest VIF value and refit the model until all VIF values are less than 5
vif(mlr) # variance inflation factor

## Check correlation
# we want to see that the correlation between each predictor and the response is not too high
cor(training.data) # correlation matrix


#remove cyl
mlr2 <- lm(mpg ~ disp+hp+wt+acc+year, data = training.data) # fit MLR model
summary(mlr2) # summary of MLR model
vif(mlr2) # variance inflation factor

#remove hp
mlr3 <- lm(mpg ~ disp+wt+acc+year, data = training.data)
summary(mlr3)
vif(mlr3)

#remove hp and select the final model
mlr4 <- lm(mpg ~ wt+acc+year, data = training.data)
summary(mlr4)
vif(mlr4)

mlr4 <- lm(mpg ~ wt + year, data = training.data)
summary(mlr4)
vif(mlr4)


## Prediction 
## Given value for x ##
x0 <- data.frame(cyl = 6, disp = 200, hp=140, wt=3220, acc = 12, year = 80) # create new data frame

## Forecasting mean response for a given value of x
predict(mlr, x0) # predict mpg for x0

# we can predict for more that one value of x by using a vector for each predictor
x0 <- data.frame(cyl = c(6, 7, 5), disp = c(200, 210, 240), hp=c(142, 140, 148), wt=c(3220, 3000, 3060), acc = c(12, 11, 10), year = c(80, 90, 85)) # create new data frame

## Forecasting mean response for a given value of x
predict(mlr, x0) # predict mpg for x0


x0 <- data.frame( wt=3220,year = 80)
## Forecasting mean response for a given value of x
predict(mlr5, x0) # predict mpg for x0

##################################################################
## Multiple linear regression model with categorical regressors ##
##################################################################

house <- read.csv('/Users/cindydunn/Desktop/Grad_School/math540/SLR_MLR_regression/data/houseprice.csv') # read in data

# the predictor view is a categorical(but is currently seen as a string) variable with 3 levels and the function factor converts it to a factor which will treat the data as categorical
mlr <- lm(price ~ sqft + factor(view) , data=house) # fit MLR model
summary(mlr) # summary of MLR model

# relevel the factor so that the reference level is the lowest level
# the reference level is the level that is dropped from the model
# the reference level is dropped because it is not needed to estimate the model parameters for the other levels of the factor
mlr <- lm(price ~ sqft + relevel(factor(view), ref = "wood") , data=house)
summary(mlr)

### plot
col.code <- ifelse(house$view == "mountain", "red", 
                   ifelse(house$view == "wood", "blue", "green")) # color code
plot(house$sqft, house$price, col = col.code, pch=19, cex=2) # plot
legend("topleft", legend = c("mountain", "wood", "city"), 
       col = c("red", "blue", "green"), lwd=2) # legend
abline(a=(mlr$coefficients[1]+mlr$coefficients[3]), b=mlr$coefficients[2], col="red") # regression line
abline(a=(mlr$coefficients[1]+mlr$coefficients[4]), b=mlr$coefficients[2], col="blue")
abline(a=(mlr$coefficients[1]), b=mlr$coefficients[2], col="green")

#dev.copy2pdf(file = "D:/Data mining/Lecture Notes/plots/cat-var.pdf")
