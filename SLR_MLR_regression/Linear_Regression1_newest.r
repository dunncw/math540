autompg <- read.csv("~/Downloads/autompg.csv")
Dat <- autompg

## Scatter plot ##
par(mfrow = c(2, 3))
plot(Dat$cyl, Dat$mpg)
plot(Dat$disp, Dat$mpg)
plot(Dat$hp, Dat$mpg)
plot(Dat$wt, Dat$mpg)
plot(Dat$acc, Dat$mpg)
plot(Dat$year, Dat$mpg)

#########################################
## Create training and test data ##
#########################################

## Include the functions required for data partitioning
source("F:/Data mining/Lecture Notes/myfunctions.R")


RNGkind (sample.kind = "Rounding") 
set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(Dat, 0.7) ## creating 70:30 partition
training.data <- p2$data.train
test.data <- p2$data.test

####################################
## Simple linear regression model ##
####################################
## Fit SLR model
slr <- lm(mpg ~ hp, data = training.data)

## Inference on model parameters ##
summary(slr)
confint(slr, level = 0.95)

## Forecasting mean response for a given value of x=160
newDat <- data.frame(hp = 160)
predict(slr, newdata = newDat)


######################################
## Multiple linear regression model ##
######################################

## Fit MLR model
mlr <- lm(mpg~., data = training.data)

## Inference on model parameters ##
summary(mlr)

## 95% Confidence interval for model parameters ##
confint(mlr, level = 0.95)




## Check multicollinearity
library(car)
vif(mlr)

## Check correlation
cor(training.data)


#remove cyl
mlr2 <- lm(mpg ~ disp+hp+wt+acc+year, data = training.data)
summary(mlr2)
vif(mlr2)

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
x0 <- data.frame(cyl = 6, disp = 200, hp=140, wt=3220, acc = 12, year = 80)

## Forecasting mean response for a given value of x
predict(mlr, x0)


x0 <- data.frame( wt=3220,year = 80)
## Forecasting mean response for a given value of x
predict(mlr5, x0)

##################################################################
## Multiple linear regression model with categorical regressors ##
##################################################################

house <- read.csv("/houseprice.csv")

mlr <- lm(price ~ sqft + factor(view) , data=house)
summary(mlr)

mlr <- lm(price ~ sqft + relevel(factor(view), ref = "wood") , data=house)
summary(mlr)

### plot
col.code <- ifelse(house$view == "mountain", "red", 
                   ifelse(house$view == "wood", "blue", "green"))
plot(house$sqft, house$price, col = col.code, pch=19, cex=2)
legend("topleft", legend = c("mountain", "wood", "city"), 
       col = c("red", "blue", "green"), lwd=2)
abline(a=(mlr$coefficients[1]+mlr$coefficients[3]), b=mlr$coefficients[2], col="red")
abline(a=(mlr$coefficients[1]+mlr$coefficients[4]), b=mlr$coefficients[2], col="blue")
abline(a=(mlr$coefficients[1]), b=mlr$coefficients[2], col="green")

#dev.copy2pdf(file = "D:/Data mining/Lecture Notes/plots/cat-var.pdf")
