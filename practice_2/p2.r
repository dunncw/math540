prop_val_df <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/practice_2/data/property_valuation.csv") # read in data

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/practice_2/myfunctions copy.r")

RNGkind (sample.kind = "Rounding") # set seed
set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(prop_val_df, 0.7) ## creating 70:30 partition
training.data <- p2$data.train ## training data
test.data <- p2$data.test ## test data

#q1
## Fit MLR model
mlr <- lm(y~., data=training.data) # fit MLR model
mlr
# answer
# mlr <- lm(y ~ . , data=training.data)
# y_hat_subi = 22.3817 + 1.0318 * x1 + 9.9967 * x2 + 0.2840 * x3 + 2.3971 *x4 + 0.7773* x5 + -2.8460 * x6 + 2.7992 * x7 + -0.1298 * x9 + 4.9929 * x9

# equation of the fitted line
mlr$coefficients
# mpg_hat_subI = 57258.9362 + 544.1261*sqft + -718254.9177*factor(view)mountain + -761526.7811*factor(view)wood

#q2
# Which variables have significant linear association with the response
summary(mlr) # summary of MLR model
# sqft, factor(view)mountain, factor(view)wood
#answer 
# none of the variables have significant linear association with the response at significance level 0.05

#q3
## Check correlation
# we want to see that the correlation between each predictor and the response is not too high
cor(training.data)

# this is something i found online but it gives you the highest pairwise correlation so you dont have to look at the whole table
library(data.table)
corMatrix <- cor(matrix(rnorm(100), 5))
setDT(melt(cor(training.data)))[order(value)]
# answer
# x6 and x7 , when you look at this do not use the response variable y

#q4
library(car)
vif(mlr) # variance inflation factor
# answer 
# yes, there is several values greater than 10 and also bunch greater than 4

#q5
## Given value for x ##
x0 <- data.frame(x1=5, x2=1, x3=5, x4=1, x5=0, x6=6, x7=3, x8=35, x9=0) # create new data frame

## Forecasting mean response for a given value of x
round(predict(mlr, x0), digits = 4) # predict response for given value of x and round to 4 digits
# answer
# 28.1342

#q6
#Report the residual standard error of the fitted model.
summary(mlr)
#answer
#2.794

#q7
#Use the fitted model to make prediction on the test data and report the RMSE
y_hat <- predict(mlr, test.data) # predict response for test data
RMSE <- sqrt(mean((y_hat - test.data$y)^2)) # calculate RMSE
round(RMSE, digits = 4) # round to 4 digits
#answer
# 3.5518