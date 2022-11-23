rm(list = ls())
autompg <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/p6/data/autompg.csv")

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/p6/myfunctions copy.r")

###############################################
## Create training, validation and test data ##
###############################################
Dat <- autompg
RNGkind (sample.kind = "Rounding") 
set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(Dat, 0.6) ## creating 60:40 partition
training.data <- p2$data.train
test.data <- p2$data.test

# Fit a regression tree using rpart() function (use method = "anova") so that the minimum number of observations in the terminal node is 5.
library(rpart)
library(rpart.plot)
ct1 <- rpart(mpg~., data = training.data, method = "anova", 
             minsplit=15, minbucket = 5)

# plot tree
prp(ct1, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)

# Evaluate the performance of this model on test data
pred.test = predict(ct1, test.data)

# computer RMSE
library(caret)
RMSE <- sqrt(mean((pred.test - test.data$mpg)^2))
RMSE


# Use cost complexity cross validation to get a pruned tree.
library(caret)
set.seed(0)
train_control <- trainControl(method="cv", number=10)
cv.ct <- train(mpg ~ . , data = training.data, method = "rpart",
               trControl = train_control, tuneLength = 10)

plot(cv.ct)

pred.test.prune = predict(cv.ct$finalModel, test.data)
#Evaluate the performance of the pruned tree on test data
RMSE.prune <- sqrt(mean((pred.test.prune - test.data$mpg)^2))
RMSE.prune

