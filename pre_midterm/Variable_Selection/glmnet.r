rm(list = ls())

autompg <- read.csv("~/Desktop/teaching 2022 fall/Math 540&440 statistical learning/yang/datasets/autompg.csv")
Dat <- autompg

## Include the functions required for data partitioning
source("~/Desktop/teaching 2022 fall/Math 540&440 statistical learning/yang/R files/myfunctions.R")

#Dat <- read.csv("D:/Data mining/datasets/prostrate_cancer.csv")


set.seed(123) ## set seed so that you get same partition each time
p2 <- partition.2(Dat, 0.7) ## creating 70:30 partition
training.data <- p2$data.train
test.data <- p2$data.test


library(glmnet)

# convert data to matrix type
trainX <- as.matrix(training.data[, -1])
testX <- as.matrix(test.data[, -1])
trainY <- training.data$mpg

lasso <- glmnet(x = trainX, y = trainY, alpha = 1)
plot(lasso, xvar = "lambda", main = "Lasso regression")


ridge <- glmnet(x = trainX, y = trainY, alpha = 0)
plot(ridge, xvar = "lambda", main = "Ridge regression")

# Using caret to perform regularized regression
library(caret)
set.seed(0)
train_control <- trainControl(method="cv", number=10)

#########################
### Lasso regression ####
#########################

glmnet.lasso <- train(mpg ~ ., data = training.data, method = "glmnet",
                      trControl = train_control, 
                      tuneGrid = expand.grid(alpha = 1,lambda = seq(0.001,0.1,by = 0.001)))
# glmnet.lasso <- train(mpg ~ ., data = training.data, method = "glmnet", 
#                       trControl = train_control, 
#                       tuneGrid = expand.grid(alpha = 1,lambda = lasso$lambda))
glmnet.lasso 
plot(glmnet.lasso)

# best parameter
glmnet.lasso$bestTune

# best coefficient
lasso.model <- coef(glmnet.lasso$finalModel, glmnet.lasso$bestTune$lambda)
lasso.model

# prediction on test data
yhat.lasso <- predict(glmnet.lasso, s = glmnet.lasso$bestTune, test.data)
# RMSE for test data
error.test.lasso <- yhat.lasso - test.data$mpg
rmse.test.lasso <- sqrt(mean(error.test.lasso^2))
rmse.test.lasso

#########################
### Ridge regression ####
#########################

glmnet.ridge <- train(mpg ~ ., data = training.data, method = "glmnet",
                      trControl = train_control, 
                      tuneGrid = expand.grid(alpha = 0,lambda = seq(0.1,1,by = 0.1)))
glmnet.ridge 
plot(glmnet.ridge)

# best parameter
glmnet.ridge$bestTune

# best coefficient
ridge.model <- coef(glmnet.ridge$finalModel, glmnet.ridge$bestTune$lambda)
ridge.model

# prediction on test data
yhat.ridge <- predict(glmnet.ridge, s = glmnet.ridge$bestTune, test.data)
# RMSE for test data
error.test.ridge <- yhat.ridge - test.data$mpg
rmse.test.ridge <- sqrt(mean(error.test.ridge^2))
rmse.test.ridge


