#######################################
## Neural net for regression problem ##
#######################################

autompg <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/NeuralNet/data/autompg.csv")

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/NeuralNet/myfunctions copy.r")

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### call the function for creating 70:30 partition
p2 <- partition.2(autompg, 0.7)
training.data <- p2$data.train
test.data <- p2$data.test

library(caret)
library(nnet)
train_control <- trainControl(method="cv", number=10)
# set linout = 1 for regression problem
# tune.grid <- expand.grid(size = seq(from = 1, to = 20, by = 1),
#                          decay = seq(from = 0.0001, to = 0.01, by = 0.001))
# cv.nn <- train(mpg ~ . , data = training.data, method = "nnet",
#                preProc = c("center", "scale"), linout = 1,
#                trControl = train_control, tuneGrid = tune.grid)
cv.nn <- train(mpg ~ . , data = training.data, method = "nnet",
               preProc = c("center", "scale"), linout = 1,
               trControl = train_control, tuneLength = 10)
print(cv.nn)
plot(cv.nn)
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/nnet7.pdf")

# Best size and decay
cv.nn$bestTune

# Prediction on test data
pred.test <- predict(cv.nn, test.data)
# RMSE for test data
error.test.nn <- pred.test - test.data$mpg
rmse.test.nn <- sqrt(mean(error.test.nn^2))
rmse.test.nn

