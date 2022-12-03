### Create tiny.data dataframe
Fat <- c(0.2, 0.1, 0.2, 0.2, 0.4, 0.3)
Salt <- c(0.9, 0.1, 0.4, 0.5, 0.5, 0.8)
Y <- c("like", "dislike", "dislike", "dislike", "like","like")
tiny.data <- data.frame(Fat, Salt, Y)

### create dummies for variables with multiple categories
table(tiny.data$Y)
tiny.data$Like <- ifelse(tiny.data$Y == "like", 1, 0)
tiny.data$Dislike <- ifelse(tiny.data$Y == "dislike", 1, 0)

### get rid of the mutiple category variables (only keep dummies)
tiny.data <- tiny.data[, -3]

library(neuralnet)
### Run neural net with 3 hidden nodes
RNGkind (sample.kind = "Rounding") 
set.seed(0)
nn <- neuralnet(Like + Dislike ~ Salt + Fat, 
                data = tiny.data, hidden = 3)

### display weights
nn$weights

### plot network
plot(nn, rep="best")
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/nnet3.pdf")

### display predictions
nn$net.result
nn$response

### run neural network with 2 layers
### layer 1 has 2 hidden nodes, layer 2 has 3 hidden nodes 
nn2 <- neuralnet(Like + Dislike ~ Salt + Fat, 
                data = tiny.data, hidden = c(2,3))
### plot network
plot(nn2, rep="best")
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/nnet4.pdf")

###########################################
## Neural net for classification problem ##
###########################################

diabetes <- read.csv("~/Desktop/teaching 2022 fall/Math 540&440 statistical learning/yang/datasets/diabetes.csv")
diabetes$Outcome <- as.factor(diabetes$Outcome)
levels(diabetes$Outcome) <- c("no", "yes")

## Include the functions required for data partitioning
source("~/Desktop/teaching 2022 fall/Math 540&440 statistical learning/yang/R files/myfunctions.R")

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### call the function for creating 70:30 partition
p2 <- partition.2(diabetes, 0.7)
training.data <- p2$data.train
test.data <- p2$data.test

library(caret)
library(nnet)
train_control <- trainControl(method="cv", number=10)
## size refers to the number of hidden nodes and decay is the learning rate
tune.grid <- expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))
cv.nn <- train(Outcome ~ . , data = training.data, method = "nnet",
               preProc = c("center", "scale"),
               trControl = train_control, tuneGrid = tune.grid)
# cv.nn <- train(Outcome ~ . , data = training.data, method = "nnet",
#                preProc = c("center", "scale"), metric = "Kappa",
#                trControl = train_control, tuneLength = 10)
print(cv.nn)
plot(cv.nn)
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/nnet5.pdf")

# Best size and decay
cv.nn$bestTune

# Prediction on test data
pred.prob <- predict(cv.nn, test.data, type = "prob")
pred.y.nn <- ifelse(pred.prob[,2] > 0.5, "yes", "no") # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.nn), as.factor(test.data$Outcome), 
                positive = "yes")

###########################################################
### Plotting the neural network using neuralnet package ###
###########################################################

training.data.nn <- training.data
### create dummies for variables with multiple categories
training.data.nn$Yes <- ifelse(training.data.nn$Outcome == "yes", 1, 0)
training.data.nn$No <- ifelse(training.data.nn$Outcome == "no", 1, 0)

### get rid of the mutiple category variables (only keep dummies)
training.data.nn <- training.data.nn[, -9]

### fit neural net model with the bestTune parameters 
nn <- neuralnet(Yes + No ~ ., data = training.data.nn, 
                hidden = cv.nn$bestTune$size, learningrate = cv.nn$bestTune$decay)
### plot network
plot(nn, rep="best")
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/nnet6.pdf")

