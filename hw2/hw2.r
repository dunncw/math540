#get data
b_housing <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/hw2/data/BostonHousing.csv")

b_housing$CAT..MEDV <- as.factor(b_housing$CAT..MEDV) # factor with 2 levels
levels(b_housing$CAT..MEDV) <- c("no", "yes") # redefine levels


## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/hw2/myfunctions copy.r")
#remove MEDV from the data set
b_housing <- b_housing[,-13]

# A. Split the data into training (80%) and test (20%) datasets.
# Create training validation test data
RNGkind (sample.kind = "Rounding") 
set.seed(123) ## set seed so that you get same partition each time
p2 <- partition.2(b_housing, 0.8) ## creating 80:20 partition
training.data <- p2$data.train # training data
test.data <- p2$data.test # test data

#1. Build a k-nearest neighbor model. Clearly show the model building steps for full credit. What is the optimal number of neighbors?

### Rescale the data
#center is the mean of each column
#scale is the standard deviation of each column
# the -9 drops that coulmn from the dataframe
training.scaled <- scale(training.data[,-13], center = TRUE, scale = TRUE) # exclude the response vairable from the data set for scaling
training.scaled.wY <- cbind(training.scaled, training.data[,13])
training.scaled.attr <- attributes(training.scaled)
test.scaled <- scale(test.data[,-13], 
                    center = training.scaled.attr$`scaled:center`, 
                    scale = training.scaled.attr$`scaled:scale`)


library(FNN)
library(caret)
### fit k-nn model for k = 1, ..., 100
K <- 100
kappa <- rep(0, K)
for (kk in 1:K){
  Knn <- knn(train = training.scaled, test = test.scaled,
             cl = training.data[,13], k = kk)
  c <- confusionMatrix(as.factor(Knn), as.factor(test.data[,13]), 
                       positive = "yes")
  kappa[kk] <- c$overall["Kappa"]
  #cat("K", kk, "Kappa", kappa[kk], "\n")
}

# create a plot for k vs. kappa
plot(c(1:K), kappa, xlab = "k", ylab = "Kappa", type = "l", col = "blue")
which.max((kappa)) # the best k
# we are looking for the largest kappa value
# 1 is the best k

# 2. Evaluate the predictive performance of this model on the test data.

### fit k-nn model on test data with k=1
Knn <- knn(train = training.scaled, test = test.scaled,
           cl = training.data[,13], k = 1)
confusionMatrix(as.factor(Knn), as.factor(test.data[,13]), 
                positive = "yes")

# Accuracy : 0.9604
# Sensitivity : 0.8235
# Specificity : 0.9881

# B. Use the BostonHousing data to accomplish the following modeling tasks.

b_housing <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/hw2/data/BostonHousing.csv")

b_housing$CAT..MEDV <- as.factor(b_housing$CAT..MEDV) # factor with 2 levels
levels(b_housing$CAT..MEDV) <- c("no", "yes") # redefine levels


## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/hw2/myfunctions copy.r")
#remove MEDV from the data set
b_housing <- b_housing[,-13]

# A. Split the data into training (80%) and test (20%) datasets.
# Create training validation test data
RNGkind (sample.kind = "Rounding") 
set.seed(123) ## set seed so that you get same partition each time
p2 <- partition.2(b_housing, 0.8) ## creating 80:20 partition
training.data <- p2$data.train # training data
test.data <- p2$data.test # test data

library(rpart)
library(rpart.plot)
# fit classification tree on training data
# minsplit refers to the minimum number of observations that must exist 
# in a node in order for a split to be attempted. The default value is 20. 
# minbucket argument can be specified to indicate the minimum number of 
# required observations in the terminal nodes for the split to happen.
ct1 <- rpart(CAT..MEDV ~ . , data = training.data, method = "class", 
             minsplit=15, minbucket = 5)
#minsplit - minimum number of observations in a node for a split to be attempted
#minbucket - minimum number of observations in a terminal node for a split to to be valid
# Ex. if u split a node with 16 observations, and get 2 terminal nodes with 13 observations and 3 then the split is not valid as 3 is less than minbucket,

# plot tree
prp(ct1, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/ct1.pdf")

summary(ct1)

# variable importance
ct1$variable.importance

# get predicted class on the test data
see_the_probs_of_each_on_print = predict(ct1, test.data, type = 'pred')

pred.test = predict(ct1, test.data, type = 'class')

# create confusion matrix
library(caret)
confusionMatrix(pred.test, test.data$CAT..MEDV, positive = "yes")