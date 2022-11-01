rm(list = ls())

###########################
### Classification Tree ###
###########################

diabetes <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/Decision Tree/data/diabetes.csv")
diabetes$Outcome <- as.factor(diabetes$Outcome)
levels(diabetes$Outcome) <- c("no", "yes")

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/Decision Tree/myfunctions copy.r")

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### call the function for creating 70:30 partition
p2 <- partition.2(diabetes, 0.7)
training.data <- p2$data.train
test.data <- p2$data.test


library(rpart)
library(rpart.plot)
# fit classification tree on training data
# minsplit refers to the minimum number of observations that must exist 
# in a node in order for a split to be attempted. The default value is 20. 
# minbucket argument can be specified to indicate the minimum number of 
# required observations in the terminal nodes for the split to happen.
ct1 <- rpart(Outcome ~ . , data = training.data, method = "class", 
             minsplit=15, minbucket = 5)
#minsplit - minimum number of observations in a node for a split to be attempted
#minbucket - minimum number of observations in a terminal node for a split to to be valid
# Ex. if u split a node with 16 observations, and get 2 terminal nodes with 13 observations and 3 then the split is not valid as 3 is less than minbucket,

# plot tree
prp(ct1, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/ct1.pdf")


# variable importance
ct1$variable.importance

# get predicted class on the test data
see_the_probs_of_each_on_print = predict(ct1, test.data, type = 'pred')

pred.test = predict(ct1, test.data, type = 'class')

# create confusion matrix
library(caret)
confusionMatrix(pred.test, test.data$Outcome, positive = "yes")

# cost complexity cross validation
library(caret)
set.seed(0)
train_control <- trainControl(method="cv", number=10)
cv.ct <- train(Outcome ~ . , data = training.data, method = "rpart",
                   trControl = train_control, tuneLength = 10)
## metric = "Kappa" may be mentioned if best tree should be selected based on that 
## control argument can be used to control the minsplit and minbucket
# cv.ct <- train(Outcome ~ . , data = training.data, method = "rpart",
#                trControl = train_control, tuneGrid = data.frame(cp = c(0.01, 0.05)),
#                control = rpart.control(minsplit = 10, minbucket = 5))

print(cv.ct)
plot(cv.ct)
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/ct3.pdf")
cv.ct$finalModel
prp(cv.ct$finalModel, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/ct2.pdf")


# variable importance
cv.ct$finalModel$variable.importance
summary(cv.ct$finalModel)

# get prediction on the test data
pred.test.prune = predict(cv.ct$finalModel, test.data, type = 'class')

# create confusion matrix
confusionMatrix(pred.test.prune, test.data$Outcome, positive = "yes")


### Using different cutoff values
# Predict the raw probability 
pred.prob = predict(cv.ct$finalModel, test.data, type = 'prob')
head(pred.prob)
cutoff <- 0.3 # if proportion of occurrences of class "yes" > cutoff then predicted label = "yes"
pred.test.cutoff <- ifelse(pred.prob[,2] >= cutoff, "yes", "no")
confusionMatrix(as.factor(pred.test.cutoff), as.factor(test.data$Outcome), positive = "yes")

# you can print out the decision tree at each level 
cv.ct$finalModel
