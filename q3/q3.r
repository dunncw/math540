#read in csv data
wine <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/q3/data/wine.csv")
wine$Type <- as.factor(wine$Type)
levels(wine$Type) <- c("red", "white")
## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/q3/myfunctions copy.r")

library(rpart)
library(rpart.plot)

Dat <- wine
RNGkind (sample.kind = "Rounding") 
set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(Dat, 0.7)
training.data <- p2$data.train
test.data <- p2$data.test

# Use the cost complexity cross-validation method to build the tree. 
library(caret)
set.seed(0)
train_control <- trainControl(method="cv", number=10)
cv.ct <- train(Type ~ . , data = training.data, method = "rpart",
                   trControl = train_control, tuneLength = 10)
plot(cv.ct)
cv.ct$finalModel
prp(cv.ct$finalModel, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)

# What is the optimal penalty factor chosen by the cross-validation method?
cv.ct$bestTune
# it seems to be cp = 0 for the optimal penalty factor

# What is the associated cost complexity? Show work.
# cost complexity = err(t)-alpha*L(t)
print(cv.ct$finalModel)
# create a confusion matrix for training data
pred.train <- predict(cv.ct$finalModel, training.data, type = 'class')
confusionMatrix(pred.train, training.data$Type)

records_misclassifed <- 17 + 22
n= 2169
errt <- records_misclassifed/n
alpha <- 0
lt <- 15
cost_complexity <- errt - alpha*lt
cost_complexity

# What is the Gini index at the root node of the final tree? Show work.
# 1117 yes
# 1052 no
total <- 1117 + 1052
gini_root <- 1 - ((1117/total)^2 + (1052/total)^2)
gini_root

# Evaluate the model on test data. Report sensitivity, specificity and kappa.
pred.test.prune = predict(cv.ct$finalModel, test.data, type = 'class')
confusionMatrix(pred.test.prune, test.data$Type, positive = "white")

# Sensitivity : 0.9843          
# Specificity : 0.9730 
# Kappa : 0.9569
