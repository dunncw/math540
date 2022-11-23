rm(list = ls())

diabetes <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/Ensemble/data/diabetes.csv")
diabetes$Outcome <- as.factor(diabetes$Outcome)
levels(diabetes$Outcome) <- c("no", "yes")

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/Ensemble/myfunctions copy.r")

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### call the function for creating 70:30 partition
p2 <- partition.2(diabetes, 0.7)
training.data <- p2$data.train
test.data <- p2$data.test

####################################
###### Bagging #####################
####################################
library(caret)
set.seed(0)
modelLookup("treebag")
train_control <- trainControl(method="cv", number=10)
## specify nbagg to control the number of trees. default value is 25 
bag <- train(Outcome ~ . , data = training.data, method = "treebag",
               trControl = train_control, nbagg = 50)
print(bag)
plot(varImp(bag))
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/bag1.pdf")
bag$finalModel

# get prediction on the test data
pred.test.bag = predict(bag$finalModel, test.data, type = 'class')

# create confusion matrix
confusionMatrix(pred.test.bag, test.data$Outcome, positive = "yes")

####################################
###### Random Forest ###############
####################################
library(caret)
set.seed(0)
modelLookup("rf")
train_control <- trainControl(method="cv", number=10)
rf <- train(Outcome ~ . , data = training.data, method = "rf",
             trControl = train_control, tuneLength = 3)
# metric = "Kappa" may be mentioned if best tree should be selected based on that
## specify tuning parameters using tuneGrid
# rf <- train(Outcome ~ . , data = training.data, method = "rf", ntree = 50,
#                trControl = train_control, tuneGrid = expand.grid(mtry = c(2,5,8)))
print(rf)
plot(varImp(rf))
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/ct3.pdf")
rf$finalModel

# get prediction on the test data
pred.test.rf = predict(rf$finalModel, test.data, type = 'class')

# create confusion matrix
confusionMatrix(pred.test.rf, test.data$Outcome, positive = "yes")


####################################
###### Adaboost ####################
####################################
library(caret)
library(ada)
modelLookup("ada")
set.seed(0)
train_control <- trainControl(method="cv", number=10)
ada <- train(Outcome ~ . , data = training.data, method = "ada",
            trControl = train_control, tuneLength = 3)
## Specify tuning parameters using tuneGrid 
# tgrid <- expand.grid(iter = c(50,100,150),         
#                      maxdepth = c(1, 2, 3),       
#                      nu = c(0.1,0.2))
# ada <- train(Outcome ~ . , data = training.data, method = "ada", metric = "Kappa",
#                trControl = train_control, tuneGrid = tgrid)
print(ada)
plot(varImp(ada))
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/ct3.pdf")
#ada$finalModel

# get prediction on the test data
pred.test.ada = predict(ada$finalModel, test.data)

# create confusion matrix
confusionMatrix(pred.test.ada, test.data$Outcome, positive = "yes")

