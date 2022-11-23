# set gwd as pwd
setwd("/Users/cindydunn/Desktop/Grad_School/math540")

# load libraries

## Include the functions required for data partitioning
source("Final_project/myfunctions copy.r")

#load the data
darwin_data <- read.csv('Final_project/DARWIN_data/DARWIN.csv')
darwin_data$class <- as.factor(darwin_data$class)
levels(darwin_data$class) <- c(1, 0) #can also be done as "P"(patient)(postive class) and "H"(healthy)
#drop the column called 'id'
darwin_data <- darwin_data[,-1]

#testing out for logit
# darwin_data <- read.csv('Final_project/DARWIN_data/DARWIN.csv')
# darwin_data$class <- ifelse(darwin_data$class == "P", 1, 0)
# #drop the column called 'id'
# darwin_data <- darwin_data[,-1]

# partition the data
RNGkind (sample.kind = "Rounding") 
set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(darwin_data, 0.7) ## creating 70:30 partition
training.data <- p2$data.train
test.data <- p2$data.test

########################################################################################
# logistic regression model (does not work with out model atm)
logistic.model <- glm(class ~ ., family=binomial(link='logit'),data=training.data) ## fit logistic regression model
# output:
# Warning message:
# glm.fit: algorithm did not converge 

summary(logistic.model) ## summary of logistic regression model
confint.default(logistic.model) ## confidence interval for regression coefficients

library(caret)
# Confusion matrix for test data
pred.prob.test <- predict(logistic.model, newdata = test.data,type = "response")
pred.y.test <- ifelse(pred.prob.test > 0.5, 1, 0) # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$class), 
                positive = "1")

# create a fancy table for the confusion matrix
library(gmodels)
CrossTable(x = pred.y.test, y = test.data$class, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("Predicted", "Observed"))

# Create ROC
library(pROC)
# use this for getting ROC curve on test data
r <- roc(test.data$class, pred.prob.test)
plot.roc(r, main = "ROC curve")# plot ROC curve

#compute Area under curve
auc(r)

# Create Lift chart ######### wont work for some reason????
library(gains)
#make the vector test.data$class numeric
test.data$class <- as.numeric(test.data$class)
# use this for getting Lift chart on test data
gain <- gains(as.numeric(test.data$class), pred.prob.test)
gain

# Plot Lift chart: Percent cumulative response
x <- c(0, gain$depth)
pred.y <- c(0, gain$cume.pct.of.total)
avg.y <- c(0, gain$depth/100)
plot(x, pred.y, main = "Cumulative Lift Chart", xlab = "deciles", 
     ylab = "Percent cumulative response", type = "l", col = "red", cex.lab = 1.5)
lines(x, avg.y, type = "l")

########################################################################################
# stepwise selection (does not work with our model atm)
# things to google 

# backward selection by default
step.model <- step(lm(class ~ ., data = training.data))
summary(step.model)

# prediction on test data
yhat = predict(step.model, newdata=data.frame(test.data))
# RMSE for test data
error.test <- yhat - test.data$mpg
rmse.test <- sqrt(mean(error.test^2))
rmse.test

# forward selection
summary(mlr_null <- lm(mpg ~ 1, data = training.data))
step.model.fwd <- step(mlr_null, scope = ~ cyl + disp + hp + wt + acc + year, direction = "forward")
summary(step.model.fwd)

########################################################################################
# KNN 
library(FNN)
library(caret)
## K-fold Cross Validation
# value of K equal to 10 
set.seed(0)
train_control <- trainControl(method = "cv", 
                              number = 10) 
# Specify tuning parameter using tuneGrid
tg <- data.frame(k = seq(1,122,3))
Knn_kcv2 <- train(class ~ ., data = training.data, method = "knn", 
                  trControl = train_control, preProcess = c("center","scale"), 
                  tuneGrid = tg)
print(Knn_kcv2)

#what metric should be used for classification
########################################################################################
# Ensemble bagging approach
library(caret)
set.seed(0)
modelLookup("treebag")
train_control <- trainControl(method="cv", number=10)
## specify nbagg to control the number of trees. default value is 25 
bag <- train(class ~ . , data = training.data, method = "treebag",
             trControl = train_control, nbagg = 50)
plot(varImp(bag))

#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/bag1.pdf")
bag$finalModel

# get prediction on the test data
pred.test.bag = predict(bag$finalModel, test.data, type = 'class')

# create confusion matrix
confusionMatrix(pred.test.bag, test.data$class, positive = "1")

########################################################################################
#decision tree
library(rpart)
library(rpart.plot)
ct1 <- rpart(class ~ . , data = training.data, method = "class", 
             minsplit=15, minbucket = 5)

# plot tree
prp(ct1, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)
#write the tree to a png file
png("Final_project/out/decision_tree.png")
dev.off()

# get predicted class on the test data
see_the_probs_of_each_on_print = predict(ct1, test.data, type = 'pred')

pred.test = predict(ct1, test.data, type = 'class')

# create confusion matrix
library(caret)
confusionMatrix(pred.test, test.data$class, positive = "1")

# see the confusion matrix as a nice table
library(gmodels)
CrossTable(pred.test, test.data$class, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("Predicted", "Actual"))
###### include this table

# create a lift chart
library(gains)
#make the vector test.data$class numeric
test.data$class <- as.numeric(test.data$class)
# use this for getting Lift chart on test data
gain <- gains(as.numeric(test.data$class), pred.test)

########################################################################################
# Adaboost
library(caret)
library(ada)
modelLookup("ada")
set.seed(0)
train_control <- trainControl(method="cv", number=10)
ada <- train(class ~ . , data = training.data, method = "ada",
             trControl = train_control, tuneLength = 3)
print(ada)
plot(varImp(ada))
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/ct3.pdf")
#ada$finalModel

# get prediction on the test data
pred.test.ada = predict(ada$finalModel, test.data)

# create confusion matrix
confusionMatrix(pred.test.ada, test.data$class, positive = "1")


########################################################################################
# Random forest
library(caret)
set.seed(0)
modelLookup("rf")
train_control <- trainControl(method="cv", number=10)
rf <- train(class ~ . , data = training.data, method = "rf",
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
confusionMatrix(pred.test.rf, test.data$class, positive = "1")
