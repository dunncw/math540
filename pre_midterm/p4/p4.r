diabetes <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/p4/data/diabetes.csv")
## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/p4/myfunctions copy.r")

RNGkind (sample.kind = "Rounding") 
set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(diabetes, 0.8) ## creating 70:20:10 partition
training.data <- p2$data.train
test.data <- p2$data.test

#q1
logistic.model <- glm(Outcome ~ ., family = binomial(link='logit'), data=training.data) ## fit logistic regression model
logistic.model$coefficients
library(car)
vif(logistic.model)

#q2
library(caret)
## K-fold Cross Validation
# value of K equal to 10 
set.seed(0)
train_control <- trainControl(method = "cv", 
                              number = 10) 

# Fit K-fold CV model  
step_kcv <- train(Outcome ~ ., data = training.data, family = "binomial", 
                 method = "glmStepAIC", trControl = train_control) 
print(step_kcv)
step_kcv$finalModel

# Confusion matrix for test data
pred.prob.test <- predict(step_kcv$finalModel, newdata = test.data,type = "response")
pred.y.test <- ifelse(pred.prob.test > 0.5, 1, 0) # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$Outcome), positive = "1")

#q3
library(glmnet)
glmnet.lasso <- train(Outcome ~ ., data = training.data, method = "glmnet",
                      family = "binomial", trControl = train_control, 
                      tuneGrid = expand.grid(alpha = 1,lambda = seq(0.001,0.1,by = 0.001)))
glmnet.lasso$finalModel$bestTune
# best parameter
glmnet.lasso$bestTune

# best coefficient # best coefficient ##this is how u print final best model
lasso.model <- coef(glmnet.lasso$finalModel, glmnet.lasso$bestTune$lambda)
lasso.model

# prediction on test data
pred.prob.lasso <- predict(glmnet.lasso, s = glmnet.lasso$bestTune, newdata= as.data.frame(test.data), type = "prob")

#q4
glmnet.ridge <- train(Outcome ~ ., data = training.data, method = "glmnet",
                      family = "binomial", trControl = train_control, 
                      tuneGrid = expand.grid(alpha = 0,lambda = seq(0.001,0.1,by = 0.001)))
glmnet.ridge 
plot(glmnet.ridge)

#report model equation
glmnet.ridge$finalModel

# best parameter
glmnet.ridge$bestTune

# best coefficient ##this is how u print final best model
ridge.model <- coef(glmnet.ridge$finalModel, glmnet.ridge$bestTune$lambda)
ridge.model

# prediction on test data
pred.prob.ridge <- predict(glmnet.ridge, s = glmnet.ridge$bestTune, test.data, type = "prob")
pred.y.test <- ifelse(pred.prob.ridge[,2] > 0.5, "yes", "no") # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$Outcome), positive = "yes")

#q5
glmnet.Elastic <- train(Outcome ~ ., data = training.data, method = "glmnet",
                      family = "binomial", trControl = train_control, 
                      tuneGrid = expand.grid(alpha = .5,lambda = seq(0.001,0.1,by = 0.001)))
glmnet.Elastic 
plot(glmnet.Elastic)

# best parameter
glmnet.Elastic$bestTune

# best coefficient ##this is how u print final best model
Elastic.model <- coef(glmnet.Elastic$finalModel, glmnet.Elastic$bestTune$lambda)
Elastic.model

# prediction on test data
pred.prob.Elastic <- predict(glmnet.Elastic, s = glmnet.Elastic$bestTune, test.data, type = "prob")
pred.y.test <- ifelse(pred.prob.Elastic[,2] > 0.5, "yes", "no") # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$Outcome), positive = "yes")
