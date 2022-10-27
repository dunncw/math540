diabetes <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/p4/data/diabetes.csv")
diabetes$Outcome <- as.factor(diabetes$Outcome)
levels(diabetes$Outcome) <- c("no", "yes")

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/p4/myfunctions copy.r")

set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(diabetes, 0.8) ## creating 70:30 partition
training.data <- p2$data.train
test.data <- p2$data.test




library(glmnet)

# convert data to matrix type
trainX <- as.matrix(training.data[, -9])
testX <- as.matrix(test.data[, -9])
trainY <- training.data$Outcome

lasso <- glmnet(x = trainX, y = trainY, alpha = 1, family = "binomial")
plot(lasso, xvar = "lambda", main = "Lasso regression")

ridge <- glmnet(x = trainX, y = trainY, alpha = 0, family = "binomial")
plot(ridge, xvar = "lambda", main = "Ridge regression")


# Using caret to perform regularized regression
library(caret)
set.seed(0)
train_control <- trainControl(method="cv", number=10)

#########################
### Lasso regression ####
#########################

glmnet.lasso <- train(Outcome ~ ., data = training.data, method = "glmnet",
                      family = "binomial", trControl = train_control, 
                      tuneGrid = expand.grid(alpha = 1,lambda = seq(0.001,0.1,by = 0.001)))
# glmnet.lasso <- train(Outcome ~ ., data = training.data, method = "glmnet",
#                       trControl = train_control,
#                       tuneGrid = expand.grid(alpha = 1,lambda = lasso$lambda))
glmnet.lasso 
plot(glmnet.lasso)

# best parameter
glmnet.lasso$bestTune

#equation of model
glmnet.lasso$finalModel

# best coefficient
lasso.model <- coef(glmnet.lasso$finalModel, glmnet.lasso$bestTune$lambda)
lasso.model

# prediction on test data
pred.prob.lasso <- predict(glmnet.lasso, s = glmnet.lasso$bestTune, test.data, type = "prob")
pred.prob.lasso
pred.y.test <- ifelse(pred.prob.lasso[,2] > 0.5, "yes", "no") # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$Outcome), positive = "yes")


#########################
### Ridge regression ####
#########################

glmnet.ridge <- train(Outcome ~ ., data = training.data, method = "glmnet",
                      family = "binomial", trControl = train_control, 
                      tuneGrid = expand.grid(alpha = 0,lambda = seq(0.001,0.1,by = 0.001)))
glmnet.ridge 
plot(glmnet.ridge)

#report model equation
glmnet.ridge$finalModel

# best parameter
glmnet.ridge$bestTune

# best coefficient
ridge.model <- coef(glmnet.ridge$finalModel, glmnet.ridge$bestTune$lambda)
ridge.model

# prediction on test data
pred.prob.ridge <- predict(glmnet.ridge, s = glmnet.ridge$bestTune, test.data, type = "prob")
pred.y.test <- ifelse(pred.prob.ridge[,2] > 0.5, "yes", "no") # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$Outcome), positive = "yes")
