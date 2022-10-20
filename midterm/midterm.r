wine <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/midterm/data/wine.csv")
source("/Users/cindydunn/Desktop/Grad_School/math540/logistic_regression/myfunctions copy.r")

RNGkind (sample.kind = "Rounding") 
set.seed(0)
p <- partition.2(wine, 0.7)
training.data <- p$data.train
test.data <- p$data.test

logistic.model <- glm(Type ~ ., family = binomial(link='logit'), data=training.data)

#backwards stepwise 
step.model <- step(logistic.model)


summary(step.model)
confint.default(step.model)
exp(confint.default(step.model))

library(caret)
pred.prob.test <- predict(step.model, newdata = test.data, type = "response")
pred.y.test <- ifelse(pred.prob.test > 0.5, 1, 0)
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$Type), positive = "1")

# Create ROC
library(pROC)
r <- roc(test.data$Type, pred.prob.test)
plot.roc(r, main = "ROC curve")
auc(r)