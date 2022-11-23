ebay <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/pre_midterm/logistic_regression/eBayAuctions2.csv")

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/pre_midterm/logistic_regression/myfunctions copy.r")

#########################################
## Create training and test data ##
#########################################
RNGkind (sample.kind = "Rounding") 
set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(ebay, 0.7) ## creating 70:30 partition
training.data <- p2$data.train
test.data <- p2$data.test

###################################
## Fit logistic regression model ##
###################################

logistic.model <- glm(Competitive ~ ., family=binomial(link='logit'),data=training.data) ## fit logistic regression model
summary(logistic.model) ## summary of logistic regression model
confint.default(logistic.model) ## confidence interval for regression coefficients

# install.packages('caret', dependencies = TRUE)
library(caret)

# Confusion matrix for training data
pred.prob.train <- logistic.model$fitted.values # predicted probabilities
pred.y.train <- ifelse(pred.prob.train > 0.5, 1, 0) # using cutoff = 0.5 
confusionMatrix(as.factor(pred.y.train), as.factor(training.data$Competitive), positive = "1")# Confusion matrix for test data

# Confusion matrix for test data
pred.prob.test <- predict(logistic.model, newdata = test.data,type = "response")
pred.y.test <- ifelse(pred.prob.test > 0.5, 1, 0) # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$Competitive), 
                positive = "1")


######################
## Create ROC curve ##
######################

# Create ROC
library(pROC)
# use this for getting ROC curve on training data
r <- roc(training.data$Competitive, logistic.model$fitted.values)
# use this for getting ROC curve on test data
r <- roc(test.data$Competitive, pred.prob.test)
plot.roc(r, main = "ROC curve")# plot ROC curve

#compute Area under curve
auc(r)

# dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/roc.pdf")

################
## Lift chart ##
################

# Create Lift chart
library(gains)
# use this for getting Lift chart on training data
gain <- gains(training.data$Competitive, logistic.model$fitted.values)
# use this for getting Lift chart on test data
gain <- gains(test.data$Competitive, pred.prob.test)
gain

# Plot Lift chart: Percent cumulative response
x <- c(0, gain$depth)
pred.y <- c(0, gain$cume.pct.of.total)
avg.y <- c(0, gain$depth/100)
plot(x, pred.y, main = "Cumulative Lift Chart", xlab = "deciles", 
     ylab = "Percent cumulative response", type = "l", col = "red", cex.lab = 1.5)
lines(x, avg.y, type = "l")

# dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/lift1.pdf")