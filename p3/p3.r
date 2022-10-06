diabetes <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/logistic_regression/diabetes.csv")
## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/logistic_regression/myfunctions copy.r")

RNGkind (sample.kind = "Rounding") 
set.seed(0) ## set seed so that you get same partition each time
p3 <- partition.3(diabetes, 0.7, 0.2) ## creating 70:20:10 partition
training.data <- p3$data.train
validation.data <- p3$data.val
test.data <- p3$data.test

#q1
logistic.model <- glm(Outcome ~ ., family = binomial(link='logit'), data=training.data) ## fit logistic regression model
summary(logistic.model)

#q2
#How does the odds of being diagnosed with diabetes change if there is an one unit increase in BMI?
#find what our coefficient is for BMI
logistic.model$coefficients
#take the exponent of the coefficient which gives us the odds ratio
exp(0.088991812)
#output is 1.093
#Odds ratio > 1 implies that if BMI is increased by one unit then the odds of success becomes higher


#q3
#How does the odds of being diagnosed with diabetes change if age increases by 10 years?
#find what our coefficient is for age
logistic.model$coefficients
#If we are interested in d unit increase in the regressor variable x, then the corresponding odds ratio can be expressed as: for this d= 10
exp(0.013771855*10)
#Odds ratio of 1.147652 implies that if age is increased by 10 units then the odds of being diagnosed becomes higher

#q4
#For a 50 year old person with no pregnancies and Glucose = 90, BloodPressure = 74, SkinThickness = 23, Insulin = 0,  BMI = 33,  DiabetesPedigreeFunction = 0.7, predict the outcome. Use cutoff = 0.5.
x0 <- data.frame(Age = 50, Pregnancies = 0, Glucose = 90, BloodPressure = 74, SkinThickness = 23, Insulin = 0,  BMI = 33,  DiabetesPedigreeFunction = 0.7)
predict(logistic.model, newdata = x0,type = "response")
# output = 0.1301474 which is less than 0.5 so we predict that the outcome is 0
# Since the estimated success probability is less than 0.5, the estimated response is 0

#q5
#Create a confusion matrix on the test data. Report the following: 1. Overall accuracy 2. Sensitivity 3. Specificity 4. Kappa statistic
# Confusion matrix for test data
library(caret)
pred.prob.test <- predict(logistic.model, newdata = test.data,type = "response")
pred.y.test <- ifelse(pred.prob.test > 0.5, 1, 0) # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$Outcome), positive = "1")
# Overall accuracy =  Accuracy : 0.8158  
# Sensitivity = Sensitivity : 0.7083 
# Specificity = Specificity : 0.8654
# Kappa statistic = Kappa : 0.5737

pred.prob.val <- predict(logistic.model, newdata = validation.data, type = "response")
pred.y.val <- ifelse(pred.prob.val > 0.5, 1, 0)
confusionMatrix(as.factor(pred.y.val), as.factor(validation.data$Outcome), positive = "1")$overall['Kappa']

#q6
# Try different cutoff values (0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9) and create a confusion matrix for validation data. Select the best cutoff for this problem based on the Kappa coefficient. Report the best cutoff.
# Confusion matrix for validation data
pred.prob.val <- predict(logistic.model, newdata = validation.data,type = "response")
set.seed(1)
cutoff <- seq(0.1, 0.9, by = 0.1)
# make a new dataframe to store cutoff values and kappa values
cutoff_and_kappa_df <- data.frame(cutoff = cutoff, kappa =0)
# create a confusion matrix for validation data for each cutoff value
for (i in 1:length(cutoff)) {
  pred.y.val <- ifelse(pred.prob.val > cutoff[i], 1, 0)
  # store the kappa value for each cutoff value
  cutoff_and_kappa_df[i,2] <- confusionMatrix(as.factor(pred.y.val), as.factor(validation.data$Outcome), positive = "1")$overall['Kappa']
}
# find the largest kappa value in the dataframe and the corresponding cutoff value
cutoff_and_kappa_df[cutoff_and_kappa_df$kappa == max(cutoff_and_kappa_df$kappa),]

#q7
# Combine training and validation data. Fit logistic regression model on the combined data using the best cutoff. Test this model on the test data and report the following: 1. Overall accuracy 2. Sensitivity 3. Specificity 4. Kappa statistic
#combine training and validation data
combined.data <- rbind(training.data, validation.data)
#fit logistic regression model on the combined data using the best cutoff. Cutoff = 0.4
logistic.model <- glm(Outcome ~ ., family = binomial(link='logit'), data=combined.data)
# Confusion matrix for test data
pred.prob.test <- predict(logistic.model, newdata = test.data,type = "response")
pred.y.test <- ifelse(pred.prob.test > 0.4, 1, 0) # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$Outcome), positive = "1")
# Overall accuracy =  Accuracy : 0.8289
# Sensitivity = Sensitivity : 0.7500
# Specificity = Specificity : 0.8654
# Kappa statistic = Kappa : 0.6086

#q8
# Create a ROC curve for the test data. Report the AUC.
# Create ROC
library(pROC)
# use this for getting ROC curve on training data
r <- roc(combined.data$Outcome, logistic.model$fitted.values)
# use this for getting ROC curve on test data
r <- roc(test.data$Outcome, pred.prob.test)
plot.roc(r, main = "ROC curve")# plot ROC curve

#compute Area under curve
auc(r)
# Area under the curve: 0.8598

#q9
#Create a lift chart for the model fitted in the above question on the test data. What percentage of people with positive diabetes outcome is captured in the top 30%?
# Create Lift chart
library(gains)
# use this for getting Lift chart on training data
gain <- gains(combined.data$Outcome, logistic.model$fitted.values)
# use this for getting Lift chart on test data
gain <- gains(test.data$Outcome, pred.prob.test)
gain

# use depth of file to determine decile and then cume pct of total resp to determine What percentage of positive outcomes are captured in given decile 
# in this case at depth of 29 we have 66.7% of positive outcomes captured