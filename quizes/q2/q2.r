

ub_df <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/quizes/q2/data/UniversalBank.csv") # read in data
#q1
# Drop variables ID and zip code
banking <- ub_df[,-1]
banking <- banking[,-4]
# treat Education as categorical
banking$Education <- factor(banking$Education, levels = c(1,2,3),
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/quizes/q2/myfunctions copy.r")

# Create training validation test data
RNGkind (sample.kind = "Rounding") 
set.seed(123) ## set seed so that you get same partition each time
p3 <- partition.3(banking, 0.7, 0.2) ## creating 70:20:10 partition
training.data <- p3$data.train # training data
validation.data <- p3$data.val # validation data
test.data <- p3$data.test # test data

#q2
#Fit a logistic model on training data by regressing Personal.Loan on the rest of the variables.
logit.model <- glm(Personal.Loan ~ ., data = training.data, family = binomial)

#q3 (come back to)
# 3. How much does the odds of getting personal loan change if the Education category is moved from "Undergrad" to "Graduate"? Write your answer in complete sentence(s).
# odds ratio for Graduate vs. Undergrad
logit.model$coefficients
exp(coef(logit.model)["EducationGraduate"])
# The odds of competitiveness increases multiplicatively by 74.50514 times times when the currency variable changes from Undergrad to Graduate and the other predictors remain constant
# the odds of getting a personal loan increases by a very large factor if the Education category is moved from "Undergrad" to "Graduate"

#q4
#a
# Create a confusion table on the test data using cutoff-value 0.5  (i.e. estimated response = 1 if estimated probability >= 0.5). Report the sensitivity
library(caret)
pred.prob.val <- predict(logit.model, newdata = validation.data, type = "response")
pred.y.val <- ifelse(pred.prob.val > 0.5, 1, 0)
confusionMatrix(as.factor(pred.y.val), as.factor(validation.data$Personal.Loan), positive = "1")
# Sensitivity : 0.6957

#b
#Create a confusion table on the test data using cutoff-value 0.1  (i.e. estimated response = 1 if estimated probability >= 0.1). Report the sensitivity
library(caret)
pred.prob.val <- predict(logit.model, newdata = validation.data, type = "response")
pred.y.val <- ifelse(pred.prob.val > 0.1, 1, 0)
confusionMatrix(as.factor(pred.y.val), as.factor(validation.data$Personal.Loan), positive = "1")
# Sensitivity : 0.9022

#c 
#the sensativity incrased from 0.6957 to 0.9022 because the cutoff value decreased from 0.5 to 0.1 which means that the model is more likely to predict a positive response when the probability is higher
# the cuttoff being lower means that the model is more likely to predict a positive response when the probability is higher therefore we see an increase in sensitivity