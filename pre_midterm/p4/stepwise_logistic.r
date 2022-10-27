diabetes <- read.csv("E:/Data mining/datasets/diabetes.csv")
diabetes$Outcome <- as.factor(diabetes$Outcome)
levels(diabetes$Outcome) <- c("no", "yes")

## Include the functions required for data partitioning
source("E:/Data mining/Lecture Notes/myfunctions.R")

set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(diabetes, 0.7) ## creating 70:30 partition
training.data <- p2$data.train
test.data <- p2$data.test

#######################
## Create full model ##
#######################
logistic_full <- glm(Outcome ~ ., family=binomial(link='logit'),data=training.data)
summary(logistic_full)
library(car)
vif(logistic_full)

# Confusion matrix for test data
pred.prob.test <- predict(logistic_full, newdata = test.data,type = "response")
pred.y.test <- ifelse(pred.prob.test > 0.5, "yes", "no") # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$Outcome), 
                positive = "yes")

###########################
### Stepwise selection ####
###########################
#backward selection
summary(logistic_full <- glm(Outcome ~ ., family=binomial(link='logit'),data=training.data))
step.model <- step(logistic_full)
summary(step.model)

# Confusion matrix for test data
pred.prob.test <- predict(step.model, newdata = test.data,type = "response")
pred.y.test <- ifelse(pred.prob.test > 0.5, "yes", "no") # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$Outcome), 
                positive = "yes")


# forward selection
summary(logistic_null <- glm(Outcome ~ 1, family=binomial(link='logit'), data = training.data))
step.model.fwd <- step(logistic_null, scope = ~ Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, direction = "forward")
summary(step.model.fwd)


# forward stepwise selection
summary(logistic_null <- glm(Outcome ~ 1, family=binomial(link='logit'), data = training.data))
step.model.both <- step(logistic_null, scope = ~ Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, direction = "both")
summary(step.model.both)



# backward stepwise selection
summary(logistic_full <- glm(Outcome ~., family=binomial(link='logit'), data = training.data))
step.model.both <- step(logistic_full, scope = ~ Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, direction = "both")
summary(step.model.both)


###########################
### Stepwise selection ####
### w Cross Validation ####
###########################

library(caret)

## K-fold Cross Validation
# value of K equal to 5 
set.seed(0)
train_control <- trainControl(method = "cv", 
                              number = 5) 

# Fit K-fold CV model  
step_kcv <- train(Outcome ~ ., data = training.data, family = "binomial", 
                 method = "glmStepAIC", trControl = train_control) 
print(step_kcv)
step_kcv$finalModel

# Confusion matrix for test data
pred.prob.test <- predict(step_kcv$finalModel, newdata = test.data,type = "response")
pred.y.test <- ifelse(pred.prob.test > 0.5, "yes", "no") # using cutoff = 0.5
confusionMatrix(as.factor(pred.y.test), as.factor(test.data$Outcome), 
                positive = "yes")

