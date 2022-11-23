# read in the data
autompg <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/p7/data/autompg.csv")

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/p6/myfunctions copy.r")

###############################################
## Create training, validation and test data ##
###############################################
Dat <- autompg
RNGkind (sample.kind = "Rounding") 
set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(Dat, 0.7)
training.data <- p2$data.train
test.data <- p2$data.test

# Fit a bagged tree using cross validation method.

library(caret)
set.seed(0)
modelLookup("treebag")
train_control <- trainControl(method="cv", number=10)
## specify nbagg to control the number of trees. default value is 25 
bag <- train(mpg ~ . , data = training.data, method = "treebag",
               trControl = train_control, nbagg = 50)
print(bag)

# get prediction on the test data
pred.test.bag = predict(bag$finalModel, test.data)

# compute the mean squared error
mean((pred.test.bag - test.data$mpg)^2)

# Fit a random forest model using cross validation method.

library(caret)
set.seed(0)
modelLookup("rf")
train_control <- trainControl(method="cv", number=10)
rf <- train(mpg ~ . , data = training.data, method = "rf",
             trControl = train_control, tuneLength = 3)
# metric = "Kappa" may be mentioned if best tree should be selected based on that
## specify tuning parameters using tuneGrid
# rf <- train(Outcome ~ . , data = training.data, method = "rf", ntree = 50,
#                trControl = train_control, tuneGrid = expand.grid(mtry = c(2,5,8)))
print(rf)

# get prediction on the test data
pred.test.rf = predict(rf$finalModel, test.data)

# compute the mean squared error
mean((pred.test.rf - test.data$mpg)^2)
