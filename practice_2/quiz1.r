#cayden dunn

#q1
prop_val_df <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/practice_2/data/Hospital.csv") # read in data

#Create a subset with variables Stay, Age, InfctRsk, Culture,    Xray,    Beds,    MedSchool,    Region,    Census,    Nurses,    Facilities
prop_val_df_subset <- prop_val_df[,c(2,3,4,5,6,7,8,9,10,11,12)] # create subset

#q2
## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/practice_2/myfunctions copy.r")

RNGkind (sample.kind = "Rounding") # set seed
set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(prop_val_df_subset, 0.7) ## creating 70:30 partition
training.data <- p2$data.train ## training data
test.data <- p2$data.test ## test data

#q3
## Fit MLR model
mlr <- lm(InfctRsk ~ Stay + Age + Culture + Xray + Beds + factor(MedSchool) + factor(Region) + Census + Nurses + Facilities, data=training.data) # fit MLR model
mlr

#q4
# Based on the output, what percentage of variation in the dependent variable is explained by the regression model?
summary(mlr)$r.squared

#q5
#Use the fitted model to make prediction on the test data and report the RMSE
y_hat <- predict(mlr, test.data) # predict response for test data
RMSE <- sqrt(mean((y_hat - test.data$InfctRsk)^2)) # calculate RMSE
round(RMSE, digits = 4) # round to 4 digits