rm(list = ls())

##############################
### knn for classification ###
##############################

diabetes <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/K-nearest_neighbor/data/diabetes.csv")
diabetes$Outcome <- as.factor(diabetes$Outcome)
levels(diabetes$Outcome) <- c("no", "yes")

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/K-nearest_neighbor/myfunctions copy.r")

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### call the function for creating 70:20:10 partition
p3 <- partition.3(diabetes, 0.7, 0.2)
training.data <- p3$data.train
validation.data <- p3$data.val
test.data <- p3$data.test

### Rescale the data
training.scaled <- scale(training.data[,-9], center = TRUE, scale = TRUE)
training.scaled.wY <- cbind(training.scaled, training.data[,9])
training.scaled.attr <- attributes(training.scaled)
val.scaled <- scale(validation.data[,-9], 
                    center = training.scaled.attr$`scaled:center`, 
                    scale = training.scaled.attr$`scaled:scale`)
test.scaled <- scale(test.data[,-9], 
                    center = training.scaled.attr$`scaled:center`, 
                    scale = training.scaled.attr$`scaled:scale`)


### Fit kNN model on a single new observation with k=5
newObs <- data.frame(Pregnancies = 3, Glucose = 120, BloodPressure = 70, 
                     SkinThickness = 20, Insulin = 80, BMI = 30,  
                     DiabetesPedigreeFunction = 0.44, Age = 46)
newObs.scaled <- scale(newObs, 
                    center = training.scaled.attr$`scaled:center`, 
                    scale = training.scaled.attr$`scaled:scale`)
library(FNN)
Knn <- knn(train = training.scaled, test = newObs.scaled,
           cl = training.data[,9], k = 5)
Knn

## labels of nearest neighbors
Knn.attr <- attributes(Knn)
training.data[Knn.attr$nn.index,9]


### Using different cutoff values
# First we need to collect the labels of the nearest neighbors
k.labels <- training.data[Knn.attr$nn.index,9]
K <- 5
cutoff <- 0.3 # if proportion of occurrences of class "yes" > cutoff then predicted label = "yes"
pred <- ifelse((sum(k.labels == "yes")/K) >= cutoff, "yes", "no")


### fit k-nn model on validation data with k=5
library(FNN)
library(caret)
Knn <- knn(train = training.scaled, test = val.scaled,
           cl = training.data[,9], k = 5)
confusionMatrix(as.factor(Knn), as.factor(validation.data[,9]), 
                positive = "yes")

### fit k-nn model for k = 1, ..., 100
K <- 100
kappa <- rep(0, K)
for (kk in 1:K){
  Knn <- knn(train = training.scaled, test = val.scaled,
             cl = training.data[,9], k = kk)
  c <- confusionMatrix(as.factor(Knn), as.factor(validation.data[,9]), 
                       positive = "yes")
  kappa[kk] <- c$overall["Kappa"]
  cat("K", kk, "Kappa", kappa[kk], "\n")
}

# create a plot for k vs. kappa
plot(c(1:K), kappa, xlab = "k", ylab = "Kappa", type = "l", col = "blue")
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/knn_class1.pdf")


### fit k-nn model on test data with k=44
training.data.all <- rbind(training.data, validation.data)
training.data.scaled.all <- rbind(training.scaled, val.scaled)
Knn <- knn(train = training.data.scaled.all, test = test.scaled,
           cl = training.data.all[,9], k = 44)
confusionMatrix(as.factor(Knn), as.factor(test.data[,9]), 
                positive = "yes")



## K-fold Cross Validation
# value of K equal to 10 
set.seed(0)
train_control <- trainControl(method = "cv", 
                              number = 10) 
training.data.all <- rbind(training.data, validation.data)
# Fit K-fold CV model  
Knn_kcv <- train(Outcome ~ ., data = training.data.all, method = "knn", 
                 trControl = train_control, preProcess = c("center","scale"), 
                 tuneLength = 20, metric = "Kappa")
print(Knn_kcv)


## Specify tuning parameter using tuneGrid
# tg <- data.frame(k = seq(1,50,2))
# Knn_kcv2 <- train(Outcome ~ ., data = training.data.all, method = "knn", 
#                  trControl = train_control, preProcess = c("center","scale"), 
#                  tuneGrid = tg, metric = "Kappa")
# print(Knn_kcv2)

plot(Knn_kcv)
Knn_kcv$finalModel
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/knn_class2.pdf")


### fit k-nn model on test data with k=11
training.data.scaled.all <- rbind(training.scaled, val.scaled)
Knn <- knn(train = training.data.scaled.all, test = test.scaled,
           cl = training.data.all[,9], k = 11)
confusionMatrix(as.factor(Knn), as.factor(test.data[,9]), 
                positive = "yes")