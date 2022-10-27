# Euclidean distance
# Suppose the scores for midterm1, midterm2 and final for Student A are (80, 78, 85) and the same for Student C are (77, 80, 81). Find theEuclidean distance between Student A and Student C. Round to 4 places after decimal.
# Student A are (80, 78, 85)
#Student C are (77, 80, 81)
sqrt((80-77)^2+(78-80)^2+(85-81)^2)


autompg <- read.csv("/Users/cindydunn/Desktop/Grad_School/math540/p5/data/autompg.csv")

## Include the functions required for data partitioning
source("/Users/cindydunn/Desktop/Grad_School/math540/p5/myfunctions copy.r")

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### call the function for creating 70:20:10 partition
p2 <- partition.2(autompg, 0.8)
training.data <- p2$data.train
test.data <- p2$data.test

### Rescale the data
#center is the mean of each column
#scale is the standard deviation of each column
# the -9 drops that coulmn from the dataframe
training.scaled <- scale(training.data[,-1], center = TRUE, scale = TRUE) # exclude the response vairable from the data set for scaling
training.scaled.wY <- cbind(training.scaled, training.data[,1])
training.scaled.attr <- attributes(training.scaled)
test.scaled <- scale(test.data[,-1], 
                    center = training.scaled.attr$`scaled:center`, 
                    scale = training.scaled.attr$`scaled:scale`)


library(FNN)
Knn <- knn.reg(train = training.scaled, test = test.scaled,
           y = training.data[,1], k = 5)
Knn

library(caret)
## K-fold Cross Validation
# value of K equal to 10 
set.seed(0)
train_control <- trainControl(method = "cv", 
                              number = 10) 
# Specify tuning parameter using tuneGrid
tg <- data.frame(k = seq(1,350,5))
Knn_kcv2 <- train(mpg ~ ., data = training.data, method = "knn", 
                 trControl = train_control, preProcess = c("center","scale"), 
                 tuneGrid = tg, metric = "RMSE")
print(Knn_kcv2)

# plot(Knn_kcv)
# Knn_kcv$finalModel



### Fit kNN model on a single new observation with k=5
newObs <- data.frame(cyl=8, disp=302,  hp=160,   wt=3500,  acc=12, year=70)

#scale entire data set
#center is the mean of each column
#scale is the standard deviation of each column
auto.scaled <- scale(autompg[,-1], center = TRUE, scale = TRUE) # exclude the response vairable from the data set for scaling
auto.scaled.attr <- attributes(auto.scaled)
newObs.scaled <- scale(newObs, 
                    center = auto.scaled.attr$`scaled:center`, 
                    scale = auto.scaled.attr$`scaled:scale`)
library(FNN)
Knn <- knn.reg(train = auto.scaled, test = newObs.scaled,
           y = autompg$mpg, k = 390)
Knn