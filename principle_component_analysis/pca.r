rm(list = ls())
autompg <- read.csv("~/Desktop/teaching 2022 fall/Math 540&440 statistical learning/yang/datasets/autompg.csv")

## Include the functions required for data partitioning
source("~/Desktop/teaching 2022 fall/Math 540&440 statistical learning/yang/R files/myfunctions.R")

###############################################
## Create training, validation and test data ##
###############################################
Dat <- autompg
RNGkind (sample.kind = "Rounding") 
set.seed(0) ## set seed so that you get same partition each time
p3 <- partition.3(Dat, 0.7, 0.2) ## creating 70:20:10 partition
training.data <- p3$data.train
validation.data <- p3$data.val
test.data <- p3$data.test

######################################
## Multiple linear regression model ##
######################################

## Fit MLR model on training data
mlr.train <- lm(mpg ~ ., data = training.data)
summary(mlr.train)

# RMSE for training data
error.train <-  training.data$mpg - mlr.train$fitted.values
rmse.train <- sqrt(mean(error.train^2))
rmse.train

# prediction on validation data
yhat = predict(mlr.train, newdata=validation.data)
# RMSE for validation data
error.val <- validation.data$mpg - yhat
rmse.val <- sqrt(mean(error.val^2))
rmse.val

# prediction on test data
yhat = predict(mlr.train, newdata=test.data)
# RMSE for test data
error.test <- test.data$mpg - yhat
rmse.test <- sqrt(mean(error.test^2))
rmse.test

#################################
## Create Principal components ##
#################################
# Check correlation
library(corrplot)
corvar <- cor(training.data[,-1], method="pearson")
corrplot(corvar, method= "color", order = "hclust", tl.pos = 'lt')
# dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/corplot1.pdf")

# Scale the data
training.scaled <- scale(training.data[,-1], center = TRUE, scale = TRUE)

# Create principal components
pca <- prcomp(training.scaled, center = TRUE, scale. = TRUE) 
summary(pca)
pca$rotation
pcs <- as.data.frame(pca$x)

# Plot principal components
plot(pca)
biplot (pca , scale =0)

# Check correlation of PCs
corvarpc <- cor(pcs, method="pearson")
corrplot(corvarpc, method= "color", order = "hclust", tl.pos = 'lt')
# dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/corplot2.pdf")

# Percent variance explained by each PC
summPC <- summary(pca)
plot(summary(pca)$importance[3,], pch = 19, ylab = "Cumulative Proportion of Variance", xlab = "Index of PC")
# dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/pcplot1.pdf")

# Scatter plot of scaled response vs. PCs
par(mfrow = c(1, 3))
plot(training.data$mpg, pcs$PC1)
plot(training.data$mpg, pcs$PC2)
plot(training.data$mpg, pcs$PC3)

############################################
## MLR using Principal component analysis ##
############################################

# MLR using all PCs
pcs <- as.data.frame(pca$x)
lr.data <- cbind(training.data$mpg, pcs) # create a data set with mpg and principal components
colnames(lr.data)[1] <- "mpg"
mlr.pc.train <- lm(mpg ~ PC1+PC2+PC3+PC4+PC5+PC6, data = lr.data)
summary(mlr.pc.train)

# Scaling validation data
training.scaled.attr <- attributes(training.scaled)
val.scaled <- scale(validation.data[,-1], 
                    center = training.scaled.attr$`scaled:center`, 
                    scale = training.scaled.attr$`scaled:scale`)

# Create PC's on validation data
val.pcs <- matrix(NA, nrow = nrow(val.scaled), ncol = ncol(val.scaled))
loading <- pca$rotation
# val.pcs[,1] <- loading[1,1]*val.scaled[,1] + loading[2,1]*val.scaled[,2] + 
#   loading[3,1]*val.scaled[,3] + loading[4,1]*val.scaled[,4] +
#   loading[5,1]*val.scaled[,5] + loading[6,1]*val.scaled[,6]
# val.pcs[,1] <- val.scaled%*%loading[,1]
val.pcs <- val.scaled%*%loading

# Model performance on validation data
yhat = predict(mlr.pc.train, newdata=as.data.frame(val.pcs))
error.val.pc <- validation.data$mpg - yhat
rmse.val.pc <- sqrt(mean(error.val.pc^2))
rmse.val.pc

#########################################
# Determining the optimal number of PCs #
#########################################

rmse.val.pc <- rep(NA, 6)
# Model with 1 PC
mlr.pc1.train <- lm(mpg ~ PC1, data = lr.data)
yhat = predict(mlr.pc1.train, newdata=as.data.frame(val.pcs))
error.val.pc <- validation.data$mpg - yhat
rmse.val.pc[1] <- sqrt(mean(error.val.pc^2))
# Model with 2 PC
mlr.pc2.train <- lm(mpg ~ PC1+PC2, data = lr.data)
yhat = predict(mlr.pc2.train, newdata=as.data.frame(val.pcs))
error.val.pc <- validation.data$mpg - yhat
rmse.val.pc[2] <- sqrt(mean(error.val.pc^2))
# Model with 3 PC
mlr.pc3.train <- lm(mpg ~ PC1+PC2+PC3, data = lr.data)
yhat = predict(mlr.pc3.train, newdata=as.data.frame(val.pcs))
error.val.pc <- validation.data$mpg - yhat
rmse.val.pc[3] <- sqrt(mean(error.val.pc^2))
# Model with 4 PC
mlr.pc4.train <- lm(mpg ~ PC1+PC2+PC3+PC4, data = lr.data)
yhat = predict(mlr.pc4.train, newdata=as.data.frame(val.pcs))
error.val.pc <- validation.data$mpg - yhat
rmse.val.pc[4] <- sqrt(mean(error.val.pc^2))
# Model with 5 PC
mlr.pc5.train <- lm(mpg ~ PC1+PC2+PC3+PC4+PC5, data = lr.data)
yhat = predict(mlr.pc5.train, newdata=as.data.frame(val.pcs))
error.val.pc <- validation.data$mpg - yhat
rmse.val.pc[5] <- sqrt(mean(error.val.pc^2))
# Model with 6 PC
mlr.pc6.train <- lm(mpg ~ PC1+PC2+PC3+PC4+PC5+PC6, data = lr.data)
yhat = predict(mlr.pc6.train, newdata=as.data.frame(val.pcs))
error.val.pc <- validation.data$mpg - yhat
rmse.val.pc[6] <- sqrt(mean(error.val.pc^2))
plot(seq(1:6), rmse.val.pc, xlab = "# PCs used in model", ylab = "RMSE", pch = 19, type = "l")
abline(v=3, col = "blue", lty=3)
# dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/pcplot2.pdf")

############################################################
# Fit final model on combined training and validation data #
############################################################
training.data.all <- rbind(training.data, validation.data)
# Scale the data
training.scaled.all <- scale(training.data.all[,-1], center = TRUE, scale = TRUE)
# Create principal components
pca <- prcomp(training.scaled.all, center = TRUE, scale. = TRUE) 
pcs <- as.data.frame(pca$x)
# MLR using all PCs
lr.data.all <- cbind(training.data.all$mpg, pcs) # create a data set with mpg and principal components
colnames(lr.data.all)[1] <- "mpg"
mlr.pc.train.all <- lm(mpg ~ PC1+PC2+PC3, data = lr.data.all) # fit model with 3 principal components
# Scaling test data
training.scaled.all.attr <- attributes(training.scaled.all)
test.scaled <- scale(test.data[,-1], 
                    center = training.scaled.all.attr$`scaled:center`, 
                    scale = training.scaled.all.attr$`scaled:scale`)

# Create PC's on test data
loading <- pca$rotation
test.pcs <- test.scaled%*%loading
# Model performance on test data
yhat = predict(mlr.pc.train.all, newdata=as.data.frame(test.pcs))
error.test.pc <- test.data$mpg - yhat
rmse.test.pc <- sqrt(mean(error.test.pc^2))
rmse.test.pc
