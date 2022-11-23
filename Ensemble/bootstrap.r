set.seed(1)
library(rsample)
height <- as.data.frame(round(runif(15, 60, 72)))
## original data
t(height)

## traditional confidence intervals
c(mean(height[,1]) - qt(.975,14)*sd(height[,1])/sqrt(15), mean(height[,1]) + qt(.975,14)*sd(height[,1])/sqrt(15))
## traditional confidence intervals using R function
library(Rmisc)
CI(height[,1], ci=0.95)

##################################
### Get CI using bootstrapping ###
##################################

## create 2000 bootstrap samples
bt_samples <- bootstraps(height, times = 2000)
## example of first bootstrap sample
height[bt_samples$splits[1][[1]]$in_id,]
## example of second bootstrap sample
height[bt_samples$splits[2][[1]]$in_id,]

## calculate mean on each bootstrap sample
bt_mean <- rep(NA, 2000)
for (i in 1:2000){
  bt_mean[i] <- mean(height[bt_samples$splits[i][[1]]$in_id,])
}
hist(bt_mean, main = "sampling distribution of bootstrap mean")
## 95% confidence interval for mean height using bootstrapping
quantile(bt_mean, probs = c(0.025, 0.975))