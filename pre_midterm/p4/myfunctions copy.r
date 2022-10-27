### R function for creating 3 partitions with training, validation and testing set
### Input: original dataset name (data)
###        proportion of records assigned to training set (prop.train)
###        proportion of records assigned to validation set (prop.val)
### Output: data.train, data.val, data.test

partition.3 <- function(data, prop.train, prop.val){
  # select a random sample of size = prop.train % of total records
  selected1 <- sample(1:nrow(data), round(nrow(data)*prop.train), replace = FALSE) 
  # create training data which has prop.train % of total records
  data.train <- data[selected1,]
  # select a random sample of size = prop.val % of the total records
  rest <- setdiff(1:nrow(data), selected1)
  selected2 <- sample(rest, round(nrow(data)*prop.val), replace = FALSE) 
  # create validation data which has prop.val % of total records
  data.val <- data[selected2,]
  # create testing data with the remaining records
  data.test <- data[setdiff(rest, selected2),]
  return(list(data.train=data.train, data.test=data.test, data.val=data.val))
}


### R function for creating 2 partitions with training and test set
### Input: original dataset name (data)
###        proportion of records assigned to training set (prop.train)
### Output: data.train, data.test

partition.2 <- function(data, prop.train){
  # select a random sample of size = prop.train % of total records
  selected <- sample(1:nrow(data), round(nrow(data)*prop.train), replace = FALSE) 
  # create training data which has prop.train % of total records
  data.train <- data[selected,]
  # create validation data
  rest <- setdiff(1:nrow(data), selected)
  data.test <- data[rest,]
  return(list(data.train=data.train, data.test=data.test))
}


### R function for calculating Euclidean distance

eucl.dist <- function(X, Y){
  dist <- sqrt(sum((X - Y)^2))
  return(dist)
}