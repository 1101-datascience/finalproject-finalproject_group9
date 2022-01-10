args = commandArgs(trailingOnly=TRUE)
i<-1 
train_word="--train"
test_word="--test"
predict_word="--predict"
while(i < length(args))
{
  if(args[i] == "--train"){
    train <- args[i+1]
    i <- i+1
  }else if(args[i] == "--test"){
    test <- args[i+1]
    i<-i+1
  }else if(args[i]== "--predict"){
    predict <- args[i+1]
  }else{
    if (train_word %in% args==FALSE || test_word %in% args==FALSE 
        || predict_word %in% args==FALSE || performance_word %in% args==FALSE){
      stop("Error: missing --train/--test/--predict")
    }else{
      stop(paste("Unknown flag", args[i]), call.=FALSE)
    }
    
  }
  i<-i+1
}
if(!require(ModelMetrics)) install.packages("ModelMetrics",repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost",repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate",repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest",repos = "http://cran.us.r-project.org")

library(ModelMetrics)
library(lubridate)
library(xgboost)
library(randomForest)

## Time count outliner
eda1 <- function(train_data,test_data){
  train_data$datetime <- ymd_hms(train_data$datetime)
  train_data$month <- month(train_data$datetime)
  train_data$wday <- wday(train_data$datetime)
  train_data$hour <- hour(train_data$datetime)
  train_data <- subset(train_data, select = -datetime)
  test_data$datetime <- ymd_hms(test_data$datetime)
  test_data$month <- month(test_data$datetime)
  test_data$wday <- wday(test_data$datetime)
  test_data$hour <- hour(test_data$datetime)
  test_data <- subset(test_data, select = -datetime)
  return(list(train_data,test_data))
}
## Time count outliner
eda2 <- function(train_data,test_data){
  train_data <- train_data[-which(train_data$count >= 3*sd(train_data$count)), ]
  train_data$datetime <- ymd_hms(train_data$datetime)
  train_data$month <- month(train_data$datetime)
  train_data$wday <- wday(train_data$datetime)
  train_data$hour <- hour(train_data$datetime)
  train_data <- subset(train_data, select = -datetime)
  test_data$datetime <- ymd_hms(test_data$datetime)
  test_data$month <- month(test_data$datetime)
  test_data$wday <- wday(test_data$datetime)
  test_data$hour <- hour(test_data$datetime)
  test_data <- subset(test_data, select = -datetime)
  return(list(train_data,test_data))
}
eda6 <- function(train_data,test_data){
  # remove outlier in train_data$count
  train_data <- train_data[-which(train_data$count >= 3*sd(train_data$count)), ]
  # fetch month, wday and hour from datetime
  raw_test_data <- test_data
  train_data$datetime <- ymd_hms(train_data$datetime)
  train_data$month <- month(train_data$datetime)
  train_data$wday <- wday(train_data$datetime)
  train_data$hour <- hour(train_data$datetime)
  train_data <- subset(train_data, select = -datetime)
  
  test_data$datetime <- ymd_hms(test_data$datetime)
  test_data$month <- month(test_data$datetime)
  test_data$wday <- wday(test_data$datetime)
  test_data$hour <- hour(test_data$datetime)
  test_data <- subset(test_data, select = -datetime)
  
  # group by period
  train_data$hour_group <- rep(100, nrow(train_data))
  test_data$hour_group <- rep(100, nrow(test_data))
  
  group_0 <- c(0, 1, 2, 3, 4, 5)
  group_1 <- c(6, 23, 22, 21, 10, 11)
  group_2 <- c(7, 9, 20, 14, 15, 12)
  group_3 <- c(8, 13, 16, 17, 18, 19)
  
  train_data$hour_group[which(train_data$hour %in% group_0)] <- 0
  train_data$hour_group[which(train_data$hour %in% group_1)] <- 1
  train_data$hour_group[which(train_data$hour %in% group_2)] <- 2
  train_data$hour_group[which(train_data$hour %in% group_3)] <- 3
  
  #summary(train_data)
  
  test_data$hour_group[which(test_data$hour %in% group_0)] <- 0
  test_data$hour_group[which(test_data$hour %in% group_1)] <- 1
  test_data$hour_group[which(test_data$hour %in% group_2)] <- 2
  test_data$hour_group[which(test_data$hour %in% group_3)] <- 3
  
  #summary(test_data)
  return(list(train_data,test_data))
}

train = "data/train.csv"
test = "data/test.csv"


traindata = read.csv(train, header=T)
testdata = read.csv(test, header=T)
datetime = testdata[,1]


data1 = eda1(traindata, testdata)
data2 = eda2(traindata, testdata)
data6 = eda6(traindata, testdata)


traindata1 = data1[[1]]
testdata1 = data1[[2]]
traindata2 = data2[[1]]
testdata2 = data2[[2]]
traindata6 = data6[[1]]
testdata6 = data6[[2]]


d1 = subset(traindata1,select=-c(casual, registered))
d2 = subset(traindata2,select=-c(casual, registered))
d6 = subset(traindata6,select=-c(casual, registered))

train = d1
dtrain = xgb.DMatrix(data = as.matrix(train[,1:8]),
                     label = train$count)
dtest = xgb.DMatrix(data = as.matrix(test[,1:8]),
                    label = test$lpsa)