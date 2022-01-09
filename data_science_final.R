train_data<-read.csv("/Users/aranka/Downloads/bike-sharing-demand/train.csv",stringsAsFactors = FALSE)
test_data<-read.csv("/Users/aranka/Downloads/bike-sharing-demand/test.csv",stringsAsFactors = FALSE)
time<-test_data$datetime
library(lubridate)
library(randomForest)
## Time count 
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
## Time count outliner wind
eda3 <- function(train_data,test_data){
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
  traindata_windspeed <- train_data[which(train_data$windspeed > 0), ]
  traindata_windspeed_0 <- train_data[which(train_data$windspeed == 0), ]
  wind_model <- randomForest(windspeed ~ ., 
                             data = traindata_windspeed, importane = T, proximity = T, do.trace = 500)
  wind_result <- predict(wind_model, newdata = traindata_windspeed_0)
  traindata_windspeed_0$windspeed <- wind_result
  train_data <- rbind(traindata_windspeed_0, traindata_windspeed)
  return(list(train_data,test_data))
}
## Time count outliner wind rank
eda4 <- function(train_data,test_data){
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
  
  traindata_windspeed <- train_data[which(train_data$windspeed > 0), ]
  traindata_windspeed_0 <- train_data[which(train_data$windspeed == 0), ]
  wind_model <- randomForest(windspeed ~ ., 
                             data = traindata_windspeed, importane = T, proximity = T, do.trace = 500)
  wind_result <- predict(wind_model, newdata = traindata_windspeed_0)
  traindata_windspeed_0$windspeed <- wind_result
  train_data <- rbind(traindata_windspeed_0, traindata_windspeed)
  return(list(train_data,test_data))
}
## Time count rank wind
eda5 <- function(train_data,test_data){
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
  
  traindata_windspeed <- train_data[which(train_data$windspeed > 0), ]
  traindata_windspeed_0 <- train_data[which(train_data$windspeed == 0), ]
  wind_model <- randomForest(windspeed ~ ., 
                             data = traindata_windspeed, importane = T, proximity = T, do.trace = 500)
  wind_result <- predict(wind_model, newdata = traindata_windspeed_0)
  traindata_windspeed_0$windspeed <- wind_result
  train_data <- rbind(traindata_windspeed_0, traindata_windspeed)
  return(list(train_data,test_data))
}
eda6 <- function(train_data,test_data){
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
  
  return(list(train_data,test_data))
}

eda1<-eda1(train_data,test_data)
eda1_train<-eda1[[1]]
eda1_test<-eda1[[2]]
eda1_train<-eda1_train[,-c(9,10)]
set.seed(1)
model <- randomForest(eda1_train$count~., data = eda1_train ,ntree=1000)
output1<-predict(model,eda1_test)
output1<-cbind(time,output1)
colnames(output1)<-c("datetime","count")
write.csv(x = output1,file = "kaggle_bike1.csv",row.names = F)
#0.65195
eda1_train1<-eda1_train[,-1]
eda1_test1<-eda1_test[,-1]
set.seed(1)
model1_1 <- randomForest(eda1_train1$count~., data = eda1_train1 ,ntree=1000)
output1_1<-predict(model1_1,eda1_test1)
output1_1<-cbind(time,output1_1)
colnames(output1_1)<-c("datetime","count")
write.csv(x = output1_1,file = "kaggle_bike1_1.csv",row.names = F)
##0.62891
eda2<-eda2(train_data,test_data)
eda2_train<-eda2[[1]]
eda2_test<-eda2[[2]]
eda2_train<-eda2_train[,-c(9,10)]
set.seed(1)
model1 <- randomForest(eda2_train$count~., data = eda2_train ,ntree=1000)
output2<-predict(model1,eda2_test)
output2<-cbind(time,output2)
colnames(output2)<-c("datetime","count")
write.csv(x = output2,file = "kaggle_bike2.csv",row.names = F)
#0.64301
set.seed(1)
eda2_train_1<-eda2_train[,-1]
eda2_test_1<-eda2_test[,-1]
model1_1 <- randomForest(eda2_train_1$count~., data = eda2_train_1 ,ntree=1000)
output2_1<-predict(model1_1,eda2_test_1)
output2_1<-cbind(time,output2_1)
colnames(output2_1)<-c("datetime","count")
write.csv(x = output2_1,file = "kaggle_bike2_1.csv",row.names = F)
#0.62406
eda3<-eda3(train_data,test_data)
eda3_train<-eda3[[1]]
eda3_test<-eda3[[2]]
eda3_train<-eda3_train[,-c(9,10)]
set.seed(1)
model2 <- randomForest(eda3_train$count~., data = eda3_train ,ntree=1000)
output3<-predict(model2,eda3_test)
output3<-cbind(time,output3)
colnames(output3)<-c("datetime","count")
write.csv(x = output3,file = "kaggle_bike3.csv",row.names = F)
##0.65044
eda4<-eda4(train_data,test_data)
eda4_train<-eda4[[1]]
eda4_test<-eda4[[2]]
eda4_train<-eda4_train[,-c(9,10)]
set.seed(1)
model3 <- randomForest(eda4_train$count~., data = eda4_train ,ntree=1000)
output4<-predict(model3,eda4_test)
output4<-cbind(time,output4)
colnames(output4)<-c("datetime","count")
write.csv(x = output4,file = "kaggle_bike4.csv",row.names = F)
#0.54325
eda5<-eda5(train_data,test_data)
eda5_train<-eda5[[1]]
eda5_test<-eda5[[2]]
eda5_train<-eda5_train[,-c(9,10)]
set.seed(1)
model4 <- randomForest(eda5_train$count~., data = eda5_train ,ntree=1000)
output5<-predict(model4,eda5_test)
output5<-cbind(time,output5)
colnames(output5)<-c("datetime","count")
write.csv(x = output5,file = "kaggle_bike5.csv",row.names = F)
###0.54068
eda5_train_1<-eda5_train[,-1]
eda5_test_1<-eda5_test[,-1]
model4_1 <- randomForest(eda5_train_1$count~., data = eda5_train_1 ,ntree=1000)
output5_1<-predict(model4_1,eda5_test_1)
output5_1<-cbind(time,output5_1)
colnames(output5_1)<-c("datetime","count")
write.csv(x = output5_1,file = "kaggle_bike5_1.csv",row.names = F)
##0.57829
eda6<-eda6(train_data,test_data)
eda6_train<-eda6[[1]]
eda6_test<-eda6[[2]]
eda6_train<-eda6_train[,-c(9,10)]
set.seed(1)
mode5<- randomForest(eda6_train$count~., data = eda6_train ,ntree=1000)
output6<-predict(mode5,eda6_test)
output6<-cbind(time,output6)
colnames(output6)<-c("datetime","count")
write.csv(x = output6,file = "kaggle_bike6.csv",row.names = F)
# eda1
ridge = glmnet(x = as.matrix(eda1_train[, -9]), 
               y = train_data[,9], 
               alpha = 1, # ridge
               family = "gaussian")

# 2. 用 cv.glmnet() 找出最佳的懲罰值 best.lambda
cv.ridge = cv.glmnet(x = as.matrix(eda1_train[, -9]), 
                     y = eda1_train[, 9], 
                     alpha = 1, # ridge
                     family = "gaussian")
best.ridge.lambda = cv.ridge$lambda.min

# 3. 使用 predict()進行預測
ridge.test = predict(ridge, 
                     s = best.ridge.lambda, 
                     newx = as.matrix(eda1_test))
for(i in 1:6493){
  if(ridge.test[i]<0){
    ridge.test[i]=0
  }
  else{
    ridge.test[i]<-round(ridge.test[i])
  }
}
output_lasso<-cbind(time,ridge.test)
colnames(output_lasso)<-c("datetime","count")
write.csv(x = output_lasso,file = "kaggle_bike_lasso.csv",row.names = F)
# eda2
ridge = glmnet(x = as.matrix(eda2_train[, -9]), 
               y = eda2_train[,9], 
               alpha = 1, # ridge
               family = "gaussian")

# 2. 用 cv.glmnet() 找出最佳的懲罰值 best.lambda
cv.ridge = cv.glmnet(x = as.matrix(eda2_train[, -9]), 
                     y = eda2_train[, 9], 
                     alpha = 1, # ridge
                     family = "gaussian")
best.ridge.lambda = cv.ridge$lambda.min

# 3. 使用 predict()進行預測
ridge.test = predict(ridge, 
                     s = best.ridge.lambda, 
                     newx = as.matrix(eda2_test))
for(i in 1:6493){
  if(ridge.test[i]<0){
    ridge.test[i]=0
  }
  else{
    ridge.test[i]<-round(ridge.test[i])
  }
}
output_lasso1<-cbind(time,ridge.test)
colnames(output_lasso1)<-c("datetime","count")
write.csv(x = output_lasso1,file = "kaggle_bike_lasso1.csv",row.names = F)
##1.22353
# eda3
ridge = glmnet(x = as.matrix(eda3_train[, -9]), 
               y = eda3_train[,9], 
               alpha = 1, # ridge
               family = "gaussian")

# 2. 用 cv.glmnet() 找出最佳的懲罰值 best.lambda
cv.ridge = cv.glmnet(x = as.matrix(eda3_train[, -9]), 
                     y = eda3_train[, 9], 
                     alpha = 1, # ridge
                     family = "gaussian")
best.ridge.lambda = cv.ridge$lambda.min

# 3. 使用 predict()進行預測
ridge.test = predict(ridge, 
                     s = best.ridge.lambda, 
                     newx = as.matrix(eda3_test))
for(i in 1:6493){
  if(ridge.test[i]<0){
    ridge.test[i]=0
  }
  else{
    ridge.test[i]<-round(ridge.test[i])
  }
}
output_lasso2<-cbind(time,ridge.test)
colnames(output_lasso2)<-c("datetime","count")
write.csv(x = output_lasso2,file = "kaggle_bike_lasso2.csv",row.names = F)
#1.22331
# eda4
ridge = glmnet(x = as.matrix(eda4_train[, -9]), 
               y = eda4_train[,9], 
               alpha = 1, # ridge
               family = "gaussian")

# 2. 用 cv.glmnet() 找出最佳的懲罰值 best.lambda
cv.ridge = cv.glmnet(x = as.matrix(eda4_train[, -9]), 
                     y = eda4_train[, 9], 
                     alpha = 1, # ridge
                     family = "gaussian")
best.ridge.lambda = cv.ridge$lambda.min

# 3. 使用 predict()進行預測
ridge.test = predict(ridge, 
                     s = best.ridge.lambda, 
                     newx = as.matrix(eda4_test))
for(i in 1:6493){
  if(ridge.test[i]<0){
    ridge.test[i]=0
  }
  else{
    ridge.test[i]<-round(ridge.test[i])
  }
}
output_lasso3<-cbind(time,ridge.test)
colnames(output_lasso3)<-c("datetime","count")
write.csv(x = output_lasso3,file = "kaggle_bike_lasso3.csv",row.names = F)
#1.05512
## eda5
ridge = glmnet(x = as.matrix(eda5_train[, -9]), 
               y = eda5_train[,9], 
               alpha = 1, # ridge
               family = "gaussian")

# 2. 用 cv.glmnet() 找出最佳的懲罰值 best.lambda
cv.ridge = cv.glmnet(x = as.matrix(eda5_train[, -9]), 
                     y = eda5_train[, 9], 
                     alpha = 1, # ridge
                     family = "gaussian")
best.ridge.lambda = cv.ridge$lambda.min

# 3. 使用 predict()進行預測
ridge.test = predict(ridge, 
                     s = best.ridge.lambda, 
                     newx = as.matrix(eda5_test))
for(i in 1:6493){
  if(ridge.test[i]<0){
    ridge.test[i]=0
  }
  else{
    ridge.test[i]<-round(ridge.test[i])
  }
}
output_lasso4<-cbind(time,ridge.test)
colnames(output_lasso4)<-c("datetime","count")
write.csv(x = output_lasso4,file = "kaggle_bike_lasso4.csv",row.names = F)
#1.11274
plot(eda1_train$hour,eda1_train$count,xlab="Hour",ylab="Count")
plot(eda1_train$count)
plot(scale(eda1_train$count, center = TRUE, scale = TRUE))
