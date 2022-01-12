train_data<-read.csv(file.path(getwd(), "data/train.csv"), stringsAsFactors = FALSE)
test_data<-read.csv(file.path(getwd(), "data/test.csv"), stringsAsFactors = FALSE)
time<-test_data$datetime
output_d <- file.path(getwd(), "results")
library(lubridate)
library(randomForest)
library(glmnet)
library(ModelMetrics)
library(xgboost)
set.seed(1)
## ignore casual and registered
removeUnnecessaryCols <- function(dataf){
  dataf <- subset(dataf, select = c(-casual, -registered))
  
  return(dataf)
}
## extract month, weekday and hour
extractMDH <- function(dataf){
  dataf$datetime <- ymd_hms(dataf$datetime)
  dataf$month <- month(dataf$datetime)
  dataf$wday <- wday(dataf$datetime)
  dataf$hour <- hour(dataf$datetime)
  dataf <- subset(dataf, select = -datetime)
  
  return(dataf)
}
## remove outliner in count column
removeOutliner <- function(dataf){
  dataf <- dataf[-which(dataf$count >= 3*sd(dataf$count)), ]
  
  return(dataf)
}
## rank time group by count
rankTimeGroup <- function(dataf){
  dataf$hour_group <- rep(100, nrow(dataf))
  
  group_0 <- c(0, 1, 2, 3, 4, 5)
  group_1 <- c(6, 23, 22, 21, 10, 11)
  group_2 <- c(7, 9, 20, 14, 15, 12)
  group_3 <- c(8, 13, 16, 17, 18, 19)
  
  dataf$hour_group[which(dataf$hour %in% group_0)] <- 0
  dataf$hour_group[which(dataf$hour %in% group_1)] <- 1
  dataf$hour_group[which(dataf$hour %in% group_2)] <- 2
  dataf$hour_group[which(dataf$hour %in% group_3)] <- 3
  return(dataf)
}

train_data <- removeUnnecessaryCols(train_data)

## random forest
runRandomForest <- function(train, test, file_name){
  model <- randomForest(train$count~., data = train, ntree = 1000)
  varImpPlot(model)
  output <- predict(model,test)
  output <- cbind(time, output)
  colnames(output) <- c("datetime","count")
  write.csv(x = output, file = file.path(output_d, paste0(file_name, "_randomForest.csv")), row.names = F)
}

## lasso
runLasso <- function(train, test, file_name){
  ridge = glmnet(x = as.matrix(train[, -9]), 
                 y = train[, 9], 
                 alpha = 1, # ridge
                 family = "gaussian")
  
  # 2. 用 cv.glmnet() 找出最佳的懲罰值 best.lambda
  cv.ridge = cv.glmnet(x = as.matrix(train[, -9]), 
                       y = train[, 9], 
                       alpha = 1, # ridge
                       family = "gaussian")
  best.ridge.lambda = cv.ridge$lambda.min
  
  # 3. 使用 predict()進行預測
  ridge.test = predict(ridge, 
                       s = best.ridge.lambda, 
                       newx = as.matrix(test))
  for(i in nrow(ridge.test)){
    if(ridge.test[i]<0){
      ridge.test[i]=0
    }
    else{
      ridge.test[i]<-round(ridge.test[i])
    }
  }
  output_lasso<-cbind(time, ridge.test)
  colnames(output_lasso)<-c("datetime","count")
  write.csv(x = output_lasso, file = file.path(output_d, paste0(file_name, "_lasso.csv")), row.names = F)
}
## xgboost
runXgboost <- function(){
  dtrain1 = xgb.DMatrix(data = as.matrix(eda1_train[,-9]),
                        label = eda1_train$count)
  dtest1 = xgb.DMatrix(data = as.matrix(eda1_test))
  dtrain2 = xgb.DMatrix(data = as.matrix(eda2_train[,-9]),
                        label = eda2_train$count)
  dtest2 = xgb.DMatrix(data = as.matrix(eda2_test))
  dtrain6 = xgb.DMatrix(data = as.matrix(eda3_train[,-9]),
                        label = eda3_train$count)
  dtest6 = xgb.DMatrix(data = as.matrix(eda3_test))
  
  xgb.params1 = list(
    colsample_bytree = 0.5,
    subsample = 0.6,                      
    booster = "gbtree",
    max_depth = 4,           
    eta = 0.03,
    objective = "reg:squaredlogerror",
    gamma = 0)
  
  cv.model1 = xgb.cv(
    params = xgb.params1, 
    data = dtrain1,
    nfold = 5,
    nrounds=250,
    early_stopping_rounds = 30, 
    print_every_n = 20
  )
  best.nrounds1 = cv.model1$best_iteration 
  cv.model2 = xgb.cv(
    params = xgb.params1, 
    data = dtrain2,
    nfold = 5,
    nrounds=250,
    early_stopping_rounds = 30, 
    print_every_n = 20
  ) 
  best.nrounds2 = cv.model1$best_iteration 
  cv.model6 = xgb.cv(
    params = xgb.params1, 
    data = dtrain6,
    nfold = 6,
    nrounds=270,
    early_stopping_rounds = 30, 
    print_every_n = 20
  ) 
  best.nrounds6 = cv.model1$best_iteration
  
  
  
  xgb.model1 = xgb.train(paras = xgb.params1, 
                         data = dtrain1,
                         nrounds = best.nrounds1) 
  xgb.model2 = xgb.train(paras = xgb.params1, 
                         data = dtrain2,
                         nrounds = best.nrounds2) 
  xgb.model6 = xgb.train(paras = xgb.params1, 
                         data = dtrain6,
                         nrounds = best.nrounds6) 
  xgb_y1 = predict(xgb.model1, dtest1)
  xgb_y2 = predict(xgb.model2, dtest2)
  xgb_y6 = predict(xgb.model6, dtest6)
  
  df1 = data.frame(time,count=ifelse(xgb_y1<0,0,xgb_y1))
  df2 = data.frame(time,count=ifelse(xgb_y2<0,0,xgb_y2))
  df6 = data.frame(time,count=ifelse(xgb_y6<0,0,xgb_y6))
  
  
  writefile <- function(df,output){
    len_output = length(strsplit(output,"/")[[1]])-1
    output_split = strsplit(output,"/")[[1]]
    a = output_split[1]
    #withpath
    if (len_output!=0){
      if (dir.exists(a)==FALSE){
        dir.create(a)
        if(len_output>1){
          for(i in c(2:len_output)){
            a = paste(a,output_split[i],sep="/")
            if (dir.exists(a)==FALSE){
              dir.create(a)
            }
          }
        }
      }else{
        #a exist!
        if(len_output>1){
          for(i in c(2:len_output)){
            a = paste(a,output_split[i],sep="/")
            if (dir.exists(a)==FALSE){
              dir.create(a)
            }
          }
        }
      }
      write.csv(df, file=output,row.names=F,quote=F)
    }else{ #without path
      split_csv = strsplit(output,"[.]")[[1]]
      b = split_csv[length(split_csv)]
      if (b!="csv"){
        output = paste0(output,".csv")
      }
      write.csv(df, file=output,row.names=F,quote=F)
    }
  }
  writefile(df1, file.path(output_d, "EDA1_extractMDH_xgboostcv.csv"))
  writefile(df2, file.path(output_d, "EDA2_extractMDH_removeOutliner_xgboostcv.csv"))
  writefile(df6, file.path(output_d, "EDA3_extractMDH_removeOutliner_rankTimeGroup_xgboostcv.csv"))
}


## EDA 1 extractMDH
eda1_train <- extractMDH(train_data)
eda1_test <- extractMDH(test_data)
# 0.65195
runRandomForest(eda1_train, eda1_test, "EDA1_extractMDH")
runLasso(eda1_train, eda1_test, "EDA1_extractMDH")

## EDA 2 extractMDH + removeOutliner
eda2_train <- removeOutliner(eda1_train)
eda2_test <- eda1_test
# 0.64301
runRandomForest(eda2_train, eda2_test, "EDA2_extractMDH_removeOutliner")
runLasso(eda2_train, eda2_test, "EDA2_extractMDH_removeOutliner")

## EDA 3 extractMDH + removeOutliner + rankTimeGroup
eda3_train <- rankTimeGroup(eda2_train)
eda3_test <- rankTimeGroup(eda2_test)
# 0.53741
runRandomForest(eda3_train, eda3_test, "EDA3_extractMDH_removeOutliner_rankTimeGroup")
runLasso(eda3_train, eda3_test, "EDA3_extractMDH_removeOutliner_rankTimeGroup")
runXgboost()

plot(eda1_train$hour,eda1_train$count,xlab="Hour",ylab="Count")
plot(eda1_train$count)
plot(scale(eda1_train$count, center = TRUE, scale = TRUE))
