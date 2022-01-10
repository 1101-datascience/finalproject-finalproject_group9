
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


traindata1 = subset(traindata1,select=-c(casual, registered))
traindata2 = subset(traindata2,select=-c(casual, registered))
traindata6 = subset(traindata6,select=-c(casual, registered))


dtrain1 = xgb.DMatrix(data = as.matrix(traindata1[,-9]),
                     label = traindata1$count)
dtest1 = xgb.DMatrix(data = as.matrix(testdata1))
dtrain2 = xgb.DMatrix(data = as.matrix(traindata2[,-9]),
                      label = traindata2$count)
dtest2 = xgb.DMatrix(data = as.matrix(testdata2))
dtrain6 = xgb.DMatrix(data = as.matrix(traindata6[,-9]),
                      label = traindata6$count)
dtest6 = xgb.DMatrix(data = as.matrix(testdata6))


xgb.params1 = list(
  #col的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  colsample_bytree = 0.5,                    
  # row的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  subsample = 0.6,                      
  booster = "gbtree",
  # 樹的最大深度，越高表示模型可以長得越深，模型複雜度越高
  max_depth = 4,           
  # boosting會增加被分錯的資料權重，而此參數是讓權重不會增加的那麼快，因此越大會讓模型愈保守
  eta = 0.03,
  # 或用'mae'也可以
  objective = "reg:squaredlogerror",
  # 越大，模型會越保守，相對的模型複雜度比較低
  gamma = 0) 

cv.model1 = xgb.cv(
  params = xgb.params1, 
  data = dtrain1,
  nfold = 5,     # 5-fold cv
  nrounds=250,   # 測試1-100，各個樹總數下的模型
  # 如果當nrounds < 30 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止                
  early_stopping_rounds = 30, 
  print_every_n = 20 # 每20個單位才顯示一次結果，
) 
best.nrounds1 = cv.model1$best_iteration 
cv.model2 = xgb.cv(
  params = xgb.params1, 
  data = dtrain2,
  nfold = 5,     # 5-fold cv
  nrounds=250,   # 測試1-100，各個樹總數下的模型
  # 如果當nrounds < 30 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止                
  early_stopping_rounds = 30, 
  print_every_n = 20 # 每20個單位才顯示一次結果，
) 
best.nrounds2 = cv.model1$best_iteration 
cv.model6 = xgb.cv(
  params = xgb.params1, 
  data = dtrain6,
  nfold = 6,     # 5-fold cv
  nrounds=270,   # 測試1-100，各個樹總數下的模型
  # 如果當nrounds < 30 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止                
  early_stopping_rounds = 30, 
  print_every_n = 20 # 每20個單位才顯示一次結果，
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

df1 = data.frame(datetime,count=ifelse(xgb_y1<0,0,xgb_y1))
df2 = data.frame(datetime,count=ifelse(xgb_y2<0,0,xgb_y2))
df6 = data.frame(datetime,count=ifelse(xgb_y6<0,0,xgb_y6))


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
writefile(df1,"performance_xgboostcv1.csv")
writefile(df2,"performance_xgboostcv2.csv")
writefile(df6,"performance_xgboostcv6.csv")
