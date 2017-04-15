setwd("/resources/rstudio/HackerEarth/ML Challenge 1")


library('parallel')
library('foreach')
library('doSNOW')

nc<-detectCores()
cl<-makeCluster(nc)
registerDoSNOW((cl))

url<-"https://he-s3.s3.amazonaws.com/media/hackathon/machine-learning-challenge-one/bank-fears-loanliness/898ce544-0-MLChallenge_1Datac63af4e.zip"

download.file(url, dest="dataset.zip", mode="wb")
unzip ("dataset.zip", exdir = "./")

train<-read.csv("train_indessa.csv")
test<-read.csv("test_indessa.csv")
sample_sub<-read.csv("sample_submission.csv")

sort(sapply(train, function(x){sum(is.na(x))/length(x)}),decreasing = T)

#test$loan_status<--1
#all<-rbind(train,test)


library('plyr')

#Batch Enrolled
u_count1 <- ddply(train, .(batch_enrolled), nrow)
colnames(u_count1)<-c('batch_enrolled','batch_enrolled_c')
train <- merge(train, u_count1, by="batch_enrolled",all.x=T)
test <- merge(test, u_count1, by="batch_enrolled",all.x=T)


u_mean1 <- ddply(train, .(batch_enrolled),summarize,u_Mean=mean(loan_status))
colnames(u_mean1)<-c('batch_enrolled','batch_enrolled_m')
train <- merge(train, u_mean1, by="batch_enrolled",all.x=T)
test <- merge(test, u_mean1, by="batch_enrolled",all.x=T)


#sub_grade
u_count2 <- ddply(train, .(sub_grade), nrow)
colnames(u_count2)<-c('sub_grade','sub_grade_c')
train <- merge(train, u_count2, by="sub_grade",all.x=T)
test <- merge(test, u_count2, by="sub_grade",all.x=T)


u_mean2 <- ddply(train, .(sub_grade),summarize,u_Mean=mean(loan_status))
colnames(u_mean2)<-c('sub_grade','sub_grade_m')
train <- merge(train, u_mean2, by="sub_grade",all.x=T)
test <- merge(test, u_mean2, by="sub_grade",all.x=T)

#emp_title
u_count3 <- ddply(train, .(emp_title), nrow)
colnames(u_count3)<-c('emp_title','emp_title_c')
train <- merge(train, u_count3, by="emp_title",all.x=T)
test <- merge(test, u_count3, by="emp_title",all.x=T)


u_mean3 <- ddply(train, .(emp_title),summarize,u_Mean=mean(loan_status))
colnames(u_mean3)<-c('emp_title','emp_title_m')
train <- merge(train, u_mean3, by="emp_title",all.x=T)
test <- merge(test, u_mean3, by="emp_title",all.x=T)

#emp_length
u_count4 <- ddply(train, .(emp_length), nrow)
colnames(u_count4)<-c('emp_length','emp_length_c')
train <- merge(train, u_count4, by="emp_length",all.x=T)
test <- merge(test, u_count4, by="emp_length",all.x=T)


u_mean4 <- ddply(train, .(emp_length),summarize,u_Mean=mean(loan_status))
colnames(u_mean4)<-c('emp_length','emp_length_m')
train <- merge(train, u_mean4, by="emp_length",all.x=T)
test <- merge(test, u_mean4, by="emp_length",all.x=T)


#desc
u_count5 <- ddply(train, .(desc), nrow)
colnames(u_count5)<-c('desc','desc_c')
train <- merge(train, u_count5, by="desc",all.x=T)
test <- merge(test, u_count5, by="desc",all.x=T)


u_mean5 <- ddply(train, .(desc),summarize,u_Mean=mean(loan_status))
colnames(u_mean5)<-c('desc','desc_m')
train <- merge(train, u_mean5, by="desc",all.x=T)
test <- merge(test, u_mean5, by="desc",all.x=T)


#title
u_count6 <- ddply(train, .(title), nrow)
colnames(u_count6)<-c('title','title_c')
train <- merge(train, u_count6, by="title",all.x=T)
test <- merge(test, u_count6, by="title",all.x=T)


u_mean6 <- ddply(train, .(title),summarize,u_Mean=mean(loan_status))
colnames(u_mean6)<-c('title','title_m')
train <- merge(train, u_mean6, by="title",all.x=T)
test <- merge(test, u_mean6, by="title",all.x=T)


#addr_state
u_count7 <- ddply(train, .(addr_state), nrow)
colnames(u_count7)<-c('addr_state','addr_state_c')
train <- merge(train, u_count7, by="addr_state",all.x=T)
test <- merge(test, u_count7, by="addr_state",all.x=T)


u_mean7 <- ddply(train, .(addr_state),summarize,u_Mean=mean(loan_status))
colnames(u_mean7)<-c('addr_state','addr_state_m')
train <- merge(train, u_mean7, by="addr_state",all.x=T)
test <- merge(test, u_mean7, by="addr_state",all.x=T)


#zip_code
u_count8 <- ddply(train, .(zip_code), nrow)
colnames(u_count8)<-c('zip_code','zip_code_c')
train <- merge(train, u_count8, by="zip_code",all.x=T)
test <- merge(test, u_count8, by="zip_code",all.x=T)


u_mean8 <- ddply(train, .(zip_code),summarize,u_Mean=mean(loan_status))
colnames(u_mean8)<-c('zip_code','zip_code_m')
train <- merge(train, u_mean8, by="zip_code",all.x=T)
test <- merge(test, u_mean8, by="zip_code",all.x=T)


#last_week_pay
u_count9 <- ddply(train, .(last_week_pay), nrow)
colnames(u_count9)<-c('last_week_pay','last_week_pay_c')
train <- merge(train, u_count9, by="last_week_pay",all.x=T)
test <- merge(test, u_count9, by="last_week_pay",all.x=T)


u_mean9 <- ddply(train, .(last_week_pay),summarize,u_Mean=mean(loan_status))
colnames(u_mean9)<-c('last_week_pay','last_week_pay_m')
train <- merge(train, u_mean9, by="last_week_pay",all.x=T)
test <- merge(test, u_mean9, by="last_week_pay",all.x=T)

train$amnt_f1<-train$loan_amnt-train$funded_amnt
train$amnt_f2<-train$loan_amnt-train$funded_amnt_inv
train$amnt_f3<-train$funded_amnt-train$funded_amnt_inv

train$loan_inc<-train$loan_amnt/train$annual_inc

train$term<-as.numeric(ifelse(train$term=='36 months',36,60))

train$EMI<-train$loan_amnt*((100+train$int_rate)/100)/train$term

train$application_type<-as.numeric(ifelse(train$application_type=='INDIVIDUAL',1,2))

train$amt_per_person<-train$loan_amnt*((100+train$int_rate)/100)/train$application_type

train$EMI_per_person<-train$amt_per_person/train$term

train$EMI_by_sal<-train$EMI/train$annual_inc


test$amnt_f1<-test$loan_amnt-test$funded_amnt
test$amnt_f2<-test$loan_amnt-test$funded_amnt_inv
test$amnt_f3<-test$funded_amnt-test$funded_amnt_inv

test$loan_inc<-test$loan_amnt/test$annual_inc

test$term<-as.numeric(ifelse(test$term=='36 months',36,60))

test$EMI<-test$loan_amnt*((100+test$int_rate)/100)/test$term

test$application_type<-as.numeric(ifelse(test$application_type=='INDIVIDUAL',1,2))

test$amt_per_person<-test$loan_amnt*((100+test$int_rate)/100)/test$application_type

test$EMI_per_person<-test$amt_per_person/test$term

test$EMI_by_sal<-test$EMI/test$annual_inc


save.image('data1')
load('data1')

predictors<-c("initial_list_status", "total_rec_int", "tot_cur_bal", "recoveries", 
              "total_rev_hi_lim", "dti", "collection_recovery_fee", "int_rate", 
              "EMI_per_person", "verification_status", "grade", "EMI", "total_acc", 
              "annual_inc", "EMI_by_sal", "open_acc", "batch_enrolled_c", "last_week_pay_c", 
              "desc_c", "batch_enrolled_m")

library('xgboost')
library('caret')

preProcValues <- preProcess(train[,predictors], method = c("medianImpute"))

library('RANN')
train[,predictors] <- predict(preProcValues, train[,predictors])

preProcValues <- preProcess(test[,predictors], method = c("medianImpute"))

library('RANN')
test[,predictors] <- predict(preProcValues, test[,predictors])



control <- rfeControl(functions = rfFuncs,
                      method = "cv",
                      number = 5)
outcomeName<-'loan_status'

train$loan_status<-as.factor(train$loan_status)

index <- createDataPartition(train$loan_status, p=0.1, list=FALSE)
trainSet <- train[ index,]


Loan_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)





dmy <- dummyVars(" ~ .", data = train[,predictors])
x_train <- data.frame(predict(dmy, newdata = train[,predictors]))

x_train$home_ownership.ANY<-NULL

dmy <- dummyVars(" ~ .", data = test[,predictors])
x_test <- data.frame(predict(dmy, newdata = test[,predictors]))

y_train<-train$loan_status

#y_train<-as.factor(ifelse(y_train==0,'zero','one'))


all<-rbind(x_train,x_test)


preProcValues <- preProcess(all, method = c("medianImpute"))

library('RANN')
all_processed <- predict(preProcValues, all)

#for(i in 1:ncol(all))
{
  all[is.na(all[,i]),i]<-mean(all[,i],na.rm=T)
}


#x_train<-all_processed[1:400000,]
#x_val<-all_processed[400001:532428,]
x_train<-all_processed[1:532428,]
x_test<-all_processed[532429:nrow(all_processed),]

#y_val<-y_train[400001:532428]
#y_train<-y_train[1:400000]
y_train<-y_train[1:532428]

dtrain <- xgb.DMatrix(data=as.matrix(x_train), label=y_train)
#dval <- xgb.DMatrix(data=as.matrix(x_val), label=y_val)
dtest <- xgb.DMatrix(data=as.matrix(x_test))


save.image('data2')
load('data2')




xgb_params <- list(booster="gbtree",
                   objective="binary:logistic",
                   eval_metric="auc",
                   nthread=14,
                   eta = .5,
                   lambda = 5,
                   gamma = 1,
                   max_depth = 5,
                   min_child_weight = 1,
                   subsample = .8,
                   colsample_bytree = .5
)


set.seed(0)
res = xgb.cv(xgb_params,
             dtrain,
             nrounds=10000,
             nfold=10,
             early.stop.round=15,
             print.every.n = 10,
             verbose= 1,
             maximize=TRUE)


xgb2 <- xgb.train(data = dtrain,
                  params = xgb_params,
                  nrounds = 100)


model <- xgb.dump(xgb2, with.stats = T)
model[1:30] 
names <- dimnames(data.matrix(x_train[,-1]))[[2]]

importance_matrix <- xgb.importance(names, model = xgb2)

xgb.plot.importance(importance_matrix[1:20,])

pred_val<-predict(object = xgb2,dval)


plotROC <- function(truth, predicted, ...){
  library(ROCR)
  pred <- prediction(abs(predicted), truth)    
  perf <- performance(pred,"tpr","fpr")
  
  plot(perf, ...)
}

library('pROC')
plotROC(y_val,pred_val)

auc(y_val,pred_val)

sub<-as.data.frame(test[,'member_id'])


colnames(sub)<-'member_id'

pred<-predict(object = xgb2,dtest)

sub$loan_status<-pred

write.csv(sub,'Submission11_holdout_94.csv',row.names = F)
