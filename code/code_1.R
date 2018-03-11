#dependent libarires
library(lattice)
library(ggplot2)
library(e1071)
library(caret)
library(pROC)
library(randomForest)

library(neuralnet)

OnlineNewsPopularity <- read.csv("../data set/OnlineNewsPopularity.csv")
View(OnlineNewsPopularity)
"""
#normalizing shares using log normalization
trying out various normalization for better results
OnlineNewsPopularity$normalizedshare<-log(OnlineNewsPopularity$shares)
#normalizing different ways to check skewness.. 
modshares<-table(log(modifieddata$shares))
rnormvalue<-rnorm(OnlineNewsPopularity$shares)
skewness(rnormvalue)
lognormvalue<-log(OnlineNewsPopularity$shares)
skewness(lognormvalue)
log10value<-log10(OnlineNewsPopularity$shares)
skewness(log10value)
sqrtvalue<-(OnlineNewsPopularity$shares)
skewness(sqrtvalue)
"""
# actual code starts here
#picking log transformation as the right transformation model. running it for all the skewed columns

modifieddata=NULL
#removing missing data i.e removing the n_token_content is 0
modifieddata<-OnlineNewsPopularity[OnlineNewsPopularity[,4]!=0,]
outlierremoved<-OnlineNewsPopularity[OnlineNewsPopularity[,4]!=0,]
#removing irrelevant data - deleting non stop word with value 1042 the outlier data
modifieddata<-modifieddata[modifieddata[,6]<=1,]
outlierremoved<-outlierremoved[outlierremoved[,6]<=1,]

#finding skewness for all columns
for( i in seq(2,61)){
  print(names(modifieddata[i]))
  print(skewness(modifieddata[,i]))
}
#processing the target variable
#normalziing the target variable and storing it in new variable
modifieddata$normalizedshare<-log(modifieddata$shares)
#plotting the target variable

boxplot(modifieddata$shares,names = "Shares",main="Box plot on number of shares",
        ylab="number of shares",sub="Before transformation")
boxplot(modifieddata$normalizedshare,names = "Shares",main="Box plot on number of shares",
        ylab="number of shares",sub="After transformation")
#trasforming the target variable

hist(modifieddata$normalizedshare)
hist(modifieddata$shares,main="Box plot on number of shares",
        ylab="number of shares",xlab="frequency",sub="Before transformation")
hist(modifieddata$normalizedshare,main="Histogram on number of shares",
     xlab="normalized number of shares",ylab="frequency",sub="After transformation")



#removing the outliers in target variable
boxplotstats<-boxplot(modifieddata$normalizedshare)$stats
print(boxplotstats)
minimum<-boxplotstats[,1][1]
lowerhinge<-boxplotstats[,1][2]
median<-boxplotstats[,1][3]
upperhinge<-boxplotstats[,1][4]
maximum<-boxplotstats[,1][5]
#calculation for mild outliers 
hspread<-upperhinge-lowerhinge
lowerouterfence<-lowerhinge-3*hspread
upperouterfence<-upperhinge+3*hspread
print(hspread)
print(lowerouterfence)
print(upperouterfence)

#removing values beyond the fences
modifieddata<-modifieddata[!(modifieddata$normalizedshare>=upperouterfence | modifieddata$normalizedshare<=lowerouterfence),]
boxplot(modifieddata$normalizedshare,xlab="Normalized Shares",main="Shares after extreme oulier removal")

#modifieddata$popularity<-ifelse(modifieddata$normalizedshare>=7.90, modifieddata$popularity<-"Viral",ifelse((modifieddata$normalizedshare<7.90 & modifieddata$shares>=6.85),modifieddata$popularity<-"Popular",
#                                modifieddata$popularity<-"Least-Popular"))

#min(modifieddata$normalizedshare)

#removing skewness in other columns
#finding skewness for all columns
for( i in seq(2,62)){
  print(names(modifieddata[i]))
  print(skewness(modifieddata[,i]))
}


#transformation !!!!!!!!
for (i in seq(2,61)){
  if((i %in% c(4,6,8,9,10,11,21,23,24,27,28,29,30,31,52)) & (skewness(modifieddata[i])>=2 | skewness(modifieddata[i])<=-2)){
    
    if(min(modifieddata[i])==0){
      print(names(modifieddata[i]))
      modifieddata[i]<-sqrt(modifieddata[i])
    }
    else{
      print(names(modifieddata[i]) )
      modifieddata[i]<-log(modifieddata[i])
    }
      }
}


#binning or categorizing the data
#table(modifieddata$popularity)
# three categories
modifieddata$popularity<-cut(modifieddata$normalizedshare,c(lowerouterfence,lowerhinge,upperhinge,upperouterfence),labels=c("Least-Popular","Popular","Viral"))
table(modifieddata$popularity)
#four categories
modifieddata$popularity<-cut(modifieddata$normalizedshare,c(lowerouterfence,lowerhinge,median,upperhinge,upperouterfence),labels=c("Least-Popular","Average","Popular","Viral"))
table(modifieddata$popularity)
#two categories
modifieddata$popularity<-cut(modifieddata$normalizedshare,c(lowerouterfence,median,upperouterfence),labels=c("Flop","Hit"))
table(modifieddata$popularity)




#splitting into training and testing data

  sampledata<- createDataPartition(y=modifieddata$popularity,p = 0.8,groups = TRUE,list=FALSE)
  trainingdata<-modifieddata[sampledata,]
  testingdata<-modifieddata[-sampledata,]
  table(trainingdata$popularity)
  table(testingdata$popularity)



#modelling 
#important attributes ?
""" 
from random forest 
kw_avg_avg
self_reference_min_shares
kw_max_avg
timedelta
self_reference_avg_sharess
is_weekend
LDA_00
kw_min_avg
LDA_02
n_tokens_content
n_unique_tokens
LDA_04
data_channel_is_entertainment
LDA_01
data_channel_is_socmed
n_non_stop_words
self_reference_max_shares
n_non_stop_unique_tokens
num_imgs
kw_avg_max
LDA_03
kw_avg_min
kw_min_max
data_channel_is_tech
rate_positive_words
num_hrefs
global_subjectivity
global_sentiment_polarity
rate_negative_words
global_rate_positive_words
weekday_is_saturday
average_token_length
kw_max_min
global_rate_negative_words
"""
"""
final flitering
kw_avg_avg
kw_max_avg
kw_min_avg
time_delta
self_reference_avg_sharess
n_tokens_content
n_unique_tokens
is_weekend
LDA_00
LDA_01
LDA_02
LDA_03
LDA_04
data_channel_is_entertainment
data_channel_is_tech
data_channel_is_socmed
n_non_stop_words
n_non_stop_unique_tokens
num_imgs
num_hrefs
global_subjectivity
global_sentiment_polarity
average_token_length
rate_positive_words
num_hrefs
rate_negative_words
global_rate_positive_words
global_rate_negative_words

"""


#formula 

formulaString<-"popularity~kw_avg_avg+
kw_max_avg+
kw_min_avg+
timedelta+
self_reference_avg_sharess+
n_tokens_content+
n_unique_tokens+
is_weekend+
LDA_00+
LDA_01+
LDA_02+
LDA_03+
LDA_04+
data_channel_is_entertainment+
data_channel_is_tech+
data_channel_is_socmed+
n_non_stop_words+
n_non_stop_unique_tokens+
num_imgs+
num_hrefs+
global_subjectivity+
global_sentiment_polarity+
average_token_length+
rate_positive_words+
rate_negative_words+
global_rate_positive_words+
global_rate_negative_words"

#logistic model
#to build a logistic regression , creating the popularity column as 0 & 1
modifieddata$popularity<-ifelse(modifieddata$popularity=="Hit",modifieddata$popularity<-1,modifieddata$popularity<-0)
table(modifieddata$popularity)


# Step 1: Build Logit Model on Training Dataset
logitMod <- glm(as.formula(formulaString), family="binomial", data = trainingdata)

# Step 2: Predict Y on Test Dataset
predictedY <- predict(logitMod, testingdata, type="response") 
predictedbinary<-ifelse(predictedY>=0.5,1,0)
table(testingdata$popularity)
confusionMatrix(predictedbinary,testingdata$popularity)

#ROC curve
ROCPredict<-roc(predictedbinary,testingdata$popularity)
plot(ROCPredict)

"""
Confusion Matrix and Statistics

Reference
Prediction    0    1
0 2628 1365
1 1279 2393

Accuracy : 0.6551          
95% CI : (0.6443, 0.6657)
No Information Rate : 0.5097          
P-Value [Acc > NIR] : < 2e-16         

Kappa : 0.3095          
Mcnemar's Test P-Value : 0.09832         
                                          
            Sensitivity : 0.6726          
            Specificity : 0.6368          
         Pos Pred Value : 0.6582          
         Neg Pred Value : 0.6517          
             Prevalence : 0.5097          
         Detection Rate : 0.3429          
   Detection Prevalence : 0.5209          
      Balanced Accuracy : 0.6547          
                                          
       'Positive' Class : 0
"""

#reverting back to categorical values
modifieddata$popularity<-ifelse(modifieddata$popularity==1,modifieddata$popularity<-"Hit",modifieddata$popularity<-"Flop")  
table(modifieddata$popularity)

#Random forest model for 2 category data
#hoping for a better performance
rfresults<-randomForest(as.formula(formulaString),data=trainingdata,mtry=8,ntree=500)
plot(rfresults, main="Random forest error rate")
#randomForest.crossValidation(rfresults,modifieddata,p=0.1,n=10, plot=TRUE, ntree=500)
rfprediction<-predict(rfresults,testingdata)
#print(rfprediction)
confusionMatrix(rfpredictionbinary,testingdata$popularity)
line(rfprediction)
rfpredictionbinary<-ifelse(rfprediction <= 0.5 ,  rfpredictionbinary<-0, rfpredictionbinary <- 1) 
ROCRF<-roc(rfprediction,testingdata$popularity)
print(ROCRF)
plot(ROCRF)


#SVM
#may work.. may not work.. 
svm_results<-svm(as.formula(formulaString),data=trainingdata)
svmprediction<-predict(svm_results,testingdata,type="response")
confusionMatrix(svmprediction,testingdata$popularity)
ROCSVM<-roc(svmprediction,testingdata$popularity)
print(ROCSVM)
plot(ROCSVM)


chunksize<-NROW(trainingdata)%/%10
print(chunksize)
learnings<-data.frame(m = integer(10),
                      dfsize=integer(10),
                      cvaccuracy = integer(10))
for (i in seq(1,10)){
  learnings$m[i]<-i
  learnings$dfsize[i]<-i*chunksize+3
  #logitMod <<- glm(as.formula(formulaString), family="binomial", data = trainingdata[1:(i*chunksize)+3,])
  
  # Step 2: Predict Y on Test Dataset
  #predictedY <<- predict(logitMod, testingdata, type="response") 
  
  rfmodel<-randomForest(as.formula(formulaString),data = trainingdata[1:(i*chunksize)+3,],ntree=500,mtry=7)
  predictedY <<- predict(rfmodel, testingdata, type="response")
  #continuous to categorical
  predictedbinary<<-ifelse(predictedY >= 0.5,1,0)
  cm<-confusionMatrix(predictedbinary,testingdata$popularity)
  print(cm)
  learnings$cvaccuracy[i]<-cm$overall['Accuracy']
  
  
  
  
}

print(learnings)
plot(learnings$dfsize,learnings$cvaccuracy,type="o",main="Learning curve", xlab="data chunk size",ylab="prediction accuracy")




"""
practice codes 
ignore
#outlier removal and creating categories
#creating categories
the target variable shares after normalization also, is very highly variant. 
It has high outliers. to eliminate outliers and the variance in the data,
and to balance the data.
we categorized the target as popular and un popular and the rest are considered as outliers as they are beyond the hinges.
table(modifieddata$popularity)

NA Not popular     Popular 
1615       17837       19010 

modifieddata$popularity<-ifelse(modifieddata$shares<9.47 & modifieddata$shares>=7.24,modifieddata$popularity<-"Popular",
                                        ifelse(modifieddata$shares>5.27 & modifieddata$shares<7.24,modifieddata$popularity<-"Not popular",modifieddata$
                                                 popularity<-"NA"))

#removing NA's
modifieddata<-modifieddata[modifieddata$popularity!='NA',]

modifieddata<-modifieddata[modifieddata$popularity!='NA',]
> table(modifieddata$popularity)

Not popular     Popular 
17837       19010 



#grouping into two based on box plot stats


#creating categories
OnlineNewsPopularity$popularity<-ifelse(OnlineNewsPopularity$normalizedshare<9.56 & OnlineNewsPopularity$normalizedshare>=7.24,OnlineNewsPopularity$popularity<-"Popular",
                                        ifelse(OnlineNewsPopularity$normalizedshare>5.56 & OnlineNewsPopularity$normalizedshare<7.24,OnlineNewsPopularity$popularity<-"Not popular",OnlineNewsPopularity$
                                                popularity<-"NA"))

#making it as 1 & 0
OnlineNewsPopularity$popularity<-ifelse(OnlineNewsPopularity$normalizedshare<9.56 & OnlineNewsPopularity$normalizedshare>=7.24,OnlineNewsPopularity$popularity<-1,
                                        ifelse(OnlineNewsPopularity$normalizedshare>5.56 & OnlineNewsPopularity$normalizedshare<7.24,OnlineNewsPopularity$popularity<-0,OnlineNewsPopularity$
                                                 popularity<-99))


#excluding NA's
onlinedata<-OnlineNewsPopularity[OnlineNewsPopularity$popularity!='NA',]
onlinedata<-OnlineNewsPopularity[OnlineNewsPopularity$popularity!=99,]
#caret package is used for data splitting
trainIndex=createDataPartition(onlinedata$popularity,p=0.8,list=FALSE);
data_train <- onlinedata[ trainIndex,]
data_test <- onlinedata[-trainIndex,]
table(data_train$popularity)
#applying model ---> Logistic Regression
glm_res<-glm(popularity~n_tokens_title++kw_avg_min+kw_max_max+kw_avg_avg+rate_positive_words+global_rate_negative_words+min_positive_polarity+max_negative_polarity,data=data_train,family="binomial")
predict <- predict(glm_res,data_test, type = 'response')
table(data_test$popularity,predict>0.5)


#apply model ----> SVM
library('e1071')
svm_model<-svm(popularity~n_tokens_title+kw_avg_min+kw_max_max+kw_avg_avg+rate_positive_words+global_rate_negative_words+min_positive_polarity+max_negative_polarity,data=data_train)
  summary(svm_model)
  pred_svm<-predict(svm_model,data_test,type="response")
summary(pred_svm)
table(data_test$popularity,pred_svm>0.5)


#testing
glm_res<-glm(popularity~n_tokens_title+kw_avg_min+kw_max_max+kw_avg_avg+rate_positive_words+global_rate_negative_words+min_positive_polarity+max_negative_polarity,data=data_test,family="binomial")
predict <- predict(glm_res, data_test,type = 'response')
table(data_test$popularity,predict>0.5)
#ROC
 ROCRpred <- prediction(predict, data_train$popularity)
 ROCRperf <- performance(ROCRpred, 'tpr','fpr')
 plot(ROCRperf, colorize = TRUE)

 
 """
 
