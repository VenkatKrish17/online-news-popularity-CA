#normalizing shares using log normalization
OnlineNewsPopularity$normalizedshare<-log(OnlineNewsPopularity$shares)
#normalizing.. 
rnormvalue<-rnorm(OnlineNewsPopularity$shares)
skewness(rnormvalue)
lognormvalue<-log(OnlineNewsPopularity$shares)
skewness(lognormvalue)
log10value<-log10(OnlineNewsPopularity$shares)
skewness(log10value)
sqrtvalue<-(OnlineNewsPopularity$shares)
skewness(sqrtvalue)

#picking log transformation as the right transformation model. running it for all the skewed columns
modifieddata=NULL
modifieddata=cbind(OnlineNewsPopularity)
#removing missing data i.e remvoing the n_token_content is 0
modifieddata<-OnlineNewsPopularity[OnlineNewsPopularity[,4]!=0,]
#removing irrelevant data - deleting non stop word with value 1042
modifieddata<-modifieddata[modifieddata[,6]!=1042,]

skewness(modifieddata$n_non_stop_words)
min(modifieddata[2])
#finding skewness for all columns
for( i in seq(2,61))
  print(skewness(modifieddata[i]))
for( i in seq(2,61))
  print(OnlineNewsPopularity[i]==0)
#transformation !!!!!!!!
for (i in seq(2,61)){
  if(!(i %in% c(1,14,15,16,17,18,19,20,21,22,23,24,25,26,27,32,33,34,35,36,37,38,51,52,53,54,55,56,58)) & (skewness(modifieddata[i])>=2 | skewness(modifieddata[i])<=-2)){
    
    if(min(modifieddata[i])>0){
      print(names(modifieddata[i]))
      modifieddata[i]<-log(modifieddata[i])
    }
    else{
      print(names(modifieddata[i]))
      modifieddata[i]<-sqrt(modifieddata[i])
    }
    
   
  }
}
  
  
#outlier removal and creating categories
#creating categories
"""

the target variable shares after normalization also, is very highly variant. 
It has high outliers. to eliminate outliers and the variance in the data,
and to balance the data.
we categorized the target as popular and un popular and the rest are considered as outliers as they are beyond the hinges.
table(modifieddata$popularity)

NA Not popular     Popular 
1615       17837       19010 
"""
modifieddata$popularity<-ifelse(modifieddata$shares<9.47 & modifieddata$shares>=7.24,modifieddata$popularity<-"Popular",
                                        ifelse(modifieddata$shares>5.27 & modifieddata$shares<7.24,modifieddata$popularity<-"Not popular",modifieddata$
                                                 popularity<-"NA"))

#removing NA's
modifieddata<-modifieddata[modifieddata$popularity!='NA',]

"""
modifieddata<-modifieddata[modifieddata$popularity!='NA',]
> table(modifieddata$popularity)

Not popular     Popular 
17837       19010 
"""


#grouping into two based on box plot stats

""" oxfacts<-boxplot(OnlineNewsPopularity.normalizedshare)
> boxfacts$stats
[,1]
[1,] 5.225747
[2,] 6.852243
[3,] 7.244228
[4,] 7.937375
[5,] 9.560997
> table(OnlineNewsPopularity$normalizedshare<7.5)

FALSE  TRUE 
15200 24444 
> table(OnlineNewsPopularity$normalizedshare<7.24)

FALSE  TRUE 
21154 18490 
> table(OnlineNewsPopularity$normalizedshare>9.56)

FALSE  TRUE 
38269  1375 
> table(OnlineNewsPopularity$normalizedshare<5.22)

FALSE  TRUE 
39487   157 
> lm(OnlineNewsPopularity$num_keywords,OnlineNewsPopularity$normalizedshare)
Error in formula.default(object, env = baseenv()) : invalid formula
> table(OnlineNewsPopularity$normalizedshare<9.56 & OnlineNewsPopularity$normalizedshare>7.24)

FALSE  TRUE 
19865 19779 
> table(OnlineNewsPopularity$normalizedshare>5.56 & OnlineNewsPopularity$normalizedshare<7.24)

FALSE  TRUE 
21384 18260 """
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

 
 
 
