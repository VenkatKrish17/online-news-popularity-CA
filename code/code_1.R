#dependent libarires
library(e1071)
library(caret)

#normalizing shares using log normalization
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

# actual code starts here
#picking log transformation as the right transformation model. running it for all the skewed columns
OnlineNewsPopularity <- read.csv("E:/STUDIES/UNIT - 5/CA Practice/Mashable/OnlineNewsPopularity.csv") View(OnlineNewsPopularity)
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
#plotting the target variable
boxplot(modifieddata$shares,names = "Shares",main="Box plot on number of shares",
        ylab="number of shares",sub="Before transformation")
boxplot(modifieddata$normalizedshare,names = "Shares",main="Box plot on number of shares",
        ylab="number of shares",sub="After transformation")
#trasforming the target variable
modifieddata$normalizedshare<-log(modifieddata$shares)
hist(modifieddata$normalizedshare)
hist(modifieddata$shares,main="Box plot on number of shares",
        ylab="number of shares",xlab="frequency",sub="Before transformation")
hist(modifieddata$normalizedshare,main="Histogram on number of shares",
     xlab="normalized number of shares",ylab="frequency",sub="After transformation")



#categorizing the target variable
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


#finding skewness for all columns
for( i in seq(2,62)){
  print(names(modifieddata[i]))
  print(skewness(modifieddata[,i]))
}
#data balancing
#sampling data to remove bias
#how to do




#splitting into training and testing data

sampledata<- createDataPartition(y=modifieddata$popularity,p = 0.8,groups = TRUE,list=FALSE)
trainingdata<-modifieddata[sampledata,]
testingdata<-modifieddata[-sampledata,]
table(trainingdata$popularity)
table(testingdata$popularity)

#transformation !!!!!!!!
for (i in seq(2,61)){
  if((i %in% c(4,6,8,9,10,11,21,23,24,27,28,29,30,31,52)) & (skewness(modifieddata[i])>=2 | skewness(modifieddata[i])<=-2)){
    
    if(min(modifieddata[i])==0){
      print(names(modifieddata[i]))
      modifieddata[i]<-sqrt(modifieddata[i])
    }
    else{
      print(names(modifieddata[i]))
      modifieddata[i]<-log(modifieddata[i])
    }
    
   
  }
}



  
""" practice codes 
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
 
