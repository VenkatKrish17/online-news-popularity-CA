library(ggplot2)
library(reshape2)
library(DAAG)
library(e1071)
library(lattice)
library(caret)
library(pROC)
library(randomForest)
library(heuristica)

raw<-read.csv("OnlineNewsPopularity.csv")
raw[,61] <- as.numeric(raw[,61])

#Plot all the variable data as boxplot
par(mfrow=c(3,4))
for(i in 2:length(raw)){boxplot(raw[,i], xlab=names(raw)[i])}

#i=1 is not considered because 1 is URL which is not numeric and hence cannot be plotted
#Plot all the variable data by histogram to check the distributions
par(mfrow=c(3,4))
for(i in 2:length(raw)){hist(raw[,i], xlab=names(raw)[i])}

#Remove missing data i.e remove rows with n_token_content as 0. 
#1181 rows are removed
modifiedData<-raw[raw[,4]!=0,]
dim(modifiedData)

#Removing outlier - delete rows with value >1 for n_non_stop_words. 1 row is removed
modifiedData<-modifiedData[modifiedData[,6]<=1,]

#Find skewness for all columns
for( i in 2:length(modifiedData)){
  print(names(modifiedData[i]))
  print(skewness(modifiedData[,i]))
}

#Plot the target variable
par(mfrow=c(1,1))	  #Use this to display only one plot in the page
hist(modifiedData$shares,main=" Histogram on number of shares ", xlab=" Number of shares ", ylab=" Frequency " ,sub="Before transformation")
boxplot(modifiedData$shares,names = "Shares",main="Box plot on number of shares",
        ylab="Number of shares",sub="Before transformation")

#Log transformation for the target variable and plot again
modifiedData$normalizedshares<-log(modifiedData$shares)
boxplot(modifiedData$normalizedshares,names = "Shares",main="Box plot on number of shares",
        ylab="Number of shares",sub="After transformation")
hist(modifiedData$normalizedshare,main="Histogram on number of shares",
     xlab="Normalized number of shares",ylab="Frequency",sub="After transformation")	

#Remove outliers in target variable
boxplotstats<-boxplot(modifiedData$normalizedshare)$stats
print(boxplotstats)
#stats returns a vector of length 5, containing the extreme of the lower whisker, the lower hinge, the median, the upper hinge and the extreme of the upper whisker. Each of these is assigned to a separate variable

minimum<-boxplotstats[1,1]
lowerhinge<-boxplotstats[2,1]
median<-boxplotstats[3,1]
upperhinge<-boxplotstats[4,1]
maximum<-boxplotstats[5,1]

#Calculation for fence to separate mild and extreme outliers 

hspread<-upperhinge-lowerhinge
lowerouterfence<-lowerhinge-3*hspread
upperouterfence<-upperhinge+3*hspread
print(hspread)
print(lowerouterfence)
print(upperouterfence)

#Removing values beyond the fences, i.e. extreme outliers. 133 rows are removed

modifiedData<-modifiedData[!(modifiedData$normalizedshare>=upperouterfence | modifiedData$normalizedshare<=lowerouterfence),]
boxplot(modifiedData$normalizedshare,xlab="Normalized Shares",main="Shares after extreme oulier removal")

#Categorizing the target variable. Select one from the below three, i.e. select how many categories you want 2, 3 or 4?

#Two categories
modifiedData$popularity<-cut(modifiedData $normalizedshare,c(lowerouterfence,median,upperouterfence),labels=c("Flop","Hit"))
table(modifiedData $popularity)

#Three categories
#modifiedData$popularity<-cut(modifiedData $normalizedshare,c(lowerouterfence,lowerhinge,upperhinge,upperouterfence),labels=c("Least-Popular","Popular","Viral"))
#table(modifiedData $popularity)

#Four categories
#modifiedData$popularity<-cut(modifiedData $normalizedshare,c(lowerouterfence,lowerhinge,median,upperhinge,upperouterfence),labels=c("Least-Popular","Average","Popular","Viral"))
#table(modifiedData $popularity)

#Find skewness for all columns
for( i in 2:length(modifiedData)){
  print(names(modifiedData[i]))
  print(skewness(modifiedData[,i]))
}

#Transformation of the variables with skewness>=2 or <=-2
for(i in c(4,6,8,9,10,11,21,23,24,27,28,29,30,31,52))
{
  if(min(modifiedData[i])==0){
    print(names(modifiedData[i]))
    modifiedData[i]<-sqrt(modifiedData[i]); names(modifiedData)[i] <- paste("sqrt_",names(modifiedData)[i], sep="")
  }
  else{
    print(names(modifiedData[i]))
    modifiedData[i]<-log(modifiedData[i]); names(modifiedData)[i] <- paste("log_",names(modifiedData)[i], sep="")
  } 
}

#Does publishing days of news matter
modifiedData$news_day <- rep("Sunday", nrow(modifiedData))
modifiedData$news_day[modifiedData$weekday_is_monday==1] <- "Monday"
modifiedData$news_day[modifiedData$weekday_is_tuesday==1] <- "Tuesday"
modifiedData$news_day[modifiedData$weekday_is_wednesday==1] <- "Wednesday"
modifiedData$news_day[modifiedData$weekday_is_thursday==1] <- "Thursday"
modifiedData$news_day[modifiedData$weekday_is_friday==1] <- "Friday"
modifiedData$news_day[modifiedData$weekday_is_saturday==1] <- "Saturday"
p1 <- ggplot(data= modifiedData, aes(as.factor(news_day), modifiedData$normalizedshares))
p1 + geom_boxplot()
#Remove the newly created column news_day
modifiedData <- modifiedData[,-c(64)]

#Finding variable importance using random forest
#Ignore url, shares and normalized shares
rf_for_VariableImportance<-randomForest(as.formula("popularity~. -url -shares -normalizedshares"),data=modifiedData,mtry=7,ntree=500, importance=TRUE)	
var <- round(importance(rf_for_VariableImportance), 2)
print(var)
var <- var[order(var[,3], decreasing=TRUE),]
print(var)
varImpPlot(rf_for_VariableImportance)

#Selected Input Variables

formulaString<-"popularity~sqrt_kw_avg_avg+sqrt_kw_max_avg+kw_min_avg+timedelta+
sqrt_self_reference_avg_sharess+log_n_tokens_content+n_unique_tokens+is_weekend+LDA_00+
LDA_01+LDA_02+LDA_03+LDA_04+data_channel_is_entertainment+data_channel_is_tech+
data_channel_is_socmed+log_n_non_stop_words+n_non_stop_unique_tokens+sqrt_num_imgs+
sqrt_num_hrefs+global_subjectivity+global_sentiment_polarity+average_token_length+
rate_positive_words+ rate_negative_words+global_rate_positive_words+
global_rate_negative_words"

#To build a logistic regression, change the popularity column as 0 & 1
modifiedData$popularity<-ifelse(modifiedData$popularity=="Hit",modifiedData$popularity<-1,modifiedData$popularity<-0)
table(modifiedData$popularity)

#splitting into training and testing data. Here,0.8 i.e. 80% is taken as training data
index <- sample(nrow(modifiedData), 0.8 * nrow(modifiedData))
trainingData <- modifiedData[index, ]
testingData <- modifiedData[-index, ]
table(trainingData$popularity)
table(testingData$popularity)

#or use createDataPartition
#library(caret)
#library(mlbench)
#index<- createDataPartition(y= modifiedData$popularity,p = 0.8,list=FALSE)
#trainingData<- modifiedData [index,]
#testingData<- modifiedData [-index,]

#Logistic Regression model
#Build Logistic Model on Training Dataset
start_time <- Sys.time()
logitMod <- glm(as.formula(formulaString), family="binomial", data = trainingData)
end_time <- Sys.time()
time_taken_LM = end_time - start_time
print(time_taken_LM)

#Predict Y on Test Dataset
predictedY <- predict(logitMod, testingData, type="response") 

#continuous to categorical
predictedbinary<-ifelse(predictedY>=0.5,1,0)
table(testingData$popularity)
table(predictedbinary)

confusionMatrix(predictedbinary,testingData$popularity)

#ROC curve
ROC_LM<-roc(predictedbinary,testingData$popularity)
print(ROC_LM)
plot(ROC_LM)

#Random forest model
start_time <- Sys.time()
rfresults<-randomForest(as.formula(formulaString),data=trainingData,mtry=7,ntree=500,importance=TRUE)
end_time <- Sys.time()
time_taken_RF = end_time - start_time
print(time_taken_RF)
plot(rfresults, main="Random forest error rate")
print(rfresults)
      
rfprediction<-predict(rfresults,testingData)
rfpredictionbinary<-ifelse(rfprediction >=0.5,1,0)

confusionMatrix(rfpredictionbinary,testingData$popularity)

ROC_RF<-roc(rfpredictionbinary,testingData$popularity)
print(ROC_RF)
plot(ROC_RF)

#SVM model
start_time <- Sys.time()
svm_results<-svm(as.formula(formulaString),data=trainingData)
end_time <- Sys.time()
time_taken_SVM = end_time - start_time
print(time_taken_SVM)

svmprediction<-predict(svm_results,testingData,type="response")
svmpredictionbinary<-ifelse(svmprediction >=0.5,1,0)

confusionMatrix(svmpredictionbinary,testingData$popularity)

ROC_SVM<-roc(svmpredictionbinary,testingData$popularity)
print(ROC_SVM)
plot(ROC_SVM)

#Comparing all the three ROC
plot(ROC_LM,col= 2, main="ROC curves comparing classification performance")
legend(-0.2, 1.0, c('lm', 'rf', 'svm'), 2:4)
plot(ROC_RF, col=3, add=TRUE)
plot(ROC_SVM, col=4, add=TRUE)

#Cross validation, learning curve
chunksize<-nrow(trainingData)%/%10
print(chunksize)
learnings<-data.frame(m = integer(10),
                      dfsize=integer(10),
                      cvaccuracy = integer(10))
for (i in seq(1,10)){
  learnings$m[i]<-i
  learnings$dfsize[i]<-i*chunksize+3
  
  #For Logistic Regression model
  #logit_Model <<- glm(as.formula(formulaString), family="binomial", data = trainingData[1:learnings$dfsize[i],])
  #predictY <<- predict(logit_Model, testingData, type="response") 
  
  #For Random Forest model
  rf_model<-randomForest(as.formula(formulaString),data = trainingData[1:learnings$dfsize[i],],ntree=500,mtry=7)
  predictY <<- predict(rf_model, testingData, type="response")
  
  #continuous to categorical
  predicted_binary<<-ifelse(predictY>=0.5,1,0)
  
  cm<-confusionMatrix(predicted_binary,testingData$popularity)
  print(cm)
  
  learnings$cvaccuracy[i]<-cm$overall['Accuracy']
}
print(learnings)
plot(learnings$dfsize,learnings$cvaccuracy,type="o",main="Learning curve", xlab="data chunk size",ylab="prediction accuracy")

#Reverting to categorical values
modifiedData$popularity<-ifelse(modifiedData$popularity==1,modifiedData$popularity<-"Hit",modifiedData$popularity<-"Flop")  
table(modifiedData$popularity)

