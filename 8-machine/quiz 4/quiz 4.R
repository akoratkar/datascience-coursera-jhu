##Quiz 4

##Q1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

dim(vowel.train)
dim(vowel.test)

str(vowel.train)
vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)
str(vowel.train)

set.seed(33833)
modelFitRF<-train(y~., method="rf", data=vowel.train)

set.seed(33833)
modelFitGBM<-train(y~., method="gbm", data=vowel.train, verbose=FALSE)

predRF<-predict(modelFitRF, vowel.test)
predGBM<-predict(modelFitGBM, vowel.test)

table(predRF, predGBM)

confusionMatrix(vowel.test$y, predRF)$overall['Accuracy']
confusionMatrix(vowel.test$y, predGBM)$overall['Accuracy']

equalPred<-predRF==predGBM
confusionMatrix(vowel.test$y[equalPred], predRF[equalPred])$overall['Accuracy']

##Answer is Option 2: 0.599, 0.52, 0.63

##Q2
library(gbm)
library(AppliedPredictiveModeling)


set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
modelFitRF<-train(diagnosis~., method="rf", data=training)

set.seed(62433)
modelFitGBM<-train(diagnosis~., method="gbm", data=training)

set.seed(62433)
modelFitLDA<-train(diagnosis~., method="lda", data=training)

predRF<-predict(modelFitRF, testing)
predGBM<-predict(modelFitGBM, testing)
predLDA<-predict(modelFitLDA, testing)

predDF = data.frame(predRF,predGBM,predLDA, diagnosis=testing$diagnosis)
combModFit <- train(diagnosis ~ ., method = "rf", data=predDF)
predComb<-predict(combModFit, testing)
  
confusionMatrix(testing$diagnosis, predRF)$overall['Accuracy'] ##0.7804878 
confusionMatrix(testing$diagnosis, predGBM)$overall['Accuracy'] ##0.804878 
confusionMatrix(testing$diagnosis, predLDA)$overall['Accuracy'] ##0.7682927 
confusionMatrix(testing$diagnosis, predComb)$overall['Accuracy'] 0.8170732 

##Answer is option 3: Stacked Accuracy: 0.80 is better than all three other methods

##Q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
modelFitLasso<-train(CompressiveStrength ~ ., data = training, method = "lasso")
plot.enet(modelFitLasso$finalModel, xvar="penalty", use.color=TRUE)
modelFitLasso$finalModel$beta.pure

##Answer is 3: Cement

##Q4
library(lubridate) # For year() function below
dat<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")

training<-dat[year(dat$date) < 2012,]
testing<-dat[(year(dat$date)) > 2011,]

tstrain<-ts(training$visitsTumblr)

library(forecast)
# build a bats model based on the original time series
smoothedvisits<-bats(tstrain)

forecastvisits<-forecast(smoothedvisits, nrow(testing))
# plot the forecast
plot(forecastvisits)

# extracting the 95% prediction boundaries
forecastvisitsl95<-forecastvisits$lower[,2]
forecastvisitsu95<-forecastvisits$upper[,2]

# see how many of the testing visit counts do actually match
x<-table ( 
  (testing$visitsTumblr>forecastvisitsl95) & 
    (testing$visitsTumblr<forecastvisitsu95))

x[2]/nrow(testing)

##Answer is Option 3: 0.9617021 

##Q5
set.seed(3523)
library(AppliedPredictiveModeling)

data(concrete)

inTrain<-createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training<-concrete[inTrain,]

testing<-concrete[-inTrain,]

library("e1071")
set.seed(325)

modelFitSVM<-svm(CompressiveStrength ~ .,data=training)
predSVM<-predict(modelFitSVM, newdata = testing)
errSVM<-predSVM - testing$CompressiveStrength

sqrt(mean(errSVM^2))

plot(predSVM, testing$CompressiveStrength, 
     pch=20, cex=2, 
     col=testing$Age)

##Answer is option 2. 6.715
