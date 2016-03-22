##Quiz 3

##Q1
library(caret)
library(AppliedPredictiveModeling)
library(ElemStatLearn)
library(rattle)

data(segmentationOriginal)

names(segmentationOriginal)
table(segmentationOriginal$Case)

##intrain<-createDataPartition(y=segmentationOriginal$Case,p=0.6, list=FALSE)
intrain <- segmentationOriginal$Case == "Train"

training<-segmentationOriginal[intrain,]
testing<-segmentationOriginal[-intrain,]
dim(training)
dim(testing)

set.seed(125)
modelFit<-train(Class~., method="rpart", data=training)
print(modelFit$finalModel)

fancyRpartPlot(modelFit$finalModel)

##TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 -->PS
##TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 -->WS
##TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 -->PS
##FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 --> Cannot be determined

##Answer is Option 3

##Q2
##If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample
##(test set) accuracy smaller or bigger? If K is small is the variance in the estimate of 
##out-of-sample (test set) accuracy smaller or bigger. Is K large or small in leave one out 
##cross validation?

##Answer is Option 3: The bias is larger and the variance is smaller. 
##Under leave one out cross validation K is equal to the sample size.

##Q3
library(pgmm)
data(olive)
olive = olive[,-1]
names(olive)
nrow(olive)

modelFit <- train(Area ~ ., method = "rpart", data = olive)

newdata = as.data.frame(t(colMeans(olive)))
predict(modelFit, newdata = newdata)

##Answer is Opiton 4: 2.783282

##Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train<-sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA<-SAheart[train,]
testSA<-SAheart[-train,]

names(trainSA)

set.seed(13234)
modelFit<-train(chd~age+alcohol+obesity+tobacco+typea+ldl, data=trainSA, method="glm", family="binomial")
modelFit

predTrain<-predict(modelFit, newdata = trainSA)
predTest<-predict(modelFit, newdata = testSA)

missClass<-function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(testSA$chd, predTest) ##0.31
missClass(trainSA$chd, predTrain) ##0.27

##Answer is Option 4 0.31 and 0.27

##Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

str(vowel.train)
vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

str(vowel.train)

set.seed(33833)
##modelFit<-train(y~., method="rf", data=vowel.train, prox=TRUE)
modelFit <- randomForest(y ~ ., data = vowel.train)

print(modelFit)

imp<-varImp(modelFit)
order(imp, decreasing = T)
##Answer is Option is 1, the order of the variables is: x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7, x.10


        