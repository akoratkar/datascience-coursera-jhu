library(caret)
library(kernlab)

data(spam)
intrain<-createDataPartition(y=spam$type,p=0.75, list=FALSE)
training<-spam[intrain,]
testing<-spam[-intrain,]

set.seed(32343)
folds<-createFolds(y=spam$type,k=10, list=TRUE, returnTrain=TRUE)
sapply(folds, length)
folds[[1]][1:10]

set.seed(32343)
folds<-createResample(y=spam$type,times=10, list=TRUE)
sapply(folds, length)
folds[[1]][1:10]

set.seed(32343)
folds<-createTimeSlices(y=1:1000,initialWindow=20, horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]

dim(training)
dim(testing)

set.seed(32343)
modelFit<-train(type~., data=training, method="glm")
modelFit
modelFit$finalModel

predictions<-predict(modelFit, newdata=testing)
predictions

confusionMatrix(predictions, testing$type)

##-----
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
summary(Wage)

intrain<-createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training<-Wage[intrain,]
testing<-Wage[-intrain,]
dim(training)
dim(testing)

featurePlot(x=training[,c("age","education", "jobclass")], y=training$wage, plot="pairs")
qplot(age, wage, data=training)
qplot(age, wage,colour=jobclass, data=training)

qq<-qplot(age, wage,colour=education, data=training)
qq+geom_smooth(method="lm", formula=y~x)

library(Hmisc)
cutWage<-cut2(training$wage, g=3)
table(cutWage)
p1<-qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot"))
p2<-qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot", "jitter"))
grid.arrange(p1,p2,ncol=2)

t1<-table(cutWage, training$jobclass)
prop.table(t1,1)

p3<-qplot(wage, colour=education, data=training, geom="density")

##---

data(spam)
intrain<-createDataPartition(y=spam$type,p=0.75, list=FALSE)
training<-spam[intrain,]
testing<-spam[-intrain,]
hist(training$capitalAve, main="", xlab="ave.capital run length")
mean(training$capitalAve)
sd(training$capitalAve)

trainCapAve<-training$capitalAve
trainCapAveS<-(trainCapAve-mean(training$capitalAve))/sd(training$capitalAve)
mean(trainCapAveS)
sd(trainCapAveS)

testCapAve<-testing$capitalAve
testCapAveS<-(testCapAve-mean(training$capitalAve))/sd(training$capitalAve)
mean(testCapAveS)
sd(testCapAveS)

preObj<-preProcess(training[,-58],method=c("center", "scale"))
testCapAveS<-predict(preObj, training[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

testCapAveS<-predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

set.seed(32343)
modelFit<-train(type~., data=training, preProcess=c("center", "scale"), method="glm")
modelFit

preObj<-preProcess(training[,-58],method=c("BoxCox"))
trainapAveS<-predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)

data(Wage)
intrain<-createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training<-Wage[intrain,]
testing<-Wage[-intrain,]
dummies<-dummyVars(wage~jobclass, data=training)
head(predict(dummies, newdata=training))

nsv<-nearZeroVar(training, saveMetrics=TRUE)
nsv

library(splines)
bsBasis<-bs(training$age, df=3)
head(bsBasis)

lm1<-lm(wage~bsBasis, data=training)
plot(training$age, training$wage)
points(training$age, predict(lm1, newdata=training), col="red")

predict(bsBasis, age=testing$age)

##-PCA-
data(spam)
intrain<-createDataPartition(y=spam$type,p=0.75, list=FALSE)
training<-spam[intrain,]
testing<-spam[-intrain,]

M<-abs(cor(training[,-58]))
diag(M)<-0
which(M>0.85, arr.ind=T)

names(spam)[c(34,32)]
plot(spam[,34], spam[,32])

##----Week 3-1: Predicting with Trees

library(ggplot2)
library(caret)
data(iris)
names(iris)
table(iris$Species)

intrain<-createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training<-iris[intrain,]
testing<-iris[-intrain,]
dim(training)
dim(testing)

qplot(Petal.Width,Sepal.Width, colour=Species, data=training)

modelFit<-train(Species~., method="rpart", data=training)
print(modelFit$finalModel)

plot(modelFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modelFit$finalModel, use.n=TRUE, all=TRUE, cex=0.8)

library(rattle)
fancyRpartPlot(modelFit$finalModel)

predict(modelFit, newdata=testing)

##----Week 3-3 Random Forest
library(ggplot2)
library(caret)
data(iris)
names(iris)
table(iris$Species)

intrain<-createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training<-iris[intrain,]
testing<-iris[-intrain,]
dim(training)
dim(testing)

modelFit<-train(Species~., method="rf", data=training, prox=TRUE)
print(modelFit)
getTree(modelFit$finalModel, k=2)

irisP<-as.data.frame(classCenter(training[,c(3,4)], training$Species, modelFit$finalModel$prox))
irisP$Species<-rownames(irisP)
p<-qplot(Petal.Width,Sepal.Width, colour=Species, data=training)
p+geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size=5, shape=4, data=irisP)

pred<-predict(modelFit, testing)
testing$predRight<-pred==testing$Species
table(pred, testing$Species)

qplot(Petal.Width,Sepal.Width, colour=predRight, data=testing)

##Week 3 - 4 Boosting
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
summary(Wage)
Wage<-subset(Wage, select=-c(logwage))

intrain<-createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training<-Wage[intrain,]
testing<-Wage[-intrain,]
dim(training)
dim(testing)

modelFit<-train(wage~., method="gbm", data=training, verbose=FALSE)
print(modelFit)

pred<-predict(modelFit, testing)
qplot(pred, wage, data=testing)

##Week 3 - 4 Model Based Prediction
library(ggplot2)
library(caret)
data(iris)
names(iris)
table(iris$Species)

intrain<-createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training<-iris[intrain,]
testing<-iris[-intrain,]
dim(training)
dim(testing)

modelFitLDA<-train(Species~., method="lda", data=training)
print(modelFitLDA)

modelFitNB<-train(Species~., method="nb", data=training)
print(modelFitNB)

predLDA<-predict(modelFitLDA, testing)
predNB<-predict(modelFitNB, testing)
table(predLDA, predNB)

equalPred<-predLDA==predNB
qplot(Petal.Width,Sepal.Width, colour=equalPred, data=testing)

##-- Week 4 -2 Combining Predictors
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
summary(Wage)
Wage<-subset(Wage, select=-c(logwage))

inBuild<-createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
buildData<-Wage[inBuild,]
validation<-Wage[-inBuild,]
inTrain<-createDataPartition(y=buildData$wage,p=0.7, list=FALSE)
training<-buildData[inTrain,]
testing<-buildData[-inTrain,]
dim(training)
dim(testing)
dim(validation)

mod1<-train(wage~., method="glm", data=training)
print(mod1)
mod2<-train(wage~., method="rf", data=training, trControl=trainControl(method="cv"), number=3)
print(mod2)

pred1<-predict(mod1, newdata=testing)
pred2<-predict(mod2, newdata=testing)
qplot(pred1, pred2, colour=wage, data=testing)

predDF<-data.frame(pred1, pred2, wage=testing$wage)
combModFit<-train(wage~., method="gam", data=predDF)
combPred<-predict(combModFit, newdata=predDF)

sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2))

pred1V<-predict(mod1, newdata=validation)
pred2V<-predict(mod2, newdata=validation)
predVDF<-data.frame(pred1=pred1V, pred2=pred2V)
combPredV<-predict(combModFit, newdata=predVDF)

sqrt(sum((pred1V-validation$wage)^2))
sqrt(sum((pred2V-validation$wage)^2))
sqrt(sum((combPredV-validation$wage)^2))

##Week 4 - 3 Forecasting
library(quantmod)
library(forecast)
from.dat<-as.Date("01/01/08", format="%m/%d/%y")
to.dat<-as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="google", from=from.dat, to=to.dat)

GOOG<-subset(GOOG, select=-c(GOOG.Volume))
head(GOOG)

mGoog<-to.monthly(GOOG)
googOpen<-Op(mGoog)
ts1<-ts(googOpen, frequency=12)
plot(ts1)

plot(decompose(ts1))

ts1Train<-window(ts1, start=1, end=5)
ts1Test<-window(ts1, start=5, end=(7-0.01))

plot(ts1Train)
lines(ma(ts1Train, order=3), col="red")

##Exponential Smoothening
ets1<-ets(ts1Train, mode="MMM")
fcast<-forecast(ets1)
plot(fcast)
lines(ts1Test, col="red")
accuracy(fcast, ts1Test)

##Week 4 - 4 Unsupervised Clustering
library(ggplot2)
library(caret)
data(iris)
names(iris)
table(iris$Species)

intrain<-createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training<-iris[intrain,]
testing<-iris[-intrain,]
dim(training)
dim(testing)

kmeans1<-kmeans(subset(training, select=-c(Species)), center=3)
training$clusters<-as.factor(kmeans1$cluster)
qplot(Petal.Width,Sepal.Width, colour=clusters, data=training)
table(training$clusters, training$Species)

modelFit<-train(clusters~., method="rpart", data=subset(training, select=-c(Species)))
predcluster<-predict(modelFit, newdata=training)
table(predcluster, training$Species)

predTestcluster<-predict(modelFit, newdata=testing)
table(predTestcluster, testing$Species)
