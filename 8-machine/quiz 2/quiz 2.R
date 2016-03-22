##Quiz 2
library(caret)
library(kernlab)

##1
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

Answer #1
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

##2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[inTrain,]
testing = mixtures[-inTrain,]

suppressMessages(library(dplyr))
suppressMessages(library(Hmisc))
suppressMessages(library(gridExtra))

training <- mutate(training, index=1:nrow(training))
cutIndex <- cut2(training$index, g=10)
breaks <- 10

qplot(index, CompressiveStrength, data=training, color=cut2(training$Cement, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$BlastFurnaceSlag, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$FlyAsh, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$Water, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$Superplasticizer, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$CoarseAggregate, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$FineAggregate, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$Age, g=breaks))

Answer #2

##3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

par(mfrow = c(2, 1), mar = c(4, 2, 2, 2))
hist(training$Superplasticizer, main = "")
hist(log(training$Superplasticizer + 1), main = "")

Answer #3
There are a large number of values that are the same and even if you took the log(SuperPlasticizer + 1)
they would still all be identical so the distribution would not be symmetric.

##4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

predName <- names(training)
ILpredictor <- predName[substr(predName, 1, 2) == "IL"]

##"IL_11"                           
##[59] "IL_13"                            "IL_16"                           
##[61] "IL_17E"                           "IL_1alpha"                       
##[63] "IL_3"                             "IL_4"                            
##[65] "IL_5"                             "IL_6"                            
##[67] "IL_6_Receptor"                    "IL_7"                            
##[69] "IL_8"   

ProcPCA <- preProcess(training[, ILpredictor], method = "pca", thresh = .8)
ProcPCA$numComp

##Answer # 4: 7

##5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# The model using all the predictors (Non-PCA)
trainingIL <- training[, c(ILpredictor, "diagnosis")]
testingIL <- testing[, c(ILpredictor, "diagnosis")]
ModelAll <- train(diagnosis ~ ., data = trainingIL, method = "glm")
confusionMatrix(testingIL$diagnosis, predict(ModelAll, testingIL))
## Confusion Matrix and Statistics

# The model using PCA with principal components explaining 80% of the variance in the predictors
preProc <- preProcess(training[, ILpredictor], method = "pca", thresh = .8)
trainPC <- predict(preProc, training[, ILpredictor])
ModelPCA <- train(trainingIL$diagnosis ~ ., method = "glm", data = trainPC)
testPC <- predict(preProc, testing[, ILpredictor])
confusionMatrix(testingIL$diagnosis, predict(ModelPCA, testPC))

##Answer #1 0.65, 0.72
