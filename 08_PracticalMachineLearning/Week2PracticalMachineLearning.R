## **CARET PACKAGE**
### SPAM Example: Data splitting

library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)



## Fit a model
set.seed(32343)
modelFit <- train(type~.,data=training, method="glm")
modelFit


modelFit$finalModel

predictions <- predict(modelFit, newdata=testing)
predictions

confusionMatrix(predictions, testing$type)


# DATA SLICING
dim(training)

## K-fold
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = TRUE)
sapply(folds, length)
folds[[1]][1:10]

## Resampling
set.seed(32323)
folds <- createResample(y=spam$type, times=10, list=TRUE)
sapply(folds, length)
folds[[1]][1:10]

##Time Slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow=20, horizon = 10)
names(folds)

folds$train[[1]]

folds$test[[1]]

# TRAINING OPTIONS

#args(train.default)
#function(x,y, method="rf", preProcess=NULL, ..., weights=NULL,
#         metric=ifelse(is.factor(y), "Accuracy", "RMSE"), maximize=ifelse(metric=="RMSE",FALSE,TRUE), 
#         trControl=trainControl=trainControl(), tuneGrid= NULL,
#         tuneLength=3)
#    NULL

## setting seed
set.seed(1235)
modelFit2 <- train(type~.,data=training, method="glm")
modelFit2

# PLOTTING PREDICTORS

### example predicting wages
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

featurePlot(x=training[,c("age", "education","jobclass")],
            y=training$wage,
            plot="pairs")
qplot(age, wage, data=training)

qplot(age, wage, colour=jobclass, data=training)

qq <- qplot(age,wage,colour=education, data=training)
qq +geom_smooth(method='lm', formula=y~x)

library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)

p1 <- qplot(cutWage,age,data=training, fill=cutWage,
            geom=c("boxplot"))
p1

library(gridExtra)
p2 <- qplot(cutWage,age,data=training, fill=cutWage,
            geom=c("boxplot","jitter"))
grid.arrange(p1,p2, ncol=2)


#Tables
t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1)

qplot(wage,colour=education,data=training, geom="density")


## plots only in the training set, 
## dont use the test set for exploration

# BASIC PREPROCESSING
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve,main="", xlab="ave. capital run length")

mean(training$capitalAve)
sd(training$capitalAve)

trainCapAve <-training$capitalAve
traincapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(traincapAveS)
sd(traincapAveS)

### preProcess function
preobj <- preProcess(training[,-58], method=c("center", "scale"))
traincapAveS <- predict(preobj, training[,-58])$capitalAve
mean(traincapAveS)
sd(traincapAveS)

set.seed(32343)
modelFit <- train(type~.,data=training, method="glm")
modelFit

preObj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)


## Standardizing - Imputing data
set.seed(13343)
### Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05==1)
training$capAve[selectNA] <- NA

### Impute and standardize
preObj <- preProcess(training[,-58], method="knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

### Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)



quantile(capAve-capAveTruth)
quantile((capAve-capAveTruth)[selectNA])
quantile((capAve-capAveTruth)[!selectNA])


# QUIZ 2
## QUESTION 1
#Load the Alzheimer's disease data using the commands:
library(AppliedPredictiveModeling)
data(AlzheimerDisease)


adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

## QUESTION 2
#Load the cement data using the commands:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[inTrain, ]
testing = mixtures[-inTrain, ]

#Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
#Color by each of the variables in the data set (you may find the cut2() 
#function in the Hmisc package useful for turning continuous covariates 
#into factors). What do you notice in these plots?
#First let's get the names of the columns to subset on them later
names <- colnames(concrete)
names <- names[-length(names)]
#Now let's make a quick feature plot to see if there is any relation between the 
#outcome CompressiveStrength and the rest of the parameters in the data:
featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")
#It is clear from this plot that there is no relation between the outcome and any of 
#the other variables int he data set.
#Now we'll make a plot of the outcome as a function of the index 
index <- seq_along(1:nrow(training))
ggplot(data = training, aes(x = index, y = CompressiveStrength)) + geom_point() + 
    theme_bw()
#It is clear from this figure that there is a step-like pattern in the data 
#that could be explained by one or more variable in the data.
#From this plot we should probably cut the outcome in 4 categories
cutCS <- cut2(training$CompressiveStrength, g = 4)
summary(cutCS)
ggplot(data = training, aes(y = index, x = cutCS)) + geom_boxplot() + geom_jitter(col = "blue") + 
    theme_bw()
featurePlot(x = training[, names], y = cutCS, plot = "box")
#Again, none of the variables in the data can explaing the step-like 
#behaviour in the outcome.
