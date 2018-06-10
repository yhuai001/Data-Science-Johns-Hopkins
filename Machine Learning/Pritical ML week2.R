#Pratical ML week2
library(caret);library(kernlab);data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75,list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32343)
modelFit <- train(type ~. , data=training, method="glm")
modelFit
modelFit$finalModel

prediction <- predict(modelFit, newdata=testing)
prediction

confusionMatrix(prediction, testing$type)

#slicing
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = TRUE)
sapply(folds,length)
folds[[1]][1:10]

folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = FALSE)
sapply(folds,length)
folds[[1]][1:10]

folds <- createResample(y=spam$type, times=10, list=TRUE)
sapply(folds,length)
folds[[1]][1:10]

tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow = 20, horizon = 10)
names(folds)
folds$train[[1]]
folds$test[[1]]


#training options
library(caret);library(kernlab);data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75,list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type ~. , data=training, method="glm")


args(train)
#RMSE root mean squared error
#RSquared 
#Accuracy = Fraction correct
#Kappa = A measure of concordance  

args(trainControl)
#boot = bootstrapping
#cv = cross validation
#repeatedcv
#LOOCV
library(ISLR);library(ggplot2);library(caret);
data(Wage)
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

featurePlot(x=training[,c('age','education','jobclass')],
            y=training$wage,
            plot = "pairs")
qplot(age,wage,data=training, colour=jobclass)
qq <- qplot(age,wage,data=training, colour=education)
qq + geom_smooth(method='lm', formula = y~x)

library(Hmisc);library(grid)
cutWage <- cut2(training$wage, g=3)
table(cutWage)

p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom = c("boxplot"))
p2 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)

t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1)

qplot(wage,colour=education,data = training, geom="density")




#Preprocessing
library(caret);library(kernlab);data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75,list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve, main ="", xlab="ave. captial run length")
mean(training$capitalAve)
sd(training$capitalAve)

trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/ sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

testCapAve <- testing$capitalAve
testCapAveS <- (trainCapAve - mean(trainCapAve))/ sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

modelFit <- train(type ~. , data=training, preProcess=c("center","scale"),method="glm")
modelFit

preObj <- preProcess(training[,-58], method = c('BoxCox'))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)


#Imputing data
set.seed(13343)
#Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob = 0.05)==1
training$capAve[selectNA] <- NA
#Impute and standardize
preObj <- preProcess(training[,-58], method = "knnImpute")
capAve <- predict(preObj, training[,-58])$capAve
#Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve - capAveTruth)
quantile((capAve-capAveTruth)[selectNA])
quantile((capAve-capAveTruth)[!selectNA])




#Covariate creation
library(kernlab);data(spam)
spam$capitalAveSq <- spam$capitalAve^2

library(ISLR);library(ggplot2);library(caret);data(Wage)
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

table(training$jobclass)
dummies <- dummyVars(wage~jobclass, data=training)
head(predict(dummies, newdata=training))

nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv

library(splines)
bsBasis <- bs(training$age, df=3)
bsBasis
lm1 <- lm(wage~bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=0.5)
predict(bsBasis, age=testing$age)


#Preprocessing with PCA
library(caret);library(kernlab);data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75,list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M>0.8, arr.ind = T)

names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

#Predicting with regression
library(caret);data(faithful);set.seed(666)
inTrain <- createDataPartition(y=faithful$waiting,
                               p=0.5,list=FALSE)
trainFaith <- faithful[inTrain,]; 
testFaith <-  faithful[-inTrain,]
head(trainFaith)
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="waiting", ylab="Duration")

lm1 <- lm(eruptions~waiting,data = trainFaith)
summary(lm1)
lines(trainFaith$waiting, lm1$fitted, lwd=3)

#predict new value
coef(lm1)[1] + coef(lm1)[2]*80

newdata <- data.frame(waiting=80)
predict(lm1, newdata)


par(mfrow=c(1,2))

plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="waiting", ylab="Duration")
lines(trainFaith$waiting, predict(lm1), lwd=3)

plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue", xlab="waiting", ylab="Duration")
lines(testFaith$waiting, predict(lm1, newdata=testFaith), lwd=3)

#get training set/test set errors
sqrt(sum((lm1$fitted - trainFaith$eruptions)^2))
sqrt(sum((predict(lm1,newdata=testFaith) - testFaith$eruptions)^2))

#prediction intervals
pred1 <- predict(lm1, newdata=testFaith, interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col='blue')
matlines(testFaith$waiting[ord], pred1[ord,], type = 'l', col = c(1,2,2), lty = c(1,1,1), lwd=3)

modelFit <- train(eruptions ~ waiting , data=traiFaith, method="lm")
summary(modelFit$finalModel)



#Multiple Covariates
library(ISLR);library(ggplot2);library(caret);data(Wage)
data(Wage); Wage <- subset(Wage, select= -c(logwage))
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training);dim(testing)

featurePlot(x=training[,c('age','education','jobclass')],
            y=training$wage,
            plot = "pairs")
qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)
qplot(age,wage,colour=education,data=training)
qplot(finMod$fitted, finMod$residuals, colour=race, data = training)

modFit <- train(wage~age+jobclass+education,
                method="lm", data=training)
finMod <- modFit$finalModel
print(modFit)

plot(finMod, 1, phc=19, cex=0.5, col="blue")
plot(finMod$residuals, pch=19)

pred <- predict(modFit, testing)
qplot(wage, pred, colour=year, data=testing)



















#quiz
#2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

uppressMessages(library(dplyr))
suppressMessages(library(Hmisc))
suppressMessages(library(gridExtra))
training <- mutate(training, index=1:nrow(training))
cutIndex <- cut2(training$index, g=10)
breaks <- 10
qplot(index, CompressiveStrength, data=training, color=cut2(training$Cement, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$BlastFurnaceSlag, g=breaks))
qplot(index, CompressiveStrength,
      data=training, color=cut2(training$FlyAsh, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$Water, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$Superplasticizer, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$CoarseAggregate, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$FineAggregate, g=breaks))
qplot(index, CompressiveStrength, data=training, color=cut2(training$Age, g=breaks))

hist(training$Superplasticizer, breaks=20)
hist(log(training$Superplasticizer+1), breaks=20)



####################
names <- colnames(concrete)
names <- names[-length(names)]
featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")
index <- seq_along(1:nrow(training))
ggplot(data = training, aes(x = index, y = CompressiveStrength)) + geom_point() + theme_bw()
cutCS <- cut2(training$CompressiveStrength, g = 4)
summary(cutCS)
ggplot(data = training, aes(y = index, x = cutCS)) + geom_boxplot() + geom_jitter(col = "blue") + theme_bw()
featurePlot(x = training[, names], y = cutCS, plot = "box")






#3
ggplot(data = training, aes(x = Superplasticizer)) + geom_histogram() + theme_bw()
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_col_idx <- grep("^[Ii][Ll].*", names(training))
preObj <- preProcess(training[, IL_col_idx], method=c("center", "scale", "pca"), thresh=0.9)
preObj
names(preObj)

#4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_col_idx <- grep("^[Ii][Ll].*", names(training))
suppressMessages(library(dplyr))
new_training <- training[, c(names(training)[IL_col_idx], "diagnosis")]
names(new_training)

IL_col_idx <- grep("^[Ii][Ll].*", names(testing))
suppressMessages(library(dplyr))
new_testing <- testing[, c(names(testing)[IL_col_idx], "diagnosis")]
names(new_testing)


#5library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


trainSmall <- data.frame(training[,grep('^IL',names(training))],training$diagnosis)
testSmall <- data.frame(testing[,grep('^IL',names(testing))],testing$diagnosis)
preProc <- preProcess(trainSmall[-13],method="pca",thres=.8)
trainPC <- predict(preProc,trainSmall[-13])
testPC <- predict(preProc,testSmall[-13])

PCFit <- train(trainSmall$training.diagnosis~.,data=trainPC,method="glm")
NotPCFit <- train(trainSmall$training.diagnosis~.,data=trainSmall,method="glm")

PCTestPredict <- predict(PCFit,newdata=testPC)
NotPCTestPredict <- predict(NotPCFit,newdata=testSmall)

confusionMatrix(PCTestPredict,testSmall$testing.diagnosis)
confusionMatrix(NotPCTestPredict,testSmall$testing.diagnosis)