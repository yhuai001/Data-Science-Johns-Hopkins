#Predicting with trees
#Decision Tree : non-linear -- interactions between variables

data(iris);library(ggplot2);library(caret)
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y=iris$Species,p=0.7,list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training);dim(testing)

qplot(Petal.Width, Sepal.Width,colour=Species,data=training)
modFit <- train(Species ~., method="rpart",data=training)
print(modFit$finalModel)

plot(modFit$finalModel,uniform = T,main="Classification Tree")
text(modFit$finalModel, use.n = T,all = T,cex=.8)

library(rattle)
fancyRpartPlot(modFit$finalModel)
predict(modFit,newdata = testing)

#Bagging: bootstrap aggregating -- resample cases and recalculate
library(ElemStatLearn); data(ozone,package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

ll <- matrix(NA, nrow = 10, ncol = 155)
for (i in 1:10){
    ss <- sample(1:dim(ozone)[1], replace = T)
    ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
    loess0 <- loess(temperature~ozone, data=ozone0, span=0.2)
    ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}

plot(ozone$ozone, ozone$temperature, pch=19, cex=0.5)
for (i in 1:10){lines(1:155, ll[i,],col="grey",lwd=2)}
lines(1:155, apply(ll,2,mean), col="red",lwd=2)

predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
treebag <- bag(predictors, temperature, B=10,
               bagControl = bagControl(fit=ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

plot(ozone$ozone, temperature, pch=19, col="lightgrey")
points(ozone$ozone, predict(treebag$fits[[1]]$fit, predictors), pch=19, col="red")
points(ozone$ozone, predict(treebag, predictors), pch=19, col="blue")  



#Random forests: extension to bagging for classification and regression trees
#Bootstrap sample - At each split, bootstrap variables - grow multiple trees and vote
data(iris);library(ggplot2);library(caret);library(randomForest)
inTrain <- createDataPartition(y=iris$Species,p=0.7,list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
modFit <- train(Species ~. , data=training, method="rf", prox=TRUE)
modFit
getTree(modFit$finalModel,k=2)

irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species, data = training)
p + geom_point(aes(x=Petal.Width, y=Petal.Length,col=Species), size=5, shape=4, data=irisP)

pred <- predict(modFit,testing)
testing$predRight <- pred==testing$Species
table(pred, testing$Species)
qplot(Petal.Width,Petal.Length, col=predRight,data = testing,main = "newdata Predictions")



#Boosting
#1.take lots of possible weak predictors
#2.weight them and add them up
#3.get stronger predictors
#h1 h2 h3 ...
#error_rate1 ->new error weight -> 2  -> 3 .... 
#a1 2 3 ....
#Hfinal=sign(0.42a+0.65b+0.92c)
library(ISLR);data(Wage);library(ggplot2);library(caret)
Wage <- subset(Wage, select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage,p=0.7,list = F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
modFit <- train(wage ~. ,method="gbm",data=training, verbose=F)
modFit
qplot(predict(modFit,testing),wage,data=testing)
      

#Model Based Prediction
#conditional distribution
#Bayes theorem;naive bayes; linear discriminant analysis
#prior probabilities
#Gaussian distribution
#parameters(uk,¦Òk^2)
#P(Y=k|X=x)
data(iris);library(ggplot2)
names(iris)
table(iris$Species)
inTrain <- createDataPartition(y=iris$Species,p=0.7,list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training);dim(testing)

modlda = train(Species ~. , data=training, method="lda")
modnb = train(Species ~. , data = training, method="nb")
plda=predict(modlda,testing)
pnb=predict(modnb,testing)
table(plda,pnb)

equalPredictions=(plda==pnb)
qplot(Petal.Width,Petal.Length, col=equalPredictions,data = testing,main = "newdata Predictions")




#quiz
#1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rattle)
str(segmentationOriginal)

trainSet <- segmentationOriginal[segmentationOriginal$Case =="Train",]
testSet <- segmentationOriginal[segmentationOriginal$Case =="Test",]

set.seed(125)
model_rpart <- train(Class~.,data=trainSet,method="rpart")
fancyRpartPlot(model_rpart$finalModel)

#2
#If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy smaller or bigger? If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger? Is K large or small in leave one out cross validation ?
#The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.

#3
library(pgmm)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))
str(olive)
table(olive$Area)
olive_rpart <- train(Area~.,data=olive,method="rpart")
fancyRpartPlot(olive_rpart$finalModel)
predict(olive_rpart,newdata=newdata)

#4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
set.seed(13234)
regModel <- train(chd~age+alcohol+obesity+tobacco+typea+ldl,data=trainSA,method="glm",family="binomial")

missClassTrain <- missClass(trainSA$chd,predict(regModel,newdata=trainSA))
missClassTest <- missClass(testSA$chd,predict(regModel,newdata=testSA))
missClassTrain
missClassTest

#5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

set.seed(33833)
str(vowel.train)
library(randomForest)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
modelRF <- randomForest(y~.,data=vowel.train)
order(varImp(modelRF),decreasing=TRUE)
