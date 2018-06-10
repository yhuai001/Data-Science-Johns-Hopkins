#Practical ML

library(ElemStatLearn)
data(prostate)
str(prostate)

small = prostate[1:5,]
lm(lpsa ~., data=small)

#Combining classifiers
#1.Bagging,boosting,random forest
#2.Model ensembling/stacking

library(ISLR);data(Wage);library(ggplot2);library(caret);
Wage <- subset(Wage, select = -c(logwage))

inBuild <- createDataPartition(y=Wage$wage,
                               p=0.7,list = FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]

inTrain <- createDataPartition(y=buildData$wage,
                               p=0.7,list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]

dim(training)

mod1 <- train(wage ~., method="glm",data = training)
mod2 <- train(wage ~., method="rf", data = training, trControl=trainControl(method="cv"),number=3)

pred1 <- predict(mod1,testing)
pred2 <- predict(mod2,testing)
qplot(pred1,pred2,colour=wage,data = testing)


predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage~., method="gam",data=predDF)
combPred <- predict(combModFit,predDF)

sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2))

pred1V <- predict(mod1,validation)
pred2V <- predict(mod2,validation)
predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
combPredV <- predict(combModFit,predVDF)

#Forecasting
library(quantmod)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="google", from=from.dat, to = to.dat)


#Unsupervised Prediction
#build predictor for clusters



#quiz
#1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
library(caret)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
rfvowel <- train(y ~ ., vowel.train, method = "rf") 
predrf <- predict(rfvowel, newdata = vowel.test)
confusionMatrix(predrf, vowel.test$y) 


gbmvowel <- train(y ~ ., vowel.train, method = "gbm", verbose = FALSE)
predgbm <- predict(gbmvowel, vowel.test)
confusionMatrix(predgbm, vowel.test$y)

modelagreed <- (predrf == predgbm)
confusionMatrix(vowel.test$y[modelagreed], predrf[modelagreed])$overall['Accuracy']


#2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)

data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
mod_rf <- train(diagnosis ~ ., training, method = "rf")
mod_gbm <- train(diagnosis ~ ., training, method = "gbm", verbose = FALSE)
mod_lda <- train(diagnosis ~ ., training, method = "lda")

rfpred <- predict(mod_rf, testing)
gbmpred <- predict(mod_gbm, testing)
ldapred <- predict(mod_lda, testing)

confusionMatrix(rfpred, testing$diagnosis)$overall['Accuracy']
confusionMatrix(gbmpred, testing$diagnosis)$overall['Accuracy']
confusionMatrix(ldapred, testing$diagnosis)$overall['Accuracy']

combine <- data.frame(rfpred, gbmpred, ldapred, diagnosis = testing$diagnosis)
mod_comb <- train(diagnosis ~ ., combine, method = "rf")
combpred <- predict(mod_comb, testing)
confusionMatrix(testing$diagnosis, combpred)$overall['Accuracy']


#3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
modlasso <- train(CompressiveStrength ~ ., training, method = "lasso")

library("elasticnet")
plot.enet(modlasso$finalModel, xvar="penalty", use.color=TRUE)





#4
library(lubridate) # For year() function below

dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)
modfc <- bats(tstrain)
predts <- forecast(modfc, level = 95, nrow(testing))
fslower95 <- predts$lower
fsupper95 <- predts$upper
table ((testing$visitsTumblr>fslower95) & (testing$visitsTumblr<fsupper95))

226/nrow(testing)


#5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]

library(e1071)
set.seed(325)
fit <- svm(CompressiveStrength ~ ., training)
predsvm <- predict(fit, testing)
error = predsvm - testing$CompressiveStrength
mse <- sqrt(mean(error^2))
mse