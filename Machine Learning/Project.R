#Project
#exactly according to the specification (Class A)
#throwing the elbows to the front (Class B)
#lifting the dumbbell only halfway (Class C)
#lowering the dumbbell only halfway (Class D)
#throwing the hips to the front (Class E)
library(caret);library(randomForest);library(gbm);library(ggplot2);library(corrplot);library(rpart);library(rpart.plot)

TrainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
TestUrl  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv("pml-training.csv")
testing  <- read.csv("pml-testing.csv")
dim(training);dim(testing)

#Remove NA ,nearly zero variance,identification
NZV <- nearZeroVar(training)
training <- training[, -NZV]
testing   <- testing[, -NZV]
dim(training);dim(testing)

training <-training[,colSums(is.na(training)) == 0]
testing <-testing[,colSums(is.na(testing)) == 0]
dim(training);dim(testing)

head(colnames(training),10)
training <- training[, -(1:7)]
testing  <- testing[, -(1:7)]
dim(training);dim(testing)

corMatrix <- cor(training[, -52])
corrplot(corMatrix, method = "square")


inTrain <- createDataPartition(training$classe,
                               p=0.7, 
                               list=FALSE)
train_data <- training[inTrain,]
test_data <- training[-inTrain,]
dim(train_data); dim(test_data)



#Decision Tree
set.seed(233)
mod_dt <- rpart(classe ~., data=train_data, method = "class")
fancyRpartPlot(mod_dt)

predictDT <- predict(mod_dt, newdata = test_data, type="class")
confDT <- confusionMatrix(predictDT, test_data$classe)
confDT

#Prediction Model
set.seed(1993)
fitControl <- trainControl(
    method = "repeatedcv",
    ## 3-fold CV
    number = 3,
    ## repeated 3 times
    repeats = 3)
mod_rf <- randomForest(classe ~ . , data=train_data, ntree=100)
mod_gbm <- train(classe ~ . , data=train_data, method = "gbm", trControl=fitControl, verbose = FALSE)
mod_lda <- train(classe ~ . , data=train_data, method = "lda", trControl=fitControl, verbose = FALSE)

rfpred <- predict(mod_rf, newdata=test_data)
gbmpred <- predict(mod_gbm, newdata=test_data)
ldapred <- predict(mod_lda, newdata=test_data)

confusionMatrix(rfpred, test_data$classe)$overall['Accuracy']
confusionMatrix(gbmpred, test_data$classe)$overall['Accuracy']
confusionMatrix(ldapred, test_data$classe)$overall['Accuracy']



    