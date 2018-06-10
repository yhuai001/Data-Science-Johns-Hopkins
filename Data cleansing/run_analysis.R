#data-cleaning week 4 assignment 
#run_analysis.R
library(data.table)

##Download and unzip
dataset <- "Dataset.zip"
if (!file.exists(dataset)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
    download.file(fileURL, dataset)
}  
if (!file.exists("UCI HAR Dataset")) { 
    unzip(dataset) 
}

## Load datasets
#1.Labels and features
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

#2.Train and test data
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
Subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
Subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

##Merge
X <- rbind(X_train, X_test)
Y <- rbind(Y_train, Y_test)
S <- rbind(Subject_train, Subject_test)

##Extract the mean and standard
measurements <- features[grep("mean\\(\\)|std\\(\\)", features[ ,2]), ]
X<- X[ ,measurements[ ,1]]

##Name the activities and subjects
colnames(Y) <- "activity"
Y$activityLabels <- factor(Y$activity, labels = as.character(activity_labels[ ,2]))
activityLabels <- Y[,-1]

colnames(X) <- features[measurements[ ,1] ,2]
colnames(S) <- "subject"

##Create the total table
total <- cbind(X, activityLabels, S)
total_mean <- setDT(total)[ ,lapply(.SD, mean), by = .(subject,activityLabels)]

##Output 
write.table(total_mean, file = "./UCI HAR Dataset/tidydateset.txt", row.names = FALSE, col.names = TRUE)