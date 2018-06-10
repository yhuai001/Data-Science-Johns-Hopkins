#Reproducible week2 
library(dplyr)
library(ggplot2)
library(scales)
library(Hmisc)

## Loading and preprocessing the data
activity <- read.csv("activity.csv")
head(activity)
str(activity)
summary(activity)

## What is mean total number of steps taken per day?
steps_day <- aggregate(activity$steps, by=list(activity$date), sum)

names(steps_day)[2] = "total_steps"
qplot(total_steps, data=steps_day, binwidth=666, main='Total number of steps taken per day')

summary(steps_day)
#mean is 10766, median is 10765

## What is the average daily activity pattern?
steps_interval <- aggregate(steps ~ interval, activity, mean)
g <- ggplot(steps_interval, aes(x = interval, y=steps)) 
g <- g + geom_line()
g <- g + labs(title = "Average Daily Activity Pattern", x = "5-min interval", y = "steps")
g
maximum <- steps_interval[which.max(steps_interval$steps), ]
maximum

## Imputing missing values
missing_values <- sum(is.na(activity$steps))
missing_values

imputed_data <- activity
imputed_data$steps <- impute(activity$steps, mean)

steps_day_imputed <- aggregate(imputed_data$steps, by=list(imputed_data$date),sum)

names(steps_day_imputed)[2] = "total_steps_imputed"
qplot(total_steps_imputed, data=steps_day_imputed, binwidth=666, main='Total number of steps taken per day(imputed)')

summary(steps_day_imputed)
#mean is 10766, median is 10766


## Are there differences in activity patterns between weekdays and weekends?
#POSIXlt wday: 0¨C6 day of the week, starting on Sunday
wday <- as.POSIXlt(imputed_data$date)$wday

#ifelse(test, yes, no)
imputed_data$weekend <- ifelse(wday %in% c(0,6), "weekend", "weekday") 

average_imputed_data <- aggregate(steps~interval + weekend, data = imputed_data, mean)

g <- ggplot(average_imputed_data , aes(interval, steps)) 
g <- g + geom_line(aes(color=weekend))  
g <- g + facet_grid(weekend ~ .) 
g <- g + labs(title="Difference", x = "interval", y = "avarage steps")
g