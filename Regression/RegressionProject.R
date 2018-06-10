#Regression Models Course Project
library(datasets)
library(MASS)
library(ggfortify)
library(car)

data("mtcars")
str(mtcars)

mtcars$am <- factor(mtcars$am, labels=c("Automatic","Manual"))
pairs(mpg ~ ., data = mtcars, panel=panel.smooth, main="Overview")
 
#Boxplot shows it is clearly Manual transmission gets more MPG than automatic

boxplot(mtcars$mpg ~ mtcars$am, 
        xlab="Transmission Types", 
        ylab="MPG", 
        main="MPG  by Transmission Types", 
        col=c("red","yellow"))

#Regression Analysis 

#Select the model
my_lms <- lm(mpg ~. ,data = mtcars)
stepAIC(my_lms)

fit1 <- lm(mpg ~ am,data = mtcars)
fit2 <- lm(mpg ~ wt + qsec + am, data = mtcars)
fit3 <- lm(mpg ~ hp + wt + qsec + am,data = mtcars)
summary(fit2)
anova(fit2,fit3)
confint(fit2)

t.test(mpg ~ am, data = mtcars)

#Residual and Diagnostics
test <- lm(mpg ~ wt + qsec + am, data = mtcars)
autoplot(test)
outlierTest(test)
influencePlot(test)

#Conclusion


