#Regression3 
#Multivariable regression
n=100;x=rnorm(n);
x2=rnorm(n);x3=rnorm(n)
y= 1 + x + x2 + x3 + rnorm(n, sd=.1)
ey = resid(lm(y ~ x2+x3))
ex = resid(lm(x ~ x2+x3))
sum(ey*ex)/ sum(ex^2)
coef(lm(ey~ex-1))
coef(lm(y~x+x2+x3))

require(datasets);data(swiss);?swiss
require(GGally);require(ggplot2)
g = ggpairs(swiss, lower = list(continuous="smooth"))
g

summary(lm(Fertility ~. , data = swiss))
summary(lm(Fertility ~. , data = swiss))$coef
#Our models estimates an expected 0.17 decrease in standardized fertility for every 1% increase in percentage of males involved in agriculture in holding the remaining variables constant
summary(lm(Fertility ~ Agriculture , data = swiss))$coef

n <- 100;x2 <- 1:n;x1 <- .01*x2+runif(n,-.1,.1);y=-x1+x2+rnorm(n,sd=.01)
summary(lm(y~x1))$coef
summary(lm(y~x1+x2))$coef



data(InsectSprays);require(stats);require(ggplot2)
g <- ggplot(data=InsectSprays,aes(y=count,x=spray,fill=spray))
g <- g + geom_violin(colour="black",size=2)
g
head(InsectSprays)
summary(lm(count~spray, data=InsectSprays))$coef
summary(lm(count~spray-1, data=InsectSprays))$coef


library(datasets);data(swiss)
head(swiss)
library(dplyr)
swiss = mutate(swiss, CatholicBin = 1 * (Catholic>50))
g <- ggplot(swiss,aes(x=Agriculture, y=Fertility, colour=factor(CatholicBin)))
g <- g + geom_point(size=6)

fit = lm(Fertility~Agriculture, data=swiss)
g <- g + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2],size=2)
g
summary(fit)$coef

fit = lm(Fertility~Agriculture * factor(CatholicBin), data=swiss)
g <- g + geom_abline(intercept = coef(fit)[1]+coef(fit)[3], slope = coef(fit)[2]+coef(fit)[4],size=2)
g
summary(fit)$coef

#Adjustment
#Residuals
data(swiss);par(mfrow=c(2,2))
fir <- lm(Fertility ~. , data=swiss);plot(fit)

n <- 100; x <- c(10,rnorm(n)); y <- c(10,c(rnorm(n)))
plot(x,y,frame=F, cex=2, pch=21, bg="lightblue", col="black")
abline(lm(y~x))
fit <- lm(y~x)
round(dfbetas(fit)[1:10,2],3)
round(hatvalues(fit)[1:10],3)


#Model Selection
#1.Variance inflation
n <- 100; nosim <- 1000
x1 <- rnorm(n);x2 <- rnorm(n);x3 <- rnorm(n)
x1 <- rnorm(n);x2 <- x1/sqrt(2)+rnorm(n)/sqrt(2);x3 <- x1*0.95+rnorm(n)*sqrt(1-0.95^2)
betas <- sapply(1:nosim, function(i){
    y <- x1 + rnorm(n,sd=.3)
    c(coef(lm(y~x1))[2],
      coef(lm(y~x1+x2))[2],
      coef(lm(y~x1+x2+x3))[2])
})
round(apply(betas,1,sd),5)


fit1 <- lm(Fertility~Agriculture,data=swiss)
fit3 <- update(fit1, Fertility~Agriculture + Examination + Education)
fit5 <- update(fit3, Fertility~Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)


#quiz
#1
mtcars <- mutate(mtcars, cyl=factor(cyl))
fit1 <- lm(mpg ~ cyl + wt, mtcars)
sf1 <- summary(fit1)$coef
sf1

#2
fit2 <- lm(mpg ~ cyl, mtcars)
sf2 <- summary(fit2)$coef
compare <- data.frame(with_wt=sf1[3],without_wt=sf2[3], row.names = "8-cyl Est.")
compare

#3
library(lmtest)
fit3 <- lm(mpg ~ cyl*wt, mtcars)
test <- lrtest(fit1,fit3)
pval <- test$`Pr(>Chisq)`[2]
test

#4
fit4 <- lm(mpg ~ I(wt * 0.5) + cyl, mtcars)
summary(fit4)$coef
compare <- data.frame(per_half_ton=sf1[4], per_ton=summary(fit4)$coef[2],
                      row.names = "wt coef")
compare

#5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit5 <- lm(y ~ x)
hatvalues(fit5)
max_hat <- hatvalues(fit5)[which.max(hatvalues(fit5))]
max_hat
plot(x, y, pch=19)
abline(fit5, col="red")
fit6 <- lm(y[-which.max(hatvalues(fit5))] ~ x[-which.max(hatvalues(fit5))])
abline(fit6, col="blue")


#6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)  
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit5 <- lm(y ~ x)
im <- influence.measures(fit5)$infmat
im
max_beta <- im[which.max(abs(im[,"hat"])),"dfb.x"]
max_beta

#7
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)  
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
set.seed(4567)
z <- rnorm(5)

fit <- lm(y ~ x)
fitz <- lm(y ~ x + z)

summary(fit)$coef
