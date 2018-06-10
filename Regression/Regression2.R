#Regression week2
#Statistical lm
library(UsingR)
data(diamond)
library(ggplot2)
g = ggplot(diamond, aes(x=carat, y=price))
g <- g + xlab("Mass(carats)")
g <- g + ylab("Pice(SIN $)")
g <- g + geom_point(size=6, colour="black", alpha=0.2)
g <- g + geom_point(size=5, colour="blue", alpha=0.2)
g <- g + geom_smooth(method='lm', colour="black")
g

fit <- lm(price~carat, data=diamond)
coef(fit)
summary(fit)
#We estimate an expected 3721.02(SIN) dollar increase in price for every carat increase in mass of diamond
#The intercept -259.63 is the expected price of a 0 carat diamond

fit2 <- lm(price~I(carat-mean(carat)), data=diamond)
summary(fit2)

newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2]*newx
predict(fit, newdata=data.frame(carat=newx))

#residuals
data("diamond")
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y~x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e-(y-yhat)))
max(abs(e-(y-coef(fit)[1]-coef(fit)[2]*x)))

plot(diamond$carat, diamond$price,
     xlab="Mass(carats)",
     ylab="Price",
     bg="lightblue",
     col="black",cex=1.1,pch=21,frame=FALSE)
abline(fit,lwd=2)
for (i in 1:n)
    lines(c(x[i],x[i]), c(y[i],yhat[i]),col="red",lwd=2)


plot(x, e,
     xlab="Mass(carats)",
     ylab="Residuals",
     bg="lightblue",
     col="black",cex=2,pch=21,frame=FALSE)
abline(h=0,lwd=2)
for (i in 1:n)
    lines(c(x[i],x[i]), c(e[i],0),col="red",lwd=2)


###########################################Non-linear
x = runif(100,-3,3); y = x + sin(x) + rnorm(100,sd=.2);
library(ggplot2)
g <- ggplot(data.frame(x=x, y=y), aes(x=x,y=y))
g <- g + geom_smooth(method="lm", colour="black")
g <- g + geom_point(size=7, colour="black", alpha=0.4)
g <- g + geom_point(size=5, colour="red", alpha=0.4)
g

g <- ggplot(data.frame(x=x,y=resid(lm(y~x))),
            aes(x=x,y=y))
g <- g + geom_hline(yintercept = 0, size=2)
g <- g + geom_point(size=7, colour="black", alpha=0.4)
g <- g + geom_point(size=5, colour="red", alpha=0.4)
g <- g + labs(x="X", y="Residual")
g

######################################################
x = runif(100,0,6); y = x + rnorm(100,mean=0,sd=.001*x);
g <- ggplot(data.frame(x=x, y=y), aes(x=x,y=y))
g <- g + geom_smooth(method="lm", colour="black")
g <- g + geom_point(size=7, colour="black", alpha=0.4)
g <- g + geom_point(size=5, colour="red", alpha=0.4)
g

g <- ggplot(data.frame(x=x,y=resid(lm(y~x))),
            aes(x=x,y=y))
g <- g + geom_hline(yintercept = 0, size=2)
g <- g + geom_point(size=7, colour="black", alpha=0.4)
g <- g + geom_point(size=5, colour="red", alpha=0.4)
g <- g + labs(x="X", y="Residual")
g
#######################################################
g <- ggplot(diamond, aes(x=carat, y=e))
g <- g + geom_hline(yintercept = 0, size=2)
g <- g + geom_point(size=7, colour="black", alpha=0.5)
g <- g + geom_point(size=5, colour="red", alpha=0.4)
g <- g + labs(x="X", y="Residual")
g
#######################################################
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y~x)
summary(fit)$sigma



#inference in regression
library(UsingR);data("diamond")
y <- diamond$price; x <- diamond$carat; n <- length(y)
beta1 <- cor(y,x)*sd(y)/sd(x)
beta0 <- mean(y)-beta1*mean(x)
e <- y - beta0 - beta1*x
sigma <- sqrt(sum(e^2)/(n-2))
ssx <- sum((x-mean(x))^2)
seBeta0 <- (1/n + mean(x)^2/ssx)^ .5 * sigma
seBeta1 <- sigma/sqrt(ssx)
tBeta0 <- beta0/seBeta0; tBeta1 <- beta1/seBeta1
pBeta0 <- 2*pt(abs(tBeta0), df=n-2, lower.tail = FALSE)
pBeta1 <- 2*pt(abs(tBeta1), df=n-2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std.Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("Intercept","x")

coefTable
fit <- lm(y~x);
summary(fit)$coefficients
sumCoef <- summary(fit)$coefficients
sumCoef[1,1]+c(-1,1)*qt(.975, df=fit$df)*sumCoef[1,2]
(sumCoef[2,1]+c(-1,1)*qt(.975, df=fit$df)*sumCoef[2,2])/10
#with 95% confidence,we estimate that a 0.1 carat increase in diamond size is going to result in a 356 to 389 increase in price in, in Singapore dollars. 






#Prediction
library(ggplot2)
newx = data.frame(x = seq(min(x), max(x), length=100))
p1 = data.frame(predict(fit, newdata = newx, interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx, interval = ("prediction")))
p1$interval="confidence"
p2$interval="prediction"
p1$x=newx$x
p2$x=newx$x
dat=rbind(p1,p2)
names(dat)[1]="y"

g <- ggplot(dat,aes(x=x,y=y))
g <- g + geom_ribbon(aes(ymin=lwr, ymax=upr, fill=interval), alpha=0.2)
g <- g + geom_line()
g <- g + geom_point(data=data.frame(x=x,y=y), aes(x=x,y=y),size=4)
g


#quiz
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
summary(lm(y~x))

x<-mtcars$wt
y<-mtcars$mpg
fit<-lm(y ~ x)
summary(fit)
predict(fit,data.frame(x=mean(x)), interval="confidence")

newdata <- data.frame(x=3000/1000)
p2 <- predict(fit, newdata, interval = ("prediction"))
print(p2)

rm(list=ls()) # clean environment
fit <- lm(mpg ~ I(wt / 2), data = mtcars)
coef <- summary(fit)$coefficients
mean <- coef[2,1] 
stderr <- coef[2,2]
df <- fit$df
#Two sides T-Tests
mean + c(-1,1) * qt(0.975, df = df) * stderr





rm(list=ls()) # clean environment
x <- mtcars$wt
y <- mtcars$mpg
fit <- lm(y ~ x)
c <- 5 # some constant
fit2 <- lm(y ~ I(x + c))

beta0 <- c(fit$coefficients[1], fit2$coefficients[1])
beta1 <- c(fit$coefficients[2], fit2$coefficients[2])
beta0; beta1

# We find that the slope is the same, but that the intercept changed.
# Next we find that:
all.equal(beta0[2], beta0[1] - c * beta1[1]) # TRUE


# Question 9
# Refer back to the mtcars data set with mpg as an outcome and weight (wt) as
# the predictor. About what is the ratio of the the sum of the squared errors,
# ¡Æni=1(Yi???Y^i)2 when comparing a model with just an intercept (denominator) to
# the model with the intercept and slope (numerator)?

fit5<-lm(y ~ 1)
fit6<-lm(y ~ x - 1)
plot(x,y)
abline(fit,col="red")
abline(fit5,col="blue")
abline(fit6,col="green")
anova(fit)
anova(fit5)
278/1126
# Question 10
# Do the residuals always have to sum to 0 in linear regression?

# If an intercept is included, then they will sum to 0.