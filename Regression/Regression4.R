#Regression week 4
install.packages("manipulate")
library(manipulate)
x <- seq(-10,10,length=1000) 
manipulate(
    plot(x,exp(beta0+beta1*x) / (1+exp(beta0+beta1*x)),
         type="l",lwd=3,frame=FALSE),
    beta1=slider(-2,2,step = .1,initial=2),
    beta0=slider(-2,2,step = .1,initial=0)
    )

logRegRavens <- glm(data$win ~ data$score, family = "binomial")
exp(logRegRavens$coeff)
exp(confint(logRegRavens))
anova(logRegRavens,test="Chisq") 


#Posisson
par(mfrow=c(1,3))
plot(0:10,dpois(0:10,lambda = 2),type = "h",frame=F)
plot(0:20,dpois(0:20,lambda = 10),type = "h",frame=F)
plot(0:200,dpois(0:200,lambda = 100),type = "h",frame=F)

plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
glm1 <- glm(gaData$visits ~ gaData$julian, family="poisson")
abline(lm1,col="red",lwd=3);
lines(gaData$julian,glm1$fitted,col="blue",lwd=3)

plot(glm1$fiited,glm1$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")
confint(glm1)
confint.agnostic(glm1)

plot(julian(gaData$date),glm1$fitted,pch=19,col="blue",ylab="Fitted Counts",xlab = "Date")









#quiz
#1
library(MASS)
data(shuttle)
str(shuttle)

shuttle$use.binary <- as.integer(shuttle$use == "auto")
fit <- glm(use.binary ~ wind - 1, data = shuttle, family = binomial)
summary(fit)$coef

unname(exp(coef(fit))[1]/exp(coef(fit))[2]) 
#2
fit2 <- glm(usebin ~ factor(wind) + factor(magn) - 1, family = "binomial", 
            data = shuttle)
(Coef2 <- coef(summary(fit2)))
coef2.odds <- exp(c(Coef2[1, 1], Coef2[2, 1]))
(odds2.ratio <- coef2.odds[1] / coef2.odds[2])

#3
fit1 <- glm(I(1 - usebin) ~ factor(wind) - 1, family = "binomial", 
            data = shuttle)
summary(fit1)$coef

#4
data("InsectSprays")
head(InsectSprays)
logRegInsect<-glm(count~factor(spray)-1, family="poisson", InsectSprays)
summary(logRegInsect)$coef

exp(coef(logRegInsect))[1]/exp(coef(logRegInsect))[2] 

fit <- glm(count ~ relevel(spray, "B"), data = InsectSprays, family = poisson)
exp(coef(fit))[2]

#5
logRegInsect51<-glm(count~factor(spray)+offset(log(count+1)), family="poisson", InsectSprays)
summary(logRegInsect51)$coef

logRegInsect52<-glm(count~factor(spray)+offset(log(count+1)+log(10)), family="poisson", InsectSprays)
summary(logRegInsect52)$coef

#The coefficient (b1) estimate is unchanged


#6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
plot(x, y, frame=FALSE, pch=21, bg="lightblue", cex=2)

l1<-lm(y[1:6]~ x[1:6])$coef
l2<-lm(y[6:11]~ x[6:11])$coef

abline(l1, col="red", lwd=2)
abline(l2, col="red", lwd=2)

knots<-c(0)
splineTerms<-sapply(knots,function(knot) (x>knot)*(x-knot))
xMat<-cbind(1,x,x^2,splineTerms)
xMat2<-cbind(1,x,splineTerms)

yhat<-predict(lm(y~xMat-1))
yhat2<-predict(lm(y~xMat2-1))

lines(x,yhat, col="blue", lwd=2)
lines(x,yhat2, col="green", lwd=2)

legend(x=-1,y=5, c("With lm","With X","With X+X^2"),lty=c(1,1), lwd=c(2.5,2.5),col=c("red", "green","blue"))
