mu0=30;mua=32;sigma=4;n=16

z <- qnorm(1 - alpha)
pnorm(mu0 + z*sigma/sqrt(n), mean = mu0, sd=sigma/sqrt(n),lower.tail = F)
pnorm(mu0 + z*sigma/sqrt(n), mean = mua, sd=sigma/sqrt(n),lower.tail = F)

power.t.test(n=16,delta = 2/4,sd=1,type = "one.sample",alt="one.sided")$power
power.t.test(power=0.8,delta = 100,sd=200,type = "one.sample",alt="one.sided")

set.seed(123)
pValues <- rep(NA, 1000)
for (i in 1:1000){
    y <- rnorm(20)
    x <- rnorm(20)
    pValues[i] <- summary(lm(y~x))$coeff[2,4]
}

sum(pValues < 0.05)

sum(p.adjust(pValues,method = "bonferroni")<0.05)


pValues <- rep(NA, 1000)
for (i in 1:1000){
    x <- rnorm(20)
    if(i <= 500){y <- rnorm(20)}else{y <- rnorm(20,mean=2*x)}
    pValues[i] <- summary(lm(y~x))$coeff[2,4]
}
trueStatus <- rep(c("zero","not zero"),each=500)
table(pValues<0.05,trueStatus)

table(p.adjust(pValues,method = "bonferroni")<0.05,trueStatus)
table(p.adjust(pValues,method = "BH")<0.05,trueStatus)



#bootstrap
library(UsingR)
data("father.son")
x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x,n*B,replace = TRUE), B, n)
resamplesMedians <- apply(resamples, 1, median)

sd(resamplesMedians)
quantile(resamplesMedians,c(0.025,0.975))



#Permutation test B vs C
subdata

















#quiz
#1
subject <- c(1,2,3,4,5)
baseline <- c(140,138,150,148,135)
week2 <- c(132,135,151,146,130)
examinations <- data.frame(subject, baseline, week2)
examinations

test <- t.test(x = examinations$baseline, y = examinations$week2, alt = "two.sided", paired = TRUE)
test$p.value

#2
n <- 9
米 <- 1100
考 <- 30
quantile = 0.975 # is 95% with 2.5% on both sides of the range
confidenceInterval = 米 + c(-1, 1) * qt(quantile, df=n-1) * 考 / sqrt(n)
confidenceInterval


#3
n <- 4
x <- 3
test <- binom.test(x=x, n=n, alt="greater")

#4
rate <- 0.01
errors <- 10
days <- 1787
test <-  poisson.test(errors, T = days, r = rate, alt="less")
test

#5
n_y <- 9 # subjects treated
n_x <- 9 # subjects placebo
考_y <- 1.5# kg/m2 std.dev. treated 
考_x <- 1.8# kg/m2 std.dev. placebo 
米_y <- -3#  kg/m2 average difference treated
米_x <- 1#  kg/m2 average difference placebo

# calculate pooled standard deviation
考_p <- (((n_x - 1) * 考_x^2 + (n_y - 1) * 考_y^2)/(n_x + n_y - 2))
pval <- pt((米_y - 米_x) / (考_p * (1 / n_x + 1 / n_y)^.5), df=n_y + n_x -2)
pval

#7
n <- 100 #subject
米 <- 0.01# m^3 brain volume loss mean
考 <- 0.04# m^3 brain volume loss std. dev.
p <- 0.05 # sign level

pow <- power.t.test(n=n, delta=米, sd=考 , sig.level=p, type="one.sample", alt="one.sided")$power
round(pow, 2)


#
米 <- 0.01# m^3 brain volume loss mean
考 <- 0.04# m^3 brain volume loss std. dev.
p <- 0.05 # sign level
pow <- 0.9 #power

n <- power.t.test(power=pow, delta=米, sd=考 , sig.level=p, type="one.sample", alt="one.sided")$n
n