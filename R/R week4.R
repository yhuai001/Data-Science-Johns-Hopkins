str(str)
str(lm)
str(ls)
x <- rnorm(100,2,4)
summary(x)
str(x)
f <- gl(40,10)
str(f)
summary(f)
library(datasets)
head(airquality)
str(airquality)
s <- split(airquality, airquality$Month)
str(s)

#rnorm, dnorm(density), pnorm(cumulative distribution), rpois
#q for quantile

dnorm(x, mean=0, sd=1, log=FALSE)
pnorm(q, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
qnorm(p, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
rnorm(n, mean=0, sd=1)
¦Õ

set.seed(1)
rnorm(5)

rpois(10,20)
ppois(6,2) ## Pr(x <= 6)

#Linear Model
#where e~N(0,2) x~N(0,1), ¦Â0 = 0.5, ¦Â1 = 2
set.seed(20)
x <- rnorm(100)
e <- rnorm(100,0,2)
y <- 0.5+2*x+e
summary(y)
plot(x,y)

set.seed(10)
x <- rbinom(100,1,0.5)
e <- rnorm(100,0,2)
y <- 0.5+2*x+e
summary(y)
plot(x,y)


set.seed(1)
x <- rnorm(100)
log.mu <- 0.5+0.3*x
y <- rpois(100, exp(log.mu))
summary(y)
plot(x,y)

#sample
set.seed(1)
sample(1:10, 4)
sample(1:10, 4)
sample(letters, 5)
sample(1:10)
sample(1:10, replace = TRUE)

#Profiler
system.time(readLines("http://www.wwe.com"))

hilbert <- function(n){
    i <- 1:n
    1 / outer(i - 1, i, "+")
}
x <- hilbert(1000)
system.time(svd(x))

system.time({
    n <- 1000
    r <- numeric(n)
    for (i in 1:n){
        x <- rnorm(n)
        r[i] <- mean(x)
    }
})


Rprof()
summaryRprof()

library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)