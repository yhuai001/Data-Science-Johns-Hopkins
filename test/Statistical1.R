#variability
nosim <- 1000
n <- 10
sd(apply(matrix(runif(nosim * n), nosim), 1, mean))
sd(apply(matrix(rpois(nosim * n, 4), nosim), 1, mean))
sd(apply(matrix(sample(0:1, nosim*n, replace = TRUE),nosim),1, mean))

library(UsingR); data(father.son);
x <- father.son$sheight
n <- length(x)
round(c(var(x),var(x)/n, sd(x), sd(x)/sqrt(n)), 2)

#Distributions

qnorm(.95, mean = 0, sd=1)

pnorm(1160, mean=1020, sd=50, lower.tail = FALSE)
pnorm(2.8, lower.tail = FALSE)

qnorm(0.75, mean = 1020, sd=50)

#Poisson
ppois(10, lambda = 5*3)


#Asymptotics
n <- 1000
means <- cumsum(sample(0:1, n, replace = TRUE))/(1:n)

#Central Limit Theorem


#quiz
pnorm(70, mean=80, sd=10)
0.1586553
qnorm(.95, mean=1100, sd=75)
1223.364
qnorm(0.95,mean=1100, sd=75/sqrt (100))
1112.336
round (pbinom(3 , prob = 0.5, size =5, lower.tail= FALSE ) * 100,1) 
