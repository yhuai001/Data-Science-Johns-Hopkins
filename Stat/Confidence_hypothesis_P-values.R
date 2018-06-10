#Confidence intervals
data(sleep)
head(sleep)
g1 <- sleep$extra[1:10]
g2 <- sleep$extra[11:20]
difference <- g2-g1
mn <- mean(difference)
s <- sd(difference)
n <- 10
t.test(difference)
t.test(extra~I(relevel(group,2)), paired=TRUE, data = sleep)

library(UsingR);data(father.son)
t.test(father.son$sheight - father.son$fheight)

library(datasets);data("ChickWeight");library(reshape2)
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1:2)] <- paste("time", names(wideCW)[-(1:2)], sep="")
library(dplyr)
wideCW <- mutate(wideCW,
                 gain=time21 - time0)
wideCW14 <- subset(wideCW, Diet %in% c(1,4))
t.test(gain ~ Diet, paired=FALSE, var.equal=TRUE, data=wideCW14)

pbinom(6,size=8,prob=0.5,lower.tail = F)
ppois(9,5,lower.tail = F)

#quiz
#1
n <- 9; u <- 1100; sigma <- 30; quantile=0.975
confidenceInterval = u + c(-1,1)*qt(quantile,df=n-1)* sigma/sqrt(n)

#2
n <- 9; averageDifference <- -2; quantile=0.975; up=0
sigma = (up-averageDifference * sqrt(n))/qt(quantile, df=n-1)
sigma

#4
quantile = 0.975 # is 95% with 2.5% on both sides of the range

n_y <- 10 # nights new system
n_x <- 10 # nights old system
var_y <- 0.60 # variance new (sqrt of 考)
var_x <- 0.68 # variance old (sqrt of 考)
米_y <- 3# average hours new system
米_x <- 5# average hours old system

# calculate pooled standard deviation
考_p <- sqrt(((n_x - 1) * var_x + (n_y - 1) * var_y)/(n_x + n_y - 2))

confidenceInterval <- 米_y - 米_x + c(-1, 1) * qt(quantile, df=n_y+n_x-2) * 考_p * (1 / n_x + 1 / n_y)^.5
round(confidenceInterval,2)

#6
quantile = 0.975 # is 95% with 2.5% on both sides of the range

n_y <- 100 # nights new system
n_x <- 100 # nights old system
考_y <- 0.50# 考 new 
考_x <- 2# 考 old 
米_y <- 4# average hours new system
米_x <- 6# average hours old system

# calculate pooled standard deviation
考_p <- sqrt(((n_x - 1) * 考_x^2 + (n_y - 1) * 考_y^2)/(n_x + n_y - 2))

confidenceInterval <-  米_x - 米_y + c(-1, 1) * qnorm(quantile) * 考_p * (1 / n_x + 1 / n_y)^.5
round(confidenceInterval,2)


#7
quantile = 0.95 # is 90% with 5% on both sides of the range

n_y <- 9 # subjects treated
n_x <- 9 # subjects placebo
考_y <- 1.5# kg/m2 std.dev. treated 
考_x <- 1.8# kg/m2 std.dev. placebo 
米_y <- -3#  kg/m2 average difference treated
米_x <- 1#  kg/m2 average difference placebo

# calculate pooled standard deviation
考_p <- sqrt(((n_x - 1) * 考_x^2 + (n_y - 1) * 考_y^2)/(n_x + n_y - 2))

confidenceInterval <-  米_y - 米_x + c(-1, 1) * qt(quantile, df=n_y+n_x-2) * 考_p * (1 / n_x + 1 / n_y)^.5
round(confidenceInterval,3)