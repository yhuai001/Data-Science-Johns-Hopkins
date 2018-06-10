#Regression week1
library(MASS)
library(HistData)
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
library(UsingR); data(galton); library(reshape); long <- melt(galton)
g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour="black", binwidth = 1)
g <- g + facet_grid(.~ variable)
g

library(manipulate)
myHist <- function(mu){
    mse <- mean((galton$child-mu)^2)
    g <- ggplot(galton, aes(x=child)) + geom_histogram(fill="salmon", colour="black", binwidth = 1)
    g <- g + geom_vline(xintercept = mu, size=3)
    g <- g + ggtitle(paste("mu", mu, ", MSE=", round(mse,2), sep=""))
    g
}
manipulate(myHist(mu), mu = slider(62, 74, step=0.5))


g <- ggplot(galton, aes(x=child)) + geom_histogram(fill="salmon", colour="black", binwidth = 1)
g <- g + geom_vline(xintercept = mean(galton$child), size=3)
g



# load necessary packages/install if needed
library(ggplot2); library(UsingR); data(galton)
# function to plot the histograms
myHist <- function(mu){
	# calculate the mean squares
    mse <- mean((galton$child - mu)^2)
    # plot histogram
    g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon",
    	colour = "black", binwidth=1)
    # add vertical line marking the center value mu
    g <- g + geom_vline(xintercept = mu, size = 2)
    g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
    g
}
# manipulate allows the user to change the variable mu to see how the mean squares changes
#   library(manipulate); manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))]
# plot the correct graph
myHist(mean(galton$child))

g <- ggplot(galton, aes(x=child, y=parent)) + geom_point()
g





myPlot <- function(beta){
  y <- galton$child - mean(galton$child)
  x <- galton$parent - mean(galton$parent)
  freqData <- as.data.frame(table(x, y))
  names(freqData) <- c("child", "parent", "freq")
  plot(
    as.numeric(as.vector(freqData$parent)), 
    as.numeric(as.vector(freqData$child)),
    pch = 21, col = "black", bg = "lightblue",
    cex = .15 * freqData$freq, 
    xlab = "parent", 
    ylab = "child"
    )
  abline(0, beta, lwd = 3)
  points(0, 0, cex = 2, pch = 19)
  mse <- mean( (y - beta * x)^2 )
  title(paste("beta = ", beta, "mse = ", round(mse, 3)))
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))
lm(I(child-mean(child)) ~ I(parent - mean(parent))-1, data = galton)



#OLS
#correlation


library(dplyr)
# constructs table for different combination of parent-child height
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child (in)", "parent (in)", "freq")
# convert to numeric values
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
# filter to only meaningful combinations
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g + scale_size(range = c(2, 20), guide = "none" )
# plot grey circles slightly larger than data as base (achieve an outline effect)
g <- g + geom_point(colour="grey50", aes(size = freq+10, show_guide = FALSE))
# plot the accurate data points
g <- g + geom_point(aes(colour=freq, size = freq))
# change the color gradient from default to lightblue -> $white
g <- g + scale_colour_gradient(low = "lightblue", high="white")
g <- g + geom_smooth(method="lm", formula = y~x)
g

y <- galton$child
x <- galton$parent
beta1 <- cor(y,x) * sd(y)/sd(x)
beta0 <- mean(y)-beta1*mean(x)
rbind(c(beta0, beta1), coef(lm(y~x)))
#reverse
beta1 <- cor(x,y) * sd(x)/sd(y)
beta0 <- mean(x)-beta1*mean(y)
rbind(c(beta0, beta1), coef(lm(x~y)))
#mean centered
yc <- y-mean(y)
xc <- x-mean(x)
beta1 <- sum(yc*xc)/ sum(xc^2)
c(beta1, coef(lm(y~x))[2])
lm(yc~xc -1)


#regression to the mean
a <- rnorm(100)
b <- rnorm(100)
odr <- order(a)
a[odr[100]]
b[odr[100]]

library(UsingR)
data(father.son)
head(father.son)
names(father.son)
library(ggplot2)
g <- ggplot(father.son, aes(x=fheight, y=sheight)) + geom_point(size=2, alpha=0.7) + xlab("Height of father") + ylab("Height of son") + ggtitle("Father-son Height Data")
# mean and standard deviations of the father and son heights
shmean <- mean(father.son$sheight)
fhmean <- mean(father.son$fheight)

shsd <- sd(father.son$sheight)
fhsd <- sd(father.son$fheight)

# correlation of father.son data - the off-diagonal terms are of interest
fscor <- cor(father.son)[1, 2] 

# slope = r*s_y/s_x - father data is on the x axis.
bhat <- fscor * shsd / fhsd

# y-intercept = mean(y) - slope*mean(x)
ahat <- shmean - bhat*fhmean

# print regression line parameters
ahat;bhat

# computation of mean squared data average of residula error squared
MSE <- sum((father.son$sheight - (ahat + bhat * father.son$fheight))^2) / dim(father.son)[1]
MSE
RMSE <- sqrt(MSE)
RMSE

# minimum and maximum father height
fhmin <- min(father.son$fheight)
fhmax <- max(father.son$fheight)

# equally space points between from the min-max height interval
xdat <- (fhmax - fhmin) * seq(0, 1, 0.01) + fhmin
ydat <- ahat + bhat*xdat

# regression line data frame
regressionLine <- data.frame(xdat, ydat)
names(regressionLine) <- c("sheight", "fheight")

# plot of data set with regression line
g <- ggplot(father.son, aes(x=fheight, y=sheight)) + 
        geom_point(size=2, alpha=0.7) + 
        geom_line(data=regressionLine, aes(x=sheight, y=fheight), lwd=1.5, colour="red") + 
        xlab("Height of father") + 
        ylab("Height of son") + 
        ggtitle("Father-son Height Data")
g

summary(lm(sheight ~ fheight, data=father.son))







#quiz
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
weighted.mean(x, w)

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ 0 + x)$coeff

#from the datasets package and fit the regression model with mpg as the outcome and weight as the predictor. Give the slope coefficient.
data(mtcars)
lm(mpg~wt, data=mtcars)

x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
zx <- (x-mean(x)) / sd(x)
zx[1]

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y~x)$coeff
b1 <- cor(x,y)*sd(y)/sd(x)
b0 <- mean(y) - b1 * mean(x)
b0


x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)





#Let the slope having fit Y as the outcome and X as the predictor be denoted as ¦Â1. Let the slope from fitting X as the outcome and Y as the predictor be denoted as ¦Ã1. Suppose that you divide ¦Â1 by ¦Ã1; in other words consider ¦Â1/¦Ã1. What is this ratio always equal to?
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

beta <- cor(x,y)*sd(y)/sd(x)
alpha <- cor(x,y)*sd(x)/sd(y)
beta/alpha
var(y)/var(x)



sx <- 1/2
sy <- 1
cor <- .5
cor * sy / sx