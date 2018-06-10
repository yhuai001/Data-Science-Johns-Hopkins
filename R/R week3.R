x <- list(a=1:5, b=rnorm(10), c=rnorm(20,1), d=rnorm(100,5))
lapply(x,mean)
sapply(x,mean)

x <- 1:4
lapply(x, runif, min=0, max=10)

x <- matrix(rnorm(200), 20, 10)
apply(x, 2, mean)
apply(x, 1, quantile, probs=c(0.25, 0.75))

a <- array(rnorm(2*2*10), c(2,2,10))
apply(a, c(1,2), mean)

mapply(rep, 1:4, 4:1)

noise <- function(n, mean, sd){
    rnorm(n, mean, sd)
}
noise(5, 1, 2)
noise(1:5, 1:5, 2)
mapply(noise, 1:5, 1:5, 2) 

x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
f
tapply(x, f, mean)
split(x, f)

library(datasets)
head(airquality)

s <- split(airquality, airquality$Month)
lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE))

x <- rnorm(10)
f1 <- gl(2,5)
f2 <- gl(5,2)
interaction(f1,f2)
str(split(x, list(f1,f2), drop = TRUE))


#Week 3
library(datasets)
data(iris)
?iris
round(mean(iris[which(iris$Species == "virginica"),]$Sepal.Length))


data("mtcars")
View(mtcars)
with(mtcars, tapply(mpg, cyl, mean))
tapply(mtcars$mpg, mtcars$cyl, mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)

a <- tapply(mtcars$hp, mtcars$cyl, mean)
round(abs(a[1]-a[3]))

