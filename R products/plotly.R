#plotly
library(plotly)
datasets::mtcars
plot_ly(mtcars, x = mtcars$wt, y=mtcars$mpg, mode = "markers")
plot_ly(mtcars, x = mtcars$wt, y=mtcars$mpg, mode = "markers", color = as.factor(mtcars$cyl))
plot_ly(mtcars, x = mtcars$wt, y=mtcars$mpg, mode = "markers", color = mtcars$disp)
plot_ly(mtcars, x = mtcars$wt, y=mtcars$mpg, mode = "markers", color = as.factor(mtcars$cyl),size=mtcars$hp)


set.seed(1993)
temp <- rnorm(100, mean=30, sd=5)
pressue <- rnorm(100)
dtime <- 1:100
plot_ly(x = temp, y = pressue, z = dtime,
        type = "scatter3d", mode="markers", color=temp)

data("airmiles")
airmiles
plot_ly(x=time(airmiles), y=airmiles)

library(plotly);library(tidyr);library(dplyr)
data("EuStockMarkets")
stocks <- as.data.frame(EuStockMarkets) %>%
    gather(index,price) %>%
    mutate(time=rep(time(EuStockMarkets),4))

plot_ly(stocks, x = stocks$time, y = stocks$price, color=stocks$index)

plot_ly(iris, x = iris$Petal.Length, type="histogram" )
plot_ly(iris, y = iris$Petal.Length, color = iris$Species, type="box" )

terrain1 <- matrix(rnorm(100*100), nrow = 100, ncol=100)
plot_ly(z=terrain1, type="heatmap")

terrain2 <- matrix(sort(rnorm(100*100)), nrow = 100, ncol=100)
plot_ly(z=terrain2, type="surface")

#gg
set.seed(100)
d <- diamonds[sample(nrow(diamonds),1000), ]
p <- ggplot(data = d, aes(x=carat, y=price)) +
    geom_point(aes(text=paste("Clarity:", clarity)), size=4)+
    geom_smooth(aes(colour=cut, fill=cut)) + facet_wrap(~cut)

(gg <- ggplotly(p))

