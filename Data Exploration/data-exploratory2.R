#data-exploratory2
#lattice
library(lattice)
library(datasets)
xyplot(Ozone~Wind, data=airquality) #y~x
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone~Wind | Month, data=airquality, layout=c(5,1))

set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each=50)
y <- x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group1", "Group2"))
xyplot(y~x| f, layout = c(2,1))

xyplot(y~x | f, panel = function(x,y,...){
    panel.xyplot(x, y, ...)
    panel.lmline(x, y, col=2)
})



#ggplot2
library(ggplot2)
str(mpg)
qplot(displ, hwy, data=mpg, color=drv, geom = c('point','smooth'))
qplot(hwy, data=mpg, fill=drv)
#facets
qplot(displ, hwy, data = mpg, facets = .~drv)
qplot(hwy, data=mpg, facets = drv~., binwidth=2)

qplot(log(pm25), log(eno), data=maacs, facets = .~ mopos)+geom_smooth(method = "lm")

#g
g <- ggplot(maacs, aes(logpm25, NocturnalSympt))
g + geom_point() + facet_grid( .~ bmicat) +geom_smooth(method = "lm")
g + geom_point(aes(color = bmicat), size=4, alpha = 1/2)
g + labs(title="abc") + labs(x=expression("log"*PM[2.5]), y = "abc")
g + geom_smooth(size=4, linetype = 3, method"lm", se = FALSE)
g + theme_bw(base_family = "Times")

g + ylim(-3,3)
g + coord_cartesian(ylim=c(-3,3))

#cut
cutpoints <- quantile(maacs$logno2_new, seq(0, 1, length=4), na.rm = TRUE)
maacs$no2dec <- cut(maacs$logno2_new, cutpoints)

#setup
g <- ggplot(maacs, aes(logpm25, NocturnalSympt))
#add layers
g + geom_point(alpha=1/3)
  + facet_wrap(bmicat~no2dec, nrow=2, ncol=4) #make panels
  + geom_smooth(method = "lm", se=FALSE, col="steelblue") #smoother
  + theme_bw(base_family = "Avenir", base_size = 10) #theme
  + labs(x=expression("log" * PM[2.5]))
  + labs(y="Nocturnal Symptoms")
  + lbas(title="MAACS Cohort")



#quiz

