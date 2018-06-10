#Part 1: Simulation Exercise Instructions
set.seed(19930319)
lambda <- 0.2; n <- 40;
means = NULL
for (i in 1:1000) means = c(means, mean(rexp(n,lambda)))
hist(means,breaks=50,xlim=c(2,8))


mean(means)
abline(v=mean(means),col="red",lwd="2")


a <- (1/lambda)^2/n
theoretical <- a
b<- var(means)
sample <- b
data.frame(theoretical,sample)

a1 <- lambda^-1
theoretical_mean <- a1
b1 <- mean(means)
sample_mean <- b1
table(theoretical_mean,sample_mean)


dataset <- matrix(rexp(n*1000,lambda),1000)
RowMeans <- apply(dataset, 1, mean)
df <- data.frame(RowMeans)

g <- ggplot(df,aes(x=RowMeans))
g <- g + geom_histogram(bins=50,binwidth = lambda,color="blue",aes(y=..density..))
g <- g + geom_vline(xintercept = sample_mean,size=1.0,color="black")
g <- g + stat_function(fun = dnorm,args=list(mean=sample_mean,sd=b),color="red",size=1)
g <- g + stat_function(fun = dnorm,args=list(mean=theoretical_mean,sd=a),color="pink",size=1)
g

#Part 2: Basic Inferential Data Analysis Instructions
library(datasets)
library(ggplot2)

data("ToothGrowth")
data <- ToothGrowth
str(data)
head(data)
summary(data)

g <- ggplot(data=ToothGrowth,aes(x=supp, y=len))
g <- g + facet_grid(.~ data$dose)
g <- g + geom_boxplot(aes(fill=supp))
g

g <- ggplot(data=ToothGrowth,aes(x=dose, y=len, color=supp))
g <- g + geom_smooth(method = "lm",se=FALSE)
g
#Hypothesis
h1 <- t.test(len ~ supp, data=subset(data,dose==0.5))
h1
p1 <- h1$p.value
h2 <- t.test(len ~ supp, data=subset(data,dose==1.0))
h2
p2 <- h2$p.value
h3 <- t.test(len ~ supp, data=subset(data,dose==2.0))
h3
p3 <- h3$p.value
data.frame(p1,p2,p3)