for(i in seq_len(nrow(x))){
    for(j in seq_len(ncol(x))){
        print(x[i,j])
    }
}

while(count < 10){
    print(count)
    count = count + 1
}

while (z>=3 && z<= 10){
    print(z)
    coin = rbinom(1,1,0.5)
    
    if(coin == 1){
        z = z + 1
    }else{
        z = z - 1
    }
}

add2 = function(x,y){
    x + y
}

above10 = function(x){
    use = x > 10
    x[use]
}

above = function(x,n=10){
    use = x > n
    x[use]
}

columnmean = function(y, removeNA = TRUE){
    nc = ncol(y)
    means = numeric(nc)
    for(i in 1:nc){
        means[i] = mean(y[,i], na.rm = removeNA)
    }
    means
}

args(lm)

myplot = function(x,y,type="1",...){
    plot(x,y,type = type,...)
}

args(plot)
args(paste)
args(cat)

paste("a","b",sep = ":")

paste("a","b",se = ":")

make.power = function(n){
    pow = function(x){
        x^n  
    }
    pow
}

cube = make.power(3)
square = make.power(2)

#Maximizing a normal likelihood
make.NegLogLik = function(data, fixed=c(FALSE,FALSE)){
    params = fixed
    function(p){
        params[!fixed] = p
        mu = params[1]
        sigma = params[2]
        a = -0.5*length(data)*log(2*pi*sigma^2)
        b = -0.5*sum((data-mu)^2) / (sigma^2)
        -(a + b)
    }
}

set.seed(1); normals = rnorm(100, 1, 2)
nLL = make.NegLogLik(normals)
nLL

optim(c(mu = 0, sigma =1), nLL)$par

nLL = make.NegLogLik(normals, c(1, FALSE))
x = seq(1.7, 1.9, len = 100)
y = sapply(x, nLL)
plot(x, exp(-(y - min(y))), type = "l")






#Week 2 Assignment
#Specdata function
pollutantmean <- function(directory, pollutant, id = 1:322){
    filelist <- list.files(path = "C:/Users/HYZH/Desktop/specdata", pattern = ".csv", full.names = TRUE)
    values <- data.frame()
    
    for (i in id){
        x <- read.csv(filelist[i])
        values <- rbind(values, x)
    }
    mean(values[, pollutant], na.rm = TRUE)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)


#part 2
complete <- function(directory, id = 1:332){
    filelist <- list.files(path = "C:/Users/HYZH/Desktop/specdata", pattern = ".csv", full.names = TRUE)
    nobs <- numeric()
    
    for (i in id){
        x <- read.csv(filelist[i])
        nobs <- c(nobs,sum(complete.cases(x)))
    }
    data.frame(id,nobs)
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

#part 3
corr <- function(directory, threshold = 0){
    
    filelist <- list.files(path = "C:/Users/HYZH/Desktop/specdata", pattern = ".csv", full.names = TRUE)
    result <- vector(mode = "numeric", length = 0)
    
    for (i in seq(filelist)){
        airquality <- read.csv(filelist[i])
        good <- complete.cases(airquality)
        airquality <- airquality[good, ]
        
        if(nrow(airquality) > threshold){
            correlation <- cor(airquality[["sulfate"]], airquality[["nitrate"]])
            result <- append(result, correlation) 
        }
        
    }
    result
}

cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)
length(cr)



set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

install.packages("swirl")
library(swirl)

