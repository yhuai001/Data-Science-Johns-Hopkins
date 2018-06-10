median(x=1:10)
x = list(1, 'a', TRUE, 1+4i)
m = matrix(1:6, nrow=2, ncol=3)


m = 1:10
dim(m) = c(2,5)
dimnames(m) = list(c('a','b'),c('c','d','e','f','g'))


cbind(x, y)
rbind(x, y)

x = factor(c(1,2,3,4,5,6))
table(x)

x = factor(c(1,2,3,4,5,6),levels = c(2,1))


df = data.frame(abc =1:10, bar = 11:20)
nrow(df)
ncol(df)
dput(df,file="df.R")
a = dget("df.R")

x = "Ziying"
y = "Zehao"
y = data.frame(a=1, b="yes")
dump(c("x","y"), file="zz.R")
rm(x,y) 
source("zz.R")
con = url("http://www.jhsph.edu", "r")
x = readLines(con)

x = c("a","b","c","c","d","a")
x[1]
x[1:4]
x[x>"a"]
u = x>"a"
x[u]

x = list(foo=1:4, bar=0.6, baz="hello")
x[c(1,3)]
x[[c(1,3)]]



x = matrix(1:6,2,3)

x[1,2]

x[2,1]

x[1,]

x[,3]


x[1, , drop=FALSE]





X = list(aaaawdosakd = 1:5)
x$a
x[["a", exact = FALSE]]


x = c(1,2,NA,4,NA,5)
bad = is.na(x)
x[!bad]

a = complete.cases(b)
b[a, ][1:6, ]

x=matrix(1:4,2,2); y = matrix(rep(10,4),2,2)
x*y
x/y
x %*% y


bad = subset(hw1,is.na(Ozone))
nrow(bad)
sub = subset(hw1, !is.na(Ozone), select = Ozone)
apply(sub, 2, mean)
sub = subset(hw1, Ozone>31 & Temp>90, select = Solar.R)
apply(sub, 2, mean)
sub20 = subset(hw1, hw1$Month==5 & !is.na(Ozone), select = Ozone)
apply(sub20, 2, max)

