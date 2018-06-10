#data-cleaning week3
#subsetting
set.seed(13435)
X <- data.frame("var1"=sample(1:5), "var2"=sample(6:10), "var3"=sample(11:15))
X <- X[sample(1:5),]; X$var2[c(1,3)] = NA
X[,1]
X[1:2, 2]
X[(X$var1 <= 3 & X$var3 > 11), ]
X[(X$var1 <= 3 | X$var3 > 15), ]
X[which(X$var2>8),]

sort(X$var1)
sort(X$var1, decreasing = TRUE)
sort(X$var2, na.last = TRUE)
X[order(X$var1),]
X[order(X$var1, X$var3),]

library(plyr)
arrange(X, var1)
arrange(X, desc(var1))
X$var4 <- rnorm(5)
Y <- cbind(X, rnorm(5))
Y

#Summarizing
if(!file.exists("./data")){dir.create(("./data"))}
fileUrl <- "http://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "restaurant.csv")
restData <- read.csv("restaurant.csv")
head(restData)
summary(restData)
str(restData)

quantile(restData$councilDistrict, na.rm = TRUE)
quantile(restData$councilDistrict, probs=c(0.5,0.75,0.9))

table(restData$zipCode, useNA = "ifany") #add new column for if any NA
table(restData$councilDistrict, restData$zipCode)

sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
all(restData$zipCode>0)

colSums(is.na(restData))
all(colSums(is.na(restData)) == 0)

table(restData$zipCode %in% c("21212","21213"))

restData[restData$zipCode %in% c("21212","21213"),]

data("UCBAdmissions")
DF =as.data.frame(UCBAdmissions)
summary(DF)
xt <- xtabs(Freq ~ Gender + Admit, data = DF)
xt

data("warpbreaks")
warpbreaks$replicate <- rep(1:9, len = 54)
xt = xtabs(breaks ~., data=warpbreaks)
xt
ftable(xt)


fakeData = rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData),units="Mb")



#Creating New variables
if(!file.exists("./data")){dir.create(("./data"))}
fileUrl <- "http://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "restaurant.csv")
restData <- read.csv("restaurant.csv")

s1 <- seq(1, 10, by=2); s1
s2 <- seq(1, 10, length=3); s2
x <-  c(1, 3, 8, 25, 100); seq(along=x)

restData$nearMe = restData$neighborhood %in% c("Roland Park","Homeland")
table(restData$nearMe)

restData$zipWrong = ifelse(restData$zipCode < 0, TRUE, FALSE)
table(restData$zipWrong, restData$zipCode<0)

restData$zipGroups = cut(restData$zipCode, breaks=quantile(restData$zipCode))
table(restData$zipGroups, restData$zipCode)

library(Hmisc);library(plyr)
restData$zipGroups = cut2(restData$zipCode, g=4)
restData = mutate(restData, zipGroups=cut2(restData$zipCode, g=4))
table(restData$zipGroups)

restData$zcf <- factor(restData$zipCode)
restData$zcf[1:10]

yesnp <- sample(c("yes","no"), size=10, replace = TRUE)
yesnofac = factor(yesno, levels=c("yes", "no"))
relevel(yesnofac, ref = "yes")

#transforms
#abs() sqrt() 
#ceiling() 四舍五入
#floor() 向下取整
#round(x, digits=n) 保留n位
#signif(x, digits=n) 一共n位
#cos sin log exp




#Reshaping 
library(reshape2)
head(mtcars)

#melt
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars,id=c("carname","gear","cyl"), measure.vars = c("mpg","hp"))
head(carMelt,n=3)
tail(carMelt,n=3)

#dcast
cylData <- dcast(carMelt, cyl~variable)
cylData

cylData <- dcast(carMelt, cyl~variable, mean)
cylData

head(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray, sum)
spIns = split(InsectSprays$count, InsectSprays$spray)
spIns
sprCount = lapply(spIns, sum)
sprCount 
unlist(sprCount)
sapply(spIns, sum)

spraySums <- ddply(InsectSprays, .(spary), summarise, sum=ave(count,FUN = sum))

#dplyr
#select, filter, arrange, rename, mutate, summarise

#Merging
if(!file.exists("./data2")){dir.create("./data2")}
fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1,destfile = "./data2/reviews.csv")
download.file(fileUrl2,destfile = "./data2/solutions.csv")
reviews = read.csv("./data2/reviews.csv");solutions <- read.csv("./data2/solutions.csv")
head(reviews);head(solutions)
names(reviews);names(solutions)
mergeData = merge(reviews, solution, by.x="solution_id", by.y="id",all=TRUE)

arrange(join(df1,df2),id)
dfList=list(df1,df2,df3)
join_all(dfList)

#quiz1
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
quiz1 = read.csv(fileUrl)
head(quiz1)
agricultureLogical <- quiz1$ACR == 3 & quiz1$AGS == 6
head(which(agricultureLogical), 3)

#quiz2
library(jpeg)
fileUrl <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg'
picture <- jpeg::readJPEG('jeff.jpg', native=TRUE)
quantile(picture, probs = c(0.3,0.8))

#quiz3
install.packages("data.table")
library("data.table")

FGDP <- data.table::fread('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
                          , skip=4
                          , nrows = 190
                          , select = c(1, 2, 4, 5)
                          , col.names=c("CountryCode", "Rank", "Economy", "Total")
                          )
FEDSTATS_Country <- data.table::fread('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv')
head(FGDP);head(FEDSTATS_Country,2)
mergedDT <- merge(FGDP, FEDSTATS_Country, by = 'CountryCode')
nrow(mergedDT)
arrange(mergedDT, desc(Rank))[13, 'Economy']
# mergedDT[order(-Rank)][13,.(Economy)]

#quiz4
tapply(mergedDT$Rank, mergedDT$'Income Group', mean)

#quiz5
mergedDT$RankGroups <- cut2(mergedDT$Rank, g=5)
table(mergedDT$RankGroups, mergedDT$'Income Group')
