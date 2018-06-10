#data cleaning week4 
Sys.setlocale("LC_TIME", "English")
#Editing Text Variables
if(!file.exists("./data3")){dir.create(("./data3"))}
fileUrl <- "http://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data3/cameras.csv")
cameraData <- read.csv("./data3/cameras.csv")
names(cameraData)

tolower(names(cameraData)) #变小写
splitNames <- strsplit(names(cameraData), "\\.")
splitNames[[5]]
splitNames[[6]]

mylist <- list(letters = c("A","b","c"), numbers=1:3, matrix(1:25,ncol=5))
mylist
mylist[2]           

splitNames[[6]][1]
firstElement <- function(x){x[1]}
sapply(splitNames, firstElement)

fileUrl1 <-"https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile = "./data3/reviews.csv")
download.file(fileUrl2, destfile = "./data3/solutions.csv")
reviews <- read.csv("./data3/reviews.csv"); solutions <- read.csv("./data/solutions.csv")
head(reviews);head(solutions,2)

names(reviews)
sub("_", "", names(reviews)) #remove first _
gsub("_","",testName) #remove all _

#search
grep("Alameda", cameraData$intersection)
length(grep("Alameda", cameraData$intersection)) # 0 means NA
grep("Alameda", cameraData$intersection,value = TRUE)
table(grepl("Alameda", cameraData$intersection))
cameraData2 <- cameraData[!grepl("Alameda", cameraData$intersection),]

library(stringr)
nchar("Jeffrey Leek")
substr("Jeffrey Leek",1,7)
paste("a","b")
paste0()
str_trim()



##Regular Expressions
# ^i think
# morning$
# [Bb][Uu][Ss][Hh]
# ^[0-9][a-zA-Z]
# [^?.]$  结尾不要问号和逗号
# 9.11  .代表任意富豪
# flood|fire
# ^[Gg]ood | [Bb]ad
# ^([Gg]ood | [Bb]ad)
# [Gg]eorge ([Ww]\.)?  [Bb]ush  可以接受中间有Ww或.
# (.*) 匹配任意字符任意次数  
# [0-9]+(.*)[0-9]+     + 至少一项
# [Bd]ush (+[^ ]+ +){1,5} debate   curly barckets{}区间限定
# {m,n} 做m次不多于n次
# +([a-zA-Z]+) +\1 +      \1表示重复项
# ^s.(.*)s   s开头s结尾
# ^s(.*?)s$


#Date
d1 = date()
d1
d2 = Sys.Date()
d2
format(d2, "%a %b %d")

x <- c("1jan1960", "2jan1960", "31mar1960","30jul1960");z <- as.Date(x,"%d%b%Y")
z
z[1] - z[2]
as.numeric(z[1]-z[2])

weekdays(d2)
months(d2)
julian(d2)

library(lubridate); ymd("20140108")
mdy("08/04/2013")
dmy("03-04-2013")

ymd_hms("2011-08-03 10:15:03")
ymd_hms("2011-08-03 10:15:03", tz="Pacific/Auckland")
?Sys.timezone

x <- dmy( c("1jan1960", "2jan1960", "31mar1960","30jul1960"))
wday(x[1])
wday(x[1], label=TRUE)



#Data resources
#Gapminder
#data.un.org
#data.gov
#asdfree.com
#infochimps.com
#Hilary Mason ; Peter Skomoroch; Jeff Hammerbacher ; Gregory Piatetsky-Shapiro
#Standford Large Network Data; UCI Machine Learning; KDD Nugets Datasets; CMU Statlib; Gene expression omnibus; ArXiv Data; AWS data
#twitteR ; rfigshare; PLoS rplos; rOpenSci; Rfacebook ; Rgooglemaps

#quiz
file <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
data <- read.csv(file)
result <- strsplit(names(data),"wgtp")
result[[123]]

library(data.table)
file2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
data2 <- read.csv(file2)
GDP <- gsub(",","",data2[,5])
mean(as.numeric(GDP[1:215]), na.rm=TRUE)
grep("^United",countryNames)


file3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
file4 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
data3 <- read.csv(file3);data4 <- read.csv(file4)
setnames(data2, c("X", "X.1", "X.3", "X.4"), c("CountryCode", "rankingGDP", "Long.Name", "gdp"))
all <- merge(data2, data3, "CountryCode")
table(grepl("june", tolower(all$Special.Notes)),grepl("fiscal year end", tolower(all$Special.Notes)))[4]

install.packages("quantmod")
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
library(lubridate)
length(grep("^2012",sampleTimes))
sampleTimes[grep("^2012",sampleTimes)]
sum(weekdays(as.Date(sampleTimes[grep("^2012",sampleTimes)]))=="Monday")
