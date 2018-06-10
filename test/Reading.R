
#download
fileUrl <- "https://data.cityofnewyork.us/api/views/kku6-nxdu/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "C:/Users/HYZH/Desktop/Coursera/test.csv")
list.files("C:/Users/HYZH/Desktop/Coursera")
date()

Demo <- read.table("C:/Users/HYZH/Desktop/Coursera/test.csv", sep = ",", header = TRUE, nrows = 5, quote = "")
head(Demo)

library(readxl)

Sheet1 <- read_excel("C:/Users/HYZH/Desktop/Sheet1.xlsx", range = "A1:D5")
View(Sheet1)

library(XML)
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl, useInternal=TRUE)
scores <- xpathSApply(doc,"//li[@class='scores']", xmlValue)
scores
teams <- xpathSApply(doc,"//li[@class='team-name']",xmlValue)
teams

jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)
names(jsonData$owner)

myjson <- toJSON(iris, pretty = TRUE)
cat(myjson) 
iris2 <- fromJSON(myjson)
head(iris2)

library(data.table)
DF = data.frame(x=rnorm(9), y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DF)
DF[c(2,3)]
DF[2,]
DF[DF$y=="a",]
DF[,c(2,3)]

DT = data.table(x=rnorm(9), y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DT,3)
DT[,list(mean(x), sum(z))] 
DT[c(2,3)]
DT[2,]
DT[DT$y=="a",]
DT[,c(2,3)]
DT[,table(y)]
DT[,w:=z^2]
DT[,m:={tmp <- (x+z); log2(tmp+5)}]
DT[,a:=x>0]
DT[,b:=mean(x+w),by=a] #groupby a

tables()
k = {print(10); 5}

set.seed(123)
DT <- data.table(x=sample(letters[1:3],1E5,TRUE))
DT[, .N, by=x] #count
DT <- data.table(x=rep(c("a","b","c"),each=100), y=rnorm(300))
setkey(DT, x)
DT['a'] 

DT1 <- data.table(x=c('a','a','b','dt1'), y=1:4)
DT2 <- data.table(x=c('a','b', 'dt2'), z=5:7)
setkey(DT1,x); setkey(DT2,x)
merge(DT1, DT2)

#Fast reading
big_df <- data.frame(x=rnorm(1E6), y=rnorm(1E6))
file <- tempfile()
write.table(big_df, file=file, row.names = FALSE, col.names = TRUE, sep="\t", quote = FALSE)
system.time(fread(file))

system.time(read.table(file, header=TRUE, sep="\t"))










#week1 quiz
quizfile <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(quizfile, destfile = "C:/Users/HYZH/Desktop/Coursera/week1quiz.csv")
quiz <- read.csv("C:/Users/HYZH/Desktop/Coursera/week1quiz.csv")
dateDownload <- date()
head(quiz$VAL)
DT = data.table(quiz)
DT[, .N, by=VAL==24] #53
head(DT$FES, 20)






library(readxl)
quiz2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(quiz2, destfile = "C:/Users/HYZH/Desktop/Coursera/week1quiz2.xlsx", mode="wb")
dateDownload2 <- date()
colIndex <- 7:15
rowIndex <- 18:23
dat <- read_xlsx("C:/Users/HYZH/Desktop/Coursera/week1quiz2.xlsx", range = "G18:O31", col_types = "numeric")

sum(dat$Zip*dat$Ext,na.rm=T)
#option 2
library(xlsx)
# if (!file.exists("data")) {
#      dir.create("data")
# }
fileXLS <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
# use mode = "wb" forces binary mode - doesn't read correctly without this!
download.file(fileXLS,destfile="NGAP.xlsx", mode = "wb")
dateDownloadedXLS <- date() # if you want to store the date of download
colIndex <- 7:15 
rowIndex <- 18:23
dat <- read.xlsx("NGAP.xlsx",sheetIndex=1, colIndex = colIndex, rowIndex = rowIndex) #select first sheet, specific col/rows.
sum(dat$Zip*dat$Ext,na.rm=T) # code lesson gives your to run






library(XML)
quiz3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(quiz3, useInternal = TRUE)
rootNode <- xmlRoot(doc)

zipcodes <- xpathApply(rootNode,"//zipcode",xmlValue)
xmlZipcodeDT <- data.table(zipcode = zipcodes)
xmlZipcodeDT[zipcode == "21231", .N]





library(data.table)
url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv'
download.file(url,"commune.csv")
file <- tempfile()
write.table(DT, file=file, row.names = F, col.names = T, sep = ',', quote = F)
DT <- fread('commune.csv')


install.packages('microbenchmark')
library(microbenchmark)

f1 <- function(x) {
    DT[,mean(pwgtp15),by=SEX]
}

f2 <- function(x) {
    sapply(split(DT$pwgtp15,DT$SEX),mean)
}

f3 <- function(x) {
    mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
}

f4 <- function(x) {
    tapply(DT$pwgtp15,DT$SEX,mean)
}

microbenchmark(f1(x), f2(x), f3(x), f4(x))

mbm = microbenchmark(
    v3 = sapply(split(DT$pwgtp15,DT$SEX),mean),
    v6 = DT[,mean(pwgtp15),by=SEX],
    v7 = tapply(DT$pwgtp15,DT$SEX,mean),
    v8 = mean(DT$pwgtp15,by=DT$SEX),
    times=100
)
mbm
            


#Debug  
#Erro,Warning,Message,Condition
#traceback,debug,browser,trace,recover
traceback(mbm)
debug(mbm)
options(error = recover)
