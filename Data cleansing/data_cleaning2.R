#MySQL
ucscDb <- dbConnect(MySQL(), user="genome",
                    host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb, "show databases;"); dbDisconnect(ucscDb);
head(result)




hg19 <- dbConnect(MySQL(), user="genome", db="hg19",
                  host="genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
length(allTables)
allTables[1:5]

dbListFields(hg19,"affyU133Plus2")
dbGetQuery(hg19, "select count(*) from affyU133Plus2")

affyData <- dbReadTable(hg19, "affyU133Plus2")
head(affyData)

query <- dbSendQuery(hg19, "select*from affyU133Plus2 where misMatches between 1 and 3")
affyMis <- fetch(query); quantile(affyMis$misMatches)
affyMisSmall <- fetch(query, n=10)
dbClearResult(query);
dim(affyMisSmall)
dbDisconnect(hg19)


#hdf5
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

library(rhdf5)
created = h5createGroup("example.h5", "foo")
created = h5createGroup("example.h5", "baa")
created = h5createGroup("example.h5", "foo/foobaa")
h5ls("example.h5")

A = matrix(1:10, nr=5, nc=2)
h5write(A, "example.h5", "foo/A")
B = array(seq(0.1, 2.0, by=0.1), dim=c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "example.h5", "foo/foobaa/B")
h5ls("example.h5")

df = data.frame(1L:5L, seq(0,1,length.out = 5),
                c("ab","cde","fghi", "a", "s"), stringAsFactors = FALSE)
h5(df, "example.5", "df")
h5ls("example.h5")

readA = h5read("example.h5", "foo/A")
readB = h5read("example.h5", "foo/foobaa/B")
readdf = h5read("example.h5", "df")
readA

h5write(c(12,13,14), "example.h5", "foo/A", index=list(1:3,1))
h5read("example.h5", "foo/A")




#Web
library(XML)
url <- "https://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url, useInternalNodes = T)
xpathSApply(html, "//title", xmlValue)
xpathSApply(html, "//td[@id='col-citedby']", xmlValue)

library(httr)
html2 = GET(url)
content2 = content(html2, as="text")
parsedHtml = htmlParse(content2, asText=TRUE)
xpathSApply(parsedHtml, "//title", xmlValue)

pg1 = GET("http://httpbin.org/basic-auth/user/passwd")
pg1

pg2 = GET("http://httpbin.org/basic-auth/user/passwd",
          authenticate("user","passwd"))
pg2
names(pg2)

google = handle("http://google.com")
pg1 = GET(handle = google, path='search')



#API
myapp = oauth_app("twitter",
                  key="qKkJ4rNbLZJI42h4w72LlA799", secret="Jx505cjdsm")
sig = sign_oauth1.0(myapp,
                     token = "yourTokenHere",
                      token_secret = "yourTokenSecretHere")
homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json", sig)

json1 = content(homeTL)
json2 = jsonlite::fromJSON(toJSON(json1))
json2[1, 1:4]


#other

library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at 
#    https://github.com/settings/developers. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("github",
                   key = "fe6aeefadf80893f482b",
                   secret = "fa6f93542b8baeb8710a64248ef00c856c6161db")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
content(req)

#quiz2
library(sqldf)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", destfile = "quiz2data.csv")
acs <- read.csv("quiz2data.csv")
sqldf("select pwgtp1 from acs where AGEP < 50")
sqldf("select distinct AGEP from acs")


#quiz4
require(httr);require(XML)
URL <- url("http://biostat.jhsph.edu/~jleek/contact.html")
lines <- readLines(URL)
close(URL)
c(nchar(lines[10]), nchar(lines[20]), nchar(lines[30]), nchar(lines[100]))

#quiz5
quiz5 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
widths <- c(1, 9, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3)
fixed <- read.fwf(quiz5, widths, header = FALSE, skip = 4)
sum(fixed$V8)