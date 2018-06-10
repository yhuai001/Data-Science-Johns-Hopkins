#air pollution case
pm0 <- read.table("RD_501_88101_1999-0.txt",comment.char = "#",header = FALSE, sep = "|",na.strings = "")
pm1 <- read.table("RD_501_88101_2012-0.txt",comment.char = "#",header = FALSE, sep = "|",na.strings = "")
dim(pm0)
head(pm0)
cnames <- readLines("RD_501_88101_1999-0.txt",1)
cnames <- readLines("RD_501_88101_2012-0.txt",1)
cnames
cnames <- strsplit(cnames, "|", fixed = TRUE)
names(pm0) <- cnames[[1]]
names(pm1) <- cnames[[1]]
names(pm0) <- make.names(cnames[[1]])
names(pm1) <- make.names(cnames[[1]])
x0 <- pm0$Sample.Value
x1 <- pm1$Sample.Value
class(x0);class(x1)
str(x0);str(x1)
summary(x0);summary(x1)
mean(is.na(x1))
boxplot(x0,x1)
boxplot(log10(x0),log10(x1))
negative <- x1<0
str(negative)
sum(negative, na.rm = TRUE)
mean(negative, na.rm = TRUE)

dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates),"%Y%m%d")
str(dates)
hist(dates,"month")
hist(dates[negative],"month")

site0 <- unique(subset(pm0,State.Code == 36, c(County.Code,Site.ID)))
site1 <- unique(subset(pm1,State.Code == 36, c(County.Code,Site.ID)))
head(site0);head(site1)

site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")

#find intersection
both <- intersect(site0,site1)  
both

pm0$county.site <- with(pm0,paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1,paste(County.Code, Site.ID, sep = "."))
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

sapply(split(cnt0,cnt0$county.site),nrow)
sapply(split(cnt1,cnt1$county.site),nrow)

pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)

date1 <- pm1sub$Date
x1sub <- pm1sub$Sample.Value
plot(date1,x1sub)
date1 <- as.Date(as.character(date1),"%Y%m%d")

date0 <- pm0sub$Date
x0sub <- pm0sub$Sample.Value
date0 <- as.Date(as.character(date0),"%Y%m%d")
plot(date0,x0sub)

par(mfrow=c(1,2), mar = c(4,4,2,1))

plot(date0,x0sub,pch=20)
abline(h = median(x0sub, na.rm = T))
plot(date1,x1sub,pch=20)
abline(h = median(x1sub, na.rm = T))

#add ylim
range(x0sub,x1sub,na.rm = T)
rng <- range(x0sub,x1sub,na.rm = T)

par(mfrow=c(1,2), mar = c(4,4,2,1))
plot(date0,x0sub,pch=20,ylim=rng)
abline(h = median(x0sub, na.rm = T))
plot(date1,x1sub,pch=20,ylim=rng)
abline(h = median(x1sub, na.rm = T))

#exploring change at the State Level
head(pm0)
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm=T))
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm=T))
summary(mn0);summary(mn1)
d0 <- data.frame(state=names(mn0), mean=mn0)
d1 <- data.frame(state=names(mn1), mean=mn1)
head(d0);head(d1)
mrg <- merge(d0,d1,by="state")
head(mrg)
par(mfrow = c(1,1))
with(mrg,plot(rep(1999,52),mrg[,2],xlim = c(1998,2013)))
with(mrg,points(rep(2012,52),mrg[,3]))
segments(rep(1999,52),mrg[,2],rep(2012,52),mrg[,3])
