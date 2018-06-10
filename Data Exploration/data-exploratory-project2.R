#data-exploratory project2
library(dplyr)
library(ggplot2)

##Download and unzip
dataset <- "FNEI_Data.zip"
if (!file.exists(dataset)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    download.file(fileURL, dataset)
}  
if (!file.exists("FNEI_Data")) { 
    unzip(dataset) 
}

## Load datasets
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI);head(SCC)

##Q1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
png("plot1.png", width = 600, height = 600)
total <- with(NEI, aggregate(Emissions, by = list(year),sum))
plot(total, 
     pch=16,
     col="purple",
     main = "1999-2008 total PM2.5 Emissions trend",
     xlab = "Years",
     ylab = "PM2.5 Emissions",
     type = "b",
     xaxt="n"
     )
a <- c(1999,2002,2005,2008)
axis(1,a)

dev.off()



##Q2:Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
subNEI <- subset(NEI, fips == "24510")
head(subNEI)
baltimore_total <- with(subNEI, aggregate(Emissions, by = list(year),sum))

png("plot2.png", width = 600, height = 600)
plot(baltimore_total, 
     pch=16,
     col="purple",
     main = "1999-2008 total PM2.5 Emissions trend in Baltimore",
     xlab = "Years",
     ylab = "PM2.5 Emissions",
     type = "b",
     xaxt="n"
)
a <- c(1999,2002,2005,2008)
axis(1,a)

dev.off()





##Q3:Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999每2008 for Baltimore City? Which have seen increases in emissions from 1999每2008? Use the ggplot2 plotting system to make a plot answer this question.

subNEI <- subset(NEI, fips == "24510")
baltimore_by_Source <- with(subNEI, aggregate(Emissions,by = list(type=subNEI$type, year=subNEI$year), sum))
names(baltimore_by_Source)[3] <- c("Emissions")

png("plot3.png", width = 600, height = 600)
g <- ggplot(baltimore_by_Source, aes(x=factor(year),y=Emissions))
g <- g + geom_line(aes(colour=type,group=type))
g <- g + geom_point(aes(colour=type),size=3)
g <- g + labs(x="Years",y="PM2.5 Emissions", title="Total PM2.5 Emissions trend in Baltimore by Source Type")
g

dev.off()



##Q4:Across the United States, how have emissions from coal combustion-related sources changed from 1999每2008?
NEI_SCC <- merge(NEI,SCC,by="SCC")
subNEI2 <- NEI_SCC[grepl("Coal", NEI_SCC$Short.Name),]
total_coal <- with(subNEI2, aggregate(Emissions, by=list(year), sum))
names(total_coal) <- c("year","Emissions")

png("plot4.png", width = 600, height = 600)
g <- ggplot(total_coal, aes(factor(year),y=Emissions))
g <- g + geom_line(stat="identity",group="year")
g <- g + geom_point()
g <- g + labs(x="Years",y="PM2.5 Emissions", title="Total PM2.5 Emissions from coal combustion-related from 1999 to 2008")
g

dev.off()


##Q5:How have emissions from motor vehicle sources changed from 1999每2008 in Baltimore City?
subNEI3 <- subset(NEI, fips == "24510" & type == "ON-ROAD")
baltimore_by_mv <- with(subNEI3, aggregate(Emissions,by=list(year),sum))
names(baltimore_by_mv) <- c("year","Emissions")

png("plot5.png", width = 600, height = 600)
g <- ggplot(baltimore_by_mv, aes(factor(year),y=Emissions))
g <- g + geom_line(stat="identity",group="year")
g <- g + geom_point()
g <- g + labs(x="Years",y="PM2.5 Emissions", title="Baltimore total PM2.5 Emissions from motor vehicle from 1999 to 2008")
g

dev.off()


##Q6:Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
subNEI4 <- subset(NEI, fips == "06037" & type == "ON-ROAD")
baltimore_by_mv <- with(subNEI3, aggregate(Emissions,by=list(year,fips),sum))
LA_by_mv <- with(subNEI4, aggregate(Emissions,by=list(year,fips),sum))
names(baltimore_by_mv) <- c("year","fips","Emissions")
names(LA_by_mv) <- c("year","fips","Emissions")
baltimore_by_mv$name <- "Baltimore"
LA_by_mv$name <- "Los Angeles"
compare_mv <- rbind(baltimore_by_mv,LA_by_mv)

png("plot6.png", width = 600, height = 600)
g <- ggplot(compare_mv, aes(factor(year),y=Emissions,fill=factor(year)))
g <- g + geom_bar(stat="identity",group="fips")
g <- g + facet_grid(.~ name)
g <- g + geom_line(stat="identity",colour="blue",group="fips")
g <- g + geom_point()
g <- g + labs(x="Years",y="PM2.5 Emissions", title="Motor Vehicle total PM2.5 Emissions in Baltimore(06037) v.s LA(24510)")
g


dev.off()

