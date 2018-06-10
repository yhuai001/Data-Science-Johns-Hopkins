#Principles
#1. Show comparisons
#2. Show causality, mechanism, explanation, systematic stucture
#3. Show multivariate data
#4. Integration of evidence
#5. Describe and document the evidence with appropriate labels, scales, sources, etc.
#6. Content is king


download.file("https://raw.githubusercontent.com/jtleek/modules/master/04_ExploratoryAnalysis/exploratoryGraphs/data/avgpm25.csv", destfile = "pollution.csv")
pollution <- read.csv("pollution.csv", colClasses = c("numeric", "character", "factor", "numeric","numeric"))

summary(pollution)
str(pollution)

boxplot(pollution$pm25, col="blue")
abline(h=12)

hist(pollution$pm25, col="green", breaks = 100)
rug(pollution$pm25)
abline(v=12, lwd=2)
abline(v= median(pollution$pm25), col="magenta", lwd=4)

barplot(table(pollution$region), col="wheat", main = "Number of Counties in Each Region")

boxplot(pm25 ~ region, data=pollution, col="red")
with(pollution, plot(latitude, pm25, col=region))
abline(h=12, lwd=2, lty=2)

par(mfrow=c(1,2), mar=c(5,4,2,1))
with(subset(pollution, region=="west"), plot(latitude, pm25, main="West"))
with(subset(pollution, region=="east"), plot(latitude, pm25, main="East"))

#Plotting
library(datasets)
data(cars)
with(cars, plot(speed,dist))
airquality <- transform(airquality, Month=factor(Month))
boxplot(Ozone~Month, airquality, xlab="Month", ylab="Ozone(ppb)")
with(airquality, plot(Wind, Ozone, main="Ozone and Wind in NY", type="n"))
with(subset(airquality, Month==5), points(Wind, Ozone, col="blue"))
with(subset(airquality, Month!=5), points(Wind, Ozone, col="red"))
legend("topright", pch=1, col=c("blue","red"), legend= c("May","Other Months"))

with(airquality, plot(Wind, Ozone, main="Ozone and Wind in NY", pch=20))
model <- lm(Ozone~Wind, airquality)
abline(model, lwd=2)

par(mfrow=c(1,3), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(airquality, {
    plot(Wind, Ozone, main="Ozone and Wind")
    plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
    plot(Temp, Ozone, main="Ozone and Temperature")
    mtext("Ozone and Weather in NY", outer = TRUE)
})


library(lattice)
state <- data.frame(state.x77, region=state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))

library(ggplot2)
data(mpg)
qplot(displ, hwy, data=mpg)



x <- rnorm(100); y <- rnorm(100); z <- rpois(100, 2)
hist(x)
plot(x,y)
par(mar=c(4,4,2,2))
plot(x,y, pch=2)

par(mfrow=c(2,1))
plot(x,y,pch=20)
plot(x,z,pch=2)

#points
g <- gl(2, 50, labels = c("Male", "Female"))
str(g)
plot(x,y, type="n")
points(x[g == "Male"], y[g == "Male"], col = "green")
points(x[g == "Female"], y[g == "Female"], col = "blue")
points(x[g == "Female"], y[g == "Female"], col = "blue", pch=19)




#Graphic Device
pdf(file="myplot.pdf")
g <- gl(2, 50, labels = c("Male", "Female"))
str(g)
plot(x,y, type="n")
title(main="Old Faithful")
points(x[g == "Male"], y[g == "Male"], col = "green")
points(x[g == "Female"], y[g == "Female"], col = "blue")
points(x[g == "Female"], y[g == "Female"], col = "blue", pch=19)
dev.copy(png, file="geyserplot.png")
dev.off()


#Assignment
#plot1.R
data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", stringsAsFactors=FALSE, dec=".")
subdata <- data[data$Date %in% c("1/2/2007","2/2/2007"),]
head(subdata)
str(subdata)


GAP <- as.numeric(subdata$Global_active_power)
png("plot1.png", width = 600, height = 600)
hist(GAP, col="red", main = "Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()

#plot2.R
data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", stringsAsFactors=FALSE, dec=".")
subdata <- data[data$Date %in% c("1/2/2007","2/2/2007"),]
head(subdata)

datetime <- strptime(paste(subdata$Date, subdata$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
GAP <- as.numeric(subdata$Global_active_power)
png("plot2.png", width=600, height=600)
plot(datetime, GAP, type="l", xlab="Thu-Fri-Sat", ylab="Global Active Power (kilowatts)")
dev.off()

#plot3.R
data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", stringsAsFactors=FALSE, dec=".")
subdata <- data[data$Date %in% c("1/2/2007","2/2/2007"),]
head(subdata)

datetime <- strptime(paste(subdata$Date, subdata$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
GAP <- as.numeric(subdata$Global_active_power)
subMetering1 <- as.numeric(subdata$Sub_metering_1)
subMetering2 <- as.numeric(subdata$Sub_metering_2)
subMetering3 <- as.numeric(subdata$Sub_metering_3)

png("plot3.png", width=600, height=600)
plot(datetime, subMetering1, type = "l", xlab="Thu-Fri-Sat", ylab="Energy Submetering")
lines(datetime, subMetering2, type="l", col="red")
lines(datetime, subMetering3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd=1, col=c("black", "red", "blue"))
dev.off()

#plot4.R
data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", stringsAsFactors=FALSE, dec=".")
subdata <- data[data$Date %in% c("1/2/2007","2/2/2007"),]
head(subdata)

datetime <- strptime(paste(subdata$Date, subdata$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
GAP <- as.numeric(subdata$Global_active_power)
GRP <- as.numeric(subdata$Global_reactive_power)
voltage <- as.numeric(subdata$Voltage)
subMetering1 <- as.numeric(subdata$Sub_metering_1)
subMetering2 <- as.numeric(subdata$Sub_metering_2)
subMetering3 <- as.numeric(subdata$Sub_metering_3)

png("plot4.png", width = 800, height = 800)
par(mfrow=c(2,2))

#1
plot(datetime, GAP, type="l", xlab="Thu-Fri-Sat", ylab="Global Active Power (kilowatts)")

#2
plot(datetime, voltage, type="l", xlab="datetime", ylab="Voltage")

#3
plot(datetime, subMetering1, type = "l", xlab="Thu-Fri-Sat", ylab="Energy Submetering")
lines(datetime, subMetering2, type="l", col="red")
lines(datetime, subMetering3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd=1, col=c("black", "red", "blue"))

#4
plot(datetime, GRP, type="l", xlab="datetime", ylab="Global_reactive_power")

dev.off()

