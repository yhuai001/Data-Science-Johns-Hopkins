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