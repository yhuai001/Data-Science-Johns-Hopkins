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