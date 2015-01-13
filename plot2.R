plot2 <- function(){
consumption <- read.csv("household_power_consumption.txt",header = TRUE, sep=";")
consumption [[1]] <- as.Date(consumption [[1]],format="%d/%m/%Y")
#head(consumption)
sample <- consumption[consumption$Date %in% as.Date(c("2007-02-01","2007-02-02"),format="%Y-%m-%d"),]

sample$DateTime <- paste(sample$Date,sample$Time)

#head(sample)
maxGlbActPow <- max(as.numeric(sample$Global_active_power))
minGlbActPow <- min(as.numeric(sample$Global_active_power))

minDate <- min(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"))
maxDate <-  max(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"))
#print(maxGlbActPow)
#print(minDate)
#print(maxDate)
#colors = c("red")
#hist(as.numeric(sample$Global_active_power)/500,col=colors,
#	main= "Global Active Power",
#	xlab="Global Active Power (kilowatts)"
#      ylab="Frequency",
#	)

plot(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"),as.numeric(sample$Global_active_power), type="l",xlim= c(minDate ,maxDate ),ylim=c(minGlbActPow ,maxGlbActPow),ann=FALSE)

title(ylab= "Global Active Power (kilowatts)")

dev.copy(png,"plot2.png")
dev.off()
}