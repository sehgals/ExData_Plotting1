plot3 <- function(){
consumption <- read.csv("household_power_consumption.txt",header = TRUE, sep=";")
consumption [[1]] <- as.Date(consumption [[1]],format="%d/%m/%Y")
#head(consumption)
sample <- consumption[consumption$Date %in% as.Date(c("2007-02-01","2007-02-02"),format="%Y-%m-%d"),]

sample$DateTime <- paste(sample$Date,sample$Time)

#head(sample)
y_range <- range(0, sample$Sub_metering_1, sample$Sub_metering_2,sample$Sub_metering_3)
x_range <- range(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"))
maxSubMt1 <- max(as.numeric(sample$Sub_metering_1))
minSubMt1 <- min(as.numeric(sample$Sub_metering_1))
maxSubMt2 <- max(as.numeric(sample$Sub_metering_2))
minSubMt2 <- min(as.numeric(sample$Sub_metering_2))
minDate <- min(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"))
maxDate <-  max(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"))
r <- as.POSIXct(round(x_range, "days"))

plot_colors <- c("blue","red","black")
plot(x_range,y_range,type="n",axes=FALSE,ann=FALSE)

axis.POSIXct(1, at=seq(r[1], r[2], by="days"), format="%a")
axis(2, at=10*0:maxSubMt1)
title(ylab= "Energy sub metering")

lines(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"),as.numeric(sample$Sub_metering_1), type="l", col=plot_colors[3])
lines(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"),as.numeric(sample$Sub_metering_2), type="l", col=plot_colors[2])
lines(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"),as.numeric(sample$Sub_metering_3), type="l", col=plot_colors[1])

legend("topright", 		     # places a legend at the appropriate place 
	c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), # puts text in the legend 
	lty=c(1,1), # gives the legend appropriate symbols (lines)
	lwd=c(2.5,2.5),col=c("black","red","blue")) # gives the legend lines the correct color and width
box()
dev.copy(png,"plot3.png")
dev.off()
}