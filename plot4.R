plot4 <- function(){
consumption <- read.csv("household_power_consumption.txt",header = TRUE, sep=";")
consumption [[1]] <- as.Date(consumption [[1]],format="%d/%m/%Y")
#head(consumption)
sample <- consumption[consumption$Date %in% as.Date(c("2007-02-01","2007-02-02"),format="%Y-%m-%d"),]

sample$DateTime <- paste(sample$Date,sample$Time)

#head(sample)
r_GlbActPow <- range(as.numeric(as.character(sample[,3])))
maxGlbActPow <- r_GlbActPow[2]
minGlbActPow <- r_GlbActPow[1]

y_range <- range(0, as.numeric(as.character(sample[,7])), as.numeric(as.character(sample[,8])),as.numeric(as.character(sample[,9])))
x_range <- range(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"))

r_SubMt1 <- range(0, as.numeric(as.character(sample[,7])))
maxSubMt1 <- r_SubMt1[2]
minSubMt1 <- r_SubMt1[1]

r_SubMt2 <- range(0, as.numeric(as.character(sample[,8])))
maxSubMt2 <- r_SubMt2[2]
minSubMt2 <- r_SubMt2[1]

minDate <- min(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"))
maxDate <-  max(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"))
r_voltage <- range(as.numeric(as.character(sample[,5])))
minVoltage <- r_voltage[1]
maxVoltage <- r_voltage[2]

r_GlbRactPow <- range(as.numeric(as.character(sample[,4])))
minGlbRactPow <- r_GlbRactPow[1]
maxGlbRactPow <- r_GlbRactPow[2]

r <- as.POSIXct(round(x_range, "days"))
par(mfrow=c(2,2))
###############PLOT-1#######################

plot(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"),as.numeric(as.character(sample[,3])), type="l",xlim= c(minDate ,maxDate ),ylim=c(minGlbActPow ,maxGlbActPow),ann=FALSE)

title(ylab= "Global Active Power (kilowatts)")
###############PLOT-2#######################
plot(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"),as.numeric(as.character(sample[,5])), type="l",xlim= c(minDate ,maxDate ),ylim=c(minVoltage ,maxVoltage),ann=FALSE)

title(xlab="datetime",ylab= "Voltage")

###############PLOT-3#######################
plot_colors <- c("blue","red","black")
plot(x_range,y_range,type="n",axes=FALSE,ann=FALSE)

axis.POSIXct(1, at=seq(r[1], r[2], by="days"), format="%a")
axis(2, at=10*0:maxSubMt1)
title(ylab= "Energy sub metering")

lines(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"),as.numeric(as.character(sample[,7])), type="l", col=plot_colors[3])
lines(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"),as.numeric(as.character(sample[,8])), type="l", col=plot_colors[2])
lines(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"),as.numeric(as.character(sample[,9])), type="l", col=plot_colors[1])

legend("topright", 		     # places a legend at the appropriate place 
	c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), # puts text in the legend 
	lty=c(1,1), # gives the legend appropriate symbols (lines)
	lwd=c(1,1),col=c("black","red","blue"),
	bty="n") # gives the legend lines the correct color and width
box()
###############PLOT-4#######################
plot(as.POSIXct(sample$DateTime,format="%Y-%m-%d %H:%M:%S"),as.numeric(as.character(sample[,4])), type="l",xlim= c(minDate ,maxDate ),ylim=c(minGlbRactPow ,maxGlbRactPow),axes=FALSE,ann=FALSE)
axis.POSIXct(1, at=seq(r[1], r[2], by="days"), format="%a")
axis(2, las=2,at=seq(0, r_GlbRactPow[2], by=0.1))
title(xlab="datetime",ylab= "Global_reactive_power")
box()
dev.copy(png,"plot4.png")
dev.off()
}