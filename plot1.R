plot1 <- function(){
consumption <- read.csv("household_power_consumption.txt",header = TRUE, sep=";")
consumption [[1]] <- as.Date(consumption [[1]],format="%d/%m/%Y")
#head(consumption)
sample <- consumption[consumption$Date %in% as.Date(c("2007-02-01","2007-02-02"),format="%Y-%m-%d"),]
#head(sample)
colors = c("red")
hist(as.numeric(as.character(sample[,3])),col=colors,
	main= "Global Active Power",
	xlab="Global Active Power (kilowatts)",
      ylab="Frequency",
	)
dev.copy(png,"plot1.png")
dev.off()
}
