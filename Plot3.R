library(lubridate)

# Set the working directory to the location of the dataset
setwd("C:/Users/User/Desktop/RStudio/datascience")

# Load the data
data <- read.csv("household_power_consumption.txt", header = TRUE, sep=";", na.strings="?")

# Parse dates and times using lubridate for convenience
data$Date <- dmy(data$Date)
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")


# Subset data for the dates 2007-02-01 and 2007-02-02
subset_data <- subset(data, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))

# Define a function to set up the PNG device
setup_png <- function(filename) {
  png(filename, width=480, height=480)
  par(mar=c(4, 4, 2, 1)) # Adjust margins to make room for labels and title as a GLOBAL function (for all plots)
}


### Plot 3
setup_png("plot3.png")
with(subset_data, {
  plot(DateTime, Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")
  lines(DateTime, Sub_metering_1, type="l", col="black")
  lines(DateTime, Sub_metering_2, type="l", col="red")
  lines(DateTime, Sub_metering_3, type="l", col="blue")
  legend("topright", col=c("black", "red", "blue"), lty=1,
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty="n")
})
dev.off()