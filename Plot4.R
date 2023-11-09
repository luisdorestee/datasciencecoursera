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


# Plot 4
setup_png("plot4.png")
par(mfrow=c(2,2)) # Set up the layout for a 2x2 panel of plots
with(subset_data, {
  # Panel 1: Sub_metering
  plot(DateTime, Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")
  lines(DateTime, Sub_metering_1, type="l", col="black")
  lines(DateTime, Sub_metering_2, type="l", col="red")
  lines(DateTime, Sub_metering_3, type="l", col="blue")
  
  # Panel 2: Voltage
  plot(DateTime, Voltage, type="l", ylab="Voltage", xlab="datetime")
  
  # Panel 3: Global_active_power vs Time
  plot(DateTime, Global_active_power, type="l", ylab="Global Active Power (kilowatts)", xlab="datetime")
  
  # Panel 4: Global_reactive_power vs Time
  plot(DateTime, Global_reactive_power, type="l", ylab="Global Reactive Power (kilowatts)", xlab="datetime")
})
dev.off()
