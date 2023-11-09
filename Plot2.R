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


### Plot 2
setup_png("plot2.png")
plot(subset_data$DateTime, subset_data$Global_active_power, type="n", xlab="", ylab="Global Active Power (kilowatts)")
lines(subset_data$DateTime, subset_data$Global_active_power, type="l", col="black")
dev.off()
