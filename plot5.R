NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

# Task 5: Changes in Emissions from Motor Vehicle Sources (1999-2008) in Baltimore City
# Load required packages
# Merge NEI and SCC data frames based on SCC column
NEI_merged <- left_join(NEI, SCC, by = "SCC")

# Filter data for motor vehicle sources in Baltimore City
motor_vehicle_emissions_baltimore <- NEI_merged %>%
  filter(fips == "24510" & grepl("Vehicle", EI.Sector))

# Aggregate total PM2.5 emissions for each year
total_motor_vehicle_emissions_baltimore <- motor_vehicle_emissions_baltimore %>%
  group_by(year) %>%
  summarise(total_pm25_emission = sum(Emissions, na.rm = TRUE))

# Plot using the base plotting system
png("plot5.png", width = 800, height = 600, res = 120)
plot(total_motor_vehicle_emissions_baltimore$year, total_motor_vehicle_emissions_baltimore$total_pm25_emission,
     type = "b", pch = 16, col = "purple",
     xlab = "Year", ylab = "Total PM2.5 Emission (tons)",
     main = "Changes in Emissions from Motor Vehicle Sources (1999-2008) in Baltimore City")
dev.off()

