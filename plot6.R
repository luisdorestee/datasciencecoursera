NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

# Task 6: Compare Motor Vehicle Emissions between Baltimore City and Los Angeles County
# Merge NEI and SCC data frames based on SCC column
NEI_merged <- left_join(NEI, SCC, by = "SCC")

# Filter data for motor vehicle sources in Baltimore City and Los Angeles County
motor_vehicle_emissions_baltimore_LA <- NEI_merged %>%
  filter(fips %in% c("24510", "06037") & grepl("Vehicle", EI.Sector))

# Aggregate total PM2.5 emissions for each year and city
total_motor_vehicle_emissions_baltimore_LA <- motor_vehicle_emissions_baltimore_LA %>%
  group_by(fips, year) %>%
  summarise(total_pm25_emission = sum(Emissions, na.rm = TRUE))

# Plot using the base plotting system
png("plot6.png", width = 800, height = 600, res = 120)
plot(total_motor_vehicle_emissions_baltimore_LA$year,
     total_motor_vehicle_emissions_baltimore_LA$total_pm25_emission,
     type = "b", pch = 16, col = c("purple", "orange"),
     xlab = "Year", ylab = "Total PM2.5 Emission (tons)",
     main = "Comparison of Motor Vehicle Emissions (1999-2008) between Baltimore City and Los Angeles County",
     legend.text = c("Baltimore City", "Los Angeles County"),
     legend.position = "topright")
legend("topright", legend = c("Baltimore City", "Los Angeles County"), col = c("purple", "orange"), pch = 16)
dev.off()


