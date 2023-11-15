NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

# Task 2: Total PM2.5 Emissions in Baltimore City (1999-2008)
# Filter data for Baltimore City
baltimore_emissions <- NEI %>%
  filter(fips == "24510")

# Aggregate total PM2.5 emissions for each year
total_emissions_baltimore <- baltimore_emissions %>%
  group_by(year) %>%
  summarise(total_pm25_emission = sum(Emissions, na.rm = TRUE))

# Plot using the base plotting system
png("plot2.png", width = 800, height = 600, res = 120)
plot(total_emissions_baltimore$year, total_emissions_baltimore$total_pm25_emission,
     type = "b", pch = 16, col = "green",
     xlab = "Year", ylab = "Total PM2.5 Emission (tons)",
     main = "Total PM2.5 Emission in Baltimore City (1999-2008)")
dev.off()
