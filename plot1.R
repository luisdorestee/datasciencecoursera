setwd("C:\\Users\\User\\Desktop\\RStudio\\datascience")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

# Task 1: Total PM2.5 Emissions in the United States (1999-2008)
# Aggregate total PM2.5 emissions for each year
total_emissions <- NEI %>%
  group_by(year) %>%
  summarise(total_pm25_emission = sum(Emissions, na.rm = TRUE))

# Plot using the base plotting system
png("plot1.png", width = 800, height = 600, res = 120)
plot(total_emissions$year, total_emissions$total_pm25_emission,
     type = "b", pch = 16, col = "blue",
     xlab = "Year", ylab = "Total PM2.5 Emission (tons)",
     main = "Total PM2.5 Emission in the United States (1999-2008)")
dev.off()
