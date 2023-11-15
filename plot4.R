NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

# Task 4: Changes in Emissions from Coal Combustion-related Sources (1999-2008) in the United States
# Merge NEI and SCC data frames based on SCC column
NEI_merged <- left_join(NEI, SCC, by = "SCC")

# Filter data for coal combustion-related sources
coal_emissions <- NEI_merged %>%
  filter(grepl("Coal", EI.Sector) & grepl("Comb", SCC.Level.Two))

# Aggregate total PM2.5 emissions for each year
total_coal_emissions <- coal_emissions %>%
  group_by(year) %>%
  summarise(total_pm25_emission = sum(Emissions, na.rm = TRUE))

# Plot using the base plotting system
png("plot4.png", width = 800, height = 600, res = 120)
plot(total_coal_emissions$year, total_coal_emissions$total_pm25_emission,
     type = "b", pch = 16, col = "red",
     xlab = "Year", ylab = "Total PM2.5 Emission (tons)",
     main = "Changes in Emissions from Coal Combustion-related Sources in the US")
dev.off()

