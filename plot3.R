NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

# Task 3: Changes in Emissions by Source Type in Baltimore City (1999-2008)
# Filter data for Baltimore City
baltimore_emissions <- NEI %>%
  filter(fips == "24510")

# Group by source type and year, calculate total emissions
source_type_emissions <- baltimore_emissions %>%
  group_by(type, year) %>%
  summarise(total_pm25_emission = sum(Emissions, na.rm = TRUE))

# Plot using ggplot2
png("plot3.png", width = 800, height = 600, res = 120)
ggplot(source_type_emissions, aes(x = year, y = total_pm25_emission, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Total PM2.5 Emission (tons)",
       title = "Changes in Emissions by Source Type in Baltimore City (1999-2008)") +
  theme_minimal()
dev.off()
