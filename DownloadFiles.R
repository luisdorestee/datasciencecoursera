# Set the URL for the file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

# Use download.file() to download the file
download.file(url, destfile = "ss06hid.csv", method = "curl")

# Read the data into R
housing_data <- read.csv("ss06hid.csv")

# Assuming 'VAL' is the column that contains the property values
# and that the value 24 corresponds to properties worth $1,000,000 or more
# Count how many properties are worth $1,000,000 or more
properties_worth_million_or_more <- sum(housing_data$VAL == 24, na.rm = TRUE)

# Print the result
print(properties_worth_million_or_more)

#######################################

# Install and load the necessary package for reading Excel files
install.packages("readxl")
library(readxl)

# Set the URL for the Excel file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"

# Download the file
download.file(url, destfile = "DATA.gov_NGAP.xlsx", method = "curl")

# Read the specified range of the dataset
dat <- read_excel("DATA.gov_NGAP.xlsx", range = cell_rows(18:23), sheet = 1)

# Calculate the requested sum
sum_dat <- sum(dat$Zip * dat$Ext, na.rm = TRUE)

# Print the result
print(sum_dat)

#######################################

# Load the required library for XML processing
if (!require("XML")) {
  install.packages("XML", repos = "http://cran.r-project.org")
  library(XML)
}

# Downloading the XML file containing Baltimore restaurants data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(url, destfile = "restaurants.xml", method = "curl")

# Parse the XML file
doc <- xmlParse("restaurants.xml")

# Use XPath to find all zipcode nodes and then count the ones that are 21231
zipcodes <- xpathSApply(doc, "//zipcode", xmlValue)
num_restaurants <- sum(zipcodes == "21231")

# Output the result
print(num_restaurants)

#######################################

# Install and load the data.table package
if (!require(data.table)) {
  install.packages("data.table", repos = "http://cran.r-project.org")
  library(data.table)
}

# Set the URL for the file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"

# Download the file using download.file()
download.file(url, destfile = "ss06pid.csv", method = "curl")

# Load the data into an R object 'DT' using fread()
DT <- fread("ss06pid.csv")

# Calculate the average value of the variable 'pwgtp15'
average_pwgtp15 <- mean(DT$pwgtp15, na.rm = TRUE)

# Output the result
print(average_pwgtp15)

#######################################

# Install and load the httr package for working with HTTP
if (!require(httr)) {
  install.packages("httr")
  library(httr)
}

# Make a GET request to the GitHub API for the 'datasharing' repository
response <- GET("https://api.github.com/repos/jtleek/datasharing")

# Parse the response to JSON
repo_info <- content(response, "parsed")

# Extract the creation time
creation_time <- repo_info$created_at

# Print the creation time
print(creation_time)

#######################################

# Load the required libraries
library(httr)

# Fetch the HTML content from the website
response <- GET("http://biostat.jhsph.edu/~jleek/contact.html")

# Extract the content as text
html_content <- content(response, "text")

# Split the content into lines
lines <- strsplit(html_content, "\n")[[1]]

# Calculate the number of characters in the specified lines
line_lengths <- nchar(lines[c(10, 20, 30, 100)])

# Print the result
print(line_lengths)

#######################################

# Define the URL where the data set is located
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"

# Download the file locally (adjust the path as necessary)
download.file(url, destfile = "wksst8110.for")

# Since it's a fixed width file, you need to know the widths of each column.
# Assuming the widths are correctly identified, read the fourth column
# (update the 'widths' argument as necessary based on the actual data)
data <- read.fwf("wksst8110.for", widths = c(9, 5, 4, 5, 4, 5, 4, 5, 4), skip = 8)

# Assuming 'data' has been read correctly using read.fwf()
data$V4 <- as.numeric(gsub("[^0-9\\.]", "", data$V4))  # Remove non-numeric characters
sum_column <- sum(data$V4, na.rm = TRUE)  # Sum the column, omitting NAs

# Print the result
print(sum_column)

#######################################

# Download the dataset
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url, destfile = "ss06hid.csv")

# Load the data
data <- read.csv("ss06hid.csv")

# Assuming 'ACR' is the variable for acres and 'AGS' for agriculture sales from the codebook
# ACR == 3 corresponds to '> 10 acres' and AGS == 6 corresponds to '> $10,000'
agricultureLogical <- data$ACR == 3 & data$AGS == 6

# Find the row indices
indices <- which(agricultureLogical)

# Return the first three values
indices[1:3]

#######################################

# Install and load the jpeg package
if (!require(jpeg)) {
  install.packages("jpeg")
  library(jpeg)
}

# Download the image file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(url, destfile = "jeff.jpg", mode = "wb")

# Read in the picture using the jpeg package
jeff_img <- readJPEG("jeff.jpg", native = TRUE)

# Convert the image data to a vector
img_data <- as.vector(jeff_img)

# Calculate the 30th and 80th quantiles
quantiles <- quantile(img_data, probs = c(0.3, 0.8))

# Print the result
print(quantiles)

#######################################

# Load the GDP data
gdp_data <- read.csv("GDP.csv", skip = 4, stringsAsFactors = FALSE)
# Assuming the first 190 rows contain the relevant data and the rank is in the first column
gdp_data <- gdp_data[1:190,]
str(gdp_data)

# Load the educational data
edu_data <- read.csv("EDSTATS_Country.csv", stringsAsFactors = FALSE)
str(edu_data)

# Correct the merge function to match 'X' with 'CountryCode'
merged_data <- merge(gdp_data, edu_data, by.x = "X", by.y = "CountryCode", all = FALSE)

# Sort the data frame in descending order by GDP rank (converted to numeric)
sorted_data <- merged_data[order(-as.numeric(as.character(merged_data$X.1))),]

# Get the 13th country in the sorted data frame using the correct column name
thirteenth_country <- sorted_data[13, "X.3"]
print(thirteenth_country)

# Count the number of matching IDs
num_matches <- nrow(merged_data)

# Print the number of matches
print(num_matches)

##

# Convert GDP rank to numeric
merged_data$GDP.Rank <- as.numeric(as.character(merged_data$X.1))

# Calculate the average GDP rank for "High income: OECD"
avg_rank_oecd <- mean(merged_data$GDP.Rank[merged_data$Income.Group == "High income: OECD"], na.rm = TRUE)

# Calculate the average GDP rank for "High income: nonOECD"
avg_rank_non_oecd <- mean(merged_data$GDP.Rank[merged_data$Income.Group == "High income: nonOECD"], na.rm = TRUE)

# Print the averages
print(paste("Average GDP rank for High income: OECD is", avg_rank_oecd))
print(paste("Average GDP rank for High income: nonOECD is", avg_rank_non_oecd))

##

# Convert GDP rank to numeric if it's not already
merged_data$GDP.Rank <- as.numeric(as.character(merged_data$X.1))

# Cut the GDP ranking into 5 separate quantile groups
merged_data$GDP.Quantile <- cut(merged_data$GDP.Rank, breaks = quantile(merged_data$GDP.Rank, probs = seq(0, 1, 0.2), na.rm = TRUE), include.lowest = TRUE, labels = FALSE)

# Make a table of Income.Group versus GDP.Quantile
table(merged_data$Income.Group, merged_data$GDP.Quantile)

# Find how many countries are 'Lower middle income' among the top 38 nations with the highest GDP
top_countries <- merged_data[order(merged_data$GDP.Rank),]
top_38_high_income <- top_countries[top_countries$GDP.Quantile == 1 & top_countries$Income.Group == "Lower middle income", ]
number_of_countries <- nrow(top_38_high_income)

# Print the result
print(number_of_countries)

#######################################

# Step 1: Download the file
file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
dest_file <- "ss06hid.csv"
download.file(file_url, dest_file, method = "curl")

# Step 2: Load the data into R
housing_data <- read.csv(dest_file)

# Step 3: Apply strsplit() to the names of the data frame
split_names <- strsplit(names(housing_data), "wgtp")

# Retrieve the value of the 123rd element
value_123 <- split_names[[123]]
value_123

#######################################

# Download the GDP data
gdp_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
gdp_file <- "GDP.csv"
download.file(gdp_url, gdp_file, method = "curl")

# Load the GDP data into R, skipping the first 4 rows
gdp_data <- read.csv(gdp_file, skip = 1, stringsAsFactors = FALSE)

# Inspect the data structure
head(gdp_data)
str(gdp_data)

# The GDP data is in the 'X.4' column, clean it to remove commas and spaces, and convert to numeric
gdp_data$GDP_millions <- as.numeric(gsub(",", "", gsub("^\\s+|\\s+$", "", gdp_data$X.4)))

# Check for NAs and remove them
gdp_data <- gdp_data[!is.na(gdp_data$GDP_millions), ]

# Calculate the average GDP, ensuring there are data left to calculate the average
if (nrow(gdp_data) > 0) {
  average_gdp <- mean(gdp_data$GDP_millions)
} else {
  average_gdp <- NA
}

# Print the average GDP
print(average_gdp)

##

count_united_countries <- sum(grepl("^United", gdp_data$X.3))

# Print the count
print(count_united_countries)

#######################################

# Step 1: Load the GDP data
gdp_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
gdp_file <- "GDP.csv"
download.file(gdp_url, gdp_file, method = "curl")
gdp_data <- read.csv(gdp_file, skip = 1, stringsAsFactors = FALSE)

# Step 2: Load the educational data
edu_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
edu_file <- "EDSTATS_Country.csv"
download.file(edu_url, edu_file, method = "curl")
edu_data <- read.csv(edu_file, stringsAsFactors = FALSE)

# Step 3: Match the data based on the country shortcode
# Ensure the country code columns match between the two data sets
merged_data <- merge(gdp_data, edu_data, by.x = "X", by.y = "CountryCode")

# Step 4: Count the number of countries with fiscal year end in June
# Look in the 'Special.Notes' column for the fiscal year end information
fiscal_year_end_june_count <- sum(grepl("June", merged_data$Special.Notes, ignore.case = TRUE))

# Print the count
print(fiscal_year_end_june_count)

#######################################

# Load the necessary library
install.packages("quantmod")
library(quantmod)

# Get Amazon's stock price data
getSymbols("AMZN")

# Extract the dates when the data was sampled
sampleTimes <- index(AMZN)

# Filter the dates for the year 2012
sampleTimes2012 <- sampleTimes[format(sampleTimes, "%Y") == "2012"]

# Count the number of values collected in 2012
count2012 <- length(sampleTimes2012)

# Filter the dates for Mondays (day of the week = 1) in 2012
mondays2012 <- sampleTimes2012[format(sampleTimes2012, "%u") == "1"]

# Count the number of values collected on Mondays in 2012
countMondays2012 <- length(mondays2012)

# Print the counts
print(paste("Total values collected in 2012:", count2012))
print(paste("Total values collected on Mondays in 2012:", countMondays2012))
