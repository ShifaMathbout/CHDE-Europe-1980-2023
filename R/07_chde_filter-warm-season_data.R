# Load the data.table library
library(data.table)

# Define the list of filenames
file_names <- c("1980-1981.csv", "1982-1983.csv", "1984-1985.csv", "1986-1987.csv",
"1988-1989.csv", "1990-1991.csv", "1992-1993.csv", "1994-1995.csv",
"1996-1997.csv", "1998-1999.csv", "2000-2001.csv", "2002-2003.csv",
"2004-2005.csv", "2006-2007.csv", "2008-2009.csv", "2010-2011.csv",
"2012-2013.csv", "2014-2015.csv", "2016-2017.csv", "2018-2019.csv",
"2020-2021.csv", "2022-2023.csv")

# Loop through each file
for (file_name in file_names) {
# Read the data
data <- fread(file_name, header = TRUE)
  
# Extract column names and filter those that represent dates between May and October
date_cols <- colnames(data)
# Identify columns where the month is May to October
keep_cols <- date_cols[grepl("-(0[5-9]|10)-", date_cols)]
  
# Select only the columns with dates from May to October
filtered_data <- data[, ..keep_cols]
  
# Create a new filename for the filtered data
new_file_name <- sub(".csv", "_May_to_October.csv", file_name)
  
# Save the filtered data to a new CSV file
fwrite(filtered_data, new_file_name)
  
# Print a message confirming the save
cat("Saved filtered data for", file_name, "as", new_file_name, "\n")}
#####################################################

# Load required libraries
library(dplyr)
library(lubridate)

# Read the CSV file
raw_data <- read.csv("EDI_D20001-D25000.csv", header = TRUE)

# Convert the first column to Date format (assuming it's named "Date")
raw_data$Date <- as.Date(raw_data$Date, format = "%Y-%m-%d")

# Extract month and day
raw_data <- raw_data %>%
mutate(Month = month(Date), Day = day(Date)) %>%
filter((Month >= 5 & Month <= 10))  # Keep only May 1st to October 31st

# Remove temporary columns using base R
raw_data <- raw_data[, !(names(raw_data) %in% c("Month", "Day"))]

# View the filtered data
head(raw_data)

# Optionally, save the filtered data
write.csv(raw_data, "EDI_D20001-D25000_Warm.csv", row.names = FALSE)

