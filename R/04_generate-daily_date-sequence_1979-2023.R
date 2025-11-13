# Load necessary library
library(lubridate)

# Generate a sequence of dates from October 1, 1979, to October 31, 2023
start_date <- as.Date("1979-10-01")
end_date <- as.Date("2023-10-31")
date_sequence <- seq(from = start_date, to = end_date, by = "day")

# Create a data frame
daily_data <- data.frame(Date = date_sequence)


# Save the data frame as a CSV file
write.csv(daily_data, "daily_data_1979_2023.csv", row.names = FALSE)

# Print completion message
cat("CSV file 'daily_data_1979_2023.csv' has been created and saved in the working directory.\n")
