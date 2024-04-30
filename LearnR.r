


# Load necessary libraries
library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation

# Read the Excel file
data_frame <- read_excel(file_path)
file_path <- "national_M2023_dl.xlsx"

# Convert A_MEDIAN to numeric. This will turn non-numeric strings to NA
data_frame$A_MEDIAN <- as.numeric(as.character(data_frame$A_MEDIAN))

# Handle possible warnings or errors in conversion
warnings()

# Now sort the data by A_MEDIAN in descending order
sorted_data <- data_frame %>%
  arrange(desc(A_MEDIAN), .na.last = TRUE)

# View the top entries in the sorted data
head(sorted_data)
View(sorted_data)


# START RUNNING FROM HERE

# Load necessary library
library(tidyr)
library(dplyr)

# Read the Excel file
data_frame <- read_excel(file_path)
file_path <- "national_M2023_dl.xlsx"

# Assuming your data is already read into 'data_frame'
# Convert all relevant columns to numeric if not already
data_frame$A_MEDIAN <- as.numeric(as.character(data_frame$A_MEDIAN))
data_frame$H_PCT10 <- as.numeric(as.character(data_frame$H_PCT10))

# Split the data into sets with and without missing A_MEDIAN values
train_data <- data_frame %>% filter(!is.na(A_MEDIAN) & !is.na(H_PCT10))
test_data <- data_frame %>% filter(is.na(A_MEDIAN) & !is.na(H_PCT10))

# Create the linear model
model <- lm(A_MEDIAN ~ H_PCT10, data = train_data)

# Predict the A_MEDIAN for missing values
predicted_values <- predict(model, newdata = test_data)

# Replace NA in original data with predicted values
data_frame$A_MEDIAN[is.na(data_frame$A_MEDIAN)] <- predicted_values

# Check results
summary(data_frame)
View(data_frame)

# Convert A_MEDIAN to numeric. This will turn non-numeric strings to NA
data_frame$A_MEDIAN <- as.numeric(as.character(data_frame$A_MEDIAN))

# Handle possible warnings or errors in conversion
warnings()

# Now sort the data by A_MEDIAN in descending order
sorted_data <- data_frame %>%
  arrange(desc(A_MEDIAN), .na.last = TRUE)

# View the top entries in the sorted data
head(sorted_data)
View(sorted_data)
