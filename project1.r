# Install the necessary packages
# install.packages("readxl")
# install.packages("writexl")
# install.packages("jsonlite")
# install.packages("readr")
# install.packages("ggplot2")

# Load the necessary libraries
library(readxl)
library(writexl)
library(ggplot2)
library(readr)

# # Read the Excel file
# data <- read_excel("project_1_data.xlsx")

# # Write the data to a CSV file
# write.csv(data, file = "project_1_data.csv")

# Read the CSV file
data <- read_csv("project_1_data.csv")

# Calculate the mean, median and standard deviation of age and %fat
mean_age <- mean(data$age, na.rm = TRUE)
median_age <- median(data$age, na.rm = TRUE)
sd_age <- sd(data$age, na.rm = TRUE)

mean_fat <- mean(data$`fat_percentage`, na.rm = TRUE)
median_fat <- median(data$`fat_percentage`, na.rm = TRUE)
sd_fat <- sd(data$`fat_percentage`, na.rm = TRUE)

# Plot the boxplots for age and %fat
ggplot(data, aes(x = "", y = age)) + geom_boxplot() + xlab("Age")
ggplot(data, aes(x = "", y = `fat_percentage`)) + geom_boxplot() + xlab("%Fat")

# Plot a scatter plot based on these two variables
ggplot(data, aes(x = age, y = `fat_percentage`)) + geom_point() + xlab("Age") + ylab("%Fat")

# Plot a q-q plot based on these two variables
ggplot(data, aes(sample = age)) + stat_qq() + xlab("Theoretical Quantiles") + ylab("Sample Quantiles")
ggplot(data, aes(sample = `fat_percentage`)) + stat_qq() + xlab("Theoretical Quantiles") + ylab("Sample Quantiles")

