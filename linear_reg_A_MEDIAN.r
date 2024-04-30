# Load necessary libraries
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)

# Define the file path and read the Excel file
file_path <- "national_M2023_dl.xlsx"
data_frame <- read_excel(file_path)

# Convert relevant columns to numeric
data_frame$A_MEDIAN <- as.numeric(as.character(data_frame$A_MEDIAN))
data_frame$A_MEAN <- as.numeric(as.character(data_frame$A_MEAN))
data_frame$H_PCT10 <- as.numeric(as.character(data_frame$H_PCT10))

# Cleaning A_MEDIAN
# Split the data into sets with and without missing A_MEDIAN values
train_data <- data_frame %>% filter(!is.na(A_MEDIAN) & !is.na(H_PCT10))
test_data <- data_frame %>% filter(is.na(A_MEDIAN) & !is.na(H_PCT10))

# Create the linear model
model <- lm(A_MEDIAN ~ H_PCT10, data = train_data)

# Predict the A_MEDIAN for missing values
predicted_values <- predict(model, newdata = test_data)

# Replace NA in original data with predicted values
data_frame$A_MEDIAN[is.na(data_frame$A_MEDIAN)] <- predicted_values

# Cleaning A_MEAN
# Split the data into sets with and without missing A_MEAN values
train_data <- data_frame %>% filter(!is.na(A_MEAN) & !is.na(A_MEDIAN))
test_data <- data_frame %>% filter(is.na(A_MEAN) & !is.na(A_MEDIAN))

# Create the linear model using A_MEDIAN to predict A_MEAN
model <- lm(A_MEAN ~ A_MEDIAN, data = train_data)

# Predict the A_MEAN for missing values
predicted_means <- predict(model, newdata = test_data)

# Replace NA in original data with predicted values
data_frame$A_MEAN[is.na(data_frame$A_MEAN)] <- predicted_means

# Sorting Data in Descending Order Using A_MEDIAN
sorted_data <- data_frame %>%
  arrange(desc(A_MEDIAN), .na.last = TRUE)

# Create a new data frame for plotting top 10 occupations by A_MEDIAN
top_occupations <- data_frame %>%
  drop_na(A_MEDIAN) %>%
  arrange(desc(A_MEDIAN)) %>%
  top_n(10) %>%
  mutate(OCC_TITLE = factor(OCC_TITLE, levels = OCC_TITLE[order(A_MEDIAN)]))

# Plotting the top 10 occupations by median annual wage
ggplot(top_occupations, aes(x = OCC_TITLE, y = A_MEDIAN, fill = OCC_TITLE)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 10 Occupations by Median Annual Wage",
       x = "Occupation Title",
       y = "Median Annual Wage (A_MEDIAN)") +
  coord_flip() # Flips the axes for better readability of occupation titles

# Scatter plot to compare A_MEDIAN and A_MEAN across all occupations
plot <- ggplot(data_frame, aes(x = A_MEDIAN, y = A_MEAN)) +
  geom_point(aes(color = OCC_TITLE), alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(title = "Comparison of Median and Mean Annual Wages Across Occupations",
       x = "Median Annual Wage (A_MEDIAN)",
       y = "Mean Annual Wage (A_MEAN)") +
  geom_smooth(method = "lm", color = "blue", se = FALSE)

# Save the ggplot to a file with increased dimensions
ggsave("my_large_plot.png", plot = plot, width = 20, height = 10, dpi = 300)

# Create an interactive plotly plot
fig <- plot_ly(data_frame, x = ~A_MEDIAN, y = ~A_MEAN, text = ~OCC_TITLE, type = 'scatter', mode = 'markers') %>%
  layout(title = 'Interactive Plot of Median and Mean Annual Wages Across Occupations')

# Print the interactive plot to the Viewer pane in RStudio
fig

theme(axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      title = element_text(size = 14))



# Execute the plot commands to display the charts
ggsave("top_occupations_bar_chart.png", width = 10, height = 8, units = "cm")
ggsave("plot.png", width = 10, height = 8, units = "cm")  # Adjust width and height as needed
