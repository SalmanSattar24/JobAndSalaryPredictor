# Load the necessary libraries
library(readr)
library(dplyr)

# Load the data from the csv file
df <- read.csv("gov_wages.csv")

# Make a copy of the dataframe
complete_df <- df

# Convert "*" to NA in the A_MEAN column
complete_df$A_MEAN[complete_df$A_MEAN == "*"] <- NA

# Delete rows with NA values in the A_MEAN column
for (i in 1:nrow(complete_df)) {
  if (is.na(complete_df$A_MEAN[i])) {
    complete_df <- complete_df[-i, ]
  }
}

# Train a linear regression model on the complete_df A_MEAN column to predict the missing values
model <- lm(A_MEAN ~ ., data = complete_df)

# Predict the missing values in the A_MEAN column
for (i in 1:nrow(df)) {
  if (is.na(df$A_MEAN[i])) {
    df$A_MEAN[i] <- predict(model, df[i, ])
  }
}


# save the cleaned data to a new csv file
write.csv(complete_df, "gov_wages_cleaned.csv", row.names = FALSE)


