Load the data from the csv file
# df <- read.csv("gov_wages.csv")
# complete_df <- df

# # set a counter for the
# counter <- 0

# # Loop through the rows of the data frame of column A_MEAN
# col_name <- "A_MEAN"

# for (i in 1:nrow(complete_df)) {
#   value <- complete_df[i, col_name]
#   counter <- counter + 1
#   print(value)
#   if (value == "*") {
#     print("Found a star")
#     complete_df <- complete_df[-i,]
#   }

# }

# # Print the complete data frame after removing the rows with a star
# print(complete_df)


# # Print the counter
# print(counter)