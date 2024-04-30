
# Import the dataset “Groceries”( predefined in the R package). This dataset comprises 9835 
# transactions, each containing a variable number of items purchased together from the grocery store. 
# Apply apriori() function to the Groceries dataset with a minimum support of 0.02 and a minimum 
# confidence of 0.2. Print the first 10 strong association rules. (Hint: use inspect() function)


# Load the necessary libraries
library(arules)
library(arulesViz)

# Load the data from the Groceries dataset
data("Groceries")

# Apply apriori() function to the Groceries dataset with a minimum support of 0.02 and a minimum confidence of 0.2
rules <- apriori(Groceries, parameter = list(support = 0.02, confidence = 0.2))

# Print the first 10 strong association rules
inspect(rules[1:10])
