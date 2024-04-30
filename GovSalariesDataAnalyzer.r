# Install necessary packages
install.packages("readxl", dependencies = TRUE)
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("openxlsx")


# Load necessary libraries
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(openxlsx)

# Read the Excel file
# job_data <- read.xlsx("C:/Users/salma/OneDrive/Desktop/COMP 541/oesm22nat/oesm22nat/national_M2022_dl.xlsx", sheet = 1)

job_data <- read_excel("C:/Users/salma/OneDrive/Desktop/COMP 541/oesm22nat/oesm22nat/national_M2022_dl.xlsx", sheet = 1)

# Standardize job titles
job_data <- job_data %>%
  mutate(standardized_titles = OCC_TITLE %>% 
           tolower() %>% 
           str_replace_all("[^[:alnum:] ]", "") %>% 
           str_trim())

# Categorize job titles
job_data <- job_data %>%
  mutate(Category = case_when(
    str_detect(standardized_titles, regex("it|information|technology|computer|software|app|developer", ignore_case = TRUE)) ~ "Information Technology",
    str_detect(standardized_titles, regex("manager|executive", ignore_case = TRUE)) ~ "Management",
    str_detect(standardized_titles, regex("physics|chemist|biologist|scientist|research|math|statistician|data|analyst|physicist|lab|geologist|environmental|biomedical", ignore_case = TRUE)) ~ "Science and Research",
    str_detect(standardized_titles, regex("engineer|architect|surveyor", ignore_case = TRUE)) ~ "Engineering and Architecture",
    str_detect(standardized_titles, regex("administrative|clerk|assistant", ignore_case = TRUE)) ~ "Administrative",
    str_detect(standardized_titles, regex("sales|marketing|customer", ignore_case = TRUE)) ~ "Sales and Marketing",
    str_detect(standardized_titles, regex("nurse|doctor|medical", ignore_case = TRUE)) ~ "Healthcare",
    TRUE ~ "Other"
  ))

# Ensure the A_MEAN (average annual salary) column is treated as numeric for visualization
job_data$A_MEAN <- as.numeric(job_data$A_MEAN)

# Visualization 1: Bar Plot of Average Salary by Job Category
avg_salary_by_category <- job_data %>%
  group_by(Category) %>%
  summarize(AvgSalary = mean(A_MEAN, na.rm = TRUE)) %>%
  arrange(desc(AvgSalary))

ggplot(avg_salary_by_category, aes(x = reorder(Category, AvgSalary), y = AvgSalary, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Salary by Job Category", x = "Job Category", y = "Average Annual Salary (USD)") +
  theme_minimal() +
  theme(legend.position = "none")

# Visualization 2: Box Plot of Salary Distribution by Job Category
ggplot(job_data, aes(x = Category, y = A_MEAN, fill = Category)) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Job Category", x = "Job Category", y = "Annual Mean Salary (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
