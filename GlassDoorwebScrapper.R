
library(tidyverse)
library(rvest)


#use saved HTML file to local file directory for testing to avoid too many htpp calls and getting blocked
#html <- read_html("/Users/stephenreilly/Desktop/CSUN/Spring 2024/541 Data mining/Assignments/project1/glassdoor_amazon_salaries_html.html")

html <- read_html("https://www.glassdoor.com/Salary/Amazon-Salaries-E6036.htm")

#each page of glassdoor as an html table of the salaries 
#extract table from html (will need to further clean it up in more steps below)
salary_table = html %>% 
  html_element(".salarylist_salary-table__QjAUn") %>% 
  html_table()

# rename columns for clarity 
salary_table <- salary_table %>%
  rename("job_title" = `Job Title`)

# rename columns for clarity 
salary_table <- salary_table %>%
  rename("dirty_salary" = `Total PayBase | Additional`)

#drop unnecessary column (open jobs)
salary_table <- salary_table %>%
  select(-`Open Jobs`)


# Define expression pattern that matches the salaries submitted section that the html parses wrong
  #[0-9]+                   matches 0-9 at the beginning going as far back as possible
  #[K]*                     matches K if it exists (ie could be 600 or 6K)
  #\\s                      matches space or new line character 
  #Salaries submitted\b     boundry to end matching after the word salaries submitted"
pattern <- "[0-9]+[K]*\\sSalaries submitted\\b"

#take out salaries submitted attribute and put it into a new column 
salary_table <- salary_table %>%
  mutate(salaries_submitted = str_extract(job_title, pattern))  

# remove all the salary data that ended up in the job title (everything after the $)
salary_table <- salary_table %>%
  mutate( job_title = str_replace(job_title, "\\$.+", "") )



# We need to break out the salary ranges in medians into the individual parts
#[0-9]+    pattern will group numbers into separate entities (there are 4 numbers to categorize)
salary_table <- salary_table %>%
  mutate( salary_breakdown = str_extract_all(dirty_salary, "[0-9]+", simplify=TRUE))


#the way glassdoor structures the Salary is as follows
#first range $estimated minimum salary no bonus - $estimated maximum salary with bonus
#second range $median salary|$median bonus
#for this analysis we decided to go with second range = Median pay (including bonus)
salary_table <- salary_table %>%
  mutate( min_salary = salary_breakdown[,1], 
          max_salary = salary_breakdown[,2], 
          median_salary = salary_breakdown[,3], 
          median_bonus = salary_breakdown[,4])

#drop the split columns since we don't need it anymore
salary_table <- salary_table %>%
  select( -salary_breakdown)



 #see output of one page 
print( salary_table)



#TODO loop through each page of a companies page and do the above process on each table.

#Get the number of page numbers for a company's salaries on Glassdoor to know how many tables to loop through 
total_salary_tables <-  html %>%  html_nodes(".pagination_ListItem__CqGNZ:nth-last-child(2)  ") %>% html_text()

total_salary_tables <- as.numeric(total_salary_tables)


#TODO get company names and employee ID to build URL 
#https://www.glassdoor.com/Explore/browse-companies.htm

#link is structured as 
#url schema::   https://www.glassdoor.com/Salary/company-name-Salries-EID.htm
#html <- read_html("https://www.glassdoor.com/Salary/Amazon-Salaries-E6036.htm")
#html <- read_html("https://www.glassdoor.com/Salary/Apple-Salaries-E1138")

link <- "https://www.glassdoor.com/Salary/Apple-Salaries-E1138"
url <- ""

#go to each page with the salary table data 
for(i in 1:total_salary_tables){ 
  
  url = paste(  link, "_P", i, ".htm", sep="" )
  print(url)

  #loop through each page of the company's salaries 
  
  #process each page using above code
  
  #r code to combine two tables together with identical columns 
  #all_salaries <- bind_rows(tibble1, tibble2)
  
  #output to csv textfile?? 
}

 