# install.packages("tidyverse")
# install.packages("RSelenium")
# install.packages("rvest")
# install.packages("netstat")
# install.packages("wdman")

library(tidyverse)
library(RSelenium)
library(rvest)
library(netstat)
library(wdman)

selenium()

# binman::list_versions("chromedriver")

selenium_object <- selenium(retcomand = T, check = F)

# Start a remote driver
rs_driver <- rsDriver(browser = "firefox",
                      port = 4444L,
                      verbose = T,
                      geckover = "0.26.0",
                      check = F,
                      retcomand = T,
                      version = "latest")

# Access the remote driver
remDr <- rs_driver$client
remDr$open()
remDr$navigate("https://www.indeed.com/jobs?q=high+paying+high+demand&start=20&vjk=4ea0d3692bdcfb9d")
