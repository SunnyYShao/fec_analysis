library(tidyverse)
library(dplyr)
library(gender)
library(wru)
# library(curl)
# library(httr)
# library(sf)

data <- read_csv("presi2020_contrib_wrace.csv")

data <- data %>% 
  separate(firstname, into = c("FIRST", "MIDDLE"), sep = " ")
#Gender coding
data_gender <- gender_df(data, name_col = "FIRST", method = "ipums")

data <- data %>%
  select(OCCUPATION) %>% 
  unique()
