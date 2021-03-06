library(haven)
library(tidyverse)
library(dplyr)
library(gender)
library(wru)
library(curl)
library(httr)
library(sf)

setwd("/Users/sunnyshao/Documents/fec_analysis/")


#load last name dictionary
# raw_names <- read_dta("/Users/sunnyshao/Documents/AAPIsvy_name_learning/geo_dta/detailed_origin_conditional.dta") %>% 
#   write_csv("detailed_aa_surnames.csv", na = "")
dictionary <- read_csv("detailed_aa_surnames.csv") %>% 
  mutate(surname = toupper(name),
         ethnicity = CountryCode) %>% 
  select(surname, ethnicity)

#load raw data
data <- read_rds("raw_2016/presi2016_contrib.rds")

#code race
census_api_key = "6d607a3d86ddac66ae79d90bd9f6d9d4e563e248"

race_out <- predict_race(data, census.key = census_api_key, 
                         surname.only = TRUE)
#[important msg note]: Probabilities were imputed for 586029 surnames that could not be matched to Census list.


#now code the detailed ethnicity
race_out_final <- race_out %>% 
  left_join(dictionary)

#code the race of reach record based on the highest prob
race_out_final %>%
  select(pred.whi,pred.bla,pred.his,pred.asi, pred.oth) -> race
race_out_final$Race <- colnames(race)[max.col(race,ties.method="first")]

race_out_final <- race_out_final %>% 
  mutate(Race = case_when(
    Race == "pred.whi" ~"Most Likely White",
    Race == "pred.bla" ~"Most Likely Black",
    Race == "pred.his" ~"Most Likely Hispanic",
    Race == "pred.asi" ~"Most Likely Asian",
    Race == "pred.oth" ~"Most Likely Other",
    TRUE ~"")) %>% 
  mutate(Race = case_when(
    is.na(ethnicity) == F ~"Most Likely Asian",
    TRUE ~Race)) %>% 
  mutate(ethnicity = case_when(
    is.na(ethnicity) == T & Race == "Most Likely Asian" ~"Other Asian",
    TRUE ~ethnicity)) %>% 
  select(-pred.whi, -pred.bla, -pred.his,
         -pred.asi, -pred.oth) %>% 
  mutate(contribution_value_negative = case_when(
    TRANSACTION_AMT < 0 ~"YES",
    TRUE ~"NO")) %>% 
  janitor::clean_names()

# race_out_final %>%
#   select(candidate_name, transaction_amt) %>%
#   group_by(candidate_name) %>%
#   mutate(total = sum(transaction_amt)) %>%
#   ungroup() %>%
#   select(-transaction_amt) %>%
#   unique() %>%
#   mutate(sum_donation = scales::comma(total)) -> table1


    

# write_csv(race_out_final, "presi2020_contrib_wrace.csv", na = "")
write_dta(race_out_final, "final_2016/final2016_contrib_wrace.dta")
