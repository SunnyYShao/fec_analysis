library(haven)
library(tidyverse)
library(dplyr)
library(gender)
library(wru)


setwd("/Users/sunnyshao/Documents/fec_analysis/")

#load last name dictionary
# raw_names <- read_dta("/Users/sunnyshao/Documents/AAPIsvy_name_learning/geo_dta/detailed_origin_conditional.dta") %>% 
#   write_csv("detailed_aa_surnames.csv", na = "")
dictionary <- read_csv("detailed_aa_surnames.csv") %>% 
  mutate(surname = toupper(name),
         ethnicity = CountryCode) %>% 
  select(surname, ethnicity)

#load raw data
# data <- final
# rm(final)
data <- read_rds("presi2020_contrib.rds")

#code race
census_api_key = "6d607a3d86ddac66ae79d90bd9f6d9d4e563e248"

race_out <- predict_race(data, census.key = census_api_key, 
                         surname.only = TRUE)
#[important msg note]: Probabilities were imputed for 676820 surnames that could not be matched to Census list.


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
  mutate(contribution_value_check = case_when(
    TRANSACTION_AMT < 0 ~"YES",
    TRUE ~"NO")) %>% 
  janitor::clean_names()

# df <- as.data.frame(table(race_out_final$cmte_nm))
# 
# cand_commit_final <- read_csv("committee_list.csv")
# cand_commit_final <- cand_commit_final %>% 
#   rename(cmte_id = V1)
# 
# final <- race_out_final %>% 
#   left_join(cand_commit_final)



# table(race_out_final$ethnicity)
# table(race_out_final$race)

race_out_final %>%
  filter(transaction_amt >= 0) %>% 
  filter(str_detect(cmte_nm, "BIDEN") | str_detect(cmte_nm, "TRUMP")) %>% 
  mutate(transaction_dt = str_pad(transaction_dt, width = 8, pad = "0")) %>% 
  mutate(month = as.numeric(str_sub(transaction_dt, start = 1, end = 2)),
         date = str_sub(transaction_dt, start = 3, end = 4),
         year = str_sub(transaction_dt, start = 5, end = 8)) %>% 
  filter(year == "2020") %>% 
  select(candidate_name, transaction_amt, cmte_nm, cmte_id, month, transaction_dt) %>%
  group_by(candidate_name, cmte_nm, month) %>%
  mutate(transaction_amt = sum(transaction_amt)) %>%
  ungroup() %>%
  unique() %>%
  mutate(sum_donation = scales::comma(transaction_amt)) -> table1

race_out_final %>%
  filter(str_detect(cmte_nm, "BIDEN") | str_detect(cmte_nm, "TRUMP")) %>%
  mutate(transaction_dt = str_pad(transaction_dt, width = 8, pad = "0")) %>% 
  mutate(month = as.numeric(str_sub(transaction_dt, start = 1, end = 2)),
         date = str_sub(transaction_dt, start = 3, end = 4),
         year = str_sub(transaction_dt, start = 5, end = 8)) %>% 
  # filter(month %in% c("04", "05", "06")) %>%
  select(candidate_name, transaction_amt) %>%
  group_by(candidate_name) %>%
  mutate(transaction_amt = sum(transaction_amt)) %>%
  ungroup() %>%
  unique() %>%
  mutate(sum_donation = scales::comma(transaction_amt)) -> table2

# race_out_final %>% 
#   filter(cmte_id == "C00744946")

# write_csv(race_out_final, "presi2020_contrib_wrace.csv", na = "")
write_dta(race_out_final, "final_2020/final_contrib_wrace.dta")
