# install.packages("devtools")
# devtools::install_github("jjchern/zipzcta")
# devtools::install_github("jjchern/zcta")
library(dplyr)
library(haven)
library(tidyverse)
library(stringr)

setwd("/Users/sunnyshao/Documents/fec_analysis/")

zip2zcta <- zipzcta::zipzcta

zip2county <- zcta::zcta_county_rel_10 %>%
  rename(zcta = zcta5) %>% 
  mutate(state = str_pad(state, width = 2, pad = "0")) %>% 
  select(zcta, county, geoid)

#load fec record data
data <- haven::read_dta("final_2020/final_contrib_wrace.dta")
data16 <- haven::read_dta("final_2016/final2016_contrib_wrace.dta")

# temp <- data16 %>%
#   select(candidate_name, race, zipcode_recode, zip_code) %>%
#   filter(candidate_name == "TRUMP, DONALD J.") %>%
#   filter(race == "Most Likely Asian")

#cover zip code to ZCTA
dta_zcta <- data %>%
  mutate(zip = str_sub(zipcode_recode, 1, 5)) %>% 
  left_join(zip2zcta) %>% 
  select(-po_name, -zip_type) %>% 
  left_join(zip2county) %>% 
  mutate(county_found = case_when(
    is.na(geoid) == F ~"YES",
    TRUE ~"NO")) %>% 
  filter(is.na(geoid) == F) %>% 
  rename(GEOID = geoid)
write_rds(dta_zcta, "final_2020/geo_final.rds")

dta_zcta16 <- data16 %>%
  mutate(zip = str_sub(zipcode_recode, 1, 5)) %>% 
  left_join(zip2zcta) %>% 
  select(-po_name, -zip_type) %>% 
  left_join(zip2county) %>% 
  mutate(county_found = case_when(
    is.na(geoid) == F ~"YES",
    TRUE ~"NO")) %>% 
  filter(is.na(geoid) == F) %>% 
  rename(GEOID = geoid)
write_rds(dta_zcta16, "final_2016/geo_final16.rds")

table1 <- dta_zcta %>% 
  mutate(GEOID = str_pad(GEOID, width = 5, pad = "0")) %>% 
  filter(is.na(race) == F) %>% 
  mutate(race = case_when(
    race == "Most Likely Asian" ~"asn",
    race == "Most Likely White" ~"wt",
    race == "Most Likely Hispanic" ~"lax",
    race == "Most Likely Black" ~"blk",
    race == "Most Likely Other" ~"oth")) %>% 
  mutate(candidate_name = case_when(
    candidate_name == "BIDEN, JOSEPH R JR" ~"T",
    candidate_name == "TRUMP, DONALD J." ~"B",
    TRUE ~NA_character_)) %>% 
  filter(is.na(candidate_name) == F) %>% 
  select(GEOID, race, candidate_name, transaction_amt) %>% 
  filter(is.na(race) == F) %>% 
  group_by(GEOID, race, candidate_name) %>% 
  mutate(donate = sum(transaction_amt)) %>% 
  ungroup() %>% 
  select(-transaction_amt) %>% 
  unique() %>% 
  mutate(group = paste(candidate_name, race, sep = "-")) %>% 
  select(-candidate_name, -race) %>% 
  spread(group, donate)

table2 <- dta_zcta %>% 
  mutate(GEOID = str_pad(GEOID, width = 5, pad = "0")) %>% 
  filter(ethnicity != "") %>% 
  mutate(candidate_name = case_when(
    candidate_name == "BIDEN, JOSEPH R JR" ~"B",
    candidate_name == "TRUMP, DONALD J." ~"T",
    TRUE ~NA_character_)) %>% 
  filter(is.na(candidate_name) == F) %>% 
  select(GEOID, ethnicity, candidate_name, transaction_amt) %>% 
  group_by(GEOID, ethnicity, candidate_name) %>% 
  mutate(donate = sum(transaction_amt)) %>% 
  ungroup() %>% 
  select(-transaction_amt) %>% 
  unique() %>% 
  mutate(group = paste(candidate_name, ethnicity, sep = "-")) %>% 
  select(-candidate_name, -ethnicity) %>% 
  spread(group, donate)

#2016 summary table------------
table3 <- dta_zcta16 %>% 
  mutate(GEOID = str_pad(GEOID, width = 5, pad = "0")) %>% 
  filter(is.na(race) == F) %>% 
  mutate(race = case_when(
    race == "Most Likely Asian" ~"asn",
    race == "Most Likely White" ~"wt",
    race == "Most Likely Hispanic" ~"lax",
    race == "Most Likely Black" ~"blk",
    race == "Most Likely Other" ~"oth")) %>% 
  mutate(candidate_name = case_when(
    candidate_name == "CLINTON, HILLARY RODHAM" ~"H",
    candidate_name == "TRUMP, DONALD J." ~"T",
    TRUE ~NA_character_)) %>% 
  filter(is.na(candidate_name) == F) %>% 
  select(GEOID, race, candidate_name, transaction_amt) %>% 
  filter(is.na(race) == F) %>% 
  group_by(GEOID, race, candidate_name) %>% 
  mutate(donate = sum(transaction_amt)) %>% 
  ungroup() %>% 
  select(-transaction_amt) %>% 
  unique() %>% 
  mutate(group = paste(candidate_name, race, sep = "$")) %>% 
  select(-candidate_name, -race) %>% 
  spread(group, donate)

table4 <- dta_zcta16 %>% 
  mutate(GEOID = str_pad(GEOID, width = 5, pad = "0")) %>% 
  filter(ethnicity != "") %>% 
  mutate(candidate_name = case_when(
    candidate_name == "CLINTON, HILLARY RODHAM" ~"H",
    candidate_name == "TRUMP, DONALD J." ~"T",
    TRUE ~NA_character_)) %>% 
  filter(is.na(candidate_name) == F) %>% 
  select(GEOID, ethnicity, candidate_name, transaction_amt) %>% 
  group_by(GEOID, ethnicity, candidate_name) %>% 
  mutate(donate = sum(transaction_amt)) %>% 
  ungroup() %>% 
  select(-transaction_amt) %>% 
  unique() %>% 
  mutate(group = paste(candidate_name, ethnicity, sep = "$")) %>% 
  select(-candidate_name, -ethnicity) %>% 
  spread(group, donate)


# table(dta_zcta16$race, dta_zcta16$candidate_name)
# table <- as.data.frame(table(dta_zcta$county_found, dta_zcta$ethnicity))
table <- dta_zcta16 %>%
  select(candidate_name, transaction_amt) %>% 
  group_by(candidate_name) %>% 
  mutate(donation = sum(transaction_amt)) %>% 
  ungroup() %>% 
  select(-transaction_amt) %>% 
  unique()

# write_csv(table, "final_2020/county_race_summary.csv")
# devtools::install_github('walkerke/tigris')
library(tigris)
library(ggplot2)
library(sf)
library(rgdal)
library(rmapshaper)
map <- counties(year = 2018, cb = TRUE, refresh = TRUE, resolution = "500k")
map_gen <- ms_simplify(map, keep = 0.05)

map_race <- map_gen %>%
  left_join(table1) %>% 
  left_join(table2) %>% 
  left_join(table3) %>% 
  left_join(table4)


st_write(map, "map/counties.shp", driver = "ESRI Shapefile")
st_write(map_race, "map/FECrace20.shp", driver = "ESRI Shapefile")

#################
table(data$transaction_amt < 0)
table(data16$transaction_amt < 0)
table(data$ethnicity, data$race)
