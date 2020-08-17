library(tidyverse)
library(haven)
library(tidyverse)
library(dplyr)
library(gender)
library(wru)

setwd("/Users/sunnyshao/Documents/fec_analysis")
#read in biden data--------------------
data_biden1 <- read.delim(file = "raw_2020/fec_record/biden_q2_2019.txt", header = FALSE, sep = ",", quote = "\"",
           fill = TRUE, comment.char = "")
data_biden2 <- read.delim(file = "raw_2020/fec_record/biden_q3_2019.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_biden3 <- read.delim(file = "raw_2020/fec_record/biden_q4_2019.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_biden4 <- read.delim(file = "raw_2020/fec_record/biden_jan2020.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_biden5 <- read.delim(file = "raw_2020/fec_record/biden_feb2020.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_biden6 <- read.delim(file = "raw_2020/fec_record/biden_march2020.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_biden7 <- read.delim(file = "raw_2020/fec_record/biden_april2020.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_biden8 <- read.delim(file = "raw_2020/fec_record/biden_may2020.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_biden9 <- read.delim(file = "raw_2020/fec_record/biden_june2020.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")

data_biden <- rbind(data_biden1, data_biden2, data_biden3, data_biden4,
                    data_biden5, data_biden6, data_biden7, data_biden8, data_biden9)
rm(data_biden1, data_biden2, data_biden3, data_biden4,
   data_biden5, data_biden6, data_biden7, data_biden8, data_biden9)

#read in trump data--------------------
data_trump1 <- read.delim(file = "raw_2020/fec_record/trump_q1_2019.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_trump2 <- read.delim(file = "raw_2020/fec_record/trump_q2_2019.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_trump3 <- read.delim(file = "raw_2020/fec_record/trump_q3_2019.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_trump4 <- read.delim(file = "raw_2020/fec_record/trump_q4_2019.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_trump5 <- read.delim(file = "raw_2020/fec_record/trump_jan2020.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_trump6 <- read.delim(file = "raw_2020/fec_record/trump_feb2020.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_trump7 <- read.delim(file = "raw_2020/fec_record/trump_march2020.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_trump8 <- read.delim(file = "raw_2020/fec_record/trump_april2020.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_trump9 <- read.delim(file = "raw_2020/fec_record/trump_may2020.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")
data_trump10 <- read.delim(file = "raw_2020/fec_record/trump_june2020.txt", header = FALSE, sep = ",", quote = "\"",
                          fill = TRUE, comment.char = "")

data_trump <- rbind(data_trump1, data_trump2, data_trump3, data_trump4, data_trump5, 
                    data_trump6, data_trump7, data_trump8, data_trump9, data_trump10)

rm(data_trump1, data_trump2, data_trump3, data_trump4, data_trump5, 
   data_trump6, data_trump7, data_trump8, data_trump9, data_trump10)

data_cleaning <- function(raw_data){
  final <- raw_data %>% 
    filter(V1 == "SA17A") %>% 
    filter(V6 == "IND") %>%
    mutate(CMTE_ID = V2,
           lastname = V8,
           firstname = V9,
           address = paste(V13, V14, sep = ", "),
           city = V15,
           state = V16,
           zipcode = case_when(
             V17 <= 10000 ~stringr::str_pad(V17, 5, pad = "0"),
             V17 > 99999 & V17 <= 100000000 ~stringr::str_pad(V17, 9, pad = "0"),
             TRUE ~as.character(V17)),
           purpose = V18,
           date = V20,
           amount = V21,
           aggregate = V22,
           employer = V24,
           occupation = V25) %>% 
    select(CMTE_ID, date, lastname, firstname, address, city, state, 
           zipcode, purpose, amount, aggregate, employer, occupation)
  
  return(final)
}


#cleaning raw data----------------------
data_trump_clean <- data_cleaning(data_trump)
data_biden_clean <- data_cleaning(data_biden)
write_rds(data_trump_clean, "final_2020/fec_trump.RDS")
write_rds(data_biden_clean, "final_2020/fec_biden.RDS")

#name match and race predict ---------
dictionary <- read_csv("detailed_aa_surnames.csv") %>% 
  mutate(surname = toupper(name),
         ethnicity = CountryCode) %>% 
  select(surname, ethnicity)

data_trump_clean <- data_trump_clean %>% mutate(surname = toupper(firstname))
data_biden_clean <- data_biden_clean %>% mutate(surname = toupper(firstname))

census_api_key = "6d607a3d86ddac66ae79d90bd9f6d9d4e563e248"

race_out_trump <- predict_race(data_trump_clean, census.key = census_api_key, 
                         surname.only = TRUE) #Probabilities were imputed for 177315 surnames that could not be matched to Census list.
race_out_biden <- predict_race(data_biden_clean, census.key = census_api_key, 
                               surname.only = TRUE) #Probabilities were imputed for 192352 surnames that could not be matched to Census list

race_out_final_trump <- race_out_trump %>% 
  left_join(dictionary)

race_out_final_biden <- race_out_biden %>% 
  left_join(dictionary)

#final cleaning --------------

race_out_final_trump %>%
  select(pred.whi,pred.bla,pred.his,pred.asi, pred.oth) -> race
race_out_final_trump$Race <- colnames(race)[max.col(race,ties.method="first")]

race_out_final_biden %>%
  select(pred.whi,pred.bla,pred.his,pred.asi, pred.oth) -> race
race_out_final_biden$Race <- colnames(race)[max.col(race,ties.method="first")]


race_out_final_trump <- race_out_final_trump %>% 
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
  mutate(candidate = "Trump 2020")

race_out_final_biden <- race_out_final_biden %>% 
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
  mutate(candidate = "Biden 2020")

race_out_final <- rbind(race_out_final_biden, race_out_final_trump)

#code zip code to county ----------
zip2zcta <- zipzcta::zipzcta

zip2county <- zcta::zcta_county_rel_10 %>%
  rename(zcta = zcta5) %>% 
  mutate(state = str_pad(state, width = 2, pad = "0")) %>% 
  select(zcta, county, geoid)

county_dictionary <- tidycensus::get_acs(table = "B01003", geography = "county", year = 2018) %>% select(GEOID, NAME) %>% rename(countyname = NAME)

dta_zcta <- race_out_final %>%
  mutate(zip = str_sub(zipcode, 1, 5)) %>% 
  left_join(zip2zcta) %>% 
  select(-po_name, -zip_type) %>% 
  left_join(zip2county) %>% 
  mutate(county_found = case_when(
    is.na(geoid) == F ~"YES",
    TRUE ~"NO")) %>% 
  # filter(is.na(geoid) == F) %>% 
  mutate(GEOID = as.character(geoid)) %>% 
  select(-zcta, -county) %>% 
  left_join(county_dictionary)

# write_rds(dta_zcta, "final_2020/fec2020_trump_biden_final.rds")
write_dta(dta_zcta, "final_2020/fec2020_trump_biden_final.dta")
###############################################################################################
###############################################################################################

test <- data_trump_clean %>% 
  # select(CMTE_ID, lastname, firstname, address, city, state, 
  #        zipcode, amount) %>% 
  # unique() %>% 
  select(amount) %>% 
  sum()

check <- data_trump_clean %>% 
  filter(stringr::str_detect(lastname, "EV")) %>% 
  filter(state == "CA")

check <- data_biden_clean %>% 
  filter(stringr::str_detect(lastname, "EV")) %>% 
  filter(state == "CA")
