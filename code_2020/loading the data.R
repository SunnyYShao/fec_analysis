library(haven)
library(tidyverse)
library(dplyr)
library(wru)
library(curl)
library(httr)
library(sf)
library(lubridate)

#load last name dictionary
setwd("/Users/sunnyshao/Documents/campaign contribution/")

candidates <- c("BIDEN, JOSEPH R JR", "SANDERS, BERNARD",
                "WARREN, ELIZABETH", "BLOOMBERG, MICHAEL R.",
                "BUTTIGIEG, PETE", "KLOBUCHAR, AMY J.",
                "GABBARD, TULSI", "YANG, ANDREW MR.",
                "DELANEY, JOHN K.", "STEYER, TOM",
                "TRUMP, DONALD J.", "BENNET, MICHAEL F.",
                "BOOKER, CORY A.", "CASTRO, JULIAN",
                "DE BLASIO, BILL", "GILLIBRAND, KIRSTEN",
                "HARRIS, KAMALA D.", "HICKENLOOPER, JOHN W.",
                "INSLEE, JAY R", "O'ROURKE, ROBERT BETO",
                "RYAN, TIMOTHY J.", "SWALWELL, ERIC MICHAEL",
                "WILLIAMSON, MARIANNE")
cand_list %>%
  taRifx::remove.factors() %>%
  filter(stringr::str_detect(V2, "TRUMP")) %>%
  select(V2, V4, V5)


cand_list <- read.delim(file = "raw/candidates.txt", header = FALSE, sep = "|", dec = ".")
cand_list <- cand_list %>% 
  taRifx::remove.factors() %>% 
  filter(V2 %in% candidates) %>% 
  filter(V6 == "P") %>% 
  select(V1, V2) %>% 
  rename(candidate_name = V2,
         cand_id = V1)

# committee_list <- read.delim(file = "raw/cm.txt", header = FALSE, sep = "|", dec = ".")
additional_com <- read_csv("raw/2020-committees.csv") %>% 
  select(CMTE_ID, CAND, CMTE_NM) %>% 
  rename(V1 = CMTE_ID,
         cmte_nm = CMTE_NM) %>% 
  mutate(candidate_name = case_when(
    CAND == "Bennet" ~"BENNET, MICHAEL F.",
    CAND == "Biden" ~"BIDEN, JOSEPH R JR",
    CAND == "Bloomberg" ~"BLOOMBERG, MICHAEL R.",
    CAND == "Booker" ~"BOOKER, CORY A.",
    CAND == "Castro" ~"CASTRO, JULIAN",
    CAND == "De Blasio" ~"DE BLASIO, BILL",
    CAND == "Delaney" ~"DELANEY, JOHN K.",
    CAND == "Gabbard" ~"GABBARD, TULSI",
    CAND == "Gillibrand" ~"GILLIBRAND, KIRSTEN",
    CAND == "Harris" ~"HARRIS, KAMALA D.",
    CAND == "Klobuchar" ~"KLOBUCHAR, AMY J.",
    CAND == "Sanders" ~"SANDERS, BERNARD",
    CAND == "Steyer" ~"STEYER, TOM",
    CAND == "Swalwell" ~"SWALWELL, ERIC MICHAEL",
    CAND == "Trump" ~"TRUMP, DONALD J.",
    CAND == "Warren" ~"WARREN, ELIZABETH",
    CAND == "Williamson" ~"WILLIAMSON, MARIANNE",
    CAND == "Yang" ~"YANG, ANDREW MR.",
    CAND == "Ryan" ~"RYAN, TIMOTHY J.")) %>% 
  select(V1, candidate_name, cmte_nm)

committee_cand_crosswalk <- read.delim(file = "raw/ccl.txt", header = FALSE, sep = "|", dec = ".")

cand_commit <- committee_cand_crosswalk %>% 
  taRifx::remove.factors() %>%
  rename(cand_id = V1) %>% 
  left_join(cand_list) %>% 
  filter(is.na(candidate_name) == F) %>% 
  rename(V1 = V4) %>% 
  select(V1, candidate_name) %>% 
  mutate(cmte_nm = NA_character_)

cand_commit_final <- rbind(cand_commit, additional_com)

cand_commit_final <- cand_commit_final %>% 
  group_by(V1, candidate_name) %>% 
  arrange(cmte_nm) %>% 
  mutate(row = row_number()) %>% 
  ungroup() %>% 
  filter(row == 1) %>% 
  mutate(cmte_nm = case_when(
    is.na(cmte_nm) == T ~paste("Presidential Campaign - ", candidate_name, sep = ""),
    TRUE ~cmte_nm)) %>% 
  select(-row)

write_csv(cand_commit_final, "committee_list.csv", na = "")
# raw <- read.delim(file = "raw/ind_campaign.txt", header = FALSE, sep = "|", dec = ".")
# write_csv(raw, "rawdata.csv", na = "")
raw <- read_csv("rawdata.csv")

merged_dta <- raw %>% 
  taRifx::remove.factors() %>% 
  left_join(cand_commit_final) %>% 
  filter(is.na(candidate_name) == F)

#test the candidate merge results
# merged_dta %>% 
#   select(candidate_name) %>% 
#   unique()
rm(raw)

merged_dta <- merged_dta %>% 
  rename(CMTE_ID = V1,
         AMNDT_IND = V2,
         RPT_TP = V3,
         TRANSACTION_PGI = V4,
         IMAGE_NUM = V5,
         TRANSACTION_TP = V6,
         ENTITY_TP = V7,
         NAME = V8,
         CITY = V9,
         STATE = V10,
         ZIP_CODE = V11,
         EMPLOYER = V12,
         OCCUPATION = V13,
         TRANSACTION_DT = V14,
         TRANSACTION_AMT = V15,
         OTHER_ID = V16,
         TRAN_ID = V17,
         FILE_NUM = V18,
         MEMO_CD = V19,
         MEMO_TEXT = V20,
         SUB_ID = V21) %>% 
  separate(NAME, into = c("surname", "firstname"), sep = ", ")

final <- merged_dta %>% 
  mutate(zipcode_recode = case_when(
    is.na(ZIP_CODE) == F ~stringr::str_pad(ZIP_CODE, 9, pad = "0"),
    TRUE ~NA_character_))

write_rds(final, "presi2020_contrib.rds")
  
