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

candidates <- c("TRUMP, DONALD J. / MICHAEL R. PENCE",
                "CLINTON, HILLARY RODHAM / TIMOTHY MICHAEL KAINE",
                "SANDERS, BERNARD",
                "RUBIO, MARCO",
                "CRUZ, RAFAEL EDWARD \"TED\"")
# cand_list %>%
#   filter(stringr::str_detect(CAND_NAME, "CRUZ")) %>%
#   select(CAND_NAME, CAND_OFFICE_ST)


cand_list <- read_csv(file = "raw_2016/candidate_list.csv")
cand_list <- cand_list %>% 
  filter(CAND_NAME %in% candidates) %>% 
  filter(CAND_OFFICE == "P") %>% 
  mutate(candidate_name = case_when(
    CAND_NAME == "TRUMP, DONALD J. / MICHAEL R. PENCE" ~"TRUMP, DONALD J.",
    CAND_NAME == "CLINTON, HILLARY RODHAM / TIMOTHY MICHAEL KAINE" ~"CLINTON, HILLARY RODHAM",
    CAND_NAME == "SANDERS, BERNARD" ~"SANDERS, BERNARD",
    CAND_NAME == "RUBIO, MARCO" ~"RUBIO, MARCO",
    CAND_NAME == "CRUZ, RAFAEL EDWARD \"TED\"" ~"CRUZ, RAFAEL EDWARD \"TED\"")) %>% 
  select(CAND_ID, candidate_name, CAND_PCC)


# committee_list <- read.delim(file = "raw/cm.txt", header = FALSE, sep = "|", dec = ".")
additional_com <- read_csv("raw_2016/additional_committee.csv") %>% 
  filter(CMTE_TP != "S" & CMTE_TP != "H") %>%
  filter(CMTE_DSGN %in% c("P", "J")) %>% 
  filter(str_detect(CMTE_NM, "TRUMP") | str_detect(CMTE_NM, "HILLARY") | 
           str_detect(CMTE_NM, "BERNIE") | str_detect(CMTE_NM, "RUBIO") |
           str_detect(CMTE_NM, "CRUZ")) %>% 
  filter(!CMTE_ID %in% c("C00628776", "C00578310", "C00565473"))

# write_csv(additional_com, "fec2016_com.csv", na = "")

# additional_com %>%
#   select(CMTE_NM)

additional_com <- additional_com %>% 
  mutate(candidate_name = case_when(
    CMTE_NM == "CRUZ FOR PRESIDENT" ~"CRUZ, RAFAEL EDWARD \"TED\"",
    CMTE_NM == "TED CRUZ VICTORY COMMITTEE" ~"CRUZ, RAFAEL EDWARD \"TED\"",
    CMTE_NM == "MARCO RUBIO FOR PRESIDENT" ~"RUBIO, MARCO",
    CMTE_NM == "RUBIO VICTORY COMMITTEE" ~"RUBIO, MARCO",
    CMTE_NM == "DONALD J. TRUMP FOR PRESIDENT, INC." ~"TRUMP, DONALD J.",
    CMTE_NM == "TRUMP MAKE AMERICA GREAT AGAIN COMMITTEE" ~"TRUMP, DONALD J.",
    CMTE_NM == "TRUMP VICTORY" ~"TRUMP, DONALD J.",
    CMTE_NM == "HILLARY FOR AMERICA" ~"CLINTON, HILLARY RODHAM",
    CMTE_NM == "HILLARY VICTORY FUND" ~"CLINTON, HILLARY RODHAM",
    CMTE_NM == "HILLARY ACTION FUND" ~"CLINTON, HILLARY RODHAM",
    CMTE_NM == "BERNIE 2016" ~"SANDERS, BERNARD",
    CMTE_NM == "BERNIE VICTORY FUND" ~"SANDERS, BERNARD")) %>% 
  rename(V1 = CMTE_ID) %>% 
  select(V1, candidate_name, CMTE_NM)

committee_cand_crosswalk <- read_csv("raw_2016/crosswalk.csv")
candidateID_list <- as.vector(cand_list$CAND_ID)

cand_commit <- committee_cand_crosswalk %>% 
  filter(CAND_ID %in% candidateID_list) %>% 
  filter(CMTE_TP %in% c("P")) %>% 
  left_join(cand_list) %>% 
  mutate(CMTE_NM = NA_character_) %>% 
  rename(V1 = CMTE_ID) %>% 
  select(V1, candidate_name, CMTE_NM)

cand_commit_final <- rbind(cand_commit, additional_com)
cand_commit_final <- cand_commit_final %>% 
  group_by(V1, candidate_name) %>% 
  arrange(CMTE_NM) %>% 
  mutate(row = row_number()) %>% 
  ungroup() %>% 
  filter(row == 1) %>% 
  select(-row)

write_csv(cand_commit_final, "raw_2016/committee_list_2016.csv", na = "")


# raw <- read.delim(file = "raw_2016/itcont.txt", header = FALSE, sep = "|", dec = ".")
# write_csv(raw, "raw_2016/rawdata.csv", na = "")
raw <- read_csv("raw_2016/rawdata.csv")

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
    ZIP_CODE <= 10000 ~stringr::str_pad(ZIP_CODE, 5, pad = "0"),
    ZIP_CODE > 99999 & ZIP_CODE <= 100000000 ~stringr::str_pad(ZIP_CODE, 9, pad = "0"),
    TRUE ~as.character(ZIP_CODE)))

write_rds(final, "raw_2016/presi2016_contrib.rds")

# final %>% 
#   select(candidate_name, TRANSACTION_AMT, CMTE_NM) %>% 
#   group_by(candidate_name, CMTE_NM) %>% 
#   mutate(don = sum(TRANSACTION_AMT)) %>% 
#   ungroup() %>% 
#   select(-TRANSACTION_AMT) %>% 
#   unique()
# 
# final %>% 
#   select(candidate_name, TRANSACTION_AMT) %>% 
#   group_by(candidate_name) %>% 
#   mutate(don = sum(TRANSACTION_AMT)) %>% 
#   ungroup() %>% 
#   select(-TRANSACTION_AMT) %>% 
#   unique()
