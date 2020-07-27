library(haven)
library(tidyverse)
library(dplyr)
library(wru)
library(curl)
library(httr)
library(sf)
library(lubridate)
library(rvest)

#load last name dictionary
setwd("/Users/sunnyshao/Documents/fec_analysis/")

candidates <- c("BIDEN, JOSEPH R JR", 
                "SANDERS, BERNARD",
                "WARREN, ELIZABETH", 
                "BLOOMBERG, MICHAEL R.",
                "BUTTIGIEG, PETE", 
                "KLOBUCHAR, AMY J.",
                "GABBARD, TULSI", 
                "YANG, ANDREW MR.",
                "DELANEY, JOHN K.", 
                "STEYER, TOM",
                "TRUMP, DONALD J.", 
                "BENNET, MICHAEL F.",
                "BOOKER, CORY A.", 
                "CASTRO, JULIAN",
                "DE BLASIO, BILL", 
                "GILLIBRAND, KIRSTEN",
                "HARRIS, KAMALA D.", 
                "HICKENLOOPER, JOHN W.",
                "INSLEE, JAY R", 
                "O'ROURKE, ROBERT BETO",
                "RYAN, TIMOTHY J.", 
                "SWALWELL, ERIC MICHAEL",
                "WILLIAMSON, MARIANNE")
cand_list %>%
  taRifx::remove.factors() %>%
  filter(stringr::str_detect(V2, "TRUMP")) %>%
  select(V2, V4, V5)


cand_list <- read.delim(file = "raw_2020/candidates.txt", header = FALSE, sep = "|", dec = ".")
cand_list <- cand_list %>% 
  taRifx::remove.factors() %>% 
  filter(V2 %in% candidates) %>% 
  filter(V6 == "P") %>% 
  select(V1, V2, V10) %>% 
  rename(candidate_name = V2,
         cand_id = V1,
         CMTE_ID2 = V10)

committee_cand_crosswalk <- read.delim(file = "raw_2020/ccl.txt", header = FALSE, sep = "|", dec = ".")
cand_commit <- committee_cand_crosswalk %>% 
  taRifx::remove.factors() %>%
  rename(cand_id = V1,
         CMTE_ID = V4) %>% 
  left_join(cand_list) %>% 
  filter(is.na(candidate_name) == F) %>% 
  select(candidate_name, CMTE_ID) %>%
  unique() %>% 
  mutate(CMTE_NM = NA_character_)

# committee_list <- read.delim(file = "raw_2020/cm 3.txt", header = FALSE, sep = "|", dec = ".")
# write_csv(committee_list, "raw_2020/additional_committee.csv", na = "")

additional_com <- read_csv("raw_2020/additional_committee.csv")
additional_com <- additional_com %>% 
  filter(CMTE_TP != "S" & CMTE_TP != "H") %>%
  filter(CMTE_DSGN %in% c("P", "J")) %>% 
  filter(KEEP == "YES") %>% 
  select(candidate_name, CMTE_ID, CMTE_NM)



cand_commit_final <- rbind(cand_commit, additional_com)

cand_commit_final <- cand_commit_final %>% 
  group_by(CMTE_ID, candidate_name) %>% 
  arrange(CMTE_NM) %>% 
  mutate(row = row_number()) %>% 
  ungroup() %>% 
  filter(row == 1) %>% 
  mutate(CMTE_NM = case_when(
    is.na(CMTE_NM) == T ~paste("Presidential Campaign - ", candidate_name, sep = ""),
    TRUE ~CMTE_NM)) %>% 
  select(-row)

list_committee <- as.vector(cand_commit_final$CMTE_ID)

# write_csv(cand_commit_final, "raw_2020/committee_list.csv", na = "")


raw <- read.delim(file = "raw_2020/ind_campaign.txt", header = FALSE, sep = "|", quote = "",
    fill = TRUE, comment.char = "")

# raw <- read.table(file = "raw_2020/ind_campaign.txt", header = FALSE, sep = "|", quote = "\"", comment.char = "", fill = TRUE)
# n <- count.fields(file = "raw_2020/ind_campaign.txt", sep = "|", quote = "\"", comment.char = "")
# table(n)
# x <- readLines("raw_2020/ind_campaign.txt")
# head(x[n == 21])

write_csv(raw, "raw_2020/rawdata.csv", na = "")
# raw <- read_csv("raw_2020/rawdata.csv")


merged_dta <- raw %>% 
  taRifx::remove.factors() %>% 
  rename(CMTE_ID = V1) %>% 
  filter(CMTE_ID == "C00580100")
  left_join(cand_commit_final) %>% 
  filter(is.na(candidate_name) == F)

table <- raw %>% 
  filter(V1 == "C00746651")
rm(raw)

merged_dta <- merged_dta %>% 
  rename(AMNDT_IND = V2,
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

write_rds(final, "final_2020/presi2020_contrib.rds")
# rm(list=setdiff(ls(), "final"))




library(xml2)
library(rvest)
webpage_url <- "https://docquery.fec.gov/cgi-bin/dcdev/forms/C00580100/1414193/sa/ALL/1"
webpage <- xml2::read_html(webpage_url)






data <- "https://docquery.fec.gov/cgi-bin/dcdev/forms/C00580100/1414193/sa/ALL/1" %>%
  xml2::read_html() %>% 
  # html() %>%
  html_nodes(xpath='//*[(@id = "sadetails")]') %>%
  html_table()

data <- data[[1]]


url_origin <- "https://docquery.fec.gov/cgi-bin/dcdev/forms/C00580100/1414193/sa/17A"
i <- 1
data_previous <- data %>% filter(`Contributor's Name` == "AA")
rm(data)

for(i in 1:5) {
  print(i)
  if(i <= 5){
    
    url <- paste(url_origin, i, sep = "/")
    
    data <- url %>%
    xml2::read_html() %>% 
    html_nodes(xpath='//*[(@id = "sadetails")]') %>%
    html_table()
    
  data <- data[[1]]
  
  data <- rbind(data, data_previous)
  
  data_previous <- data
  }else{
    data_final <- data_previous
    return(data_final)
  }
  i <- i + 1
  
}







