library(tidyverse)
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
           amount = V21,
           aggregate = V22,
           employer = V24,
           occupation = V25) %>% 
    select(CMTE_ID, lastname, firstname, address, city, state, 
           zipcode, purpose, amount, aggregate, employer, occupation)
  
  return(final)
}


#cleaning raw data----------------------
data_trump_clean <- data_cleaning(data_trump)
data_biden_clean <- data_cleaning(data_biden)
data_trump10 <- data_cleaning(data_trump10)

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
