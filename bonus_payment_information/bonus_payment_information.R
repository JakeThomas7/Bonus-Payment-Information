# Load Packages
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(jsonlite)

data_csv <- read_csv('iframev2.csv')
data_json <- fromJSON('iframev2.json')
# Edit start date
start_date <- ymd("2024-06-03")

# Edit this to change specific data collection
qualtrics_raw <- data_csv

qualtrics <- qualtrics_raw %>% 
  janitor::clean_names() %>%
  slice(3:nrow(.)) %>% 
  filter(!is.na(participant_id)) %>% 
  mutate(across(c(start_date,end_date,recorded_date), ~ymd_hms(.))) %>%
  select(start_date, end_date, duration_in_seconds, imposter_id_1:status_code4) %>% 
  filter(!is.na(difficulty_fraud)) %>% 
  rename(
    status_code_fraud_attempt_1 = status_code,
    status_code_fraud_attempt_2 = status_code2,
    status_code_genuine_attempt_1 = status_code3,
    status_code_genuine_attempt_2 = status_code4) %>% 
  rename(
    form_gen_street = genuine_id_1,
    form_gen_city = genuine_id_2,
    form_gen_state = genuine_id_3,
    form_gen_zip = genuine_id_4,
    form_imp_street = imposter_id_1,
    form_imp_city = imposter_id_2,
    form_imp_state = imposter_id_3,
    form_imp_zip = imposter_id_4)

# Read and Clean JSON file.
# Read in, convert to tibble
# Edit file for data collection.
json_data <- data_json
result_raw <- flatten(json_data) %>% as_tibble()

result_clean <- result_raw %>% 
  janitor::clean_names() %>% 
  select(transaction_id, customer_uid, created_at, user_info_qualtrics_part1_response_id, starts_with('metrics')) %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  filter(created_at >= ymd(start_date),
         metrics_status == 'COMPLETED') %>% 
  mutate(across(ends_with('is_answer_correct'), ~ as.integer(.x))) 

results_use <- result_clean %>% 
  mutate(fraud_cond = if_else(transaction_id %in% qualtrics$fraud_trans_id, 1, 
                              if_else(transaction_id %in% qualtrics$authentic_trans_id, 0, NA_integer_))) %>%
  filter(!is.na(fraud_cond)) %>% 
  mutate(fraud_cond = as.factor(if_else(fraud_cond==1,'fraud','genuine')))

results_join <- results_use %>% 
  select(transaction_id, contains("is_answer_correct")) %>% 
  rename("id" = transaction_id)

qualtrics %>%
  select(fraud_trans_id, authentic_trans_id, participant_id) %>%
  left_join(results_join, by=c("fraud_trans_id"="id")) %>%
  left_join(results_join, by=c("authentic_trans_id"="id"), suffix=c("_fraud","_authentic")) %>%
  select(-contains("trans")) %>% 
  mutate(number_correct = rowSums(select(., contains("correct")) == 1)) %>%
  arrange(desc(number_correct)) %>%
  write.csv(("bonus_payment_information.csv"), row.names = FALSE)










