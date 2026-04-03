library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

source("R/dbStatementHelpers.R")
source("R/Load_Helpers.R")

responses <- load_most_recent_by_mtime("data","TTA_metadata")
rsp_map <- readRDS("data/response_map 2.rds")

kuperman_table <- read_csv("data/AoA_51715_words.csv") %>%
  mutate(
    id = seq_len(n()),
    word = tolower(str_trim(Word))
  ) %>%
  select(id, word, aoa = AoA_Kup_lem) %>%
  drop_na()

subtlex_table <- read_csv("data/SUBTLEXusfrequencyabove1.csv") %>%
  mutate(
    id = seq_len(n()),
    word = tolower(str_trim(Word))
  ) %>%
  select(id, word, Lg10WF, Lg10CD) %>%
  drop_na()


subjects_table <- responses %>%
  select(
    participant
  ) %>%
  unique() %>%
  mutate(participant= as.factor(participant),
         id = 1:nrow(.),.before = participant)

# Prepare tables ----
cue_table <- responses %>%
  select(cue) %>%
  distinct() %>%
  arrange(cue) %>%
  mutate(id = seq_len(n())) %>%
  select(id, cue)


response_behavior_table <- responses %>%
  mutate(id = 1:n(),.before = participant) %>%
  group_by(participant) %>%
  mutate(cue_order = 1:n()) %>%
  select(
    id,
    cue,
    cue_order,
    response
  ) %>%
  mutate(
    response = tolower(str_trim(response))
  ) %>%
  left_join(
    cue_table %>% select(cue, cue_id = id),
    by = "cue"
  ) %>%
  left_join(
    subjects_table %>% select(participant, subject_id = id), 
    by = "participant"
    ) %>%
  ungroup() %>%
  select(-cue,-participant)

responses_table <- response_behavior_table %>%
  select(response) %>%
  drop_na() %>%
  distinct() %>%
  arrange(response) %>%
  mutate(id = seq_len(n()))

response_behavior_table <- response_behavior_table %>%
  left_join(responses_table %>% rename(response_id = id), by = "response") %>%
  select(id, subject_id, cue_order, cue_id, response_id, -response) %>%
  drop_na()

cues_responses_table <- response_behavior_table %>%
  select(cue_id, response_id) %>%
  distinct() %>%
  arrange(cue_id, response_id) %>%
  mutate(id = seq_len(n())) %>%
  select(id, cue_id, response_id)

response_map_table <- cues_responses_table %>%
  left_join(
    responses_table %>% rename(response_id = id),
    by = "response_id"
  ) %>%
  left_join(
    kuperman_table %>% select(response = word, kuperman_id = id),
    by = "response"
  ) %>%
  left_join(
    subtlex_table %>% select(response = word, subtlex_id = id),
    by = "response"
  ) %>%
  select(-cue_id) %>%
  left_join(
    cue_table %>% select(cue_id = id, response = cue),
    by = "response"
  ) %>%
  left_join(
    rsp_map %>% select(response, kuperman_id, subtlex_id) %>% unique(),
    by = "response",
    suffix = c("",".new")
  ) %>%
  mutate(kuperman_id = ifelse(!is.na(kuperman_id.new),kuperman_id.new,kuperman_id),
         subtlex_id = ifelse(!is.na(subtlex_id.new),subtlex_id.new,subtlex_id)) %>%
  select(-kuperman_id.new,-subtlex_id.new) %>%
  #filter(!(is.na(kuperman_id) & is.na(subtlex_id))) %>%
  rename(cue_response_id = id) %>%
  mutate(revision = NA, researcher_id = NA, timestamp = now(), id = seq_len(n())) %>%
  select(id, cue_response_id, kuperman_id, subtlex_id, cue_id, revision, researcher_id, timestamp, -response)


words_meta_table <- kuperman_table %>%
  select(word, kuperman_id = id) %>%
  full_join(subtlex_table %>% select(word, subtlex_id = id), by = "word") %>%
  full_join(cue_table %>% select(word = cue, cue_id = id), by = "word")

# Build database ----
con <- dbConnect(RSQLite::SQLite(), "Cleaning/databases/TTA2_WordAssociation-DB.db")
dbExecute(con, "PRAGMA foreign_keys = ON;")
sql_schema <- read_sql_schema("Cleaning/databases/Word-AssociationRTWM_schema.sql")
dbExecuteList(con, sql_schema)

dbWriteTable(con, "cues", cue_table, append = TRUE)
dbWriteTable(con, "kuperman", kuperman_table, append = TRUE)
dbWriteTable(con, "subtlex", subtlex_table, append = TRUE)
dbWriteTable(con, "subjects", subjects_table, append = TRUE)
dbWriteTable(con, "responses", responses_table, append = TRUE)
dbWriteTable(con, "response_behaviors", response_behavior_table, append = TRUE)
dbWriteTable(con, "cues_responses", cues_responses_table, append = TRUE)
dbWriteTable(con, "response_map", response_map_table, append = TRUE)
dbWriteTable(con, "words_meta", words_meta_table, append = TRUE)

dbDisconnect(con)
