#############################
# Load in packages and data
#############################

# Packages
library(tidyverse)
library(ggplot2)
source("R/Load_Helpers.R")

d <- readRDS("data/TTA2_metadata_2026-03-22.rds") # doesn't work 
d2 <- d <- load_most_recent_by_mtime("data/", pattern = "TTA2_meta_response_filtered-")

####################
# Cue descriptives
####################

# Which cues have the lowest number of responses

cue_count <- d %>% 
  filter(cue_rt_mili > 200) %>% ## no cue response times quicker than 200 ms
  group_by(participant) %>% 
  mutate(z_cue_rt_mili = (cue_rt_mili - mean(cue_rt_mili))/sd(cue_rt_mili), ## participant-wise rt z-scores
         z_type_dur_mili = (type_dur_mili - mean(type_dur_mili))/sd(type_dur_mili)) %>% ## participant-wise typing z-scores
  filter(abs(z_cue_rt_mili) <= 2 & abs(z_type_dur_mili) <= 2) %>% ## removing response times > 2 z-scores from mean
  mutate(response = na_if(response, 'idk')) %>% # replace idk with NA for counting
  group_by(cue, response) %>% 
  mutate(response = ifelse((response == cue), NA, response)) %>% 
  filter(!is.na(response)) %>% 
  group_by(cue,response) %>% 
  count() %>%
  group_by(cue) %>% 
  mutate(count = sum(n)) %>% 
  select(cue, count) %>% 
  distinct() %>% 
  arrange(count)
# these cue-response pairs counted up differently compared to
# when I used meta_response_filtered

cue_count2 <- d2 %>% 
  filter(!is.na(response)) %>% 
  group_by(cue,response) %>% 
  count() %>%
  group_by(cue) %>% 
  mutate(count = sum(n)) %>% 
  select(cue, count) %>% 
  distinct() %>% 
  arrange(count)

ggplot(cue_count, aes(x = count, y = cue, fill = count)) +
  geom_col(position = 'stack') 

# Flag participants not responding to large amount of cues excluding the top 5 cues not responded to

x <- cue_count2 %>% 
  head(5L) %>% 
  select(cue)

lowest_resp <- x$cue # make into a vector

pp_prop <- d %>% 
  mutate(response = ifelse((cue_rt_mili < 200), NA, response)) %>% # cue response times quicker than 200 ms turned into NA
  # group_by(participant) %>% 
  # mutate(z_cue_rt_mili = (cue_rt_mili - mean(cue_rt_mili))/sd(cue_rt_mili), ## participant-wise rt z-scores
  #        z_type_dur_mili = (type_dur_mili - mean(type_dur_mili))/sd(type_dur_mili)) %>% ## participant-wise typing z-scores
  # mutate(response = ifelse((abs(z_cue_rt_mili) >= 2 & abs(z_type_dur_mili) >= 2), NA, response)) %>% ## changes responses with response times >= 2 z-scores from mean into NA
  mutate(response = na_if(response, 'idk')) %>% # replace idk with NA for counting
  group_by(cue, response) %>% 
  mutate(response = ifelse((response == cue), NA, response)) %>% 
  filter(!(cue %in% lowest_resp)) %>% # removes lowest 5 cues from proportions
  group_by(participant) %>% 
  mutate(na_count_pp = sum(is.na(response))) %>% 
  mutate(na_proportion = na_count_pp/59) %>% # proportion of NA responses out of 59 cues (64 minus 5 lowest responses)
  arrange(desc(na_proportion)) %>% 
  select(participant, na_proportion) %>% 
  distinct() 

ggplot(pp_prop, aes(x = na_proportion)) +
  geom_histogram(bins = 100)

# Flag participants responding with the cue 50% of the time
cue_match_resp <- d %>% 
  filter(!(cue %in% lowest_resp)) %>% # removes lowest 5 cues from proportions
  mutate(cue_resp_match = ifelse((response == cue & (!is.na(response))), 1, 0)) %>% 
  group_by(participant) %>% 
  mutate(cue_match_count = sum(as.integer(cue_resp_match))) %>% 
  mutate(cue_match_prop = cue_match_count/59) %>%  # proportion of cue-response matches out of 59 cues (64 minus 5 lowest responses)
  select(participant, cue_match_prop) %>% 
  distinct() %>% 
  arrange(desc(cue_match_prop))

ggplot(cue_match_resp, aes(x = cue_match_prop)) +
  geom_histogram(bins = 50)
