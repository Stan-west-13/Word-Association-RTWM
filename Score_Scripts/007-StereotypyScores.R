#############################
# Load in packages and data
#############################

# Packages
library(tidyverse)
library(ggplot2)
source("R/Load_Helpers.R")

# Data 
#load_most_recent_by_mtime("data/","TTA2_response*")
filtered_corrected <- readRDS("Score_Scripts/rds_data/filter_corrected_2026-03-05.rds")
norms_dict <- readRDS("Score_Scripts/rds_data/norms_dict_2026-03-05.rds")
norms_list <- readRDS("Score_Scripts/rds_data/norms_list_2026-03-05.rds")

###################
# Stereotypy Scores 
###################

stereotypy_all <- filtered_corrected %>%
  group_by(context, cue, response) %>% 
  mutate(frequency_response = n(), .after = response) %>%
  group_by(context, cue) %>% 
  mutate(max_response = ifelse(frequency_response == max(frequency_response), TRUE, FALSE),
         .after = frequency_response) %>% 
  # group_by(context, cue, corrected_response) %>% 
  # mutate(frequency_corrected = n(), .after = corrected_response) %>% 
  # group_by(context, cue) %>% 
  # mutate(max_corrected = ifelse(frequency_corrected == max(frequency_corrected), TRUE, FALSE),
  #        .after = frequency_corrected) %>%
  ungroup() %>% 
  arrange(participant, cue, desc(frequency_response)) %>% 
  group_by(context, cue) %>% 
  mutate(stereotypy_score = as.integer(
    response %in% norms_list$response[norms_list$max_response == TRUE & 
                                        norms_list$cue %in% cue &
                                        norms_list$context %in% context]), 
    .after = response) %>%
  # mutate(stereotypy_score_corrected = as.integer(
  #   corrected_response %in% norms_list$corrected_response[norms_list$max_corrected == TRUE & 
  #                                                           norms_list$cue %in% cue &
  #                                                           norms_list$context %in% context]),
  #   .after = corrected_response) %>% 
  # this adds the count of stereotypical responses per participant
  group_by(participant) %>% 
  mutate(stereotypy_count = sum(as.integer(stereotypy_score == 1)), .after = response) %>%
  # mutate(stereotypy_count_corrected = sum(as.integer(stereotypy_score_corrected == 1)), 
  #        .after = corrected_response) %>% 
  #this adds the proportion of stereotypical responses per participant
  mutate(stereotypy_proportion = stereotypy_count/60, .after = stereotypy_count) 
  #mutate(stereotypy_proportion_c = stereotypy_count_corrected/60, .after = stereotypy_count_corrected) %>%

################
# Descriptives
################

hist_df <- stereotypy_all %>% 
  select(participant, context, stereotypy_proportion) %>% 
  distinct()

ggplot(hist_df, aes(x = stereotypy_proportion, fill = context)) +
  geom_histogram(position = "identity", alpha = .6, bins = 40) +
  geom_density(color = "black", alpha = 0.3)

