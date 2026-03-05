#############################
# Load in packages and data
#############################

# Packages
library(tidyverse)
library(ggplot2)
source("R/Load_Helpers.R")

# Load data 
d <- load_most_recent_by_mtime("data/","TTA2_response*")

## Filtering out responses faster than 250 ms and more than
## 2.5 standard deviations away from participant response time 
## mean.
filter_participants <- d %>%
  filter(!is.na(response)) %>% 
  filter(cue_rt_mili > 250) %>% 
  group_by(participant) %>% 
  mutate(z_rt_pp = (cue_rt_mili - mean(cue_rt_mili))/sd(cue_rt_mili)) %>% 
  filter(z_rt_pp < 2.5) %>% 
  ungroup() %>% 
  mutate(
    participant = as.factor(participant),
    context = factor(context, c("child", "peer", "short", "creative")),
    cue = factor(cue),
    response = str_trim(response)
  )

## This data frame establishes the corrected version of responses while 
## keeping the original response and revision columns. It also removes 
## blanks and responses that are not relevant to cue (aka, anything 
## signaling they did not know the cue). This also removes any responses
## or corrected responses that match the cue.

filter_participants_corrected <- filter_participants %>%
  # mutate(corrected_response = ifelse((is.na(revision) != TRUE) & revision != "", revision, response),
  #        .after = revision) %>%
  filter(response != "") %>%
  filter(response != "notsure", response != "idk", response != "notsre") %>%
  filter(response != cue) #change to corrected_response when we get data cleaning

##########################################
# Creating norms list and norms dictionary
##########################################

# Norms List 

## This norms list has the frequency and the most commonly given
## cue-response pair for EACH CONTEXT. The columns frequency_response
## and frequency_corrected have the number of cue-response pairs made
## WITHIN each context. The columns max_response and max_corrected 
## reflect the most commonly given cue-response pair for EACH context.
## The latter two columns are used for the stereotypy score. 

norms_list <- filter_participants_corrected %>%
  #select(context, cue, response, revision, corrected_response) %>%
  select(context, cue, response) %>% 
  group_by(context, cue, response) %>% 
  mutate(frequency_response = n()) %>%
  group_by(context, cue) %>% 
  mutate(max_response = ifelse(frequency_response == max(frequency_response), TRUE, FALSE)) %>% 
  # group_by(context, cue, corrected_response) %>% 
  # mutate(frequency_corrected = n()) %>% 
  # group_by(context, cue) %>% 
  # mutate(max_corrected = ifelse(frequency_corrected == max(frequency_corrected), TRUE, FALSE)) %>%
  ungroup() %>% 
  distinct() %>% 
  #arrange(cue, desc(frequency_response), desc(frequency_corrected))
  arrange(cue, context, desc(frequency_response))

# Norms Dictionary

## This will be used for creativity scores. 
norms_dict <- norms_list %>% 
  #select(context, cue, response, corrected_response, frequency_response, max_response,
         #frequency_corrected, max_corrected) 
  select(context, cue, response, frequency_response, max_response)


############################################
# Save out .rds of what is needed for scores
############################################

#filter_participants_corrected
saveRDS(filter_participants_corrected, paste0("Score_Scripts/rds_data/filter_corrected_",Sys.Date(),".rds"))

#norm_list
saveRDS(norms_list, paste0("Score_Scripts/rds_data/norms_list_",Sys.Date(),".rds"))

#norms_dict
saveRDS(norms_dict, paste0("Score_Scripts/rds_data/norms_dict_",Sys.Date(),".rds"))

################
# Descriptives
################

# Looking at how many responses were made to cues (or at least how many are left over)
count_df <- norms_list %>% 
  group_by(cue, context) %>% 
  mutate(count_cue = sum(frequency_response)) %>% 
  select(-response, -frequency_response, -max_response) %>% 
  distinct()

ggplot(count_df, aes(x = count_cue, y = cue)) +
  geom_col(position = 'stack') +
  facet_wrap(~context)





