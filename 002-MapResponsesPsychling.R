library(ggplot2)
library(dplyr)
library(emmeans)
library(lme4)
library(lmerTest)
library(statmod)
library(tidyverse)
library(rstatix)
source("R/Load_Helpers.R")




## Load data
d <- load_most_recent_by_mtime(directory = "data/", pattern = "TTA_metadata_*")

## Load psycholingistics
rsp_map <- readRDS("data/response_map 2.rds")
sub_map <- readRDS("data/subtlex 1.rds")
aoa_map <- readRDS("data/kuperman 1.rds")
sub <- read.csv("data/SUBTLEXusfrequencyabove1.csv")
aoa <- read.csv("data/AoA_51715_words.csv")

## Join previously mapped responses with psycholinguistics 
rsp_map_joined <- rsp_map %>%
  left_join(select(sub_map,-word), by = c("subtlex_id" = "id")) %>%
  left_join(select(aoa_map,-word), by = c("kuperman_id" = "id"))

## Combine mapped responses with unmapped psycholinguistic databases
all_psychling <- full_join(sub,aoa) %>%
  select(response = Word,Lg10WF, Lg10CD,aoa = AoA_Kup_lem) %>%
  rbind(select(rsp_map_joined, response, Lg10CD,Lg10WF, aoa)) %>%
  unique() %>%
  rowwise() %>%
  mutate(sum_na = sum(is.na(c_across(c(Lg10WF,Lg10CD,aoa))))) %>%
  group_by(response) %>%
  slice_min(sum_na, n = 1) ## keep the mapping with least amount of missing values

## Join psycholing to responses and add nchar
response_psychling <- d %>%
  left_join(all_psychling) %>%
  mutate(nchar = nchar(response))
saveRDS(response_psychling,file = paste0("data/TTA2_response_mapped_meta-",Sys.Date(),".rds"))

