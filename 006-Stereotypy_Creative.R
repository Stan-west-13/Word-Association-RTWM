library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lme4)
library(rstatix)
library(lmerTest)
library(emmeans)
library(ggsignif)
source("R/Load_Helpers.R")

z <- function(x){
  return((x - mean(x,na.rm = T))/sd(x,na.rm = T))
}


## Load data 
d <- load_most_recent_by_mtime("data/", pattern = "TTA2_meta_response_filtered-")

## Stereotypy & Creative scores by context
d_sc <- d %>%
  group_by(context,cue) %>%
  count(response, sort = TRUE,name = "resp_count") %>%
  left_join(d, by = c("response","cue","context")) %>%
  group_by(context,cue) %>%
  mutate(mst_freq = ifelse(resp_count == max(resp_count), TRUE,FALSE)) %>%
  group_by(context,cue,response) %>%
  mutate(is_unique = ifelse(resp_count == 1,TRUE,FALSE)) %>%
  group_by(participant) %>%
  mutate(sum_stereo = sum(mst_freq),
         sum_creative = sum(is_unique))


ggplot(d_sc, aes(x = context, y = sum_stereo, fill = condition))+
  stat_summary(fun = "mean", 
               geom = "col", 
               position = "dodge") +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90))+
  geom_signif(
                 comparisons = list(c("peer", "child"), c("child", "short"), c("short","creative"),c("peer","short")),
                 map_signif_level = TRUE,  # Converts p-values to stars
                 test = "t.test",y_position = 23           # Statistical test to run
               )+
  labs(y = "avg_stereptypy")

ggplot(d_sc, aes(x = context, y = sum_creative, fill = condition))+
  stat_summary(fun = "mean", geom = "col", position = "dodge")+
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90))+
  geom_signif(
    comparisons = list(c("peer", "child"), c("child", "short"), c("short","creative"),c("peer","short")),
    map_signif_level = TRUE,  # Converts p-values to stars
    test = "t.test",y_position = 23           # Statistical test to run
  )+
  labs(y = "avg_creativity")


m_stereo <- lmer(sum_stereo ~ context*condition + (1|cue) , data = d_sc)
summary(m_stereo)


m_creative <- lmer(sum_creative ~ context*condition + (1|cue) , data = d_sc)
summary(m_creative)
