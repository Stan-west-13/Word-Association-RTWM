## 002
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

#003
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(ez)
library(lme4)
library(lmerTest)
source("R/Load_Helpers.R")

z <- function(x){
  return((x - mean(x,na.rm = T))/sd(x,na.rm = T))
}


## Load data 
d <- load_most_recent_by_mtime("data/","TTA2_response*")
## Remove un-responded trials and responses < 200 ms

d_filt <- d %>%
  filter(!is.na(response))%>% # only trials with responses
  filter(cue_rt_mili > 200) %>% ## no cue response times quicker than 200 ms
  group_by(participant) %>% 
  mutate(z_cue_rt_mili = (cue_rt_mili - mean(cue_rt_mili))/sd(cue_rt_mili), ## participant-wise rt z-scores
         z_type_dur_mili = (type_dur_mili - mean(type_dur_mili))/sd(type_dur_mili)) %>% ## participant-wise typing z-scores
  filter(abs(z_cue_rt_mili) <= 2 & abs(z_type_dur_mili) <= 2) %>% ## removing response times > 2 z-scores from mean
  mutate(context = relevel(context,ref = "child")) %>% ## set "child" as the reference
  ungroup() %>%
  mutate(wf_z = z(Lg10WF),
         aoa_z = z(aoa),
         wl_z = z(nchar),
         cd_z = z(Lg10CD)) %>%
  mutate(context = factor(context, levels = c("peer", "child", "short", "creative")))
write_rds(d_filt, file = paste0("data/TTA2_meta_response_filtered-",Sys.Date(),".rds"))
write.csv(d_filt, "data/Julia_df_meta.csv")

## Long-formatted psycholing for splitting

d_long_filt_normalized <- d_filt %>%
  select(participant,
         cue,
         context,
         condition,
         counterbalance,
         block,
         aoa_z,
         wf_z,
         cd_z,
         wl_z) %>%
  pivot_longer(cols = ends_with("_z"),
               names_to = "measure",
               values_to = "value") %>%
  drop_na()

d_long_filt_nonnormalized <- d_filt %>%
  select(participant,
         cue,
         context,
         condition,
         counterbalance,
         block,
         aoa,
         Lg10WF,
         Lg10CD,
         nchar) %>%
  pivot_longer(cols = c("aoa",starts_with("Lg"),"nchar"),
               names_to = "measure",
               values_to = "value") %>%
  drop_na()

lst_mods <- list(normalized = d_long_filt_normalized, nonnormal = d_long_filt_nonnormalized)

## Split into lists for mapping analysis
d_split <- map(lst_mods, function(x){
  split(x,x$measure)
})


## Run LMER, plot interaction plots and bar plots.
mods <- imap(d_split, function(y,name){
  map(y, function(x){
    ## random intercepts for participants and cue
    m_lmer <- lmer(value ~ condition * context + (1|cue) + (1|participant), data = x ) 
    print(paste("############## Model output for ", unique(x$measure),name,"########################"))
    print(summary(m_lmer))
    p <- interaction.plot(
      x.factor = x$condition,
      trace.factor = x$context,
      response = x$value,
      fun = mean,
      type = "b",
      col = c("blue", "red","green","purple"),
      pch = c(19, 17),
      ylab = paste("Mean",unique(x$measure)),
      xlab = "Condition",
      trace.label = "Context")
    
    g <- ggplot(x, aes(x = context, y = value, fill = condition))+
      stat_summary(fun = "mean", geom = "col", position = "dodge")+
      ggtitle(paste0("Barplot by Context ",unique(x$measure)))+
      theme_classic()
    
    return(list(model = m_lmer, p,plot(g)))
  })
})

#004
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lme4)
library(rstatix)
library(lmerTest)
library(emmeans)
source("R/Load_Helpers.R")

z <- function(x){
  return((x - mean(x,na.rm = T))/sd(x,na.rm = T))
}


## Load data 
d <- load_most_recent_by_mtime("data/", pattern = "TTA2_meta_response_filtered-")


############ Response time to Word Association ################################

ggplot(d, aes(x = z_cue_rt_mili,fill =context))+
  geom_histogram()+
  facet_grid(condition~context)

ggplot(d, aes(x = condition, y = cue_rt_mili))+
  stat_summary(fun = "mean", geom="col")

## Looking at counterbalances: looks like people are faster in block 2 
## regardless of condition - maybe practice effects/wanting to be done. 
ggplot(d, aes(x = context, y = cue_rt_mili, fill = counterbalance))+
  stat_summary(fun = "mean", geom = "col", position = "dodge")+
  facet_grid(block~condition, labeller = "label_both") +
  geom_text(stat = "summary",fun = "mean",vjust = 12, aes(label = round(after_stat(y),2)),
            position = position_dodge(0.9))+
  theme_bw()


## Difference plots
d_diff <- d %>%
  group_by(context,condition,block,counterbalance) %>%
  summarize(m = mean(cue_rt_mili)) %>%
  group_by(context,block) %>%
  mutate(diff_cond = m[condition == "load"] - m[condition == "no_load"]) %>%
  group_by(context,condition) %>%
  mutate(diff_block = m[block == 2] - m[block == 1]) %>%
  ungroup()

ggplot(d_diff, aes(x = context, y = diff_cond, fill = block))+
  stat_summary(fun = "identity", geom = "col",position = "dodge")+
  theme_bw()+
  annotate("text",label = "FASTER IN LOAD",x=2.25,y = 125,size = 8)+
  annotate("text",label = "FASTER IN NO LOAD",x=2.25,y = -125,size = 8)



ggplot(d_diff, aes(x = context, y = diff_block, fill = condition))+
  stat_summary(fun = "identity", geom = "col",position = "dodge")+
  theme_bw()+
  annotate("text",label = "FASTER IN BLOCK 1",x=2.25,y = 125,size = 8)+
  annotate("text",label = "FASTER IN BLOCK 2",x=1.75,y = -300,size = 8)


## Just looking at block 1
ggplot(d %>% filter(block == 2), aes(x = context, y = cue_rt_mili, fill = condition))+
  stat_summary(fun = "mean", geom = "col", position = "dodge")+
  geom_text(stat = "summary",fun = "mean",vjust = 12, aes(label = round(after_stat(y),2)),
            position = position_dodge(0.9))+
  theme_bw()


glmer_fit <- glmer(
  cue_rt_mili ~ context * condition   + (1 | cue) + (1 | participant),
  data = d,
  family = inverse.gaussian("identity")
)

summary(glmer_fit)

em_condition <- emmeans(glmer_fit,~context|condition)
pairs(em_condition)
em_context <- emmeans(glmer_fit,~condition|context)
pairs(em_context)

glmer_plot_main <-  d %>%
  group_by(condition,context) %>%
  get_summary_stats(cue_rt_mili, type = c('mean_se'))

ggplot(glmer_plot_main, aes(x = condition, y = mean, fill = context))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9),
                width = 0.2)+
  geom_text(stat = "identity", aes(label = after_stat(y)), vjust = 45,
            position = position_dodge(0.9))

glmer_plot_type <- word_assoc_filt %>%
  group_by(condition,type,strength_strat,context) %>%
  get_summary_stats(cue_rt_mili, type = c('mean_se'))

ggplot(glmer_plot_type, aes(x = type, y = mean, fill = strength_strat))+
  geom_col(position = "dodge")+
  facet_grid(context~condition)


ggplot(d %>%
         select(context,
                condition,
                aoa,
                Lg10WF,
                Lg10CD,
                nchar) %>%
         pivot_longer(cols = c("aoa",starts_with("Lg"),"nchar"),
                      names_to = "measure",
                      values_to = "value"), aes(x = context, y = value))+
  stat_summary(fun = "mean",geom = "bar")+
  facet_grid(measure~condition, scales= "free_y")

ez_long <- word_assoc_filt %>%
  select(participant,
         cue,
         context,
         condition,
         aoa,
         Lg10WF,
         Lg10CD,
         nchar) %>%
  pivot_longer(cols = c("aoa",starts_with("Lg"),"nchar"),
               names_to = "measure",
               values_to = "value") %>%
  drop_na()



