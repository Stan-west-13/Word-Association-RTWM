library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lme4)
library(rstatix)
z <- function(x){
  return((x - mean(x,na.rm = T))/sd(x,na.rm = T))
}


## Load data 
d <- read_rds("data/TTA2_meta_response_filtered-2026-02-26.rds")


############ Response time to Word Association ################################

ggplot(d, aes(x = z_cue_rt_mili,fill =context))+
  geom_histogram(aes(alpha = 0.5))+
  facet_grid(condition~context)


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
