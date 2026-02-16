library(ggplot2)
library(dplyr)
library(emmeans)
library(lme4)
library(statmod)
library(tidyverse)
library(rstatix)

## Load data

d <- readRDS("data/TTA_metadata_2026-02-16.rds")
rsp_map <- readRDS("data/response_map 2.rds")
sub_map <- readRDS("data/subtlex 1.rds")
aoa_map <- readRDS("data/kuperman 1.rds")

rsp_map_joined <- rsp_map %>%
  left_join(select(sub_map,-word), by = c("subtlex_id" = "id")) %>%
  left_join(select(aoa_map,-word), by = c("kuperman_id" = "id"))
## Map psychling
sub <- read.csv("data/SUBTLEXusfrequencyabove1.csv")
aoa <- read.csv("data/AoA_51715_words.csv")
all_psychling <- full_join(sub,aoa) %>%
  select(response = Word,Lg10WF, Lg10CD,aoa = AoA_Kup_lem) %>%
  rbind(select(rsp_map_joined, response, Lg10CD,Lg10WF, aoa)) %>%
  unique() %>%
  rowwise() %>%
  mutate(sum_na = sum(is.na(c_across(c(Lg10WF,Lg10CD,aoa))))) %>%
  group_by(response) %>%
  slice_min(sum_na, n = 1)



## Filter out non-responded trials
responded_trials <- d %>%
  filter(!is.na(response)) %>%
  left_join(all_psychling) %>%
  mutate(nchar = nchar(response))


################ Accuracy and RT for spatial WM task #################################################
d_plot <- responded_trials %>%
  group_by(condition,trial_type) %>%
  summarize(mean_accuracy = mean(accuracy),
            sd_accuracy = sd(accuracy),
            se_accuracy = sd_accuracy/sqrt(n()),
            mean_rt = mean(square_rt_mili),
            sd_rt = sd(square_rt_mili),
            se_rt = sd_rt/sqrt(n()))

## Accuracy
ggplot(d_plot, aes(x = condition, y = mean_accuracy, fill = trial_type))+
  geom_col(position ="dodge")+
  geom_errorbar(aes(ymin  = mean_accuracy -se_accuracy, 
                    ymax = mean_accuracy+se_accuracy),
                width = 0.2, 
                size = 0.8,
                position = position_dodge(0.9))

ggplot(responded_trials, aes(x = accuracy, fill = condition))+
  geom_histogram()+
  facet_grid(context~condition)


t.test(d$accuracy[d$condition == "load"],d$accuracy[d$condition == "no_load"] )

## ANOVA of accuracy 
m <-aov(accuracy ~ context * condition * trial_type, data = responded_trials)
summary(m)
simp_effect_by_condition <- emmeans(m, ~context|condition)
simp_effect_by_context <- emmeans(m, ~condition|context)

pairs(simp_effect_by_condition, adjust = "tukey")
pairs(simp_effect_by_context)

## Simple effects plot

plot_simp <- pairs(simp_effect_by_condition)[1:12] %>% 
  as.data.frame()

ggplot(plot_simp, aes(x = contrast, y = estimate, color = condition))+
  geom_point(position = position_dodge(0.9))+
  geom_hline(yintercept = 0,linetype = 2)+
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE),
                width = 0.2,
                position = position_dodge(0.9))+
  facet_grid(~condition)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  geom_text(stat = "identity", aes(label = round(p.value,2)),vjust = 4)


## RT

responded_trials_rt <- responded_trials %>%
  group_by(participant) %>%
  mutate(square_rt_z_pp = (square_rt_mili-mean(square_rt_mili))/sd(square_rt_mili)) %>%
  filter(abs(square_rt_z_pp) <= 2)

d_plot_rt <- responded_trials_rt %>%
  group_by(condition,trial_type) %>%
  summarize(mean_rt = mean(square_rt_mili),
            sd_rt = sd(square_rt_mili),
            se_rt = sd_rt/sqrt(n()))

ggplot(d_plot_rt, aes(x = condition, y = mean_rt, fill = trial_type))+
  geom_col(position ="dodge")+
  geom_errorbar(aes(ymin  = mean_rt -se_rt, 
                    ymax = mean_rt+se_rt),
                width = 0.2, 
                size = 0.8,
                position = position_dodge(0.9))


ggplot(responded_trials_rt, aes(x = square_rt_mili, fill = condition))+
  geom_histogram()+
  facet_grid(context~condition)



t.test(responded_trials_rt$square_rt[responded_trials_rt$condition == "load"],
       responded_trials_rt$square_rt[responded_trials_rt$condition == "no_load"] )


## ANOVA of RT 
m_rt <-aov(square_rt ~ context * condition * trial_type, data = responded_trials_rt)
summary(m_rt)
simp_effect_by_condition_rt <- emmeans(m_rt, ~context|condition)
simp_effect_by_context_rt <- emmeans(m_rt, ~condition|context)

pairs(simp_effect_by_condition_rt, adjust = "tukey")
pairs(simp_effect_by_context_rt, adjust = "tukey")

## Simple effects plot

plot_simp_rt <- pairs(simp_effect_by_condition_rt)[1:12] %>% 
  as.data.frame()

ggplot(plot_simp_rt, aes(x = contrast, y = estimate, color = condition))+
  geom_point(position = position_dodge(0.9))+
  geom_hline(yintercept = 0,linetype = 2)+
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE),
                width = 0.2,
                position = position_dodge(0.9))+
  facet_grid(~condition)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  geom_text(stat = "identity", aes(label = round(p.value,2)),vjust = 4)


############################################################################################################

############ Response time Word Association #####################################

word_assoc_filt <- responded_trials %>%
  filter(cue_rt_mili > 200) %>%
  group_by(participant) %>%
  mutate(z_cue_rt = (cue_rt - mean(cue_rt))/sd(cue_rt),
         z_type_dur = (type_dur - mean(type_dur))/sd(type_dur)) %>%
  filter(abs(z_cue_rt) <= 2 & abs(z_type_dur) <= 2) %>%
  mutate(context = relevel(context,ref = "peer"))

z_pp <- responded_trials %>%
  filter(cue_rt_mili > 200) %>%
  group_by(participant) %>%
  mutate(z_cue_rt = (cue_rt - mean(cue_rt))/sd(cue_rt),
         z_type_dur = (type_dur - mean(type_dur))/sd(type_dur))

ggplot(z_pp, aes(x = z_cue_rt,fill =context))+
  geom_histogram(aes(alpha = 0.5))+
  facet_grid(condition~context)


glmer_fit <- glmer(
  cue_rt_mili ~ context * condition   + (context + condition | cue) + (condition | participant),
  data = word_assoc_filt,
  family = inverse.gaussian("identity")
)

summary(glmer_fit)

em_condition <- emmeans(glmer_fit,~context|condition)
pairs(em_condition)
em_context <- emmeans(glmer_fit,~condition|context)
pairs(em_context)

glmer_plot_main <-  word_assoc_filt %>%
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


ggplot(word_assoc_filt %>%
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

ez_split <- split(ez_long,ez_long$measure)
library(ez)

map(ez_split, function(x){
  m <- ezANOVA(dv = value,
          wid = cue,
          within = condition,
          between = context,
          data = x)
  contr_context <- emmeans_test(x %>% ungroup(), formula = value ~ context)
  contr_condition <-emmeans_test(x %>% ungroup(), formula = value ~ condition)
  
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

  return(list(m,contr_context, contr_condition,p))
  
})


