library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(emmeans)
library(rstatix)
source("R/Load_Helpers.R")
z <- function(x){
  return((x - mean(x,na.rm = T))/sd(x,na.rm = T))
}


## Load data 
d <- load_most_recent_by_mtime("data","TTA2_meta_response_filtered")



################ Accuracy and RT for spatial WM task ###########################
d_plot <- d %>%
  group_by(condition,trial_type) %>%
  summarize(mean_accuracy = mean(accuracy),
            sd_accuracy = sd(accuracy),
            se_accuracy = sd_accuracy/sqrt(n()),
            mean_rt = mean(square_rt_mili),
            sd_rt = sd(square_rt_mili),
            se_rt = sd_rt/sqrt(n()))
d_plot <- d %>%
  select(condition,trial_type,accuracy,square_rt_mili) %>%
  group_by(condition,trial_type) %>%
  get_summary_stats(type = "mean_ci")

## Plot Accuracy
ggplot(d_plot, aes(x = condition, y = mean, fill = trial_type))+
  geom_col(position ="dodge")+
  geom_errorbar(aes(ymin  = mean -ci, 
                    ymax = mean+ci),
                width = 0.2, 
                size = 0.8,
                position = position_dodge(0.9))+
  theme_classic(base_size = 18)+
  facet_wrap(~variable,scales = "free_y",
             labeller = as_labeller(c("accuracy" = "mean accuracy", "square_rt_mili" = "response time (ms)")),
             strip.position = "left")+
  theme(legend.title = element_blank(),
        legend.position = c(0.1,0.9),
        axis.title.y  = element_blank(),
        rect = element_blank(),strip.placement = "outside")+
  scale_x_discrete(labels = c("load", "no load"))

ggplot(responded_trials, aes(x = accuracy, fill = condition))+
  geom_histogram()+
  facet_grid(context~condition)


t.test(d$accuracy[d$condition == "load"],d$accuracy[d$condition == "no_load"] )
d %>% 
  group_by(condition) %>%
  summarize(m = mean(accuracy),
            sd = sd(accuracy))

## ANOVA of accuracy 
m <-lm(accuracy ~  condition * trial_type, data = d)
summary(m)
simp_effect_by_condition <- emmeans(m, ~trial_type|condition)
simp_effect_by_context <- emmeans(m, ~condition|trial_type)

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


## Response time to cue

## Remove participant-wise z-scores greater than 2
responded_trials_rt <- d %>%
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
                position = position_dodge(0.9))+
  theme_classic(base_size = 18)

## Histogram of response times 
ggplot(responded_trials_rt, aes(x = square_rt_mili, fill = condition))+
  geom_histogram()+
  facet_grid(context~condition)

responded_trials_rt %>%
  group_by(condition,trial_type) %>%
  summarize(m = mean(square_rt_mili),
            sd = sd(square_rt_mili)) %>%
  as.data.frame()

t.test(responded_trials_rt$square_rt[responded_trials_rt$condition == "load"],
       responded_trials_rt$square_rt[responded_trials_rt$condition == "no_load"] )


## ANOVA of RT 
m_rt <-lm(square_rt ~  condition * trial_type, data = responded_trials_rt)
summary(m_rt)
simp_effect_by_condition_rt <- emmeans(m_rt, ~trial_type|condition)
simp_effect_by_context_rt <- emmeans(m_rt, ~condition|trial_type)

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

