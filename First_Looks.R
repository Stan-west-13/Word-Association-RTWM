library(ggplot2)
library(dplyr)
library(emmeans)
## Load data
d <- readRDS("data/TTA_metadata.rds")

## Filter out non-responded trials
responded_trials <- d %>%
  filter(!is.na(response))


## Accuracy and RT for spatial WM task
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

pairs(simp_effect_by_condition)
pairs(simp_effect_by_context)

## Interaction plot

plot_int <- responded_trials %>%
  group_by(context,condition) %>%
  summarize(mean = mean(accuracy)) %>%
  pivot_wider(names_from = context,
              values_from = mean) %>%
  mutate(child_creative = child-creative,
         child_peer = child-peer,
         child_short = child-short,
         creative_peer = creative-peer,
         creative_short = creative-short,
         peer_short = peer - short) %>%
  pivot_longer(cols = 2:5,
               names_to = "context",
               values_to = "value") %>%
  pivot_longer(cols = 2:7,
               names_to = "contrast",
               values_to = "value_contr")

ggplot(responded_trials, aes(x = condition, y = accuracy, color = context))+
  stat_summary(fun = "mean", geom = "point")+
  geom_line(aes(group=context))

## RT

responded_trials_rt <- responded_trials %>%
  group_by(participant,condition) %>%
  mutate(mean_rt_pp_cond = mean(square_rt_mili)) %>%
  group_by(condition) %>%
  mutate(z_pp_cond = (mean_rt_pp_cond - mean(square_rt_mili))/sd(square_rt_mili))


ggplot(d_plot, aes(x = condition, y = mean_rt, fill = trial_type))+
  geom_col(position ="dodge")+
  geom_errorbar(aes(ymin  = mean_rt -se_rt, 
                    ymax = mean_rt+se_rt),
                width = 0.2, 
                size = 0.8,
                position = position_dodge(0.9))


ggplot(responded_trials %>%
         filter(square_rt_mili >200 & square_rt_mili < quantile(square_rt_mili, 0.75) ), aes(x = square_rt_mili, fill = condition))+
  geom_histogram()+
  facet_grid(context~condition)



t.test(d$square_rt[d$condition == "load"],d$square_rt[d$condition == "no_load"] )
