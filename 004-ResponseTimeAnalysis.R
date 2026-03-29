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

## Histograms
ggplot(d, aes(x = z_cue_rt_mili,fill =context))+
  geom_histogram()+
  facet_grid(condition~context)
## Barplot of Means
ggplot(d, aes(x = condition, y = cue_rt_mili))+
  stat_summary(fun = "mean", geom="col")


## Looking at counterbalances: looks like people are faster in block 2 
## regardless of condition - maybe practice effects/wanting to be done. 
ggplot(d %>%
         filter(cue_rt_mili <= 5000), aes(x = context, y = cue_rt_mili, fill = counterbalance))+
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

## Difference between Conditions
ggplot(d_diff, aes(x = context, y = diff_cond, fill = block))+
  stat_summary(fun = "identity", geom = "col",position = "dodge")+
  theme_bw()+
  annotate("text",label = "FASTER IN LOAD",x=2.25,y = 125,size = 8)+
  annotate("text",label = "FASTER IN NO LOAD",x=2.25,y = -125,size = 8)
## Difference between Blocks. Participants are faster in block 2 across the 
## board.
ggplot(d_diff, aes(x = context, y = diff_block, fill = condition))+
  stat_summary(fun = "identity", geom = "col",position = "dodge")+
  theme_bw()+
  annotate("text",label = "FASTER IN BLOCK 1",x=2.25,y = 125,size = 8)+
  annotate("text",label = "FASTER IN BLOCK 2",x=2.25,y = -300,size = 8)


## Just looking at block 1
ggplot(d , aes(x = context, y = cue_rt_mili, fill = condition))+
  stat_summary(fun = "mean", geom = "col", position = "dodge")+
  geom_text(stat = "summary",fun = "mean",vjust = 12, aes(label = round(after_stat(y),2)),
            position = position_dodge(0.9),size = 8)+
  theme_bw(base_size = 24)+
  scale_fill_discrete(labels = c("Load", "No Load"))+
  labs(y = "response time (ms)")
ggsave("Figures/response_time_plot.png")

glmer_fit <- glmer(
  cue_rt_mili ~ context * condition + (1 | cue) + (1 | participant),
  data = d %>%
    filter(cue_rt_mili <= 5000) %>%
    mutate(context = relevel(context,ref="child")),
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

## Plot max model marginals
mm_rt <- read.csv('data/marginal_means_rt.csv') %>%
  mutate(context = factor(context,levels = c("peer",'child','short','creative')))

ggplot(aes(x = context, y = mean, fill = condition), data = mm_rt)+
  geom_col(position =position_dodge(0.9))+
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(0.9),
                width = 0.2)+
  geom_text(stat = "identity",vjust = 7, aes(label = round(after_stat(y),2)),
            position = position_dodge(0.9),size = 6)+
  theme_bw(base_size = 24)+
  theme(legend.position = c(0.6, 0.9),
        legend.background = element_rect(fill = alpha("white", 0.2)),
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 24),
        legend.title = element_blank(),
        legend.key.size = unit(0.3,"cm"),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "#FCFBFF"))+
  scale_fill_discrete(labels = c("load","no load"))+
  labs(y = "model marginal means (95% CI)")

ggsave(filename = 'Figures/rt_plot_condition_context.png' ,width = 12, height = 6, dpi = 600, units = "in", device='png')


ggplot(glmer_plot_main, aes(x = context, y = mean, fill = condition))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9),
                width = 0.2)+
  geom_text(stat = "identity", aes(label = after_stat(y)), vjust = 45,
            position = position_dodge(0.9))

glmer_plot_type <- d %>%
  group_by(condition,type,strength_strat,context) %>%
  get_summary_stats(cue_rt_mili, type = c('mean_se'))

ggplot(glmer_plot_type, aes(x = type, y = mean, fill = strength_strat))+
  geom_col(position = "dodge")+
  facet_grid(context~condition)

ggplot(d %>%
         group_by(participant,context,condition) %>%
         mutate(avg_rt = mean(cue_rt_mili),
                avg_acc = mean(accuracy)), aes(x = avg_rt, y = avg_acc, color = context))+
  geom_point()+
  geom_smooth(method = "loess")+
  facet_grid(~condition, scales = "free")

