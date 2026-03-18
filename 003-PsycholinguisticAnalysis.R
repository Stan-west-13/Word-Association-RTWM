library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(ez)
library(purrr)
library(lme4)
library(lmerTest)
library(codingMatrices)
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
  mutate(context = relevel(context,ref = "peer")) %>% ## set "child" as the reference
  ungroup() %>%
  mutate(wf_z = z(Lg10WF),
         aoa_z = z(aoa),
         wl_z = z(nchar),
         cd_z = z(Lg10CD)) %>%
  mutate(context = factor(context, levels = c("peer","child","short","creative")))
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
    
    contrasts(x$context) <- code_diff(4)
    m_lmer_diff <- lmer(value ~ condition * context + (1|cue) + (1|participant), data = x ) 
    print(paste("############## Model output for ", unique(x$measure),name," DIFF ","########################"))
    print(summary(m_lmer_diff))
    
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
    
    return(list(model = m_lmer,model_helm = m_lmer_diff, p,plot(g)))
  })
})


d_plot <- d_filt %>%
  select(condition,context, c(aoa,nchar,Lg10CD,Lg10WF)) %>%
  pivot_longer(cols = c(aoa,nchar,Lg10CD,Lg10WF),
               names_to = "measure",
               values_to = "value") %>%
  ungroup()

## All_metrics plot

model <- lm(value ~ context *measure, data = d_plot %>% filter(!context == "creative"))

# Estimated marginal means and contrasts
em <- emmeans(model, ~ context | measure)
contr <- contrast(em, method = "pairwise")

# Convert to data frame for plotting
df <- as.data.frame(contr)

ggplot(df, aes(x = contrast, y = estimate, ymin = estimate - SE, ymax = estimate + SE)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Difference of Means", title = "Pairwise Contrasts by Measure")+
  facet_grid(~measure)+
  theme_bw(base_size = 24)+ 
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5))



### Plot with correct SE


cis <- map_dfr(split(d_plot,d_plot$measure), ~{
  rtrn <- .x %>%
    group_by(context,condition) %>%
    summarize(means = mean(value, na.rm = TRUE),
              SE = sd(value, na.rm = TRUE) / sqrt(n()),.groups = "keep") %>%
    ungroup() %>%
    mutate(`child - peer_diff_load` = means[context == "child" & condition == "load"] - means[context == "peer" & condition == "load"],
           `short - peer_diff_load` = means[context == "short" & condition == "load"] - means[context == "peer" & condition == "load"],
           `child - short_diff_load` = means[context == "child" & condition == "load"] - means[context == "short" & condition == "load"],
           `child - peer_CIU_load` = `child - peer_diff_load` + 1.96*SE,
           `short - peer_CIU_load` = `short - peer_diff_load`+ 1.96*SE,
           `child - short_CIU_load` = `child - short_diff_load`+ 1.96*SE,
           `child - peer_CIL_load` = `child - peer_diff_load` - 1.96*SE,
           `short - peer_CIL_load` = `short - peer_diff_load`- 1.96*SE,
           `child - short_CIL_load` = `child - short_diff_load`- 1.96*SE,
           `child - peer_diff_noload` = means[context == "child"& condition == "no_load"] - means[context == "peer"& condition == "no_load"],
           `short - peer_diff_noload` = means[context == "short"& condition == "no_load"] - means[context == "peer"& condition == "no_load"],
           `child - short_diff_noload` = means[context == "child"& condition == "no_load"] - means[context == "short"& condition == "no_load"],
           `child - peer_CIU_noload` = `child - peer_diff_noload` + 1.96*SE,
           `short - peer_CIU_noload` = `short - peer_diff_noload`+ 1.96*SE,
           `child - short_CIU_noload` = `child - short_diff_noload`+ 1.96*SE,
           `child - peer_CIL_noload` = `child - peer_diff_noload` - 1.96*SE,
           `short - peer_CIL_noload` = `short - peer_diff_noload`- 1.96*SE,
           `child - short_CIL_noload` = `child - short_diff_noload`- 1.96*SE,) %>%
    mutate(measure = unique(.x$measure))
  return(rtrn)
}) %>%
  select(-condition) %>%
  pivot_longer(cols = contains("-"),
               names_to = c("contrast","value","Condition"),
               values_to = c("x"),
               names_sep = "_") %>%
  pivot_wider(names_from = "value",
              values_from = "x") %>%
  mutate(contrast = factor(contrast, levels = c("child - peer", "short - peer", "child - short")),
         measure = factor(measure, levels = c("aoa", "nchar", "Lg10CD", "Lg10WF")))

ggplot(aes(x = contrast, y = diff, color = Condition), data = cis) +
  stat_summary(geom = "point", fun = "mean",size = 4)+
  geom_errorbar(aes(ymin = CIL, ymax = CIU,width = 0),size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~measure,
             nrow = 2,
             ncol = 2, 
             labeller = as_labeller(c("aoa" = "Age of Acquisition",
                                      "nchar" = "Word Length",
                                      "Lg10CD" = "Contextual Diversity",
                                      "Lg10WF" = "Frequency")))+
  scale_color_discrete(labels = c("Load", "No Load"))+
  theme_bw(base_size = 18)+
  labs( x = "Contrast")
ggsave("Figures/psychlong_contrasts.png", width = 12, height = 6)








