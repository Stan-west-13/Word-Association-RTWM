library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(ez)
library(lme4)

z <- function(x){
  return((x - mean(x,na.rm = T))/sd(x,na.rm = T))
}


## Load data 
d <- read_rds("data/TTA2_response_mapped_meta-2026-02-26.rds")

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
         cd_z = z(Lg10CD))
write_rds(d_filt, file = paste0("data/TTA2_meta_response_filtered-",Sys.Date(),".rds"))
write.csv(d_filt, "data/Julia_df_meta.csv")

## Long-formatted psycholing for splitting

d_long_filt_normalized <- d_filt %>%
  select(participant,
         cue,
         context,
         condition,
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
mods <- map(d_split, function(y){
  map(y, function(x){
    ## random intercepts for participants and cue
    m_lmer <- lmer(value ~ condition * context + (1|cue) + (1|participant), data = x ) 
    print(paste("############## Model output for ", unique(x$measure),"########################"))
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
    
    return(list(summary(m_lmer), p,plot(g)))
  })
})



