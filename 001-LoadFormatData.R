library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)
library(ggplot2)
library(progressr)

## Choose datapath
datapath <- choose.dir()


unzip("data_WMRT.zip")
datapath <- list.files("data_WMRT/", full.names = TRUE)

datapath <- datapath[-73]

# Read in cue
words_meta <- read.csv("data/stim_64_NNVB.csv")

# Get participant counterbalances
temp <- read_xlsx(paste0(datapath,"/TTAWM Counterbalance tracker.xlsx")) %>%
  select(PPID, Date, Condition, Counterbalance)%>%
  drop_na()

pptracker <- temp %>%
  mutate(participant = str_remove(PPID, "TTA_"), .before = Counterbalance) %>% 
  mutate(participant = str_remove(participant, "^0+")) %>% 
  mutate(counterbalance = as.factor(Counterbalance)) %>% 
  select(-PPID,-Counterbalance)


files <- list.files(paste0(datapath,"/data_WMRT/"), full.names = T)[-73]

# Make metadata df
x <- map_dfr(files, function(x){
    exp_load <- read.csv(list.files(x,pattern = "TTA_[0-9][0-9][0-9]_LOAD_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]trialstest.csv",full.names = TRUE))[1:32,] 
    exp_noload <- read.csv(list.files(x,pattern = "TTA_[0-9][0-9][0-9]_NO_LOAD_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]trialstest.csv",full.names = TRUE))[1:32,] 
    condition_df <- read.csv(list.files(x,pattern = "TTA_[0-9][0-9][0-9]_LOAD_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].csv",full.names = TRUE))[c(1,10),] %>%
      mutate(pp = as.character(pp))
    d_all <- rbind(exp_load,exp_noload) %>%
      left_join(select(condition_df,pp,context = condition ),by="pp") %>%
      mutate(participant = as.factor(pp), .after = pp,
             context = as.factor(context),
             response = as.character(tolower(trimws(str_replace_all(input_textbox.text_raw,"'","")))),
             response = ifelse(gsub("\\n","",response, fixed = TRUE) == "",NA,gsub("\\n","",response, fixed = TRUE)))%>%
      select(-pp) %>%
      mutate(sq_resp = factor(str_replace(str_replace(yesno_resp.keys_raw,"'",""),"'",""), levels = c("f","j"), labels = c("mismatch","match"))) %>%
      group_by(participant,condition) %>%
      mutate(correct = ifelse(TT == sq_resp,TRUE,FALSE),
             accuracy = sum(correct)/n()) %>%
      select(participant, 
             context,
             cue, 
             response, 
             starts_with("sq"), 
             condition, 
             trial_type = TT, 
             type_dur = input_key.rt_raw, 
             cue_rt = key_space.rt_raw, 
             square_rt = yesno_resp.rt_raw,
             sq_resp,
             accuracy,
             correct) %>%
      mutate(condition = as.factor(condition),
             trial_type = as.factor(trial_type),
             square_rt_mili = square_rt * 1000,
             cue_rt_mili = cue_rt * 1000,
             type_dur_mili = type_dur * 1000) %>%
      left_join(select(words_meta,cue,strength_strat,type), by = "cue") %>% 
      group_by(participant) %>%
      ungroup() %>% 
    return(d_all)
})


x_count <- x %>%
  left_join(select(pptracker,participant,counterbalance)) %>%
  mutate(block = ifelse(counterbalance == 1 & condition == "load", 1,
                    ifelse(counterbalance == 2 & condition == "no_load", 1,2)),
         participant = as.factor(participant)) %>%
  arrange(participant, block) %>%
  group_by(participant) %>%
  mutate(trialNum = seq.int(1,64,1))

# Save out metadata df
saveRDS(x_count, paste0("data/TTA_metadata_",Sys.Date(),".rds"))
