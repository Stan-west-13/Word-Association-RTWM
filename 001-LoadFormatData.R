library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
datapath <- list.files(path = "C:/Users/westa/OneDrive - Louisiana State University/WordAssociationRTWM/data_WMRT", full.names = TRUE)
datapath <- datapath[-73]

words_meta <- read.csv("data/stim_64_NNVB.csv")

x <- map_dfr(datapath, function(x){
    exp_load <- read.csv(list.files(x,pattern = "TTA_[0-9][0-9][0-9]_LOAD_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]trialstest.csv",full.names = TRUE))[1:32,] 
    exp_noload <- read.csv(list.files(x,pattern = "TTA_[0-9][0-9][0-9]_NO_LOAD_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]trialstest.csv",full.names = TRUE))[1:32,] 
    condition_df <- read.csv(list.files(x,pattern = "TTA_[0-9][0-9][0-9]_NO_LOAD_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].csv",full.names = TRUE))[c(1,10),] %>%
      mutate(pp = as.character(pp))
    d_all <- rbind(exp_load,exp_noload) %>%
      left_join(select(condition_df,pp,context = condition),by="pp") %>%
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
      left_join(select(words_meta,cue,strength_strat,type), by = "cue")
    return(d_all)
})





saveRDS(x, "data/TTA_metadata.rds")
