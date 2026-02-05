library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
datapath <- list.files(path = "C:/Users/Stan/OneDrive - Louisiana State University/WordAssociationRTWM/data_WMRT", full.names = TRUE)
datapath <- datapath[-73]



x <- map_dfr(datapath, function(x){
    exp_load <- read.csv(list.files(x,pattern = "TTA_[0-9][0-9][0-9]_LOAD_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]trialstest.csv",full.names = TRUE))[1:32,] 
    exp_noload <- read.csv(list.files(x,pattern = "TTA_[0-9][0-9][0-9]_NO_LOAD_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]trialstest.csv",full.names = TRUE))[1:32,]  
    d_all <- rbind(exp_load,exp_noload) %>%
      mutate(participant = as.factor(pp), .after = pp,
             response = as.character(tolower(trimws(str_replace_all(input_textbox.text_raw,"'","")))),
             response = ifelse(gsub("\\n","",response, fixed = TRUE) == "",NA,gsub("\\n","",response, fixed = TRUE)))%>%
      select(-pp) %>%
      mutate(sq_resp = factor(str_replace(str_replace(yesno_resp.keys_raw,"'",""),"'",""), levels = c("f","j"), labels = c("mismatch","match"))) %>%
      group_by(participant) %>%
      mutate(correct = ifelse(TT == sq_resp,TRUE,FALSE)) %>%
      select(participant, 
             cue, 
             response, 
             starts_with("sq"), 
             condition, 
             trial_type = TT, 
             type_dur = input_key.rt_raw, 
             cue_rt = key_space.rt_raw, 
             square_rt = yesno_resp.rt_raw,
             sq_resp,
             correct)
    return(d_all)
})

saveRDS(x, "data/TTA_metadata.rds")
