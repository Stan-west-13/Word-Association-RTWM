library(dplyr)
source("R/Load_Helpers.R")
datapath <- choose_directory()

d <- readxl::read_xlsx(paste0(datapath,"/Demographics.xlsx"))

d %>%
  group_by(Condition) %>%
  summarize(m_age = mean(age, na.rm = T),
            sd_age = sd(age, na.rm = T),
            prop_toddler = sum(toddler_interaction == "yes",na.rm = T)/n())
