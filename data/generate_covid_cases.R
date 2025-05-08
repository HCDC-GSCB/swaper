# ===== THIS FILE IS MEANT FOR DOCUMENTING DATA GENERATION PROCESS ====== 
# install.packages("outbreaks")
# install.packages("tidyverse")
# Generate covid_cases data
library(tidyverse)
library(outbreaks)
covid_cases <- sarscov2_who_2019 %>% 
  select(date, matches("^cases_[[:alpha:]]{3}$")) %>% 
  mutate_at(
    # compute new cases per day from cumulative cases
    vars(matches("^cases_[[:alpha:]]{3}$")), 
    ~ c(0, diff(., differences = 1))
  )
saveRDS(file.path(getwd(), "day1", "data", "covid_cases.rds"))