library(tidyverse)
library(googlesheets4)
library(googledrive)
library(jsonlite)

## Load data
load_data <- function(url, sheet) {
  gs4_deauth()
  df <- read_sheet(url, sheet = sheet)
  return(df)
}

## Tranmission
calc_transmission <- function(df, disease_name) {
  
  hist_years <- as.character(get_fixed_ref_years(disease_name))
  
  df_hist <- df %>%
    select(all_of(hist_years)) %>% 
    pivot_longer(cols = everything(), names_to = "Year", values_to = "Cases") %>%
    mutate(Cases = replace_na(Cases, 0))
  
  seasonal_median <- median(df_hist$Cases)
  
  stats_peak <- df_hist %>%
    group_by(Year) %>%
    summarise(Peak = max(Cases), .groups = "drop") %>%
    summarise(Mean_Peak = mean(Peak), SD_Peak = sd(Peak))
  
  list(
    Seasonal = seasonal_median,
    Moderate = stats_peak$Mean_Peak + (stats_peak$SD_Peak * 0.53),
    High     = stats_peak$Mean_Peak + (stats_peak$SD_Peak * 1.65),
    Extra    = stats_peak$Mean_Peak + (stats_peak$SD_Peak * 2.24)
  )
}

circular_shift <- function(x, shift) {
  n <- length(x)
  if (shift == 0) return(x)
  if (shift > 0) return(c(tail(x, shift), head(x, n - shift)))
  return(c(tail(x, n + shift), head(x, -shift)))
}

## Severity
calc_severity <- function(df_inpatient, disease_name, current_year_col) {
  
  hist_years <- as.character(get_fixed_ref_years(disease_name))
  
  df_hist <- df_inpatient %>% 
    select(Week, all_of(hist_years)) %>% 
    mutate(across(all_of(hist_years), ~replace_na(., 0)))
  
  peak_info <- df_hist %>%
    pivot_longer(cols = all_of(hist_years), names_to = "Year", values_to = "Cases") %>%
    group_by(Year) %>% slice_max(Cases, n = 1, with_ties = FALSE) %>% ungroup()
  
  ref_peak <- peak_info %>% slice_max(Cases, n = 1, with_ties = FALSE)
  target_week <- ref_peak$Week
  
  df_aligned <- df_hist %>% select(Week)
  for (year in hist_years) {
    curr_peak <- peak_info$Week[peak_info$Year == year]
    df_aligned[[year]] <- circular_shift(df_hist[[year]], target_week - curr_peak)
  }
  
  df_final <- df_aligned %>%
    rowwise() %>%
    mutate(
      Mean = mean(c_across(all_of(hist_years))),
      SD = sd(c_across(all_of(hist_years))),
      Limit_Low = Mean,
      Limit_Mod = Mean + SD,
      Limit_High = Mean + 3*SD
    ) %>% ungroup()
  
  df_curr_inpatient <- df_inpatient %>% 
    select(Week, Inpatient = all_of(current_year_col)) %>% 
    replace_na(list(Inpatient = 0))
  
  df_final <- df_final %>% 
    left_join(df_curr_inpatient, by = "Week")
  
  return(df_final)
}