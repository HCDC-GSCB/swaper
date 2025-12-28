source("functions.R")

# Dengue Fever
sxh_thr <- load_data("https://docs.google.com/spreadsheets/d/1tkoFRYLPNrojiAFzdbpT2aZGkIjuaIn7EcDSRJcPPHY",
                     sheet = "SXH_3KV")
## Farrington
out_f_sxh <- run_algo(sxh_thr, 
                      ref_years = c(2016, 2017, 2018, 2020, 2023, 2025), 
                      method = "farrington")
## CUSUM
out_c_sxh <- run_algo(sxh_thr, 
                      ref_years = c(2016, 2017, 2018, 2020, 2023, 2025), 
                      method = "cusum")
## Seasonal
seasonal_sxh <- remake(sxh_thr, c(2016, 2017, 2018, 2020, 2023))

## Plotly
sxh_interactive <- plot_algo_plotly(
  df_cases = out_f_sxh$df,       
  res_far = out_f_sxh$result,    
  res_cusum = out_c_sxh$result,  
  seasonal_df = seasonal_sxh,   
  year_target = 2025
)

sxh_interactive


# Hand-Foot-Mouth 
tcm_thr <- load_data("https://docs.google.com/spreadsheets/d/1ouVcS4B-sU07j4BT2VjHhxY13f2JUlqSeCY541vH6vA",
                     sheet = "TCM_3KV")
## Farrington
out_f_tcm <- run_algo(tcm_thr, 
                      ref_years = c(2017, 2019, 2020, 2022, 2024, 2025), 
                      method = "farrington")
## CUSUM
out_c_tcm <- run_algo(tcm_thr, 
                      ref_years = c(2017, 2019, 2020, 2022, 2024, 2025), 
                      method = "cusum")
## Seasonal
seasonal_tcm <- remake(tcm_thr, c(2017, 2019, 2020, 2022, 2024))

## Plotly
tcm_interactive <- plot_algo_plotly(
  df_cases = out_f_tcm$df,       
  res_far = out_f_tcm$result,    
  res_cusum = out_c_tcm$result,  
  seasonal_df = seasonal_tcm,   
  year_target = 2025
)

tcm_interactive

# Information Card for Dengue Fever (Right of trends)
cur_w <- 51

sxh_thr2025 <- sxh_thr %>%
  filter(year==2025) %>% 
  mutate(week = ifelse(week==53,52,week))

val_sxh_sea <- seasonal_sxh$seasonal 
val_sxh_far <- out_f_sxh$result$upperbound[cur_w]
val_sxh_cus <- out_c_sxh$result$upperbound[cur_w]
val_sxh_cdc <- out_f_sxh$df$cdc[out_f_sxh$df$year == 2025 & out_f_sxh$df$week == cur_w]
sxh_cur_cases <- sxh_thr2025 %>% 
  filter(week == cur_w) %>% 
  pull(cases)

card_sxh <- create_info_card(
  current_cases = sxh_cur_cases,
  current_week = cur_w,
  val_seasonal = val_sxh_sea,
  val_cusum = val_sxh_cus,
  val_farrington = val_sxh_far,
  val_cdc = val_sxh_cdc
)

# Information Card for HFMD (Right of trends)
cur_w <- 51

tcm_thr2025 <- tcm_thr %>%
  filter(year==2025) %>% 
  mutate(week = ifelse(week==53,52,week))

val_tcm_sea <- seasonal_tcm$seasonal
val_tcm_far <- out_f_tcm$result$upperbound[cur_w]
val_tcm_cus <- out_c_tcm$result$upperbound[cur_w]
val_tcm_cdc <- out_f_tcm$df$cdc[out_f_tcm$df$year == 2025 & out_f_tcm$df$week == cur_w]
tcm_cur_cases <- tcm_thr2025 %>% 
  filter(week == cur_w) %>% 
  pull(cases)

card_tcm <- create_info_card(
  current_cases = tcm_cur_cases,
  current_week = cur_w,
  val_seasonal = val_tcm_sea,
  val_cusum = val_tcm_cus,
  val_farrington = val_tcm_far,
  val_cdc = val_tcm_cdc
)
