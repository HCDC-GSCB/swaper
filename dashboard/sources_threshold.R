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
