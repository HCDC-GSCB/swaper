source("functions_threshold.R")

# 1. Process data

###### Dengue Fever #####
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


##### Hand-Foot-Mouth #####
tcm_thr <- load_data("https://docs.google.com/spreadsheets/d/1ouVcS4B-sU07j4BT2VjHhxY13f2JUlqSeCY541vH6vA",
                     sheet = "TCM_3KV")
out_f_tcm <- run_algo(tcm_thr, 
                      ref_years = c(2017, 2019, 2020, 2022, 2024, 2025), 
                      method = "farrington")

out_c_tcm <- run_algo(tcm_thr, 
                      ref_years = c(2017, 2019, 2020, 2022, 2024, 2025), 
                      method = "cusum")

seasonal_tcm <- remake(tcm_thr, c(2017, 2019, 2020, 2022, 2024))

tcm_interactive <- plot_algo_plotly(
  df_cases = out_f_tcm$df,       
  res_far = out_f_tcm$result,    
  res_cusum = out_c_tcm$result,  
  seasonal_df = seasonal_tcm,   
  year_target = 2025
)

# 2. Key hightlights

##### Dengue Fever #####
sxh_2025 <- out_f_sxh$df %>% filter(year==2025)
n <- 50 ## <----- Sửa n mới
a_sxh <- sxh_2025[n,"cases"] %>% pull() 
b_sxh <- sxh_2025[n-1,"cases"] %>% pull() 
c_sxh <- sxh_2025 %>% 
  filter(week>(n-5) & week<n) %>% 
  summarise(n = mean(cases)) %>% 
  pull() 

f_sxh <- out_f_sxh$result$upperbound[[n]] 
g_sxh <- sxh_2025[n, "cdc"] %>% pull() 
h_sxh <- remake(sxh_thr, c(2016, 2017, 2018, 2020, 2023))$seasonal
k_sxh <- out_c_sxh$result$upperbound[[n]] 

muc_do_lay_truyen_sxh <- "Trung bình"
muc_do_nang_sxh <- "Trung bình"

##### Hand-Foot-Mouth #####
tcm_2025 <- out_f_tcm$df %>% filter(year==2025)
n <- 50 ## <----- Sửa n mới
a_tcm <- tcm_2025[n,"cases"] %>% pull() 
b_tcm <- tcm_2025[n-1,"cases"] %>% pull() 
c_tcm <- tcm_2025 %>% 
  filter(week>(n-5) & week<n) %>% 
  summarise(n = mean(cases)) %>% 
  pull()

f_tcm <- out_f_tcm$result$upperbound[[n]] 
g_tcm <- tcm_2025[n, "cdc"] %>% pull() 
h_tcm <- remake(tcm_thr, c(2016, 2017, 2018, 2020, 2023))$seasonal
k_tcm <- out_c_tcm$result$upperbound[[n]]

muc_do_lay_truyen_tcm <- "Thấp"
muc_do_nang_tcm <- "Cao"

df_combined <- tibble(
  ChiTieu = c(
    "<b>Số ca trong tuần</b>",
    "&nbsp;&nbsp;- So với tuần trước",
    "&nbsp;&nbsp;- So với TB 4 tuần trước",
    "<b>So với ngưỡng cảnh báo:</b>",
    "&nbsp;&nbsp;- So với ngưỡng mùa",
    "&nbsp;&nbsp;- So với ngưỡng CUSUM",
    "&nbsp;&nbsp;- So với ngưỡng Farrington",
    "&nbsp;&nbsp;- So với ngưỡng CDC (Mean+2SD)",
    "<b>Đánh giá nguy cơ:</b>",
    "&nbsp;&nbsp;- Mức độ lây truyền",
    "&nbsp;&nbsp;- Mức độ nặng"
  ),
  SXH = c(
    format(a_sxh, big.mark = "."), 
    format_metric(a_sxh, b_sxh),
    format_metric(a_sxh, c_sxh),
    "", 
    format_metric(a_sxh, h_sxh),
    format_metric(a_sxh, k_sxh), 
    format_metric(a_sxh, f_sxh),
    format_metric(a_sxh, g_sxh),
    "", 
    format_badge(muc_do_lay_truyen_sxh),
    format_badge(muc_do_nang_sxh)
  ),
  TCM = c(
    format(a_tcm, big.mark = "."), 
    format_metric(a_tcm, b_tcm),
    format_metric(a_tcm, c_tcm),
    "", 
    format_metric(a_tcm, h_tcm),
    format_metric(a_tcm, k_tcm), 
    format_metric(a_tcm, f_tcm),
    format_metric(a_tcm, g_tcm),
    "", 
    format_badge(muc_do_lay_truyen_tcm),
    format_badge(muc_do_nang_tcm)
  )
)

# 3. Phường/Xã
df_sxh_clean <- readRDS("data/df_sxh_clean.rds")
df_tcm_clean <- readRDS("data/df_tcm_clean.rds")

df_sxh_clean <- df_sxh_clean %>% 
  mutate(
    dif_farrington = round(((SoCa - Nguong_Farrington)*100/Nguong_Farrington), 1),
    dif_cusum = round(((SoCa - Nguong_CUSUM)*100/Nguong_CUSUM), 1)
  )

df_tcm_clean <- df_tcm_clean %>% 
  mutate(
    dif_farrington = round(((SoCa - Nguong_Farrington)*100/Nguong_Farrington), 1),
    dif_cusum = round(((SoCa - Nguong_CUSUM)*100/Nguong_CUSUM), 1)
  )

df_kv <- load_data("https://docs.google.com/spreadsheets/d/1RipF4OKVerlJlNOVA4dLxZtsU4tdPB_VuToqp-Dd1KM/edit?usp=sharing",
                   sheet = "Sheet1") %>%
  mutate(phuong_clean = standardize_name_func(Phuong)) %>%
  select(phuong_clean, KV, STT, Phuong_Display = Phuong) %>%
  distinct()

map_sxh <- plot_px_map(
  shp_path = "TPHCM_XA_2025_JUL_AP",
  px_clean = df_sxh_clean,
  df_kv = df_kv,
  TUAN_BAO_CAO = 52
)

map_tcm <- plot_px_map(
  shp_path = "TPHCM_XA_2025_JUL_AP",
  px_clean = df_tcm_clean,
  df_kv = df_kv,
  TUAN_BAO_CAO = 52
)


