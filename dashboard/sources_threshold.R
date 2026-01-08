source("functions_threshold.R")

##############################
######### Thành phố ##########
##############################
sxh_thr <- load_data("https://docs.google.com/spreadsheets/d/1tkoFRYLPNrojiAFzdbpT2aZGkIjuaIn7EcDSRJcPPHY",
                     sheet = "SXH_3KV")

out_f_sxh <- run_algo(sxh_thr, 
                      ref_years = c(2016, 2017, 2018, 2020, 2023, 2025), 
                      method = "farrington")

out_c_sxh <- run_algo(sxh_thr, 
                      ref_years = c(2016, 2017, 2018, 2020, 2023, 2025), 
                      method = "cusum")

seasonal_sxh <- remake(sxh_thr, c(2016, 2017, 2018, 2020, 2023))


sxh_interactive <- plot_algo_plotly(
  df_cases = out_f_sxh$df,       
  res_far = out_f_sxh$result,    
  res_cusum = out_c_sxh$result,  
  seasonal_df = seasonal_sxh,   
  year_target = 2025
)

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

cur_w <- 52
# Information Card for Dengue Fever (Right of trends)
sxh_thr2025 <- sxh_thr %>%
  filter(year == 2025) %>% 
  mutate(week = ifelse(week == 53, 52, week)) %>%
  group_by(year, week) %>% 
  summarise(
    cases = sum(cases, na.rm = TRUE), 
    across(contains(c("sea", "cus", "far", "cdc")), ~max(.x, na.rm = TRUE)), 
    .groups = "drop"
  )

val_sxh_sea <- seasonal_sxh$seasonal 
val_sxh_far <- out_f_sxh$result$upperbound[cur_w]
val_sxh_cus <- out_c_sxh$result$upperbound[cur_w]
val_sxh_cdc <- out_f_sxh$df$cdc[out_f_sxh$df$year == 2025 & out_f_sxh$df$week == cur_w]
sxh_cur_cases <- sxh_thr2025 %>% 
  filter(week == cur_w) %>% 
  pull(cases)
sxh_last_cases <- sxh_thr2025 %>% 
  filter(week == cur_w-1) %>% 
  pull(cases)
sxh_last4m_cases <- sxh_thr2025 %>% 
  filter(week == cur_w-4) %>% 
  pull(cases)
muc_do_lay_truyen_sxh <- "Cao"
muc_do_nang_sxh <- "Trung bình"

# Information Card for HFMD
tcm_thr2025 <- tcm_thr %>%
  filter(year == 2025) %>% 
  mutate(week = ifelse(week == 53, 52, week)) %>%
  group_by(year, week) %>% 
  summarise(
    cases = sum(cases, na.rm = TRUE), 
    across(contains(c("sea", "cus", "far", "cdc")), ~max(.x, na.rm = TRUE)), 
    .groups = "drop"
  )

val_tcm_sea <- seasonal_tcm$seasonal
val_tcm_far <- out_f_tcm$result$upperbound[cur_w]
val_tcm_cus <- out_c_tcm$result$upperbound[cur_w]
val_tcm_cdc <- out_f_tcm$df$cdc[out_f_tcm$df$year == 2025 & out_f_tcm$df$week == cur_w]
tcm_cur_cases <- tcm_thr2025 %>% 
  filter(week == cur_w) %>% 
  pull(cases)
tcm_last_cases <- tcm_thr2025 %>% 
  filter(week == cur_w-1) %>% 
  pull(cases)
tcm_last4m_cases <- tcm_thr2025 %>% 
  filter(week == cur_w-4) %>% 
  pull(cases)
muc_do_lay_truyen_tcm <- "Cao"
muc_do_nang_tcm <- "Trung bình"

##### Table tổng
col_chitieu <- c(
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
)

df_sxh <- tibble(
  ChiTieu = col_chitieu,
  KetQua = c(
    format(sxh_cur_cases, big.mark = "."), 
    format_metric(sxh_cur_cases, sxh_last_cases),
    format_metric(sxh_cur_cases, sxh_last4m_cases),
    "", 
    format_metric(sxh_cur_cases, val_sxh_sea),
    format_metric(sxh_cur_cases, val_sxh_cus), 
    format_metric(sxh_cur_cases, val_sxh_far),
    format_metric(sxh_cur_cases, val_sxh_cdc),
    "", 
    format_badge(muc_do_lay_truyen_sxh),
    format_badge(muc_do_nang_sxh)
  )
)

df_tcm <- tibble(
  ChiTieu = col_chitieu,
  KetQua = c(
    format(tcm_cur_cases, big.mark = "."), 
    format_metric(tcm_cur_cases, tcm_last_cases),
    format_metric(tcm_cur_cases, tcm_last4m_cases),
    "", 
    format_metric(tcm_cur_cases, val_tcm_sea),
    format_metric(tcm_cur_cases, val_tcm_cus), 
    format_metric(tcm_cur_cases, val_tcm_far),
    format_metric(tcm_cur_cases, val_tcm_cdc),
    "", 
    format_badge(muc_do_lay_truyen_tcm),
    format_badge(muc_do_nang_tcm)
  )
)

table_sxh <- create_beautiful_table(df_sxh, "Tình hình Sốt xuất huyết", header_color = "#0056b3")

table_tcm <- create_beautiful_table(df_tcm, "Tình hình Tay chân miệng", header_color = "#d35400")

#############################
######## Phường/Xã ##########
#############################

#Dengue
df_sxh_clean <- readRDS("data/df_sxh_clean.rds") #Update mỗi tuần
df_tcm_clean <- readRDS("data/df_tcm_clean.rds") #Update mỗi tuần

px_sxh <- render_epitable(df_sxh_clean, cur_w, "#0056b3")
px_tcm <- render_epitable(df_tcm_clean, cur_w, "#d35400")

##### Map 
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
