library(readxl)
library(tidyverse)
library(surveillance)
library(googlesheets4)
library(googledrive)
library(patchwork)
library(sf)
library(gt)
library(DT)
library(htmltools)
library(rmapshaper)
library(purrr)

## Load data GG Drive
load_data <- function(url, sheet) {
  gs4_deauth()
  df <- read_sheet(url, sheet = sheet)
  return(df)
}

# 1. Hàm lấy danh sách năm tham chiếu (CỐ ĐỊNH)
get_fixed_ref_years <- function(disease_name) {
  if (disease_name == "Sốt xuất huyết") {
    return(c(2016, 2017, 2020, 2023, 2024))
  } else {
    return(c(2017, 2019, 2020, 2022, 2024))
  }
}

# 2. HÀM TÍNH TOÁN CORE (Đã bỏ map_df, dùng bind_rows thuần)
calculate_all_thresholds <- function(df, ref_years, target_year) {
  
  # --- A. CHUẨN BỊ DỮ LIỆU ---
  
  # Lấy dữ liệu 5 năm Ref và 1 năm Target
  df_ref <- df %>% filter(year %in% ref_years)
  df_target <- df %>% filter(year == target_year)
  
  # Tìm tuần lớn nhất hiện có của năm Target (để ngắt biểu đồ sau này)
  max_week_current <- max(df_target$week, na.rm = TRUE)
  
  df_final_input <- bind_rows(df_ref, df_target) %>%
    arrange(year, week)
  
  observed_cases <- df_final_input$cases
  
  stsObj <- sts(observed = observed_cases, start = c(2020, 1), frequency = 52)
  disProgObj <- sts2disProg(stsObj)
  
  control_range <- 261:(260 + max_week_current)
  
  # 1. Farrington
  ctrl_far <- list(range = control_range, b = 5, w = 1, reweight = TRUE, verbose = FALSE)
  res_far <- algo.farrington(disProgObj, control = ctrl_far)
  far_vals <- res_far$upperbound
  
  # 2. CUSUM
  ctrl_cus <- list(range = control_range, k = 1.04, h = 2.26, trans = "rossi")
  res_cus <- algo.cusum(disProgObj, control = ctrl_cus)
  cus_vals <- res_cus$upperbound
  
  grand_median <- median(df_ref$cases, na.rm = TRUE)
  
  stats_ref <- df_ref %>%
    group_by(week) %>%
    summarise(
      Seasonal = grand_median,
      mean_val = mean(cases, na.rm = TRUE),
      sd_val = sd(cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(CDC = mean_val + 2 * sd_val) %>%
    mutate(CDC = pmax(0, CDC)) %>%
    select(week, Seasonal, CDC)
  
  # --- E. TỔNG HỢP KẾT QUẢ ---
  # Tạo khung kết quả cho năm Target
  result_df <- data.frame(
    Year = target_year, 
    Week = 1:52,
    Farrington = NA, 
    Cusum = NA
  )
  
  # Điền giá trị thuật toán vào các tuần đã tính (1 -> max_week)
  result_df$Farrington[1:max_week_current] <- far_vals
  result_df$Cusum[1:max_week_current] <- cus_vals
  
  # Ghép Seasonal và CDC
  result_df <- result_df %>%
    left_join(stats_ref, by = c("Week" = "week"))
  
  # Cắt bỏ dữ liệu tương lai (để biểu đồ ngắt quãng)
  result_df$Seasonal[result_df$Week > max_week_current] <- NA
  result_df$CDC[result_df$Week > max_week_current] <- NA
  
  return(result_df)
}

# 3. HÀM WRAPPER (Giữ nguyên)
process_unit_multi_years <- function(df, unit_name, disease_name, target_years_list) {
  
  fixed_refs <- get_fixed_ref_years(disease_name)
  
  final_res <- list()
  
  for (yr in target_years_list) {
    res <- calculate_all_thresholds(df, fixed_refs, target_year = yr)
    
    if (!is.null(res)) {
      res$WardId <- unit_name
      res$Disease <- disease_name
      final_res[[as.character(yr)]] <- res
    }
  }
  
  return(bind_rows(final_res))
}
