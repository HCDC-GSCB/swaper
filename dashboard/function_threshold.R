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

calculate_all_thresholds <- function(df, ref_years, target_year) {
  
  # --- 1. KIỂM TRA DỮ LIỆU ĐẦU VÀO ---
  # Lấy dữ liệu tham chiếu và mục tiêu
  df_ref <- df %>% filter(year %in% ref_years)
  df_target <- df %>% filter(year == target_year)
  
  # [FIX LỖI QUAN TRỌNG]
  # Nếu không có dòng dữ liệu nào cho năm mục tiêu -> BỎ QUA LUÔN
  if (nrow(df_target) == 0) return(NULL)
  
  # Nếu có dòng nhưng toàn NA -> max trả về -Inf -> BỎ QUA LUÔN
  max_week_current <- suppressWarnings(max(df_target$week, na.rm = TRUE))
  if (is.infinite(max_week_current)) return(NULL)
  
  # --- 2. CHUẨN BỊ DỮ LIỆU ---
  df_final_input <- bind_rows(df_ref, df_target) %>%
    arrange(year, week)
  
  observed_cases <- df_final_input$cases
  
  # Nếu dữ liệu quá ngắn (ví dụ chỉ có vài tuần) -> Không đủ chạy thuật toán -> Bỏ qua
  if (length(observed_cases) < 52) return(NULL)
  
  # --- 3. CẤU HÌNH THUẬT TOÁN ---
  stsObj <- sts(observed = observed_cases, start = c(2020, 1), frequency = 52)
  disProgObj <- sts2disProg(stsObj)
  
  # Xác định vùng dự báo (Control Range)
  start_idx <- 261 # Bắt đầu từ năm thứ 6 (sau 5 năm ref)
  end_idx <- 260 + max_week_current
  
  # [FIX LỖI VECTOR] Kiểm tra index có hợp lệ không
  if (start_idx > length(disProgObj$observed)) return(NULL)
  end_idx <- min(end_idx, length(disProgObj$observed)) # Cắt nếu vượt quá
  
  control_range <- start_idx:end_idx
  
  # --- 4. CHẠY THUẬT TOÁN (BỌC TRYCATCH ĐỂ KHÔNG SẬP) ---
  
  # Farrington
  res_far <- tryCatch({
    algo.farrington(disProgObj, control = list(range = control_range, b = 5, w = 1, reweight = TRUE, verbose = FALSE))
  }, error = function(e) return(NULL))
  
  if (is.null(res_far)) return(NULL) # Nếu lỗi thì dừng
  far_vals <- res_far$upperbound
  
  # Cusum
  res_cus <- tryCatch({
    algo.cusum(disProgObj, control = list(range = control_range, k = 1.04, h = 2.26, trans = "rossi"))
  }, error = function(e) return(NULL))
  
  if (is.null(res_cus)) return(NULL)
  cus_vals <- res_cus$upperbound
  
  # --- 5. TÍNH SEASONAL & CDC ---
  grand_median <- median(df_ref$cases, na.rm = TRUE)
  stats_ref <- df_ref %>%
    group_by(week) %>%
    summarise(
      Seasonal = grand_median,
      mean_val = mean(cases, na.rm = TRUE),
      sd_val = sd(cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(CDC = pmax(0, mean_val + 2 * sd_val)) %>%
    select(week, Seasonal, CDC)
  
  # --- 6. TỔNG HỢP KẾT QUẢ ---
  result_df <- data.frame(
    Year = target_year, 
    Week = 1:52,
    Farrington = NA, 
    Cusum = NA
  )
  
  # Điền giá trị
  n_cal <- length(far_vals)
  if (n_cal > 0) {
    result_df$Farrington[1:n_cal] <- far_vals
    result_df$Cusum[1:n_cal] <- cus_vals
  }
  
  # Ghép các chỉ số
  result_df <- result_df %>%
    left_join(stats_ref, by = c("Week" = "week"))
  
  # Ghép số ca thực tế (để vẽ biểu đồ)
  if ("cases" %in% names(df_target)) {
    result_df <- result_df %>%
      left_join(df_target %>% select(week, cases), by = c("Week" = "week"))
  }
  
  # Clean data tương lai (các tuần chưa tới)
  result_df$Seasonal[result_df$Week > max_week_current] <- NA
  result_df$CDC[result_df$Week > max_week_current] <- NA
  
  return(result_df)
}

process_unit_multi_years <- function(df_unit, unit_name, disease_name, target_years) {
 
  all_years_res <- list()

  fixed_refs <- get_fixed_ref_years(disease_name)
  
  for (yr in target_years) {

    res <- calculate_all_thresholds(df_unit, fixed_refs, target_year = yr)
    
    if (!is.null(res)) {
      res$Disease <- disease_name
      res$WardId <- unit_name
      all_years_res[[as.character(yr)]] <- res
    }
  }
  
  if (length(all_years_res) > 0) {
    return(bind_rows(all_years_res))
  } else {
    return(NULL)
  }
}
