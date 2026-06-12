################################################################################
##### TÍNH TOÁN HỆ SỐ LÂY NHIỄM (Rt) CHO TOÀN TP VÀ TỪNG PHƯỜNG/XÃ (CDC STYLE) #
################################################################################
message("--- Đang khởi động EpiNow2 tính Rt cho Toàn TP và các Phường trọng điểm ---")
library(EpiNow2)
library(tidyr)
library(purrr)
library(dplyr)

# Hàm tính Rt cốt lõi cho 1 đơn vị
calc_rt_single_unit <- function(df_unit, global_max_date, gt_mean, gt_sd, inc_mean, inc_sd, delay_mean, delay_sd) {
  
  df_daily <- df_unit %>%
    mutate(date = as.Date(NgayNhapVien)) %>%
    filter(!is.na(date)) %>%
    group_by(date) %>%
    summarise(confirm = n(), .groups = "drop")
  
  # Tạo chuỗi thời gian CỨNG ĐÚNG 90 NGÀY cho mọi phường
  # Tránh lỗi phường mới có ca bệnh bị thiếu hụt dữ liệu đầu vào
  date_sequence <- seq.Date(global_max_date - 89, global_max_date, by = "day")
  
  df_daily <- data.frame(date = date_sequence) %>%
    left_join(df_daily, by = "date") %>%
    mutate(confirm = replace_na(confirm, 0)) %>%
    arrange(date)
  
  # ============================================================================
  # BỘ LỌC KIỂM ĐỊNH US CDC (ĐÃ ĐỊA PHƯƠNG HÓA CHO CẤP PHƯỜNG/XÃ)
  # ============================================================================
  
  # 1. Dữ liệu không đủ để tính toán xu hướng (Insufficient Data)
  recent_56_days <- tail(df_daily, 56)
  zero_days_count <- sum(recent_56_days$confirm == 0)
  
  # Tùy chỉnh SWAPER: Ở cấp Phường, cho phép tối đa 80% số ngày không có ca bệnh.
  # Nếu số ngày 0 ca >= 45 ngày (trong 56 ngày) -> Chuỗi quá thưa thớt, không thể nội suy -> LOẠI
  if (zero_days_count >= 28) {
    return(NULL) 
  }
  
  # 2. Bất thường kéo dài trong dữ liệu (Batch Reporting / Đổ đống dữ liệu)
  recent_14_days <- tail(df_daily, 14)
  total_14d <- sum(recent_14_days$confirm)
  max_single_day <- max(recent_14_days$confirm)
  
  # Chỉ áp dụng lọc bất thường nếu tổng số ca tương đối đáng kể (>= 10 ca)
  # Nếu 1 ngày chiếm >= 70% số ca của 2 tuần (nhập bù sau Lễ/Tết) -> LOẠI
  if (total_14d >= 10 && (max_single_day / total_14d) >= 0.7) {
    return(NULL) 
  }
  
  # 3. Lọc vi mô hỗ trợ: Đảm bảo có đà lây nhiễm ở hiện tại (Luật cũ đã chốt)
  cases_week_1 <- sum(tail(recent_14_days, 7)$confirm) 
  cases_week_2 <- sum(head(recent_14_days, 7)$confirm) 
  if (cases_week_1 < 3 && cases_week_2 < 3) {
    return(NULL)
  }
  
  # ============================================================================
  # CẤU HÌNH THUẬT TOÁN MCMC STAN 
  # ============================================================================
  
  gen_time <- generation_time_opts(LogNormal(mean = gt_mean, sd = gt_sd, max = 15))
  
  # Gộp Thời gian ủ bệnh + Độ trễ báo cáo thực tế
  all_delays <- delay_opts(
    LogNormal(mean = inc_mean, sd = inc_sd, max = 15) + 
      LogNormal(mean = delay_mean, sd = delay_sd, max = 10)
  )
  
  res <- tryCatch({
    epinow(
      data = df_daily, 
      generation_time = gen_time, delays = all_delays,
      rt = rt_opts(prior = Normal(mean = 1, sd = 0.5)),
      stan = stan_opts(cores = 4, samples = 1000, warmup = 250), 
      verbose = FALSE 
    )
  }, error = function(e) return(NULL))
  
  if(is.null(res)) return(NULL)
  
  # --- ÉP LẤY DUY NHẤT VARIABLE = "R" ĐỂ FILE .DAT NHẸ NHẤT ---
  summary_df <- res$estimates$summarised %>% 
    filter(variable == "R") %>% 
    select(variable, date, type, estimate = mean, lower = lower_90, upper = upper_90)
  
  # KẾT NỐI VỚI SỐ CA BỆNH GỐC ĐỂ VẼ BIỂU ĐỒ TRÊN DASHBOARD
  summary_df <- summary_df %>%
    left_join(df_daily, by = "date")
  
  return(summary_df)
}

# Hàm vòng lặp tính cho Toàn TP và Từng Phường
run_epinow_multi_wards <- function(raw_data, disease_name, gt_mean, gt_sd, inc_mean, inc_sd, delay_mean, delay_sd) {
  
  df_disease <- raw_data %>% filter(ChanDoanChinhName == disease_name)
  
  if (nrow(df_disease) == 0) {
    message("⚠️ Cảnh báo: Không tìm thấy ca bệnh nào cho [", disease_name, "]. Bỏ qua tính Rt.")
    return(NULL)
  }
  
  # Tìm ngày báo cáo mới nhất của toàn hệ thống để làm mốc thời gian chung
  global_max_date <- max(as.Date(df_disease$NgayNhapVien), na.rm = TRUE)
  
  # 1. Tính cho Toàn Thành phố
  message(">> Đang tính Rt Toàn Thành phố cho bệnh: ", disease_name, " (Ngày chốt sổ: ", global_max_date, ")")
  rt_city <- calc_rt_single_unit(df_disease, global_max_date, gt_mean, gt_sd, inc_mean, inc_sd, delay_mean, delay_sd)
  if(!is.null(rt_city)) rt_city$WardId <- "Toàn Thành phố"
  
  # 2. Tính cho từng Phường/Xã
  list_wards <- unique(df_disease$NoiOHienTai_SauKhiSapNhap_WardId)
  list_wards <- list_wards[!is.na(list_wards) & list_wards != ""]
  
  message(">> Đang quét Rt cấp Phường/Xã (Áp dụng tiêu chuẩn lọc dữ liệu US CDC)...")
  rt_wards <- df_disease %>%
    group_split(NoiOHienTai_SauKhiSapNhap_WardId) %>%
    map_dfr(function(df_w) {
      ward_name <- unique(df_w$NoiOHienTai_SauKhiSapNhap_WardId)[1]
      res <- calc_rt_single_unit(df_w, global_max_date, gt_mean, gt_sd, inc_mean, inc_sd, delay_mean, delay_sd)
      if(!is.null(res)) {
        res$WardId <- ward_name
        return(res)
      }
      return(NULL)
    })
  
  return(bind_rows(rt_city, rt_wards))
}

