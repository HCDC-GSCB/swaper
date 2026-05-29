################################################################################
##### TÍNH TOÁN HỆ SỐ LÂY NHIỄM (Rt) CHO TOÀN TP VÀ TỪNG PHƯỜNG/XÃ (CDC STYLE) #
################################################################################
message("--- Đang khởi động EpiNow2 tính Rt cho Toàn TP và các Phường trọng điểm ---")
library(EpiNow2)
library(tidyr)
library(purrr)

# Hàm tính Rt cốt lõi cho 1 đơn vị
calc_rt_single_unit <- function(df_unit, global_max_date, gt_mean, gt_sd, inc_mean, inc_sd) {
  
  df_daily <- df_unit %>%
    mutate(date = as.Date(NgayNhapVien)) %>%
    filter(!is.na(date)) %>%
    group_by(date) %>%
    summarise(confirm = n(), .groups = "drop")
  
  if(nrow(df_daily) == 0) return(NULL)
  
  # FIX LỖI DỊCH TỄ: Ép tất cả các phường chốt sổ cùng 1 ngày với Toàn Thành phố
  df_daily <- df_daily %>%
    complete(date = seq.Date(min(date), global_max_date, by = "day"), fill = list(confirm = 0)) %>%
    arrange(date) %>%
    filter(date >= (global_max_date - 90)) # Lấy 90 ngày tính từ ngày báo cáo mới nhất
  
  # --- ÁP DỤNG ĐIỀU KIỆN LOẠI TRỪ CỦA US CDC (ĐÃ HẠ NGƯỠNG) ---
  if(nrow(df_daily) >= 14) {
    recent_14_days <- tail(df_daily, 14)
    cases_week_1 <- sum(tail(recent_14_days, 7)$confirm) # Tổng ca 7 ngày gần nhất
    cases_week_2 <- sum(head(recent_14_days, 7)$confirm) # Tổng ca 7 ngày trước đó
    
    # Nếu CẢ 2 tuần đều có dưới 3 ca -> KHÔNG TÍNH Rt
    if (cases_week_1 < 3 && cases_week_2 < 3) {
      return(NULL)
    }
  } else {
    return(NULL) # Dữ liệu rác không đủ 14 ngày cũng bỏ qua
  }
  
  gen_time <- generation_time_opts(LogNormal(mean = gt_mean, sd = gt_sd, max = 15))
  all_delays <- delay_opts(LogNormal(mean = inc_mean, sd = inc_sd, max = 15) + 
                             LogNormal(mean = 2.5, sd = 1.5, max = 10))
  
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
  
  return(summary_df)
}

# Hàm vòng lặp tính cho Toàn TP và Từng Phường
run_epinow_multi_wards <- function(raw_data, disease_name, gt_mean, gt_sd, inc_mean, inc_sd) {
  
  df_disease <- raw_data %>% filter(ChanDoanChinhName == disease_name)
  
  # Tìm ngày báo cáo mới nhất của toàn hệ thống để làm mốc thời gian chung
  global_max_date <- max(as.Date(df_disease$NgayNhapVien), na.rm = TRUE)
  
  # 1. Tính cho Toàn Thành phố
  message(">> Đang tính Rt Toàn Thành phố cho bệnh: ", disease_name, " (Ngày chốt sổ: ", global_max_date, ")")
  rt_city <- calc_rt_single_unit(df_disease, global_max_date, gt_mean, gt_sd, inc_mean, inc_sd)
  if(!is.null(rt_city)) rt_city$WardId <- "Toàn Thành phố"
  
  # 2. Tính cho từng Phường/Xã
  list_wards <- unique(df_disease$NoiOHienTai_SauKhiSapNhap_WardId)
  list_wards <- list_wards[!is.na(list_wards) & list_wards != ""]
  
  message(">> Đang quét Rt cấp Phường/Xã (Bỏ qua các phường < 3 ca trong 2 tuần gần nhất)...")
  rt_wards <- df_disease %>%
    group_split(NoiOHienTai_SauKhiSapNhap_WardId) %>%
    map_dfr(function(df_w) {
      ward_name <- unique(df_w$NoiOHienTai_SauKhiSapNhap_WardId)[1]
      res <- calc_rt_single_unit(df_w, global_max_date, gt_mean, gt_sd, inc_mean, inc_sd)
      if(!is.null(res)) {
        res$WardId <- ward_name
        return(res)
      }
      return(NULL)
    })
  
  return(bind_rows(rt_city, rt_wards))
}