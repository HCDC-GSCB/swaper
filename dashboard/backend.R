rm(list=ls())
graphics.off()

library(jsonlite)
library(digest)
library(stringr)
library(readxl)

wd <- getwd()
if (dir.exists("dashboard")) {
  base_path <- "dashboard/"
} else {
  base_path <- ""
}

# Load hàm (phải chỉ đúng đường dẫn)
source(paste0(base_path, "function_analysis.R"))
source(paste0(base_path, "function_threshold.R"))
source(paste0(base_path, "function_pisa.R"))

##########################
##### OVERALL DATA #####
##########################

## Load data
raw_df <- load_data_direct("data/clean_data.rds")

# Loại tuần mới nhất của năm mới nhất
remove_latest_week <- function(df) {
  latest_year  <- max(df$Year, na.rm = TRUE)
  latest_week  <- max(df$Week[df$Year == latest_year], na.rm = TRUE)
  
  df |> 
    dplyr::filter(!(Year == latest_year & Week == latest_week))
}

## Clean data
sxh <- clean_data(raw_df, "Sốt xuất huyết Dengue") |> 
  remove_latest_week()

tcm <- clean_data(raw_df, "Tay - chân - miệng") |> 
  remove_latest_week()

## Aggregate data
agg_sxh <- aggregate_data(sxh)
agg_tcm <- aggregate_data(tcm)

## Encrypt data
encrypt_data(agg_sxh, paste0(base_path, "sxh.dat"), "Swaper@234")
encrypt_data(agg_tcm, paste0(base_path, "tcm.dat"), "Swaper@234")

message("✅ Hoàn tất filexh.dat và tcm.dat lên GitHub.")


##########################
##### THRESHOLD DATA #####
##########################
target_years <- c(2025,2026) 

message("--- Đang xử lý Sốt xuất huyết ---")

sxh_city <- load_data("https://docs.google.com/spreadsheets/d/1tkoFRYLPNrojiAFzdbpT2aZGkIjuaIn7EcDSRJcPPHY", "SXH_3KV")
df_sxh_city <- process_unit_multi_years(sxh_city, "Toàn Thành phố", "Sốt xuất huyết", target_years)

px_sxh <- load_data("https://docs.google.com/spreadsheets/d/1Qg5zNehb86sRHaDWRdVrQPz_s9R0g0GbEV9lOajSBkw/edit?usp=sharing", "all_time")

df_sxh_wards <- px_sxh %>%
  group_split(ward) %>%
  map_dfr(function(d) {
    process_unit_multi_years(d, unique(d$ward), "Sốt xuất huyết", target_years)
  })

message("--- Đang xử lý Tay chân miệng ---")

tcm_city <- load_data("https://docs.google.com/spreadsheets/d/1ouVcS4B-sU07j4BT2VjHhxY13f2JUlqSeCY541vH6vA", "TCM_3KV")
df_tcm_city <- process_unit_multi_years(tcm_city, "Toàn Thành phố", "Tay chân miệng", target_years)

px_tcm <- load_data("https://docs.google.com/spreadsheets/d/1H8E1Ou7HplqMPS09-ctHmtHM-1e18tUPFO0FTDguons/edit?usp=sharing", "all_time")

df_tcm_wards <- px_tcm %>%
  group_split(ward) %>%
  map_dfr(function(d) {
    process_unit_multi_years(d, unique(d$ward), "Tay chân miệng", target_years)
  })

message("--- Đang đóng gói dữ liệu ---")

final_threshold_df <- bind_rows(df_sxh_city, df_sxh_wards, df_tcm_city, df_tcm_wards) %>%
  mutate(NoiOHienTai_SauKhiSapNhap_WardId = str_trim(WardId))

pop_data <- load_data("https://docs.google.com/spreadsheets/d/1ZlfExROncZcCpm8LzGlaw8wim9T_0eNNhpAsyB836kA/edit?usp=sharing", sheet = "Sheet1")

pop_data_clean <- pop_data %>%
  mutate(ward = str_trim(ward)) %>%
  select(ward, KV, danso) # Chỉ lấy các cột cần thiết

# 4. Join vào bảng chính
# Lúc này mỗi dòng dữ liệu sẽ có thêm cột KV và danso
final_threshold_df <- final_threshold_df %>%
  left_join(pop_data_clean, by = c("NoiOHienTai_SauKhiSapNhap_WardId" = "ward"))

# Kiểm tra xem có bị NA dân số không (nếu tên phường không khớp)
na_check <- final_threshold_df %>% filter(is.na(danso) & NoiOHienTai_SauKhiSapNhap_WardId != "Toàn Thành phố")
if(nrow(na_check) > 0) {
  warning("⚠️ Cảnh báo: Có phường bị lệch tên không ghép được dân số:")
  print(unique(na_check$NoiOHienTai_SauKhiSapNhap_WardId))
}

# ==============================================================================

encrypt_data(final_threshold_df, paste0(base_path, "threshold.dat"), "Swaper@234")

##########################
######## MAP DATA ########
##########################

# ==============================================================================
#  XỬ LÝ BẢN ĐỒ (MAP PROCESSING)
# ==============================================================================
# library(sf)
# library(rmapshaper) 
# library(geojsonio)  
# library(stringr)
# library(stringi)
# 
# # Xử lý final_threshold_df
# final_threshold_df <- final_threshold_df %>%
#   filter(NoiOHienTai_SauKhiSapNhap_WardId != "Toàn Thành phố") %>%
#   mutate(
#     NoiOHienTai_SauKhiSapNhap_WardId = clean_vn_text(NoiOHienTai_SauKhiSapNhap_WardId)
#   )
# 
# data_names <- unique(final_threshold_df$NoiOHienTai_SauKhiSapNhap_WardId)
# 
# # 1. Đọc Shapefile
# shp_path <- "dashboard/TPHCM_XA_2025_JUL_AP" 
# hcm_map <- st_read(shp_path, quiet = TRUE) 
# 
# # 2. Xử lý tên Phường/Xã (SẠCH SẼ)
# hcm_map_clean <- hcm_map %>%
#   mutate(tenXa = clean_vn_text(tenXa)) |> 
#   mutate(NoiOHienTai_SauKhiSapNhap_WardId = case_match(tenXa,
#                                 "Phường Thới An" ~ "Phường Thới An",
#                                 "Phường Tân Thuận" ~ "Phường Tân Thuận",
#                                 "Xã Long Hòa" ~ "Xã Long Hoà",              
#                                 "Xã Phước Hòa" ~ "Xã Phước Hoà", 
#                                 "Phường Tân Thới Hiệp" ~ "Phường Tân Thới Hiệp",
#                                 "Phường Đông Hưng Thuận" ~ "Phường Đông Hưng Thuận",
#                                 .default = tenXa
#   ))
# 
# map_names <- unique(hcm_map_clean$NoiOHienTai_SauKhiSapNhap_WardId)
# 
# hcm_simple <- ms_simplify(hcm_map_clean, keep = 0.05, keep_shapes = TRUE)
# 
# # 4. Xuất ra file GeoJSON
# geojson_write(hcm_simple, file = "dashboard/hcm_map.json")
# 
# message("✅ Đã xuất file bản đồ: dashboard/hcm_map.json")

##########################
######## PISA DATA ########
##########################

sxh_cases <- load_data("https://docs.google.com/spreadsheets/d/13_o7NAlfBjckO6PspzITbWhlkWbfp4eKkqYCa7Dvvgg/edit?usp=sharing", sheet = "ca")
sxh_transmission <- calc_transmission(sxh_cases, "Sốt xuất huyết")
sxh_inp <- load_data("https://docs.google.com/spreadsheets/d/13_o7NAlfBjckO6PspzITbWhlkWbfp4eKkqYCa7Dvvgg/edit?usp=sharing", sheet = "noi")
sxh_severity <- calc_severity(sxh_inp, "Sốt xuất huyết", as.character(2026))

tcm_cases <- load_data("https://docs.google.com/spreadsheets/d/1jSAuGFUkcHBL5iie999qn7ClW2dTYB3AOSOaRwOBCGY/edit?usp=sharing", sheet = "ca")
tcm_transmission <- calc_transmission(tcm_cases, "Tay chân miệng")
tcm_inpt <- load_data("https://docs.google.com/spreadsheets/d/1jSAuGFUkcHBL5iie999qn7ClW2dTYB3AOSOaRwOBCGY/edit?usp=sharing", sheet = "noi")
tcm_severity <- calc_severity(tcm_inpt, "Tay chân miệng", as.character(2026))

df_transmission <- bind_rows(
  as_tibble(sxh_transmission) %>% mutate(Disease = "Sốt xuất huyết"),
  as_tibble(tcm_transmission) %>% mutate(Disease = "Tay chân miệng")
)
encrypt_data(df_transmission, paste0(base_path, "transmission.dat"), "Swaper@234")

df_severity <- bind_rows(
  sxh_severity %>% select(Week, Limit_Low, Limit_Mod, Limit_High) %>% mutate(Disease = "Sốt xuất huyết"),
  tcm_severity %>% select(Week, Limit_Low, Limit_Mod, Limit_High) %>% mutate(Disease = "Tay chân miệng")
)
encrypt_data(df_severity, paste0(base_path, "severity.dat"), "Swaper@234")

##########################
######## PRED DATA #######
##########################
message("--- Đang đóng gói dữ liệu dự báo ---")
# Đọc file excel (nhớ điều chỉnh đường dẫn nếu cần)
df_pred <- read_excel("dashboard/data_pred.xlsx")    

# Mã hoá file
encrypt_data(df_pred, paste0(base_path, "pred.dat"), "Swaper@234")
message("✅ Hoàn tất file pred.dat")


