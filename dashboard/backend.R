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


##########################
##### AUTO RENDER QMD ####
##########################
message("--- Đang xử lý tự động tạo file báo cáo tuần ---")

# 1. Xác định đường dẫn thư mục snapshot
snap_path <- ifelse(dir.exists("snapshot"), "snapshot/", "../snapshot/")
template_file <- paste0(snap_path, "template.qmd")
posts_dir <- paste0(snap_path, "posts/")

# Tạo thư mục posts nếu chưa có
if (!dir.exists(posts_dir)) {
  dir.create(posts_dir, recursive = TRUE)
}

# 2. Lấy Year và Week mới nhất từ dữ liệu đã clean (sxh)
# Vì bạn đã chạy remove_latest_week() ở trên, nên đây chính là "tuần hiện tại" (tuần hoàn thiện dữ liệu mới nhất)
target_year <- max(sxh$Year, na.rm = TRUE)
target_week <- max(sxh$Week[sxh$Year == target_year], na.rm = TRUE)

# 3. Tạo định dạng tên file: ví dụ "tuan_32_2026.qmd"
out_file <- sprintf("%s%04d_W%02d.qmd", posts_dir, target_week, target_year)

# 4. Kiểm tra và tạo file báo cáo mới
if (!file.exists(out_file)) {
  if (file.exists(template_file)) {
    # Đọc nội dung file template
    tpl <- readLines(template_file, warn = FALSE)
    
    # Thay thế các thẻ bằng giá trị thực
    tpl <- gsub("\\{\\{TARGET_YEAR\\}\\}", target_year, tpl)
    tpl <- gsub("\\{\\{TARGET_WEEK\\}\\}", target_week, tpl)
    
    # Bơm thêm YAML title, date, author để file index.qmd có thể listing và sắp xếp được
    yaml_insert <- c(
      sprintf('title: "Bản tin nhanh - Tuần %02d/%04d"', target_week, target_year),
      sprintf('date: "%s"', Sys.Date()),
      'author: "Surveillance, Warning and Preparedness, Emergency Response to Epidemic"'
    )
    
    # Chèn vào ngay dưới dòng --- đầu tiên
    first_dash <- which(tpl == "---")[1]
    if (!is.na(first_dash)) {
      tpl <- append(tpl, yaml_insert, after = first_dash)
    }
    
    # Lưu ra file mới
    writeLines(tpl, out_file)
    message("✅ Đã tạo thành công báo cáo tuần mới: ", out_file)
  } else {
    warning("⚠️ Không tìm thấy file template.qmd tại: ", template_file)
  }
} else {
  message(sprintf("✅ Báo cáo Tuần %02d/%04d đã tồn tại, không ghi đè.", target_week, target_year))
}



