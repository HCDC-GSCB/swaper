rm(list=ls())
graphics.off()

library(jsonlite)
library(digest)
library(stringr)

wd <- getwd()
if (dir.exists("dashboard")) {
  base_path <- "dashboard/"
} else {
  base_path <- ""
}

# Load hàm (phải chỉ đúng đường dẫn)
source(paste0(base_path, "function_analysis.R"))
source(paste0(base_path, "function_threshold.R"))

##########################
##### OVERALL DATA #####
##########################

## Load data
raw_df <- load_data_direct("data/tuan06.rds")

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
  rename(NoiOHienTai_SauKhiSapNhap_WardId = WardId)

final_threshold_df <- final_threshold_df %>%
  mutate(NoiOHienTai_SauKhiSapNhap_WardId = str_trim(NoiOHienTai_SauKhiSapNhap_WardId))

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

print(paste("Tổng số dòng dữ liệu ngưỡng:", nrow(final_threshold_df)))
print(head(final_threshold_df))

output_path <- "dashboard/threshold.dat" 
encrypt_data(final_threshold_df, output_path, "Swaper@234")

message("✅ HOÀN TẤT! Hãy kiểm tra file tại: ", output_path)


##########################
##### MAP DATA #####
##########################

# ==============================================================================
# 🛠️ BƯỚC XỬ LÝ BẢN ĐỒ (MAP PROCESSING)
# ==============================================================================
library(sf)
library(rmapshaper) 
library(geojsonio)  
library(stringr)

message("--- Đang xử lý Bản đồ ---")

# 1. Đọc Shapefile
shp_path <- "dashboard/TPHCM_XA_2025_JUL_AP" 
hcm_map <- st_read(shp_path, quiet = TRUE) |> 
  st_transform(crs = 4326)

# 2. Xử lý tên Phường/Xã (SẠCH SẼ)
hcm_map_clean <- hcm_map %>%
  mutate(
    # Bước 1: Lấy tên gốc và chuẩn hóa form chữ (Viết hoa chữ cái đầu)
    name = str_to_title(tenXa),
    name = str_trim(name)
  ) %>%
  mutate(
    # Bước 2: SỬA LỖI "PHƯỜNG PHƯỜNG", "PHƯỜNG XÃ"
    # Logic: Nếu tên chưa có tiền tố thì thêm, nếu có rồi thì sửa lại cho đúng
    name = case_when(
      # Trường hợp đặc biệt (Sửa tay các xã hay bị lỗi font hoặc tên lạ)
      str_detect(name, "Thanh An") ~ "Xã Thạnh An",
      str_detect(name, "Long Hoa") ~ "Xã Long Hòa",
      
      # Nếu bắt đầu bằng "Phường Phường" -> Thay bằng "Phường"
      str_detect(name, "^Phường Phường") ~ str_replace(name, "^Phường Phường", "Phường"),
      
      # Nếu bắt đầu bằng "Phường Xã" -> Thay bằng "Xã" (Do code cũ cộng nhầm)
      str_detect(name, "^Phường Xã") ~ str_replace(name, "^Phường Xã", "Xã"),
      
      # Nếu bắt đầu bằng "Phường Thị Trấn" -> Thay bằng "Thị trấn"
      str_detect(name, "^Phường Thị Trấn") ~ str_replace(name, "^Phường Thị Trấn", "Thị trấn"),
      
      # Nếu tên gốc CHƯA CÓ chữ Phường/Xã/Thị trấn ở đầu -> Mặc định thêm "Phường"
      !str_detect(name, "^(Phường|Xã|Thị Trấn)") ~ paste("Phường", name),
      
      # Các trường hợp còn lại (đã đúng) -> Giữ nguyên
      TRUE ~ name
    )
  )

# Kiểm tra lại lần cuối xem còn bị lỗi không
message("Kiểm tra thử vài tên sau khi fix:")
print(head(unique(hcm_map_clean$name), 10))

hcm_simple <- ms_simplify(hcm_map_clean, keep = 0.05, keep_shapes = TRUE)

# 4. Xuất ra file GeoJSON
geojson_write(hcm_simple, file = "dashboard/hcm_map.json")

message("✅ Đã xuất file bản đồ: dashboard/hcm_map.json")

