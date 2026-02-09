rm(list=ls())
graphics.off()

library(jsonlite)
library(digest)

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
raw_df <- load_data_direct("data/tuan05.rds")

## Clean data
sxh <- clean_data(raw_df, "Sốt xuất huyết Dengue")
tcm <- clean_data(raw_df, "Tay - chân - miệng")

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

print(paste("Tổng số dòng dữ liệu ngưỡng:", nrow(final_threshold_df)))
print(head(final_threshold_df))

output_path <- "dashboard/threshold.dat" 

encrypt_data(final_threshold_df, output_path, "Swaper@234")

message("✅ HOÀN TẤT! Hãy kiểm tra file tại: ", output_path)

