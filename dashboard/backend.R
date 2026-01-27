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

## Load data
raw_df <- load_data_direct("data/tuan03.rds")

## Clean data
sxh <- clean_data(raw_df, "Sốt xuất huyết Dengue")
tcm <- clean_data(raw_df, "Tay - chân - miệng")

## Aggregate data
agg_sxh <- aggregate_data(sxh)
agg_tcm <- aggregate_data(tcm)

## Encrypt data
encrypt_data(agg_sxh, paste0(base_path, "sxh.dat"), "Swaper@234")
encrypt_data(agg_tcm, paste0(base_path, "tcm.dat"), "Swaper@234")

message("✅ XONG! Hãy push file sxh.dat và tcm.dat lên GitHub.")
