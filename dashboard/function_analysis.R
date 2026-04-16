library(curl)
library(tidyverse)
library(jsonlite)
library(httr)
library(lubridate)
library(digest)

# Function App Password (Chạy tự động)
load_data_direct <- function(filename_on_server) {
  
  base_url <- "https://gscb.hcdc.vn/remote.php/dav/files/gscbao/" 
  full_url <- paste0(base_url, filename_on_server)
  
  my_user <- "gscbao"
  my_app_pass <- "dijHc-JDKoY-zyGDf-23d5w-3q8Qy" 
  
  message("🚀 Đang kéo dữ liệu trực tiếp từ: ", full_url)
  
  tryCatch({
    response <- httr::GET(
      url = full_url,
      httr::authenticate(my_user, my_app_pass),
      httr::config(ssl_verifypeer = 0),
      httr::timeout(600)
    )
    
    if (status_code(response) == 200) {
      
      message("✅ Kết nối thành công! Đang đọc vào RAM...")
      content_raw <- content(response, "raw")
      con <- gzcon(rawConnection(content_raw))
      data <- readRDS(con)
      close(con)
      
      message("📊 Đã load xong dataframe! Số dòng: ", nrow(data))
      
      return(data)
      
    } else {
      stop(paste("❌ Lỗi tải (Mã", status_code(response), "). Link:", full_url))
    }
    
  }, error = function(e) {
    stop("❌ Lỗi kết nối: ", e$message)
  })
}

## Function clean data
clean_data <- function(raw_df, diagnosis) {
  message("🧹 Đang làm sạch dữ liệu...")
  
  clean_df <- raw_df %>%
    
    as_tibble() %>%

    filter(str_detect(ChanDoanChinhName, diagnosis)) %>%
    
    mutate(
      Week = isoweek(NgayNhapVien),
      Year = isoyear(NgayNhapVien),
      ThangTuoi = floor(time_length(interval(dmy(NgaySinh), NgayNhapVien), "month")),
      NhomTuoi = case_when(
        ThangTuoi<12 ~ "<1 tuổi",
        ThangTuoi>=12 & ThangTuoi<=60 ~ "1-5 tuổi",     
        ThangTuoi>60 & ThangTuoi<=120 ~ "6-10 tuổi",
        ThangTuoi>120 & ThangTuoi<=180 ~ "11-15 tuổi",
        TRUE ~ ">15 tuổi"
      ),
      # NhomTuoi = factor(NhomTuoi, levels = c("<1 tuổi", "1-5 tuổi", "6-10 tuổi", "11-15 tuổi", ">15 tuổi")),
      KetQua = ifelse(HinhThucDieuTriName == "Tử vong", "Tử vong", "Sống"),
      NhomDieuTri = case_when(
        HinhThucDieuTriName == "Điều trị ngoại trú" ~ "Ngoại trú",
        TRUE ~ "Nội trú"
      ),
      GioiTinh = as.factor(GioiTinh),
      PhanDoBenhName = as.factor(PhanDoBenhName)
    ) %>%
    
    filter(!is.na(NhomDieuTri)) %>%
    
    mutate(
      NoiOHienTai_SauKhiSapNhap_WardId = stringr::str_trim(NoiOHienTai_SauKhiSapNhap_WardId), # Xóa khoảng trắng thừa 2 đầu
      NoiOHienTai_SauKhiSapNhap_WardId = stringr::str_squish(NoiOHienTai_SauKhiSapNhap_WardId) # Xóa khoảng trắng kép ở giữa (nếu có)
    ) |> 
    
    select(Year, Week, ThangTuoi, NhomTuoi, GioiTinh, NhomDieuTri, PhanDoBenhName, 
           NoiOHienTai_SauKhiSapNhap_WardId, KetQua, area)
  
  message("✅ Xử lý hoàn tất. Số dòng sạch: ", nrow(clean_df))
  return(clean_df)
}

## Function Aggregate data
aggregate_data <- function(df_clean) {
  df_agg <- df_clean %>%
    group_by(
      Year,
      Week, 
      NhomTuoi, 
      GioiTinh, 
      NhomDieuTri, 
      PhanDoBenhName,
      NoiOHienTai_SauKhiSapNhap_WardId,
      KetQua,
      area
    ) %>%
    summarise(SoCa = n(), .groups = "drop")
  return(df_agg)
}

agg_2026_city <- function(df_clean) {
  df_clean %>%
    filter(Year == 2026) %>%
    group_by(Year, Week) %>%
    summarise(Cases = n(), .groups = "drop") 
}

agg_2026_ward <- function(df_clean) {
  df_clean %>%
    filter(Year == 2026) %>%
    group_by(Year, Week, NoiOHienTai_SauKhiSapNhap_WardId) %>%
    summarise(Cases = n(), .groups = "drop") %>%
    rename(ward = NoiOHienTai_SauKhiSapNhap_WardId) # Đổi tên cột cho khớp với map_dfr bên dưới
}

## Encrypt data
encrypt_data <- function(data_df, filename, password) {
  
  json_str <- toJSON(data_df, dataframe = "rows", auto_unbox = TRUE) 

  key_raw <- digest(password, algo = "sha256", serialize = FALSE, raw = TRUE)
  data_raw <- charToRaw(json_str)

  key_long <- rep(key_raw, length.out = length(data_raw))
  encrypted_raw <- as.raw(bitwXor(as.integer(data_raw), as.integer(key_long)))
  
  writeBin(encrypted_raw, filename)
  message(paste("🔒 Đã mã hóa và lưu:", filename))
}
