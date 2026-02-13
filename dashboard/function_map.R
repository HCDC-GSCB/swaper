library(stringi) # Cần cài package này: install.packages("stringi")
library(stringr)

clean_vn_text <- function(x) {
  if (is.null(x)) return(x)
  
  # 1. Chuyển đổi về dạng chuẩn Unicode NFC (Dựng sẵn)
  # Đây là bước quan trọng nhất để sửa lỗi "nhìn giống mà không giống"
  x <- stri_trans_nfc(x)
  
  # 2. Chuẩn hóa các lỗi gõ dấu phổ biến (Hòa/Hoà, Thủy/Thuỷ)
  x <- str_replace_all(x, "oà", "òa")
  x <- str_replace_all(x, "oá", "óa")
  x <- str_replace_all(x, "uỷ", "ủy")
  x <- str_replace_all(x, "uỹ", "ũy")
  
  # 3. Xóa khoảng trắng thừa và viết hoa chữ cái đầu
  x <- str_trim(x)
  x <- str_squish(x) # Xóa cả khoảng trắng kép ở giữa nếu có
  
  return(x)
}
