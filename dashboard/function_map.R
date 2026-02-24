library(stringi) 
library(stringr)

clean_vn_text <- function(x) {
  if (is.null(x)) return(x)
  
  x <- stri_trans_nfc(x)
  
  x <- str_replace_all(x, "oà", "òa")
  x <- str_replace_all(x, "oá", "óa")
  x <- str_replace_all(x, "uỷ", "ủy")
  x <- str_replace_all(x, "uỹ", "ũy")
  
  x <- str_trim(x)
  x <- str_squish(x) 
  
  return(x)
}
