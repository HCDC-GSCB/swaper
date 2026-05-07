library(plumber)
library(jsonlite)
library(httr2)

# API KEY GEMINI CỦA SẾP (Gắn trực tiếp vào đây)
GEMINI_API_KEY <- Sys.getenv("GEMINI_API_KEY")

#* @filter cors
function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  plumber::forward()
}

#* @options /api/chat
function() { list() }

#* @serializer unboxedJSON
#* @post /api/chat
function(req, message = "") {
  safe_reply <- tryCatch({
    # Đọc dữ liệu từ file context
    data_context <- paste(readLines("ai_context.txt", encoding = "UTF-8", warn = FALSE), collapse = "\n")
    
    prompt <- paste0("DỮ LIỆU:\n", data_context, "\n\nCÂU HỎI: ", message)
    url_api <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key=", GEMINI_API_KEY)
    
    req_api <- request(url_api) %>%
      req_headers("Content-Type" = "application/json") %>%
      req_body_json(list(contents = list(list(parts = list(list(text = prompt))))), auto_unbox = TRUE)
    
    resp <- req_perform(req_api)
    res_data <- resp_body_json(resp)
    ans <- res_data$candidates[[1]]$content$parts[[1]]$text
    list(reply = paste(ans, collapse = "\n"))
    
  }, error = function(e) {
    list(reply = paste("❌ LỖI:", conditionMessage(e)))
  })
  return(safe_reply)
}