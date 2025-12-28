library(readxl)
library(tidyverse)
library(surveillance)
library(googlesheets4)
library(googledrive)
library(patchwork)
library(plotly)
library(gt)

## Load data GG Drive
load_data <- function(url, sheet) {
  gs4_deauth()
  df <- read_sheet(url, sheet = sheet)
  return(df)
}

## Calculate Mean+2SD and seasonal
remake <- function(df, ref_years) {
  df <- df %>%
    mutate(
      week = ifelse(week == 53, 52, week)
    ) %>%
    group_by(year, week) %>%
    summarise(
      cases = if (all(is.na(cases))) NA_integer_
      else sum(cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(year, week)
  
  seasonal <- df %>% 
    filter(year %in% ref_years) %>% 
    summarise(seasonal = median(cases, na.rm = TRUE)) %>% 
    pull(seasonal)
  
  df_cdc <- df %>% 
    filter(year %in% ref_years) %>% 
    group_by(week) %>% 
    summarise(
      mean = mean(cases, na.rm = TRUE),
      sd = sd(cases, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(cdc = mean + 2 * sd)
  
  df <- df %>% 
    left_join(df_cdc, by = "week") %>% 
    mutate(outbreak_cdc = ifelse(!is.na(cdc) & cases >= cdc, 1, 0))
  
  return(list(data = df, seasonal = seasonal))
}

## Run Farrington or CUSUM
run_algo <- function(df, ref_years, method = c("farrington", "cusum"),
                     start_year = 2020, start_week = 1, range_weeks = 261:312,
                     cusum_k = 1.04, cusum_h = 2.26) {
  
  method <- match.arg(method)
  
  remake_out <- remake(df, ref_years)
  df <- remake_out$data
  
  dff <- df %>% filter(year %in% ref_years)
  
  stsObj <- with(dff, sts(observed = cases,
                          state = outbreak_cdc,
                          start = c(start_year, start_week),
                          frequency = 52))
  disProgObj <- sts2disProg(stsObj)
  
  if (method == "farrington") {
    control <- list(b = 5, w = 1, range = range_weeks, reweight = TRUE, verbose = FALSE)
    res <- algo.farrington(disProgObj, control = control)
  } else if (method == "cusum") {
    control <- list(range = range_weeks, k = cusum_k, h = cusum_h)
    res <- algo.cusum(disProgObj, control = control)
  }
  
  return(list(df = df, result = res))
}

## Plotly
plot_algo_plotly <- function(df_cases, res_far, res_cusum, seasonal_df, year_target) {
  
  plot_data <- df_cases %>%
    filter(year == year_target) %>%
    mutate(
      thr_farrington = res_far$upperbound, 
      thr_cusum = res_cusum$upperbound,
      thr_cdc = cdc,
      thr_seasonal = seasonal_df$seasonal
    )
  
  fig <- plot_ly(data = plot_data, x = ~week)
  
  fig <- fig %>% add_bars(
    y = ~cases, name = "Ca bệnh", 
    marker = list(color = '#1f77b4'),
    hovertemplate = "<b>Số ca: %{y}</b><extra></extra>"
  )
  
  fig <- fig %>% add_lines(
    y = ~thr_seasonal, name = "Ngưỡng mùa", 
    line = list(color = 'black', width = 1.5), visible = TRUE,
    hovertemplate = "Ngưỡng mùa: %{y:.1f}<extra></extra>"
  )

  fig <- fig %>% add_lines(
    y = ~thr_cusum, name = "CUSUM", 
    line = list(color = 'orange', dash = 'dash'), visible = FALSE,
    hovertemplate = "CUSUM: %{y:.1f}<extra></extra>"
  )

  fig <- fig %>% add_lines(
    y = ~thr_farrington, name = "Farrington", 
    line = list(color = 'red', dash = 'dash'), visible = FALSE,
    hovertemplate = "Farrington: %{y:.1f}<extra></extra>"
  )

  fig <- fig %>% add_lines(
    y = ~thr_cdc, name = "CDC", 
    line = list(color = 'darkblue', dash = 'dot'), visible = FALSE,
    hovertemplate = "CDC: %{y:.1f}<extra></extra>"
  )
  
  updatemenus <- list(
    list(
      type = "buttons", direction = "right",
      x = 0.5, y = 1.15, xanchor = 'center', yanchor = 'top',
      bgcolor = "lightblue", bordercolor = "black",
      font = list(size = 11, color = "black"),
      
      buttons = list(
        
        list(
          label = "None",
          method = "update",
          args = list(
            list(visible = list(TRUE, TRUE, FALSE, FALSE, FALSE))
          )
        ),

        list(
          label = "FARRINGTON",
          method = "update",
          args = list(
            list(visible = list(TRUE, TRUE, FALSE, TRUE, FALSE))
          )
        ),

        list(
          label = "CUSUM",
          method = "update",
          args = list(
            list(visible = list(TRUE, TRUE, TRUE, FALSE, FALSE))
          )
        ),

        list(
          label = "CDC",
          method = "update",
          args = list(
            list(visible = list(TRUE, TRUE, FALSE, FALSE, TRUE))
          )
        ),

        list(
          label = "All",
          method = "update",
          args = list(
            list(visible = list(TRUE, TRUE, TRUE, TRUE, TRUE))
          )
        )
      )
    )
  )
  
  fig <- fig %>% layout(
    
    hovermode = "x unified",
    hoverlabel = list(bgcolor = "white", font = list(size = 13), namelength = -1),
    
    xaxis = list(
      title = "Tuần",
      showline = TRUE,        
      mirror = FALSE,        
      linecolor = "black",    
      tickfont = list(color = "black"),  
      titlefont = list(color = "black") 
    ),
    
    yaxis = list(
      title = "Số ca",
      showline = TRUE,
      mirror = FALSE,         
      linecolor = "black",
      tickfont = list(color = "black"),
      titlefont = list(color = "black"),
      showgrid = TRUE, gridcolor = "#E5E5E5"
    ),
    
    updatemenus = updatemenus,
    legend = list(orientation = "h", x = 0.5, y = -0.1, xanchor = "center")
  )
  
  return(fig)
}

## Create cards information
create_info_card <- function(current_cases, current_week, 
                             val_seasonal, val_cusum, val_farrington, val_cdc) {
  
  # 1. Hàm con để tạo định dạng html cho mũi tên và màu sắc
  format_diff <- function(curr, thr) {
    if (is.na(thr) || thr == 0) return("-")
    
    diff_val <- curr - thr
    pct <- (diff_val / thr) * 100
    
    # Định dạng: Mũi tên lên (đỏ), xuống (xanh)
    if (diff_val > 0) {
      symbol <- "&#9650;" # Mũi tên lên
      color <- "#d62728"  # Màu đỏ
      sign_txt <- "+"
    } else {
      symbol <- "&#9660;" # Mũi tên xuống
      color <- "darkgreen"  # Màu xanh dương
      sign_txt <- ""
    }
    
    # Trả về chuỗi HTML
    sprintf("<span style='color:%s; font-weight:bold'>%s %s%.1f%%</span>", 
            color, symbol, sign_txt, pct)
  }
  
  # 2. Tạo Data Frame dữ liệu hiển thị
  df_card <- data.frame(
    Chi_so = c(
      paste0("Số ca tuần ", current_week), # Dòng 1
      "So với Ngưỡng mùa",                 # Dòng 2
      "So với Ngưỡng CUSUM",               # Dòng 3
      "So với Ngưỡng Farrington",          # Dòng 4
      "So với Ngưỡng CDC"                  # Dòng 5
    ),
    Gia_tri = c(
      paste0("<b>", format(current_cases, big.mark=","), "</b>"), # In đậm số ca
      format_diff(current_cases, val_seasonal),
      format_diff(current_cases, val_cusum),
      format_diff(current_cases, val_farrington),
      format_diff(current_cases, val_cdc)
    )
  )
  
  # 3. Vẽ bảng bằng gt
  tbl <- df_card %>%
    gt() %>%
    # Ẩn tiêu đề cột
    tab_options(column_labels.hidden = TRUE) %>% 
    # Căn chỉnh text
    cols_align(align = "left", columns = 1) %>%
    cols_align(align = "right", columns = 2) %>%
    # Xử lý HTML cho cột Giá trị
    fmt_markdown(columns = Gia_tri) %>% 
    # Thêm style cho bảng đẹp hơn (viền mỏng, font chữ)
    tab_style(
      style = list(cell_text(size = "large")),
      locations = cells_body(rows = 1) # Dòng đầu tiên to hơn
    ) %>%
    opt_table_lines(extent = "none") %>% # Bỏ bớt kẻ bảng cho giống Card
    tab_style(
      style = cell_borders(sides = "bottom", color = "lightgrey", weight = px(1)),
      locations = cells_body()
    )
  
  return(tbl)
}
