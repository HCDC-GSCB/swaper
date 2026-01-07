library(readxl)
library(tidyverse)
library(surveillance)
library(googlesheets4)
library(googledrive)
library(patchwork)
library(plotly)
library(gt)
library(DT)
library(sf)

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
    legend = list(orientation = "h", x = 0.5, y = -0.1, 
                  xanchor = "center",
                  yanchor = "bottom")
  )
  
  return(fig)
}

## Key hightlight table

format_metric <- function(current, baseline) {
  if (baseline == 0) return("-")
  diff_val <- (current - baseline) / baseline * 100
  
  color <- ifelse(diff_val > 0, "#e74c3c", "#2ecc71") # Đỏ / Xanh
  arrow <- ifelse(diff_val > 0, "&uarr;", "&darr;")   # Lên / Xuống
  
  return(sprintf(
    "<span style='color:%s; font-weight:bold'>%s %s%%</span>", 
    color, arrow, format(round(abs(diff_val), 1), decimal.mark = ",", big.mark = ".")
  ))
}

format_badge <- function(level) {
  bg_color <- switch(as.character(level),
                     "Rất thấp"   = "#2E7D32",  
                     "Thấp"       = "#F9A825",  
                     "Trung bình" = "#EF6C00", 
                     "Vừa"        = "#EF6C00",  
                     "Cao"        = "#C62828",  
                     "Rất cao"    = "#6A1B9A"                
  )
  
  text_color <- "white"
  return(sprintf(
    '<span style="background-color:%s; color:%s; padding: 4px 10px; border-radius: 12px; font-size: 13px; font-weight: 500; display: inline-block;">%s</span>',
    bg_color, text_color, level
  ))
}

## Xử lý sheet Khu vực 
standardize_name_func <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
  x <- stringi::stri_trans_nfkc(x)
  x <- str_to_lower(x)
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_squish(x)
  
  case_when(
    x == "xã đảo thạnh an" ~ "xã thạnh an",
    x == "xã long hoà" ~ "xã long hòa", 
    x == "xã phước hoà" ~ "xã phước hòa",
    TRUE ~ x 
  )
}

## Map phường/xã
plot_px_map <- function(
    shp_path,
    px_clean,
    df_kv,
    TUAN_BAO_CAO
) {
  
  if (!dir.exists(shp_path)) return(NULL)
  
  # --- 1. Xử lý dữ liệu (Giữ nguyên) ---
  hcm_map <- st_read(shp_path, quiet = TRUE) %>%
    mutate(phuong_clean = standardize_name_func(tenXa))
  
  data_map_source <- px_clean %>%
    filter(Tuan == TUAN_BAO_CAO) %>%
    select(ward_clean, SoCa, Phuong_Display)
  
  stt_lookup <- df_kv %>%
    select(phuong_clean, STT) %>%
    distinct()
  
  map_final <- hcm_map %>%
    left_join(data_map_source, by = c("phuong_clean" = "ward_clean")) %>%
    left_join(stt_lookup, by = "phuong_clean") %>%
    mutate(
      SoCa = ifelse(is.na(SoCa), 0, SoCa),
      text_tooltip = paste0(
        "<b>", Phuong_Display, "</b><br>",
        "Số ca: ", SoCa
      )
    )
  
  map_main    <- map_final %>% filter(phuong_clean != "đặc khu côn đảo")
  map_condao <- map_final %>% filter(phuong_clean == "đặc khu côn đảo")
  
  lims <- range(map_final$SoCa, na.rm = TRUE)
  
  # --- 2. Tạo ggplot ---
  p_main <- ggplot(map_main) +
    geom_sf(aes(fill = SoCa, text = text_tooltip), size = 0.1, color = "grey50") +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = lims, name = "Số ca") +
    theme_minimal() +
    theme(axis.title = element_blank(), legend.position = "bottom")
  
  p_inset <- ggplot(map_condao) +
    geom_sf(aes(fill = SoCa, text = text_tooltip), size = 0.1, color = "black") +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = lims) +
    theme_void() 
  
  ply_main  <- ggplotly(p_main, tooltip = "text")
  ply_inset <- ggplotly(p_inset, tooltip = "text")
  
  final_map <- subplot(ply_main, ply_inset) %>% 
    layout(
      # Map Chính
      xaxis = list(domain = c(0, 1), title = ""),
      yaxis = list(domain = c(0, 1), title = ""),
      
      # Map Phụ: 
      xaxis2 = list(
        domain = c(0.55, 0.75), 
        anchor = "y2"
      ),
      yaxis2 = list(
        domain = c(0.45, 0.65), 
        anchor = "x2"
      ),
      # Annotation: 
      annotations = list(
        list(
          x = 0.7, y = 0.75, 
          text = "Côn Đảo",
          showarrow = FALSE,
          xref = 'paper', yref = 'paper',
          font = list(size = 12, color = "black")
        )
      ),
      
      legend = list(orientation = "h", x = 0.4, y = -0.05)
    )
  
  return(final_map)
}





