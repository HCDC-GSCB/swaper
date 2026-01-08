library(readxl)
library(tidyverse)
library(surveillance)
library(googlesheets4)
library(googledrive)
library(patchwork)
library(plotly)
library(sf)
library(gt)
library(DT)
library(htmltools)
library(rmapshaper)

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
    legend = list(orientation = "h", x = 0.5, y = -0.3, 
                  xanchor = "center",
                  yanchor = "bottom")
  )
  
  return(fig)
}

#### ------------------------- ###

## Layout for plotly at characteristics tabset
common_layout <- list(
  legend = list(
    orientation = "h",   # Nằm ngang
    yanchor = "bottom",  # Neo ở đáy của legend
    y = 1.02,            # Đẩy lên trên biểu đồ một chút
    xanchor = "center",  # Canh giữa
    x = 0.5              # Vị trí giữa trục x
  ),
  xaxis = list(
    title = "Tuần",
    showline = TRUE,     # Hiện đường kẻ trục
    mirror = TRUE,       # Hiện khung bao quanh (tùy chọn, nhìn sẽ gọn hơn)
    linecolor = "black"  # Màu đường kẻ
  ),
  yaxis_count = list(    # Cấu hình cho trục Y đếm số ca
    title = "Số ca",
    showline = TRUE,
    mirror = TRUE,
    linecolor = "black"
  ),
  yaxis_pct = list(      # Cấu hình cho trục Y phần trăm
    title = "Tỷ lệ % số ca",
    showline = TRUE,
    mirror = TRUE,
    linecolor = "black",
    ticksuffix = "%"
  ),
  bargap = 0.2  
)

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

## Format bảng dưới epithreshold
create_beautiful_table <- function(data, title, header_color = "#0073e6") {
  datatable(
    data,
    colnames = c("", "Số ca"), 
    escape = FALSE, 
    caption = htmltools::tags$caption(
      style = paste0("caption-side: top; text-align: center; color: ", header_color, "; font-weight: bold; font-size: 150%;"),
      title
    ),
    options = list(
      dom = 't',
      ordering = FALSE,
      pageLength = 15,
      columnDefs = list(
        list(className = 'dt-center', targets = 1)
      ),
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Vietnamese.json'),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#f8f9fa', 'color': '#333'});",
        "}"
      )
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'ChiTieu',
      target = 'row',
      backgroundColor = styleEqual(
        c("<b>Số ca trong tuần</b>", "<b>So với ngưỡng cảnh báo:</b>", "<b>Đánh giá nguy cơ:</b>"), 
        c('#e9ecef', '#e9ecef', '#e9ecef')
      )
    ) %>%
    formatStyle(
      columns = 1:2,
      fontFamily = "Arial, sans-serif",
      fontSize = "14px",
      padding = "8px"
    ) %>%
    formatStyle(
      'ChiTieu',
      color = "#2c3e50"
    )
}

#################################
########## Phường/Xã ############
#################################
render_epitable <- function(data, cur_w, header_color = "#0056b3", table_caption = NULL) {
  
  df_processed <- data %>%
    arrange(Phuong, Tuan) %>%
    group_by(Phuong) %>%
    mutate(
      SoCa100k = round(SoCa * 100 / DanSo, 2),
      lag_1 = lag(SoCa, 1, default = 0),
      lag_4 = lag(SoCa, 4, default = 0),
      Pct_TuanTruoc = case_when(
        lag_1 == 0 & SoCa > 0 ~ 100, 
        lag_1 == 0 & SoCa == 0 ~ 0,
        TRUE ~ (SoCa - lag_1) * 100 / lag_1
      ),
      Pct_4TuanTruoc = case_when(
        lag_4 == 0 & SoCa > 0 ~ 100,
        lag_4 == 0 & SoCa == 0 ~ 0,
        TRUE ~ (SoCa - lag_4) * 100 / lag_4
      ),
      Both_Warn = ifelse(CanhBao_Farrington == 1 & CanhBao_CUSUM == 1, 1, 0),
      is_3_both = (Both_Warn + lag(Both_Warn, 1, default = 0) + lag(Both_Warn, 2, default = 0)) == 3,
      is_3_farr = (CanhBao_Farrington + lag(CanhBao_Farrington, 1, default = 0) + lag(CanhBao_Farrington, 2, default = 0)) == 3,
      is_3_cusum = (CanhBao_CUSUM + lag(CanhBao_CUSUM, 1, default = 0) + lag(CanhBao_CUSUM, 2, default = 0)) == 3
    ) %>%
    ungroup() %>%
    filter(Tuan == cur_w) %>%
    mutate(
      Rank_Priority = case_when(
        is_3_both ~ 1,
        is_3_farr ~ 2,
        is_3_cusum ~ 3,
        TRUE ~ 4
      )
    ) %>%
    arrange(Rank_Priority, Phuong)
  
  df_display <- df_processed %>%
    mutate(
      SoVoiTuanTruoc_HienThi = case_when(
        Pct_TuanTruoc > 0 ~ paste0("<span style='color: #d9534f; font-weight: bold'>&#8593; ", sprintf("%.1f", Pct_TuanTruoc), "%</span>"),
        Pct_TuanTruoc < 0 ~ paste0("<span style='color: #5cb85c; font-weight: bold'>&#8595; ", sprintf("%.1f", abs(Pct_TuanTruoc)), "%</span>"),
        TRUE ~ paste0("<span style='color: gray'>", sprintf("%.1f", Pct_TuanTruoc), "%</span>")
      ),
      SoVoi4TuanTruoc_HienThi = case_when(
        Pct_4TuanTruoc > 0 ~ paste0("<span style='color: #d9534f; font-weight: bold'>&#8593; ", sprintf("%.1f", Pct_4TuanTruoc), "%</span>"),
        Pct_4TuanTruoc < 0 ~ paste0("<span style='color: #5cb85c; font-weight: bold'>&#8595; ", sprintf("%.1f", abs(Pct_4TuanTruoc)), "%</span>"),
        TRUE ~ paste0("<span style='color: gray'>", sprintf("%.1f", Pct_4TuanTruoc), "%</span>")
      ),
      Farrington_HienThi = ifelse(CanhBao_Farrington == 1, "<span style='color: red; font-weight: bold; font-size: 1.2em'>(+)</span>", ""),
      CUSUM_HienThi = ifelse(CanhBao_CUSUM == 1, "<span style='color: red; font-weight: bold; font-size: 1.2em'>(+)</span>", "")
    ) %>%
    select(Phuong, SoCa, SoCa100k, SoVoiTuanTruoc_HienThi, SoVoi4TuanTruoc_HienThi, Farrington_HienThi, CUSUM_HienThi)
  
  datatable(
    df_display,
    colnames = c("Phường/Xã", "Số ca", "Số ca/100k dân", "So với tuần trước", "So với 4 tuần trước", "Cảnh báo Farrington", "Cảnh báo CUSUM"),
    escape = FALSE,
    rownames = FALSE,
    fillContainer = FALSE,
    height = "auto",
    caption = if(!is.null(table_caption)) htmltools::tags$caption(
      style = 'caption-side: top; text-align: left; font-style: italic; font-size: 0.9em; color: #555;', 
      table_caption
    ) else NULL,
    options = list(
      paging = TRUE,        
      pageLength = 10,
      autoWidth = TRUE,
      dom = 'frtip',
      order = list(),
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Vietnamese.json'),
      columnDefs = list(
        list(className = 'dt-left', targets = 0),
        list(className = 'dt-center', targets = 1:6),
        list(width = '15%', targets = c(3, 4))
      ),
      initComplete = JS(
        paste0("function(settings, json) {$(this.api().table().header()).css({'background-color': '", header_color, "', 'color': '#fff'});}")
      )
    ) 
  ) %>%
    formatStyle('Phuong', fontWeight = 'bold')
}

## Map 
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

## Plotly
plot_px_map <- function(
    shp_path,
    px_clean,
    df_kv,
    TUAN_BAO_CAO
) {
  
  if (!dir.exists(shp_path)) return(NULL)
  
  hcm_map_raw <- st_read(shp_path, quiet = TRUE) 

  hcm_map <- ms_simplify(hcm_map_raw, keep = 0.05, keep_shapes = TRUE) %>%
    st_make_valid() %>%             
    st_cast("MULTIPOLYGON") %>%  
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
  
  p_main <- ggplot(map_main) +
    geom_sf(aes(fill = SoCa, text = text_tooltip), size = 0.1, color = "grey50") +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = lims, name = "Số ca") +
    theme_minimal() +
    theme(axis.title = element_blank(), legend.position = "bottom")
  
  p_inset <- ggplot(map_condao) +
    geom_sf(aes(fill = SoCa, text = text_tooltip), size = 0.1, color = "black", show.legend = FALSE) +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = lims) +
    theme_void() 
  
  ply_main  <- ggplotly(p_main, tooltip = "text")
  ply_inset <- ggplotly(p_inset, tooltip = "text")
  
  final_map <- subplot(ply_main, ply_inset) %>% 
    layout(
      xaxis = list(domain = c(0, 1), title = ""),
      yaxis = list(domain = c(0, 1), title = ""),
      xaxis2 = list(domain = c(0.55, 0.75), anchor = "y2"),
      yaxis2 = list(domain = c(0.45, 0.65), anchor = "x2"),
      annotations = list(
        list(x = 0.7, y = 0.75, text = "Côn Đảo", showarrow = FALSE, xref = 'paper', yref = 'paper', font = list(size = 12, color = "black"))
      ),
      legend = list(orientation = "h", x = 0.4, y = -0.05)
    ) %>%
    partial_bundle()
  
  return(final_map)
}




