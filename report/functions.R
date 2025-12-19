library(readxl)
library(tidyverse)
library(surveillance)
library(googlesheets4)
library(googledrive)
library(patchwork)
library(DT)

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

## Theme
theme_an <- function() {
  theme_classic() +
    theme(
      axis.text.x = element_text(family = "Times New Roman", size = 11, color = "black"),
      axis.title.x = element_text(family = "Times New Roman", size = 13, color = "black", face = "bold"),
      axis.text.y = element_text(family = "Times New Roman", size = 11, color = "black"),
      axis.title.y = element_text(family = "Times New Roman", size = 13, color = "black", face = "bold"),
      plot.title = element_text(family = "Times New Roman", size = 16, color = "black", face = "bold", hjust = 0.5),
      legend.text = element_text(family = "Times New Roman", size = 12, color = "black"),
      legend.title = element_text(family = "Times New Roman", size = 13, color = "black", face = "bold"),
      legend.position = "bottom",
      panel.grid = element_blank()
    )
}


## Plot 
plot_algo_dual <- function(df, farrington, cusum, year_target, 
                           week_target, seasonal, ref_years) {
  
  df_target <- df %>% filter(year == year_target & week <= week_target) %>% 
    select(week, cases, cdc, outbreak_cdc)
  
  df_target$farrington <- farrington$upperbound[1:week_target,]
  df_target$outbreak_f <- farrington$alarm[1:week_target,]
  
  df_target$cusum <- cusum$upperbound[1:week_target]
  df_target$outbreak_c <- cusum$alarm[1:week_target]
  
  y_max <- max(c(df_target$cdc, df_target$farrington, df_target$cusum, df_target$cases, seasonal), na.rm = TRUE)
 
  p <- ggplot() + 
    
    geom_col(data = df_target, aes(x = week, y = cases),
             fill = "steelblue", color = "black", alpha = 0.4, size = 0.2) +
    
    geom_line(data = df_target, aes(x = week, y = farrington), color = "#E41A1C", size = 1) +
    geom_line(data = df_target, aes(x = week, y = cdc, color = "Mean+2SD"), , size = 1) +
    geom_hline(aes(yintercept = seasonal, color = "Ngưỡng mùa"), 
               linetype = "dashed", size = 1) +
    
    geom_point(data = filter(df_target, outbreak_f == 1),
               aes(x = week, y = cases/2, color = "Farrington"),
               size = 3, shape = 17, stroke = 1.2) +
    geom_point(data = filter(df_target, outbreak_c == 1),
               aes(x = week, y = cases/4, color = "CUSUM"),
               size = 3, shape = 17, stroke = 1.2) +
    geom_point(data = filter(df_target, outbreak_cdc == 1),
               aes(x = week, y = cases), color = "#4DAF4A",
               size = 3, shape = 17, stroke = 1.2) +
    
    scale_x_continuous(breaks = seq(1, week_target, 4)) +
    scale_y_continuous(limits = c(0, y_max),
                       breaks = seq(0,y_max,500),
                       expand = c(0,0)) +
    
    labs(
      x = "Tuần", 
      y = "Số ca bệnh"
      # caption = paste("Năm lịch sử:", paste(ref_years, collapse = ", "))
    ) +
    
    scale_color_manual(
      name = "",
      values = c(
        "Farrington" = "#E41A1C",
        "CUSUM" = "orange",
        "Mean+2SD" = "#4DAF4A",
        "Ngưỡng mùa" = "black"
      )
    ) +
    theme_an() +
    theme(
      plot.caption = element_text(hjust = 0, family = "Times New Roman", size = 10, color = "black")
    )
  
  return(p)
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
  # 1. Định nghĩa màu nền (Background) - Tông màu khoa học, đậm
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


