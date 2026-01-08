library(tidyverse)
library(plotly)
library(bsicons)
library(bslib)
library(lubridate)
library(summarywidget)
library(sparkline)

source("functions_characteristics.R")

df <- readRDS("data/tuan52.rds")
df <- df %>% 
  mutate(week = isoweek(NgayNhapVien),
         age_month = floor(time_length(interval(dmy(NgaySinh), NgayNhapVien), "month")),
         age_group = case_when(
           age_month<12 ~ "<1 tuổi",
           age_month>=12 & age_month<=60 ~ "1-5 tuổi",     
           age_month>60 & age_month<=120 ~ "6-10 tuổi",
           age_month>120 & age_month<=180 ~ "11-15 tuổi",
           TRUE ~ ">15 tuổi"
         ),
         age_group = factor(age_group, levels = c("<1 tuổi", "1-5 tuổi", "6-10 tuổi", "11-15 tuổi", ">15 tuổi")))

cur_w <- 51

## Dengue
metrics_sxh <- calculate_metrics(df, cur_w, "Sốt xuất huyết Dengue")

sxh1 <- value_box(
  title = paste0("Số ca mắc tuần ", cur_w),
  value = metrics_sxh$val_cur_total,
  showcase = sparkline(metrics_sxh$spark_total, type = "line", lineColor = "white", fillColor = FALSE, width = "100%", height = "100%"),
  showcase_layout = showcase_bottom(),
  theme = value_box_theme(bg = "#0d6efd", fg = "white")
)

sxh2 <- value_box(
  title = paste0("Số nội trú tuần ", cur_w),
  value = metrics_sxh$val_cur_inp,
  showcase = sparkline(metrics_sxh$spark_inp, type = "line", lineColor = "white", fillColor = FALSE, width = "100%", height = "100%"),
  showcase_layout = showcase_bottom(),
  theme = value_box_theme(bg = "#6610f2", fg = "white")
)

sxh3 <- value_box(
  title = paste0("Số ngoại trú tuần ", cur_w),
  value = metrics_sxh$val_cur_outp,
  showcase = sparkline(metrics_sxh$spark_outp, type = "line", lineColor = "white", fillColor = "rgba(255,255,255,0.3)", width = "100%", height = "100%"),
  showcase_layout = showcase_bottom(),
  theme = value_box_theme(bg = "#198754", fg = "white")
)

sxh4 <- value_box(
  title = paste0("Tử vong tích lũy đến tuần ", cur_w),
  value = metrics_sxh$val_cum_death,
  showcase = sparkline(metrics_sxh$spark_death, type = "line", lineColor = "white", fillColor = FALSE, width = "100%", height = "100%"),
  showcase_layout = showcase_bottom(),
  theme = value_box_theme(bg = "#dc3545", fg = "white")
)

## HFMD
metrics_tcm <- calculate_metrics(df, cur_w, "Tay - chân - miệng")

tcm1 <- value_box(
  title = paste0("Số ca mắc tuần ", cur_w),
  value = metrics_tcm$val_cur_total,
  showcase = sparkline(metrics_tcm$spark_total, type = "line", lineColor = "white", fillColor = FALSE, width = "100%", height = "100%"),
  showcase_layout = showcase_bottom(),
  theme = value_box_theme(bg = "#0d6efd", fg = "white")
)

tcm2 <- value_box(
  title = paste0("Số nội trú tuần ", cur_w),
  value = metrics_tcm$val_cur_inp,
  showcase = sparkline(metrics_tcm$spark_inp, type = "line", lineColor = "white", fillColor = FALSE, width = "100%", height = "100%"),
  showcase_layout = showcase_bottom(),
  theme = value_box_theme(bg = "#6610f2", fg = "white")
)

tcm3 <- value_box(
  title = paste0("Số ngoại trú tuần ", cur_w),
  value = metrics_tcm$val_cur_outp,
  showcase = sparkline(metrics_tcm$spark_outp, type = "line", lineColor = "white", fillColor = "rgba(255,255,255,0.3)", width = "100%", height = "100%"),
  showcase_layout = showcase_bottom(),
  theme = value_box_theme(bg = "#198754", fg = "white")
)

tcm4 <- value_box(
  title = paste0("Tử vong tích lũy đến tuần ", cur_w),
  value = metrics_tcm$val_cum_death,
  showcase = sparkline(metrics_tcm$spark_death, type = "line", lineColor = "white", fillColor = FALSE, width = "100%", height = "100%"),
  showcase_layout = showcase_bottom(),
  theme = value_box_theme(bg = "#dc3545", fg = "white")
)



