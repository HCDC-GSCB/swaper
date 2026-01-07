library(tidyverse)
library(plotly)
library(crosstalk) # Thư viện cần thiết cho interactive-plots 
library(bsicons)
library(bslib)
library(lubridate)
library(summarywidget)

df <- readRDS("tuan51.rds")
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


out_dengue <- create_disease_dashboard(df, "Sốt xuất huyết Dengue", 
                                       w_cur = 51, common_layout)
sd_dengue <- out_dengue$sd

out_hfmd <- create_disease_dashboard(df, "Tay - chân - miệng", 
                                     w_cur = 51, common_layout)
sd_hfmd <- out_hfmd$sd

