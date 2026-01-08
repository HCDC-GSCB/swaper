calculate_metrics <- function(data, w_report, diagnosis) {
  
  df_trend <- data %>%
    filter(ChanDoanChinhName == diagnosis) %>% 
    filter(week <= w_report) %>%
    group_by(week) %>%
    summarise(
      All_Ca = n(),
      NoiTru = sum(HinhThucDieuTriName == "Điều trị nội trú", na.rm = TRUE),
      NgoaiTru = sum(HinhThucDieuTriName == "Điều trị ngoại trú", na.rm = TRUE),
      TuVong = sum(HinhThucDieuTriName == "Tử vong", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(week = 1:w_report, fill = list(All_Ca = 0, NoiTru = 0, NgoaiTru = 0, TuVong = 0)) %>%
    arrange(week)
  
  current_week_data <- df_trend %>% filter(week == w_report)

  if(nrow(current_week_data) == 0) {
    val_total <- 0; val_inp <- 0; val_outp <- 0
  } else {
    val_total <- current_week_data$All_Ca
    val_inp   <- current_week_data$NoiTru
    val_outp  <- current_week_data$NgoaiTru
  }
  
  val_death_cum <- sum(df_trend$TuVong, na.rm = TRUE)
  
  return(list(
    val_cur_total = val_total,
    val_cur_inp   = val_inp,
    val_cur_outp  = val_outp,
    val_cum_death = val_death_cum,

    spark_total   = df_trend$All_Ca,
    spark_inp     = df_trend$NoiTru,
    spark_outp    = df_trend$NgoaiTru,
    spark_death   = df_trend$TuVong
  ))
}





