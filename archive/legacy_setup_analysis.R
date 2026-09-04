concerts <- tibble::tibble(
  artist = c("Taylor Swift", "Taylor Swift", "Beyoncé", "Beyoncé",
             "Ed Sheeran", "Ed Sheeran", "Bad Bunny", "Bad Bunny"),
  city = c("Atlanta", "New_York", "New_York", "Chicago",
           "New_York", "Chicago", "New_York", "Las_Vegas"),
  start_date = c("2023-04-01", "2023-05-10", "2023-06-15", "2023-07-01",
                 "2023-08-10", "2023-08-20", "2023-09-01", "2023-09-10"),
  end_date   = c("2023-04-03", "2023-05-12", "2023-06-17", "2023-07-03",
                 "2023-08-12", "2023-08-22", "2023-09-03", "2023-09-12"),
  df_prefix = c("Atlanta", "New_York", "New_York", "Chicago",
                "New_York", "Chicago", "New_York", "Las_Vegas")
)

concerts$year <- format(as.Date(concerts$start_date), "%Y")  # 自动加上年份列






artist_summary <- list()

for (i in 1:nrow(concerts)) {
  row <- concerts[i, ]
  city <- row$city
  artist <- row$artist
  start <- as.Date(row$start_date)
  end <- as.Date(row$end_date)
  df_prefix <- row$df_prefix
  year <- as.character(row$year)  # 转为字符以匹配数据名
  
  df_name <- paste0("Market_Daily_", df_prefix, "_", year)
  
  if (!exists(df_name)) {
    warning(paste("数据不存在:", df_name))
    next
  }
  
  df <- get(df_name)
  
  period_data <- df %>%
    filter(Date >= start & Date <= end) %>%
    mutate(
      Artist = artist,
      City = city,
      OCC_PC = `Rooms OCC PC`,
      ADR_PC = `Rooms ADR PC`,
      RevPAR_PC = `Rooms RevPAR PC`
    ) %>%
    select(Artist, City, Date, OCC_PC, ADR_PC, RevPAR_PC)
  
  artist_summary[[i]] <- period_data
}

artist_df <- bind_rows(artist_summary)





library(tidyr)
artist_city_means <- artist_df %>%
  group_by(Artist, City) %>%
  summarise(
    OCC_PC = mean(OCC_PC, na.rm = TRUE),
    ADR_PC = mean(ADR_PC, na.rm = TRUE),
    RevPAR_PC = mean(RevPAR_PC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(OCC_PC, ADR_PC, RevPAR_PC),
               names_to = "Metric",
               values_to = "Avg_PC")





library(ggplot2)

for (artist in unique(artist_city_means$Artist)) {
  df_plot <- filter(artist_city_means, Artist == artist)
  
  p <- ggplot(df_plot, aes(x = City, y = Avg_PC, fill = Metric)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
    labs(
      title = paste("Average % Change During", artist, "Concerts"),
      x = "City",
      y = "Compared to Previous Year"
    ) +
    theme_minimal(base_size = 13) +  # 设置基础字体大小
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),  # 标题大一点
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 12, angle = 15, hjust = 1),
      axis.text.y = element_text(size = 12),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12)
    )
  
  print(p)
}


