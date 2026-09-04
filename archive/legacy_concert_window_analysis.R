

# 封装画图函数
plot_concert_comparison <- function(df_2022, df_2023, city, artist, start_date, end_date, metric, save_path = NULL) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  window_days <- 7
  
  # 取前后7天窗口
  window_start <- start_date - window_days
  window_end <- end_date + window_days
  window_start_2022 <- window_start - 365
  window_end_2022 <- window_end - 365
  
  # 提取该时间段数据
  data_2023 <- df_2023 %>%
    filter(Date >= window_start & Date <= window_end)
  
  data_2022 <- df_2022 %>%
    filter(Date >= window_start_2022 & Date <= window_end_2022)
  
  if (nrow(data_2023) == 0 || nrow(data_2022) == 0) return(NULL)
  
  # 添加相对天数和年份
  data_2023 <- data_2023 %>%
    mutate(Relative_Day = as.numeric(Date - start_date), Year = "2023")
  
  data_2022 <- data_2022 %>%
    mutate(Relative_Day = as.numeric(Date - (start_date - 365)), Year = "2022")
  
  # 合并绘图
  plot_df <- bind_rows(
    data_2023 %>% select(Relative_Day, Value = all_of(metric), Year),
    data_2022 %>% select(Relative_Day, Value = all_of(metric), Year)
  )
  
  p <- ggplot(plot_df, aes(x = Relative_Day, y = Value, color = Year, linetype = Year)) +
    geom_line(size = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = paste(metric, "-", artist, "(", city, format(start_date, "%Y-%m-%d"), ")"),
      x = "Days Relative to Concert Start",
      y = metric
    ) +
    theme_minimal()
  
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = 10, height = 5)
  } else {
    print(p)
  }
}

plot_concert_comparison <- function(df_2022, df_2023, city, artist, start_date, end_date, metric, save_path = NULL) {
  library(ggplot2)
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  window_days <- 7
  
  window_start <- start_date - window_days
  window_end <- end_date + window_days
  window_start_2022 <- window_start - 365
  window_end_2022 <- window_end - 365
  
  # 提取窗口数据（允许为空）
  data_2023 <- df_2023 %>%
    filter(Date >= window_start & Date <= window_end) %>%
    mutate(Relative_Day = as.numeric(Date - start_date), Year = "2023")
  
  data_2022 <- df_2022 %>%
    filter(Date >= window_start_2022 & Date <= window_end_2022) %>%
    mutate(Relative_Day = as.numeric(Date - (start_date - 365)), Year = "2022")
  
  # 处理数据缺失情况（如果没有该列，创建空列）
  if (!(metric %in% colnames(data_2023))) {
    data_2023[[metric]] <- NA
  }
  if (!(metric %in% colnames(data_2022))) {
    data_2022[[metric]] <- NA
  }
  
  # 合并
  plot_df <- bind_rows(
    data_2023 %>% select(Relative_Day, Value = all_of(metric), Year),
    data_2022 %>% select(Relative_Day, Value = all_of(metric), Year)
  )
  
  # 生成画布
  p <- ggplot(plot_df, aes(x = Relative_Day, y = Value, color = Year, linetype = Year)) +
    geom_line(size = 1.2, na.rm = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = paste(metric, "-", artist, "(", city, format(start_date, "%Y-%m-%d"), ")"),
      x = "Days Relative to Concert Start",
      y = metric
    ) +
    theme_minimal()
  
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = 10, height = 5)
  } else {
    print(p)
  }
}


library(purrr)

concerts <- tribble(
  ~artist, ~city, ~start_date, ~end_date, ~df_prefix, ~year,
  "Taylor Swift", "Atlanta, GA", "2023-04-28", "2023-04-30", "Atlanta", 2023,
  "Taylor Swift", "Los Angeles, CA", "2023-08-03", "2023-08-05", "Los_Angeles", 2023,
  "Beyoncé", "Chicago, IL", "2023-07-22", "2023-07-23", "Chicago", 2023,
  "Ed Sheeran", "New York, NY", "2023-06-10", "2023-06-11", "New_York", 2023,
  "Bad Bunny", "Las Vegas, NV", "2022-09-23", "2022-09-24", "Las_Vegas", 2022,
  "Bad Bunny", "New York, NY", "2022-08-27", "2022-08-28", "New_York", 2022,
  "Lollapalooza", "Chicago, IL", "2023-08-03", "2023-08-06", "Chicago", 2023,
  "EDC Las Vegas", "Las Vegas, NV", "2023-05-19", "2023-05-21", "Las_Vegas", 2023,
  "Ed Sheeran", "Chicago, IL", "2023-06-29", "2023-07-01", "Chicago", 2023,
  "Beyoncé", "Los Angeles, CA", "2023-09-02", "2023-09-03", "Los_Angeles", 2023
)


metrics <- c("Rooms OCC", "Rooms ADR", "Rooms Revenue")

walk(metrics, function(metric) {
  pwalk(concerts, function(artist, city, start_date, end_date, df_prefix, year) {
    # 如果是2022年，没有对比年份（跳过 df_2021）
    if (year == 2022) {
      df_main <- get(paste0("Market_Daily_", df_prefix, "_2022"))
      df_comp <- df_main[0, ]  # 空数据框，代表“无对比”
    } else {
      df_main <- get(paste0("Market_Daily_", df_prefix, "_2023"))
      df_comp <- get(paste0("Market_Daily_", df_prefix, "_2022"))
    }
    
    plot_concert_comparison(
      df_2022 = if (year == 2022) df_main else df_comp,
      df_2023 = if (year == 2023) df_main else df_comp,
      city = city,
      artist = artist,
      start_date = start_date,
      end_date = end_date,
      metric = metric,
      save_path = paste0("plots/",
                         gsub(" ", "_", artist), "_",
                         gsub(" ", "_", df_prefix), "_",
                         gsub(" ", "_", metric), ".png")
    )
  })
})



length(list.files("plots", pattern = "\\.png$"))

# 📁 plots 文件夹保存图像
dir.create("plots", showWarnings = FALSE)

# 📌 1. 函数定义
plot_revpar_comparison <- function(df_main, df_compare, concert_start, concert_end, city, artist, main_year, compare_year, save_path = NULL) {
  concert_start <- as.Date(concert_start)
  concert_end <- as.Date(concert_end)
  window_days <- 7
  
  window_start <- concert_start - window_days
  window_end <- concert_end + window_days
  window_start_compare <- window_start - 365
  window_end_compare <- window_end - 365
  
  df_main_filtered <- df_main %>%
    filter(Date >= window_start & Date <= window_end) %>%
    mutate(Relative_Day = as.numeric(Date - concert_start),
           Year = as.character(main_year),
           RevPAR = `Rooms RevPAR`)
  
  df_compare_filtered <- df_compare %>%
    filter(Date >= window_start_compare & Date <= window_end_compare) %>%
    mutate(Relative_Day = as.numeric(Date - concert_start + 365),
           Year = as.character(compare_year),
           RevPAR = `Rooms RevPAR`)
  
  plot_df <- bind_rows(
    df_main_filtered %>% select(Relative_Day, RevPAR, Year),
    df_compare_filtered %>% select(Relative_Day, RevPAR, Year)
  )
  
  p <- ggplot(plot_df, aes(x = Relative_Day, y = RevPAR, color = Year, linetype = Year)) +
    geom_line(size = 1.2, na.rm = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = paste("RevPAR Trend:", artist, "-", city),
      x = "Days Relative to Concert Start",
      y = "RevPAR ($)"
    ) +
    theme_minimal()
  
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = 10, height = 5)
  } else {
    print(p)
  }
}

# 📌 2. 演唱会表格（你可以调整）
concerts <- tribble(
  ~artist, ~city, ~start_date, ~end_date, ~df_prefix,
  "Taylor Swift", "Atlanta, GA", "2023-04-28", "2023-04-30", "Atlanta",
  "Taylor Swift", "Los Angeles, CA", "2023-08-03", "2023-08-05", "Los_Angeles",
  "Beyoncé", "Chicago, IL", "2023-07-22", "2023-07-23", "Chicago",
  "Beyoncé", "Los Angeles, CA", "2023-09-02", "2023-09-03", "Los_Angeles",
  "Ed Sheeran", "Chicago, IL", "2023-06-29", "2023-07-01", "Chicago",
  "Ed Sheeran", "New York, NY", "2023-06-10", "2023-06-11", "New_York",
  "Lollapalooza", "Chicago, IL", "2023-08-03", "2023-08-06", "Chicago",
  "EDC Las Vegas", "Las Vegas, NV", "2023-05-19", "2023-05-21", "Las_Vegas"
)

# 📌 3. 批量输出图
library(purrr)
pwalk(concerts, function(artist, city, start_date, end_date, df_prefix) {
  start_date <- as.Date(start_date)
  df_main <- get(paste0("Market_Daily_", df_prefix, "_2023"))
  df_compare <- get(paste0("Market_Daily_", df_prefix, "_2022"))
  
  save_path <- paste0("plots/",
                      gsub(" ", "_", artist), "_",
                      gsub(" ", "_", df_prefix), "_RevPAR.png")
  
  plot_revpar_comparison(
    df_main = df_main,
    df_compare = df_compare,
    concert_start = start_date,
    concert_end = as.Date(end_date),
    city = city,
    artist = artist,
    main_year = 2023,
    compare_year = 2022,
    save_path = save_path
  )
})
