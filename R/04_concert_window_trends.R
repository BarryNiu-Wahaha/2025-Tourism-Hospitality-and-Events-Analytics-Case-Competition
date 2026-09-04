library(dplyr)
library(ggplot2)

window_days <- 7

plot_concert_window <- function(market_data, concert, metric) {
  window_start <- concert$Start_Date - window_days
  window_end <- concert$End_Date + window_days

  plot_data <- market_data %>%
    filter(
      .data$City == concert$City,
      .data$Date >= window_start,
      .data$Date <= window_end
    ) %>%
    mutate(Relative_Day = as.numeric(.data$Date - concert$Start_Date))

  ggplot(plot_data, aes(x = .data$Relative_Day, y = .data[[metric]])) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    labs(
      title = paste(metric, "Around", concert$Artist, "Concerts"),
      subtitle = paste(concert$City, format(concert$Start_Date, "%Y-%m-%d")),
      x = "Days Relative to Concert Start",
      y = metric
    ) +
    theme_minimal()
}

dir.create("results/figures/concert_windows", recursive = TRUE, showWarnings = FALSE)

for (index in seq_len(nrow(concerts))) {
  concert <- concerts[index, ]

  for (metric in c("Rooms OCC", "Rooms ADR", "Rooms Revenue")) {
    plot <- plot_concert_window(market_data, concert, metric)
    file_name <- paste(
      gsub(" ", "_", concert$Artist),
      gsub(" ", "_", concert$City),
      gsub(" ", "_", metric),
      sep = "_"
    )

    ggsave(
      file.path("results/figures/concert_windows", paste0(file_name, ".png")),
      plot,
      width = 10,
      height = 5
    )
  }
}