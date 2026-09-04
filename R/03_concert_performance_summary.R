library(dplyr)
library(tidyr)
library(ggplot2)

performance_metrics <- c("OCC_PC", "ADR_PC", "RevPAR_PC")

concert_data <- bind_rows(lapply(seq_len(nrow(concerts)), function(index) {
  concert <- concerts[index, ]

  market_data %>%
    filter(
      City == concert$City,
      Date >= concert$Start_Date,
      Date <= concert$End_Date
    ) %>%
    transmute(
      Artist = concert$Artist,
      City,
      Date,
      OCC_PC = `Rooms OCC PC`,
      ADR_PC = `Rooms ADR PC`,
      RevPAR_PC = `Rooms RevPAR PC`
    )
}))

summary_data <- concert_data %>%
  group_by(Artist, City) %>%
  summarise(
    across(all_of(performance_metrics), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = all_of(performance_metrics),
    names_to = "Metric",
    values_to = "Average_Percentage_Change"
  )

dir.create("results/figures/summaries", recursive = TRUE, showWarnings = FALSE)

summary_plot <- ggplot(
  summary_data,
  aes(x = City, y = Average_Percentage_Change, fill = Metric)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  facet_wrap(~ Artist, scales = "free_x") +
  labs(
    title = "Hotel Performance Changes During Concerts",
    x = "City",
    y = "Average Percentage Change Compared with Previous Year"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(summary_plot)
ggsave(
  "results/figures/summaries/concert_performance_summary.png",
  summary_plot,
  width = 12,
  height = 8
)