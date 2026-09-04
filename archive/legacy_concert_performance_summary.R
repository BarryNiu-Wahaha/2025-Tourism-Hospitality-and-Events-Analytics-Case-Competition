library(dplyr)
library(tidyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

performance_metrics <- c("OCC_PC", "ADR_PC", "RevPAR_PC")

artist_city_expected <- list(
  "Taylor Swift" = c("Atlanta", "New_York"),
  "Beyoncé" = c("New_York", "Chicago"),
  "Ed Sheeran" = c("New_York", "Chicago"),
  "Bad Bunny" = c("New_York", "Las_Vegas")
)

# -----------------------------------------------------------------------------
# Extract performance changes for each concert
# -----------------------------------------------------------------------------

concert_periods <- lapply(seq_len(nrow(concerts)), function(index) {
  concert <- concerts[index, ]
  data_name <- paste0("Market_Daily_", concert$df_prefix, "_", concert$year)

  if (!exists(data_name, inherits = TRUE)) {
    warning("Skipping missing data frame: ", data_name)
    return(NULL)
  }

  get(data_name, inherits = TRUE) %>%
    filter(
      Date >= as.Date(concert$start_date),
      Date <= as.Date(concert$end_date)
    ) %>%
    transmute(
      Artist = concert$artist,
      City = concert$city,
      Date,
      OCC_PC = `Rooms OCC PC`,
      ADR_PC = `Rooms ADR PC`,
      RevPAR_PC = `Rooms RevPAR PC`
    )
})

artist_df <- bind_rows(concert_periods)

# -----------------------------------------------------------------------------
# City-level overview
# -----------------------------------------------------------------------------

city_means <- artist_df %>%
  group_by(City) %>%
  summarise(
    across(all_of(performance_metrics), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = all_of(performance_metrics),
    names_to = "Metric",
    values_to = "Avg_PC"
  )

ggplot(city_means, aes(x = City, y = Avg_PC, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, color = "gray40", linetype = "dashed") +
  labs(
    title = "Average Percentage Change During Concerts by City",
    x = "City",
    y = "Average Percentage Change Compared with Previous Year"
  ) +
  theme_minimal()

# -----------------------------------------------------------------------------
# Artist-level summaries and plots
# -----------------------------------------------------------------------------

artist_city_means <- artist_df %>%
  filter(Artist %in% names(artist_city_expected)) %>%
  group_by(Artist, City) %>%
  summarise(
    across(all_of(performance_metrics), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = all_of(performance_metrics),
    names_to = "Metric",
    values_to = "Avg_PC"
  )

city_check <- artist_city_means %>%
  distinct(Artist, City) %>%
  group_by(Artist) %>%
  summarise(Cities = paste(City, collapse = ", "), .groups = "drop")

print(city_check)

for (artist in names(artist_city_expected)) {
  plot_data <- filter(artist_city_means, Artist == artist)

  artist_plot <- ggplot(plot_data, aes(x = City, y = Avg_PC, fill = Metric)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
    labs(
      title = paste("Average Percentage Change in Hotel Performance During", artist, "Concerts"),
      x = "City",
      y = "Percentage Change Compared with Previous Year"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 15, hjust = 1)
    )

  print(artist_plot)
}




