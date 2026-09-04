library(dplyr)
library(ggplot2)

seasonal_concert_data <- market_data %>%
  mutate(
    Season_Type = if_else(Season == "Winter", "Low", "High"),
    Group = case_when(
      Season_Type == "High" & Concert_Window ~ "High + Concert",
      Season_Type == "High" & !Concert_Window ~ "High + No Concert",
      Season_Type == "Low" & Concert_Window ~ "Low + Concert",
      TRUE ~ "Low + No Concert"
    )
  )

ggplot(seasonal_concert_data, aes(x = Group, y = `Rooms RevPAR`, fill = Group)) +
  geom_boxplot() +
  labs(
    title = "RevPAR by Season and Concert Window",
    x = NULL,
    y = "Rooms RevPAR"
  ) +
  theme_minimal()

summary(aov(`Rooms RevPAR` ~ Group, data = seasonal_concert_data))