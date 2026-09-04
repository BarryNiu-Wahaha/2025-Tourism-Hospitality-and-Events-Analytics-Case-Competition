library(dplyr)
library(ggplot2)
library(lubridate)

seasonal_data <- market_data %>%
  mutate(
    Season = case_when(
      month(Date) %in% c(3, 4, 5) ~ "Spring",
      month(Date) %in% c(6, 7, 8) ~ "Summer",
      month(Date) %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ "Winter"
    )
  )

ggplot(seasonal_data, aes(x = Season, y = `Rooms Demand`)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Seasonal Variation in Hotel Room Demand",
    x = "Season",
    y = "Room Demand"
  ) +
  theme_minimal()

summary(aov(`Rooms Demand` ~ Season, data = seasonal_data))