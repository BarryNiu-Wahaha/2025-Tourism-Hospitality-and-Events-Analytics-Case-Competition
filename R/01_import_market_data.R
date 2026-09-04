library(readr)
library(dplyr)

market_data <- read_csv(
  "data/raw/Market_Daily_Exported.csv",
  show_col_types = FALSE
) %>%
  rename(City = `Markets - Market`) %>%
  mutate(
    Date = as.Date(Date),
    City = sub(", .*$", "", City),
    Year = as.integer(format(Date, "%Y"))
  )

concert_window_data <- read_csv(
  "data/raw/Market_Daily_SixGroup_Cleaned.csv",
  show_col_types = FALSE
) %>%
  rename(City = `Markets - Market`) %>%
  mutate(
    Date = as.Date(Date),
    City = sub(", .*$", "", City)
  ) %>%
  select(Date, City, Concert_Window, Season, Six_Group)

market_data <- market_data %>%
  left_join(concert_window_data, by = c("Date", "City"))