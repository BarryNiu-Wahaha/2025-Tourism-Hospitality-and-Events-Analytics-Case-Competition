library(tibble)

concerts <- tribble(
  ~Artist, ~City, ~Start_Date, ~End_Date,
  "Taylor Swift", "Atlanta", "2023-04-28", "2023-04-30",
  "Taylor Swift", "Los Angeles", "2023-08-03", "2023-08-05",
  "Beyonce", "Chicago", "2023-07-22", "2023-07-23",
  "Beyonce", "Los Angeles", "2023-09-02", "2023-09-03",
  "Ed Sheeran", "Chicago", "2023-06-29", "2023-07-01",
  "Ed Sheeran", "New York", "2023-06-10", "2023-06-11",
  "Lollapalooza", "Chicago", "2023-08-03", "2023-08-06",
  "EDC Las Vegas", "Las Vegas", "2023-05-19", "2023-05-21"
) %>%
  mutate(
    Start_Date = as.Date(Start_Date),
    End_Date = as.Date(End_Date),
    Year = as.integer(format(Start_Date, "%Y"))
  )