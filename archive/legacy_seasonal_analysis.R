library(dplyr)
library(lubridate)
library("tidyverse")
# 创建季度变量
Market_Daily$Quarter <- ifelse(month(Market_Daily$Date) %in% c(3,4,5), "Spring",
                               ifelse(month(Market_Daily$Date) %in% c(6,7,8), "Summer",
                                      ifelse(month(Market_Daily$Date) %in% c(9,10,11), "Fall", "Winter")))
summary(aov(`Rooms Demand` ~ Quarter, data = Market_Daily))

library(ggplot2)

ggplot(Market_Daily, aes(x = Quarter, y = `Rooms Demand`)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 0)), 
               vjust = -1, color = "red", size = 3.5) +
  labs(title = "Seasonal Variation in Hotel Room Demand",
       x = "Season",
       y = "Room Demand") +
  theme_minimal()

# 基于已有 Season 列（值为 "spring", "summer", "fall", "winter"）
Market_Daily$Season_Type <- ifelse(
  Market_Daily$Quarter == "Winter", "Low", "High"
)

Market_Daily$Group <- case_when(
  Market_Daily$Season_Type == "High" & Market_Daily$Concert_Window ~ "A. High + Concert",
  Market_Daily$Season_Type == "High" & !Market_Daily$Concert_Window ~ "B. High + No Concert",
  Market_Daily$Season_Type == "Low" & Market_Daily$Concert_Window ~ "C. Low + Concert",
  Market_Daily$Season_Type == "Low" & !Market_Daily$Concert_Window ~ "D. Low + No Concert"
)
ggplot(Market_Daily, aes(x = Group, y = `Rooms RevPAR`, fill = Group)) +
  geom_boxplot() +
  labs(title = "RevPAR by Season and Concert", y = "Rooms RevPAR", x = NULL) +
  theme_minimal()

# Rooms Demand 箱线图
ggplot(Market_Daily, aes(x = Group, y = `Rooms Demand`, fill = Group)) +
  geom_boxplot() +
  labs(title = "Rooms Demand by Season and Concert", y = "Rooms Demand", x = NULL) +
  theme_minimal()