Market_Daily$Date <- as.Date(as.character(Market_Daily$Date), format = "%Y%m%d")



ggplot(springwinterfall, aes(x = Six_Group, y = `Rooms RevPAR`, fill = Six_Group)) +
  geom_boxplot() +
  labs(title = "RevPAR by Season and Concert Window", x = NULL, y = "RevPAR") +
  theme_minimal()

summary(aov(`Rooms RevPAR` ~ Six_Group, data = springwinterfall))
summary(aov(`Rooms Demand` ~ Six_Group, data = springwinterfall))

mean_values <- df %>%
  group_by(Six_Group) %>%
  summarise(mean_revpar = mean(`Rooms RevPAR`, na.rm = TRUE)) %>%
  ungroup()

# 绘制箱线图 + 红色均值点 + 均值数字
ggplot(df, aes(x = Six_Group, y = `Rooms RevPAR`, fill = Six_Group)) +
  geom_boxplot(outlier.shape = NA) +  # 不画离群点，避免遮挡
  geom_point(data = mean_values, aes(x = Six_Group, y = mean_revpar),
             color = "red", size = 2) +
  geom_text(data = mean_values, aes(x = Six_Group, y = mean_revpar,
                                    label = sprintf("%.1f", mean_revpar)),
            vjust = -0.8, color = "red", size = 3.5) +
  labs(title = "RevPAR by Season and Concert Window",
       x = NULL, y = "Rooms RevPAR ($)") +
  theme_minimal()