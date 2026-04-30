library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

crime_type_graph_data <- crimeHollywood_filtered %>%
  group_by(Year, Crime_Category) %>%
  summarise(Count = n(), .groups = "drop")

crime_type_table_wide <- crimeHollywood_filtered %>%
  group_by(Crime_Category, Year) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Year,
    values_from = Count,
    values_fill = 0
  ) %>%
  rename(
    `Crime Category` = Crime_Category,
    ''
  ) %>%
  arrange(`Crime Category`)

View(crime_type_table_wide)

crime_type_table_formatted <- crime_type_table_wide %>%
  kable(
    caption = "Crime Categories in Hollywood: 2020 vs 2024",
    align = "lrr",
    row.names = FALSE
  ) %>%
  kable_classic(
    full_width = TRUE,
    lightable_options = "striped",
    font_size = 14
  )

crime_type_table_formatted

crime_category_bar <- ggplot(
  crime_type_graph_data,
  aes(x = Crime_Category, y = Count, fill = factor(Year))
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.65) +
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 2.8
  ) +
  expand_limits(y = max(crime_type_graph_data$Count) * 1.15) +
  labs(
    title = "Crime Categories in Hollywood: 2020 vs 2024",
    x = "Crime Category",
    y = "Number of Crimes",
    fill = "Year"
  ) +
  scale_fill_manual(
    values = c("2020" = "#BC2048", "2024" = "#1E407C")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 11, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.position = "top"
  )


crime_category_bar