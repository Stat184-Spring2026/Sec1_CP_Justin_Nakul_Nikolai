# Step 1: Load packages needed for summarizing data, reshaping/formatting tables, and creating plots
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(ggplot2)

# Step 2: Create a long-format summary table for the bar graph

# Step 2a: Start with the cleaned Hollywood crime data set
crime_type_graph_data <- crimeHollywood_filtered %>%
  # Step 2b: Group the data by year and crime category
  group_by(Year, Crime_Category) %>%
  # Step 2c: Count the number of crimes in each year/category group
  summarise(Count = n(), .groups = "drop")

# Step 3: Create a wide-format summary table for viewing crime categories by year

# Step 3a: Start with the cleaned Hollywood crime data set
crime_type_table_wide <- crimeHollywood_filtered %>%
  # Step 3b: Group the data by crime category and year
  group_by(Crime_Category, Year) %>%
  # Step 3c: Count the number of crimes in each crime category
  summarise(Count = n(), .groups = "drop") %>%
  # Step 3d: Reshape the table so 2020 and 2024 become separate columns
  pivot_wider(
    names_from = Year,
    values_from = Count,
    values_fill = 0
  ) %>%
  # Step 3e: Rename columns and organize rows alphabetically by crime category
  rename(
    `Crime Category` = Crime_Category,
    `2020 Crimes` = `2020`,
    `2024 Crimes` = `2024`
  ) %>%
  arrange(`Crime Category`)

# Step 4: Create a formatted table with a title, centered number column, and striped styling

# Step 4a: Start with the wide-format table created above
crime_type_table_formatted <- crime_type_table_wide %>%
  # Step 4b: Use kable() to add a caption, align columns, and remove row names
  kable(
    caption = "Crime Categories in Hollywood: 2020 vs 2024",
    align = "lcc",
    row.names = FALSE
  ) %>%
  # Step 4c: Use kable_classic() to make the table wider, striped, and easier to read
  kable_classic(
    full_width = TRUE,
    lightable_options = "striped",
    font_size = 14
  )

# Step 4d: Display the formatted table
crime_type_table_formatted

# Step 5: Create a grouped bar graph comparing category counts in 2020 and 2024

# Step 5a: Start the plot using the long-format graph data
crime_category_bar <- ggplot(
  crime_type_graph_data,
  # Step 5b: Set crime category on the x-axis, count on the y-axis, and year as the fill color
  aes(x = Crime_Category, y = Count, fill = factor(Year))
) +
  # Step 5c: Create side-by-side bars for 2020 and 2024 within each crime category
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.65
           ) +
  # Step 5d: Add the count labels above each bar
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 2.8
  ) +
  # Step 5e: Add extra space above the tallest bar so the labels are not cut off
  expand_limits(y = max(crime_type_graph_data$Count) * 1.15) +
  # Step 5f: Add the graph title, axis labels, and legend title
  labs(
    title = "Crime Categories in Hollywood: 2020 vs 2024",
    x = "Crime Category",
    y = "Number of Crimes",
    fill = "Year"
  ) +
  # Step 5g: Manually set the colors for 2020 and 2024
  scale_fill_manual(
    values = c("2020" = "#BC2048", "2024" = "#1E407C")
  ) +
  # Step 5h: Use a clean minimal theme
  theme_minimal() +
  # Step 5i: Adjust title, axis text, axis labels, and legend placement
  theme(
    plot.title = element_text(hjust = 0, size = 11, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.position = "top"
  )

# Step 5j: Display the grouped bar graph
crime_category_bar
