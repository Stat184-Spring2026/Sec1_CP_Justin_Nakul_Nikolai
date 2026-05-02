library(tidyverse)
library(janitor)
library(knitr)
library(kableExtra)
library(ggplot2)
library(scales)

##Goal 1: Create a clean data table that shows the number of monthly crime incidents in Hollywood for 2020 and 2024.

#Step 1: Create a summary data set of monthly crime count from the tidied crime data set.
crime_summary <- crimeHollywood_filtered %>%
##Step 1a: Group the data by year and month using group_by().
  group_by(Year, Month) %>%
##Step 1b: Count the total number of crime incidents in each group and remove grouping afterward for reproducibility.
  summarise(crime_count = n(), .groups = "drop") %>%
##Step 1c: Use arrange() to sort the date in chronological order by year and month.
  arrange(Year, Month)

#Step 2: Change the table from long to wide format.
crime_summary_wide <- crime_summary %>%
##Step 2a: Use month.abb to change the integer into characters. (e.g. 1 -> Jan)
  mutate(Month = factor(month.abb[Month])) %>%
##Step 2b: Use pivot_wider to transform the table into wide format.
  pivot_wider(
    names_from = Month,
    values_from = crime_count
  )



#Step 3: Create a neat summary table from the monthly crime summary data set using functions from knitr and kableExtra packages.
crime_month_table <- crime_summary_wide %>%
##Step 3a: Use kable() to create caption and set column for the table.
  kable(
    caption = "Monthly Crime Counts in Hollywood (2020 vs 2024)",
    align = "lrrrrrrrrrrrr",
    row.names = FALSE
  ) %>%
##Step 3b: Use kable_classic() to apply styling to the table.
  kable_classic(
    lightable_options = "striped",
    font_size = 12
  )

##Goal 2: Create a line graph that analyzes and compares monthly crime trends in Hollywood between 2020 and 2024.
#Step 1: Create the framework and specify the mappings.
crime_month_plot <- ggplot(
  data = crime_summary,
  mapping = aes(
    x = Month,
    y = crime_count,
    color = factor(Year)
  )
) +
#Step 2: Use geom_point() to create a point of the count and geom_line() to connect the point.
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
##Step 2a: Add axis labels, legend title, and graph title.
  labs(
    x = "Month",
    y = "Crime Count",
    color = "Year",
    title = "Change of Crime Counts In Hollywood (2020 vs 2024)"
  ) +
##Step 2b: Set the scale for x-axis from 1 to 12 to represent each month of the year.
scale_x_continuous(
    limits = c(1, 12),
    breaks = 1:12
  ) +

#Step 3: Use scale_color_manual() to assign different colors to each year.
  scale_color_manual(
    values = c(
      "#BC204B", "#1E407C"
    )
  ) +
#Step 4: Add a black and white background with legends placed at the bottom of the graph.
  theme_bw() +
  theme(legend.position = "bottom")

#Step 5: Check and polish the code using PCIP method.

crime_month_plot
crime_month_table

