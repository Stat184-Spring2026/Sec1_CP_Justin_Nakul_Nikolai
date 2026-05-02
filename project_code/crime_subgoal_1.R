#Goal 1: Create 2 separate tables for 2020 and 2024. These tables will 
#include both age ranges (1-10, 11-20, etc.) and the number of victims per 
#that age range

library(kableExtra)

#Step 1: Create a table that changes the age column into the age range it's in
crimeHollywood_ages_filtered <- crimeHollywood_filtered %>%
  mutate(Age_Range = cut (
    Victim_Age,
    #Step 1a: splits age into bins of 0-10, 11-20, all the way to 100
    breaks = seq(0, 100, by = 10),
    #Step 1b: assigns the readable table values
    labels = c("1-10", "11-20", "21-30", "31-40", "41-50", 
               "51-60", "61-70", "71-80", "81-90", "90-100"),
    include.lowest = TRUE
  ))

#Step 2: creates 2020 table with age range and number of victims in that age range
age_table_2020 <- crimeHollywood_ages_filtered %>%
  filter(Year == 2020) %>%
  group_by(Age_Range) %>%
  summarise(Victim_Count = n())

#Step 3: creates a neater version of the above table using kableExtra
crime_age_table_2020 <- age_table_2020 %>%
  kable(
    #Step 3a: Gives the table a header
    caption = "Victim Age Range Distribution (2020)",
    col.names = c("Age Range", "Number of Victims"),
    align = "lr",
    row.names = FALSE
  ) %>%
  
  #Step 3b: add stripes and change font size to give a cleaner look  
  kable_classic(
    lightable_options = "striped",
    font_size = 12
  )

#Step 4: creates 2024 table with age range and number of victims in that age range
age_table_2024 <- crimeHollywood_ages_filtered %>%
  filter(Year == 2024) %>%
  group_by(Age_Range) %>%
  summarise(Victim_Count = n())

#Step 5: creates a neater version of the above table using kableExtra
crime_age_table_2024 <- age_table_2024 %>%
  kable(
    caption = "Victim Age Range Distribution (2024)",
    col.names = c("Age Range", "Number of Victims"),
    align = "lr",
    row.names = FALSE
  ) %>%
  
  kable_classic(
    lightable_options = "striped",
    font_size = 12
  )

#Step 6: Display the tables
crime_age_table_2020
crime_age_table_2024

#Goal 2: Create 2 separate bar charts for 2020 and 2024. The x axis will be
#the age ranges while the y axis will be the number of victims in that age range

library(ggplot2)
library(patchwork)

#Step 1: Create a bar chart for the 2020 victim age distribution

#Step 1a: Create a plot with the x axis being the age range and y axis being the
#number of victims
crime_age_plot_2020 <- ggplot(
  age_table_2020, aes(x = Age_Range, y = Victim_Count)) +
  #Step 1b: Creates the bars from the values in victim_count
  geom_col(fill = c("#BC2048")) +
  #Step 1c: Creates the title and renames the x and y axes
  labs(
    title = "Victim Age Distribution (2020)",
    x = "Age Range",
    y = "Number of Victims"
  ) +
  theme_minimal()

#Step 2: Create a bar chart for the 2024 victim age distribution following same
#steps as above for Step 1
crime_age_plot_2024 <- ggplot(
  age_table_2024, aes(x = Age_Range, y = Victim_Count)) +
  geom_col(fill = c("#1E407C")) +
  labs(
    title = "Victim Age Distribution (2024)",
    x = "Age Range",
    y = "Number of Victims"
  ) +
  theme_minimal()

#Step 3: Display box plots
crime_age_plot_2020 + crime_age_plot_2024 +
  plot_annotation(
    title = "Victim Age Distribution Comparison (2020 vs 2024)"
  )






