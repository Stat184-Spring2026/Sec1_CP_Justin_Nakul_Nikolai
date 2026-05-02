# Step 1: Load packages needed for data wrangling
library(tidyverse)
library(dplyr)
library(lubridate)

# Step 2: Import the raw crime data set and view it
crimeRaw <- read.csv("Crime_Data_from_2020_to_2024.csv")

View(crimeRaw)

# Step 3: Select only the variables needed for this project
crimeTidy <- crimeRaw %>%
  select(DATE.OCC, AREA.NAME, Crm.Cd.Desc, Vict.Age, Vict.Sex)

# Step 4: Filter the data set to only include crimes that occurred in Hollywood
crimeHollywood <- crimeTidy %>%
  filter(AREA.NAME == "Hollywood")

# Step 5: Convert the data column into a dat-time format and keep only entries from 2020 and 2024
crimeHollywood$DATE.OCC <- as.POSIXct(crimeHollywood$DATE.OCC, format = "%m/%d/%Y %I:%M:%S %p")
crimeHollywood_filtered <- crimeHollywood %>%
  filter(format(DATE.OCC, "%Y") %in% c("2020", "2024"))

# Step 6: Clean the data by removing incomplete or invalid victim information
crimeHollywood_filtered <- crimeHollywood_filtered %>%
  filter(
    complete.cases(.),        # removes any NA
    Vict.Age != 0,            # remove age = 0
    Vict.Sex != "X",          # remove invalid sex
    Vict.Age != "",           # remove empty strings (if stored as text)
    Vict.Sex != ""
  )

# Step 7: Check the unique crime descriptions before grouping crimes into broader categories
unique(crimeHollywood_filtered$Crm.Cd.Desc)

# Step 8: Create separate year and month columns from the date column
crimeHollywood_filtered <- crimeHollywood_filtered %>%
  mutate(
    Year = year(DATE.OCC),
    Month = month(DATE.OCC)
  )

# Step 9: Remove the original date column which is no longer needed after extracting the year and month
crimeHollywood_filtered <- crimeHollywood_filtered %>%
  select(-DATE.OCC)

# Step 10: Rename columns to make the data set more readable
crimeHollywood_filtered <- crimeHollywood_filtered %>%
  rename(
    Area = AREA.NAME,
    Crime_Type = Crm.Cd.Desc,
    Victim_Age = Vict.Age,
    Victim_Sex = Vict.Sex
  )

# Step 11: Group detailed crime descriptions into broader crime categories and creating a separate column for the broader categories 
crimeHollywood_filtered <- crimeHollywood_filtered %>%
  mutate(crime_category = case_when(
    grepl("BURGLARY", Crime_Type) ~ "Burglary",
    grepl("THEFT", Crime_Type) ~ "Theft",
    grepl("ASSAULT", Crime_Type) ~ "Assault",
    grepl("ROBBERY", Crime_Type) ~ "Robbery",
    grepl("VANDALISM", Crime_Type) ~ "Vandalism",
    TRUE ~ "Other"
  ))

# Step 12: Remove the detailed crime type column after creating the broader category
crimeHollywood_filtered <- crimeHollywood_filtered %>%
  select(-Crime_Type)

# Step 13: Rename the new crime category column to keep consistent with other columns
crimeHollywood_filtered <- crimeHollywood_filtered %>%
  rename(Crime_Category = crime_category)

# Step 14: Keep only male and female victim record and recode the labels for readability
crimeHollywood_filtered <- crimeHollywood_filtered %>%
  filter(Victim_Sex %in% c("M", "F")) %>%
  mutate(
    Victim_Sex = recode(Victim_Sex,
                        "M" = "Male",
                        "F" = "Female")
  )








