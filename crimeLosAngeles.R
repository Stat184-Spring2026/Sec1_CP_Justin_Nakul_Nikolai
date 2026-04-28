library(tidyverse)

crimeRaw <- read.csv("/Users/nakulbhatia/Downloads/Crime_Data_from_2020_to_2024.csv")

View(crimeRaw)

library(dplyr)
crimeTidy <- crimeRaw %>%
  select(DATE.OCC,TIME.OCC, AREA.NAME, Crm.Cd.Desc, Vict.Age, Vict.Sex)

crimeHollywood <- crimeTidy %>%
  filter(AREA.NAME == "Hollywood")

crimeHollywood$DATE.OCC <- as.POSIXct(crimeHollywood$DATE.OCC, format = "%m/%d/%Y %I:%M:%S %p")
crimeHollywood_filtered <- crimeHollywood %>%
  filter(format(DATE.OCC, "%Y") %in% c("2020", "2024"))

crimeHollywood_filtered <- crimeHollywood_filtered %>%
  filter(
    complete.cases(.),          # removes any NA
    Vict.Age != 0,            # remove age = 0
    Vict.Sex != "X",          # remove invalid sex
    Vict.Age != "",           # remove empty strings (if stored as text)
    Vict.Sex != ""
  )

crimeHollywood_filtered <- crimeHollywood_filtered %>%
  mutate(
    Time = sprintf("%04d", TIME.OCC),  # ensures 4 digits
    Time = sub("(\\d{2})(\\d{2})", "\\1:\\2", time)
  )
crimeHollywood_filtered <- crimeHollywood_filtered %>%
  select(-TIME.OCC)

crimeHollywood_filtered <- crimeHollywood_filtered %>%
  select(-time)

crimeHollywood_filtered <- crimeHollywood_filtered %>%
  select(-Time)

unique(crimeHollywood_filtered$Crm.Cd.Desc)

library(lubridate)
crimeHollywood_filtered <- crimeHollywood_filtered %>%
  mutate(
    Year = year(DATE.OCC),
    Month = month(DATE.OCC)
  )

crimeHollywood_filtered <- crimeHollywood_filtered %>%
  select(-DATE.OCC)

crimeHollywood_filtered <- crimeHollywood_filtered %>%
  rename(
    Area = AREA.NAME,
    Crime_Type = Crm.Cd.Desc,
    Victim_Age = Vict.Age,
    Victim_Sex = Vict.Sex
  )

crimeHollywood_filtered <- crimeHollywood_filtered %>%
  mutate(crime_category = case_when(
    grepl("BURGLARY", Crime_Type) ~ "Burglary",
    grepl("THEFT", Crime_Type) ~ "Theft",
    grepl("ASSAULT", Crime_Type) ~ "Assault",
    grepl("ROBBERY", Crime_Type) ~ "Robbery",
    grepl("VANDALISM", Crime_Type) ~ "Vandalism",
    TRUE ~ "Other"
  ))

crimeHollywood_filtered <- crimeHollywood_filtered %>%
  select(-Crime_Type)