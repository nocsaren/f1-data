library(tidyverse)
library(lubridate)

# Set working directory
setwd("D:/OneDrive/Example Projects/f1")

### EXTRACTION ###

source("get_csvs.R")

get_csvs("./data")

### TRANSFORMATION ###

# Total Times per Race per Driver
# Clean lap_times
lap_times <- cleaned_lap_times
lap_times$lap_time <- gsub("0 days ", "", lap_times$lap_time, fixed = TRUE)
lap_times$lap_time_period <- hms(lap_times$lap_time)
lap_times$total_seconds <- round(as.numeric(lap_times$lap_time_period), 3)

# Calculate total race times
race_time_sd <- lap_times %>%
  group_by(raceId, driverId) %>%
  summarise(total_race_time = sum(total_seconds)) %>% 
  group_by(raceId) %>% 
  summarise(standard_deviation = sd(total_race_time)) %>% 
  left_join(cleaned_races, by = "raceId") %>% 
  select(-round) %>% 
  left_join(cleaned_circuits, by = "circuitId") %>% 
  group_by(circuit_name) %>%
  filter(n() >= 9) %>%
  ungroup() %>% 
  select(- circuitId, -circuit_location, -circuit_country, -circuit_nationality)

write.csv(race_time_sd, "race_time_sd.csv", row.names = FALSE)
