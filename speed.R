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

# Calculate mean and median lap times per driver per race
mean_lap_times <- lap_times %>%
  group_by(raceId, driverId) %>%
  summarise(mean_time_per_lap = mean(total_seconds), median_time_per_lap = median(total_seconds))

# Calculate total race times
lap_times2 <- lap_times %>%
  group_by(raceId, driverId) %>%
  summarise(total_race_time = sum(total_seconds)) %>%
  left_join(mean_lap_times, by = c("raceId", "driverId")) %>%
  select(raceId, driverId, total_race_time, mean_time_per_lap)  # Retain mean_time_per_lap

# Removing Bahrain Races ( too many length variations )

bahrein_races <- cleaned_races %>% 
  filter(circuitId == 3)

lap_times2 <- lap_times2 %>%
  filter(!raceId %in% bahrein_races$raceId)

# Filter out races 1063 and 847 (too long or short races)
lap_times2 <- lap_times2 %>%
  filter(raceId != 1063 & raceId != 847)

# Calculate mean and median race times
race_times <- lap_times2 %>%
  group_by(raceId) %>%
  summarise(mean_lap_time = mean(mean_time_per_lap, na.rm = TRUE),
            median_lap_time = median(mean_time_per_lap, na.rm = TRUE),
            mean_race_time = mean(total_race_time, na.rm = TRUE),
            median_race_time = median(total_race_time, na.rm = TRUE)) 

# Define circuit lengths
circuits <- cleaned_circuits %>%
  mutate(circuit_length = case_when(
    circuit_name == "Albert Park Grand Prix Circuit" ~ 5278,
    circuit_name == "Autodromo Enzo e Dino Ferrari" ~ 4909,
    circuit_name == "Autódromo José Carlos Pace" ~ 4309,
    circuit_name == "Autodromo Nazionale di Monza" ~ 5793,
    circuit_name == "Bahrain International Circuit" ~ 5412,
    circuit_name == "Circuit de Barcelona-Catalunya" ~ 4657,
    circuit_name == "Circuit de Monaco" ~ 3337,
    circuit_name == "Circuit de Nevers Magny-Cours" ~ 4411,
    circuit_name == "Circuit de Spa-Francorchamps" ~ 7004,
    circuit_name == "Circuit Gilles Villeneuve" ~ 4361,
    circuit_name == "Circuit of the Americas" ~ 5513,
    circuit_name == "Hockenheimring" ~ 4574,
    circuit_name == "Hungaroring" ~ 4381,
    circuit_name == "Istanbul Park" ~ 5338,
    circuit_name == "Marina Bay Street Circuit" ~ 4940,
    circuit_name == "Nürburgring" ~ 5148,
    circuit_name == "Red Bull Ring" ~ 4318,
    circuit_name == "Sepang International Circuit" ~ 5543,
    circuit_name == "Shanghai International Circuit" ~ 5451,
    circuit_name == "Silverstone Circuit" ~ 5891,
    circuit_name == "Suzuka Circuit" ~ 5807,
    circuit_name == "Yas Marina Circuit" ~ 5281,
    TRUE ~ NA_real_
  ))

# Join races with circuit information
races <- cleaned_races %>%
  left_join(circuits, by = "circuitId") %>%
  select(raceId, circuitId, year, round, circuit_name, circuit_length)

# Join race_times with races
race_times <- race_times %>%
  left_join(races, by = "raceId") 

# Filter out circuits with less than 9 races
race_times <- race_times %>%
  group_by(circuit_name) %>%
  filter(n() >= 9) %>%
  ungroup()

# Calculate mean speed
race_times <- race_times %>% 
  mutate(mean_speed = (circuit_length / mean_lap_time) * 3.6)

# Write to CSV
write.csv(race_times, "speed.csv", row.names = FALSE)
rm(list = ls())
