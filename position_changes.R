library(tidyverse)
library(lubridate)

# Set working directory
setwd("D:/OneDrive/Example Projects/f1")

### EXTRACTION ###

source("get_csvs.R")

get_csvs("./data")


position_changes <- cleaned_results %>% 
  select(resultId, raceId, driverId, grid_position, position) %>% 
  mutate(position_change_race_driver = position-grid_position) %>% 
  filter(position_change_race_driver >= 0) %>% 
  group_by(raceId) %>% 
  summarise(position_change_race = mean(position_change_race_driver)) %>% 
  left_join(cleaned_races, by = "raceId") %>% 
  select(-raceId, -round) %>% 
  left_join(cleaned_circuits, by = "circuitId") %>% 
  select(year, circuit_name, position_change_race)


write.csv(position_changes, "position_changes.csv", row.names = FALSE)
