library(tidyverse)
library(lubridate)

# Set working directory
setwd("D:/OneDrive/Example Projects/f1")

### EXTRACTION ###

source("get_csvs.R")

get_csvs("./data")

accidents <- cleaned_results %>%
  filter(statusId %in% c(104, 3, 4, 20, 73, 130)) %>%
  left_join(cleaned_drivers, by = "driverId") %>%
  left_join(cleaned_races, by = "raceId") %>%
  left_join(cleaned_circuits, by = "circuitId") %>%
  left_join(cleaned_status, by = "statusId") %>% 
  mutate(driver = paste(driver_forename, driver_surname)) %>% 
  select(year, round, circuit_name, driver, status)

write.csv(accidents, "accidents.csv", row.names = FALSE)
