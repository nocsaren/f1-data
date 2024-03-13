library(tidyverse)

get_csvs <- function(directory) {
  # Find, list, and import CSV files
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Loop for creating data frames and putting into global environment
  for (file in csv_files) {
    df_name <- tools::file_path_sans_ext(basename(file))
    assign(df_name, read_csv(file), envir = .GlobalEnv)
  }
}

