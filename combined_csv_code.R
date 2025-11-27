# Load tidyverse
library(dplyr)
library(readr)
library(purrr)

# Path to folder
path <- "/Users/ronysmac/Desktop/phd/street_tree_project/fall_2025_data/slope"

# List all CSV files
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Read and combine
combined_df <- files %>%
  map_df(read_csv)

# Save combined CSV
write_csv(combined_df,
          file = file.path(path, "combined_slope_data.csv"))

