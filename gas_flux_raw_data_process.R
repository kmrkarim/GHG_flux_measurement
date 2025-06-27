#THIS CODE IS FOR INITIAL TIME TEMPLATE TO FINAL CLEAN RAW GAS DATA PROCESS
################################################################################
#AUTHOR OF THIS CODE @REZAUL KARIM (rezaul.karim@mail.utoronto.ca)
################################################################################








################################################################################
#FIRST TIME DATA CONVERSION TO EXPANDED TIME
################################################################################


# Load required libraries
library(tidyverse)
library(lubridate)

# Read the CSV (the csv which have the initial time data)
df <- read_csv("/Users/ronysmac/Downloads/WasteWoodExperiment_sample.csv", col_types = cols(
  date = col_character(),
  id = col_character(),
  lgr_start = col_character(),
  lgr_end = col_character(),
  n2o_start = col_character(),
  n2o_end = col_character(),
  temp = col_double(),
  oxygen = col_double(),
  avg_collar_height = col_double()
))

# Function to expand time sequences per row
expand_times <- function(date, id, lgr_start, lgr_end, n2o_start, n2o_end, temp, oxygen, avg_collar_height) {
  date_fmt <- dmy(date)
  
  # Parse timestamps
  lgr_start_time <- as.POSIXct(paste(date_fmt, lgr_start), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  lgr_end_time   <- as.POSIXct(paste(date_fmt, lgr_end),   format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  n2o_start_time <- as.POSIXct(paste(date_fmt, n2o_start), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  n2o_end_time   <- as.POSIXct(paste(date_fmt, n2o_end),   format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Skip if time sequence invalid
  if (is.na(lgr_start_time) || is.na(lgr_end_time) || lgr_start_time > lgr_end_time ||
      is.na(n2o_start_time) || is.na(n2o_end_time) || n2o_start_time > n2o_end_time) {
    return(tibble())
  }
  
  # Create time sequences
  lgr_seq <- seq(lgr_start_time, lgr_end_time, by = "1 sec")
  n2o_seq <- seq(n2o_start_time, n2o_end_time, by = "1 sec")
  
  # Align sequence lengths
  len <- min(length(lgr_seq), length(n2o_seq))
  
  tibble(
    id = id,
    date = format(date_fmt, "%Y-%m-%d"),  # Date as YYYY-MM-DD
    lgr_time = format(lgr_seq[1:len], "%H:%M:%S"),  # Time only
    n2o_time = format(n2o_seq[1:len], "%H:%M:%S"),
    temp = temp,
    oxygen = oxygen,
    avg_collar_height = avg_collar_height
  )
}

# Expand all rows
expanded_df <- pmap_dfr(df, expand_times)

# Write the output CSV
write_csv(expanded_df, "/Users/ronysmac/Downloads/WasteWoodExperiment_expanded.csv")









################################################################################
#ATTACH THE N2O data
################################################################################

# Load required libraries
library(tidyverse)

# Read expanded dataset
expanded_df <- read_csv("/Users/ronysmac/Downloads/WasteWoodExperiment_expanded.csv", col_types = cols(
  id = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  lgr_time = col_character(),
  n2o_time = col_character(),
  temp = col_double(),
  oxygen = col_double(),
  avg_collar_height = col_double()
))

# Read N2O sensor data
n2o_data <- read_csv("/Users/ronysmac/Desktop/phd/n2o_2024_v2.csv", col_types = cols(
  date = col_date(format = "%Y-%m-%d"),
  time = col_character()
))

# Remove duplicated time entries per date
n2o_data_clean <- n2o_data %>%
  distinct(date, time, .keep_all = TRUE)

# Safe left join on date + time
merged_df <- expanded_df %>%
  left_join(n2o_data_clean, by = c("date" = "date", "n2o_time" = "time"))

# Save result
write_csv(merged_df, "/Users/ronysmac/Downloads/WasteWoodExperiment_n2o_matched.csv")






################################################################################
#ATTACH THE LGR data
################################################################################
# Load required libraries
library(tidyverse)

# Read the N2O-matched dataset (main file)
main_df <- read_csv("/Users/ronysmac/Downloads/WasteWoodExperiment_n2o_matched.csv", col_types = cols(
  id = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  lgr_time = col_character(),
  n2o_time = col_character()
))

# Read the LGR data file
lgr_df <- read_csv("/Users/ronysmac/Desktop/phd/2024-06-27_10-05-37_14-01-51.csv", col_types = cols(
  Date = col_date(format = "%Y-%m-%d"),
  Time = col_character()
))

# Drop duplicate entries by Date and Time
lgr_df_clean <- lgr_df %>%
  distinct(Date, Time, .keep_all = TRUE)

# Perform left join on date and lgr_time = Time
merged_all <- main_df %>%
  left_join(lgr_df_clean, by = c("date" = "Date", "lgr_time" = "Time"))

# Write final dataset with LGR and N2O data
write_csv(merged_all, "/Users/ronysmac/Downloads/WasteWoodExperiment_full_matched.csv")




#NOW THIS FULL_MATCHED DATA CSV IS THE RAW CLEAN CONCENTRATION CSV









