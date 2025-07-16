library(stringr)
library(data.table)
library(tibble)
library(dplyr)

cleanAndCombineLgrData <- function(parent_directory, skip = 3, output_file) {
  
  # List all subdirectories (date folders)
  date_folders <- list.dirs(parent_directory, recursive = TRUE, full.names = TRUE)
  
  # Initialize an empty list to store all cleaned data
  all_clean_data <- list()
  
  # Loop through each subdirectory
  for (directory in date_folders) {
    
    # List all TXT files in each subdirectory
    all_files <- list.files(directory, pattern = ".*\\.txt$", full.names = TRUE)
    
    # Skip if no files found
    if (length(all_files) == 0) next
    
    # Process each file
    for (raw_file in all_files) {
      
      # Skip missing or short files
      if (!file.exists(raw_file) || length(readLines(raw_file)) <= skip) next
      
      # Read data
      tempData <- fread(raw_file, skip = skip, data.table = FALSE, header = FALSE)
      if (nrow(tempData) <= 1) next
      
      # Convert first column datetime
      tempData$V1 <- strptime(as.character(tempData$V1), "%m/%d/%Y %H:%M:%S")
      Date <- format(as.POSIXct(tempData$V1), "%Y-%m-%d")
      Time <- format(as.POSIXct(tempData$V1), "%H:%M:%S")
      
      # Add Date and Time columns
      tempData <- tibble::add_column(tempData, Date, Time, .before = "V2")
      tempData[1] <- NULL  # Remove original datetime column
      
      # Assign column names
      column_headings <- c(
        'Date', 'Time', 'ch4_wet', 'ch4_sd', 'h2o', 'h2o_sd',
        'co2', 'co2_sd', 'ch4_dry', 'ch4_dry_sd', 'co2_dry', 'co2_dry_sd',
        'gasP_torr', 'GasP_torr_sd', 'gasT_C', 'gasT_C_sd', 'ambT_C', 'ambT_C_sd',
        'rd0_us', 'rd0_us_sd', 'rd1_us', 'rd1_us_sd', 'fit_flag', 'miu_val', 'miu_desc'
      )
      colnames(tempData) <- column_headings
      
      # Store cleaned data
      all_clean_data[[length(all_clean_data) + 1]] <- tempData
    }
  }
  
  # Combine all cleaned data
  if (length(all_clean_data) > 0) {
    combined_data <- bind_rows(all_clean_data)
    # Write combined data to CSV
    write.csv(combined_data, file = output_file, row.names = FALSE)
    message(paste0("Done writing combined file: ", output_file))
  } else {
    message("No valid LGR .txt data found.")
  }
}


# Run the function for your parent LGR directory
cleanAndCombineLgrData(
  parent_directory = "/Users/ronysmac/Desktop/street_tree_project/lgr_street_trees/lgr",
  output_file = "/Users/ronysmac/Desktop/street_tree_project/lgr_street_trees/lgr/combined_lgr_data.csv"
)














###################################################################################
##################################################################################
##################################################################################
##################################################################################

# Load libraries
library(dplyr)
library(readr)

# Function to combine Date and Time into a single datetime column
create_datetime_column <- function(df) {
  df <- df %>%
    mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S")) %>%
    dplyr::select(-Date, -Time)
  
  if (any(is.na(df$datetime))) {
    message("There is NA in the newly created datetime column.")
  } else {
    message("All date-time parsed well in datetime column.")
  }
  
  return(df)
}

# Function to identify numeric and categorical columns
identify_columns <- function(df) {
  numeric_columns <- df %>% select_if(is.numeric) %>% colnames()
  categorical_columns <- df %>% select_if(is.character) %>% colnames()
  list(numeric_columns = numeric_columns, categorical_columns = categorical_columns)
}

# Function to calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to aggregate the data by datetime column
aggregate_data <- function(df, numeric_columns, categorical_columns) {
  duplicates_before <- sum(duplicated(df$datetime))
  
  df_grouped <- df %>%
    group_by(datetime) %>%
    summarise(across(all_of(numeric_columns), \(x) mean(x, na.rm = TRUE)),
              across(all_of(categorical_columns), ~ Mode(.)),
              .groups = 'drop')
  
  duplicates_after <- sum(duplicated(df_grouped$datetime))
  
  message(paste0("Duplicates before aggregation: ", duplicates_before))
  message(paste0("Duplicates after aggregation: ", duplicates_after))
  
  if (duplicates_after > 0) {
    message("WARNING: There are still duplicates in the aggregated dataframe.")
  } else {
    message("All duplicates successfully removed.")
  }
  
  return(df_grouped)
}

# Function to split datetime back into Date and Time columns
split_datetime_column <- function(df_grouped) {
  df_grouped <- df_grouped %>%
    mutate(Date = as.Date(datetime),
           Time = format(datetime, "%H:%M:%S")) %>%
    dplyr::select(-datetime)
  
  return(df_grouped)
}

# Main function to remove duplicates and process the dataframe
remove_duplicated_data <- function(input_file, output_file) {
  df <- read_csv(input_file, show_col_types = FALSE)
  
  # Combine Date and Time into datetime
  df <- create_datetime_column(df)
  
  # Identify numeric and categorical columns
  columns <- identify_columns(df)
  
  # Aggregate data
  df_grouped <- aggregate_data(df, columns$numeric_columns, columns$categorical_columns)
  
  # Split datetime into Date and Time columns
  df_grouped <- split_datetime_column(df_grouped)
  
  # Write to CSV
  write_csv(df_grouped, output_file)
  
  # Report final row count
  message(paste0("Final output rows: ", nrow(df_grouped)))
  message(paste0("Cleaned data saved to: ", output_file))
}


# Specify file paths
input_file <- '/Users/ronysmac/Desktop/street_tree_project/lgr_street_trees/lgr/combined_lgr_data.csv'
output_file <- '/Users/ronysmac/Desktop/street_tree_project/lgr_street_trees/lgr/street_tree_summer_lgr_data.csv'

# Run the data processing
remove_duplicated_data(input_file, output_file)
