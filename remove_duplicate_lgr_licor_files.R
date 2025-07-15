# load libraries
library(dplyr)
library(readr)


# function to combine meas_date and time into a single datetime column
create_datetime_column <- function(df) {
  # mutate the dataframe to add the datetime column
  df <- df %>%
    mutate(datetime = as.POSIXct(paste(meas_date, time), format = "%d.%m.%y %H:%M:%S")) %>%
    select(-meas_date, -time)

  # check for NA in the new datetime column
  if (any(is.na(df$datetime))) {
    message("There is NA in the newly created datetime column.") # if this happens check date-time format above
  } else{
    message("All date-time parsed well in datetime column.")
  }

  # return the modified dataframe
  return(df)
}


# function to identify numeric and categorical columns
identify_columns <- function(df) {
  numeric_columns <- df %>% select_if(is.numeric) %>% colnames()
  categorical_columns <- df %>% select_if(is.character) %>% colnames()
  list(numeric_columns = numeric_columns, categorical_columns = categorical_columns)
}


# function to calculate the mode as there is no built in mode function in r
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# function to aggregate the data by datetime column
aggregate_data <- function(df, numeric_columns, categorical_columns) {
  df_grouped <- df %>%
    group_by(datetime) %>%
    summarise(across(all_of(numeric_columns), \(x) mean(x, na.rm = TRUE)),
              across(all_of(categorical_columns), ~ Mode(.)),
              .groups = 'drop')

  # check for duplicates after aggregation
  if (any(duplicated(df_grouped$datetime))) {
    message("There are duplicates in the aggregated dataframe.")
  } else {
    message("No duplicates found in the aggregated dataframe.")
  }

  return(df_grouped)
}


# function to split datetime back into meas_date and time columns
split_datetime_column <- function(df_grouped) {
  df_grouped <- df_grouped %>%
    mutate(meas_date = as.Date(datetime),
           time = format(datetime, "%H:%M:%S")) %>%
    select(-datetime)  # remove the datetime column

  return(df_grouped)
}

# main function to remove duplicates and process the dataframe
remove_duplicated_data <- function(input_file, output_file) {
  df <- read_csv(input_file, show_col_types = FALSE)

  # combine date and time into a datetime column
  df <- create_datetime_column(df)

  # identify numeric and categorical columns -- to decide whether to use mean or mode
  columns <- identify_columns(df)

  # aggregate data by datetime, which should be done based on each unique date-time
  df_grouped <- aggregate_data(df, columns$numeric_columns, columns$categorical_columns)

  # split datetime into separate date and time columns
  df_grouped <- split_datetime_column(df_grouped)

  # write the final data-frame to a new CSV file
  write_csv(df_grouped, output_file)
}


# specify the file paths
input_file <- '/Users/mah/Downloads/leaf_lgr.csv'
output_file <- '/Users/mah/Downloads/processed_leaf_lgr.csv'

# run the data processing
remove_duplicated_data(input_file, output_file)
