
# Function to clean raw LGR concentration data (in ppm)
# last updated on 19 Aug 2023

library(stringr)
library(data.table)
library(tibble)


cleanRawLgrData <- function(directory, skip = 3, output_directory = NULL) {

    # List all TXT files in the directory that ends with .txt
    # all_files <- list.files(directory, pattern = "*.txt", full.names = TRUE)
    all_files <- list.files(directory, pattern = ".*\\.txt$", full.names = TRUE)

    # Check if any TXT files are found
    if (length(all_files) == 0) {
        message("No TXT files found in the specified directory.")
        return(NULL)
    }

    # Loop through each file to process
    for (raw_lgr_data_files in all_files) {

        # Check if the file exists
        if (!file.exists(raw_lgr_data_files)) {
            message(paste("File", raw_lgr_data_files, "does not exist. Skipping..."))
            next
        }

        # Check number of lines in the file
        num_lines <- length(readLines(raw_lgr_data_files))

        # If there are not enough lines to skip, print a warning and continue to the next file
        if (num_lines <= skip) {
            message(paste("File", raw_lgr_data_files, "has ", num_lines, "lines. Skipping..."))
            next
        }

        # Read the TXT data, skipping the specified number of lines at the start
        # This is helpful to skip any metadata or headers in the TXT file
        tempData <- fread(raw_lgr_data_files, skip = skip, data.table = FALSE, header = FALSE)

        # If the data read has only 1 row or is empty, it may mean it has only headers or is empty, so skip processing
        if (nrow(tempData) <= 1) {
            message(paste("File", raw_lgr_data_files, "is empty or has only column names. Skipping..."))
            next
        }

        # Convert the date and time column (assuming it's the first column) into a datetime format
        tempData$V1 <- strptime(as.character(tempData$V1), "%m/%d/%Y %H:%M:%S")

        # Extract the date and time separately
        Date <- format(as.POSIXct(tempData$V1), format = "%Y-%m-%d")
        Time <- format(as.POSIXct(tempData$V1), format = "%H:%M:%S")

        # Add the extracted date and time to the dataset
        tempData <- tibble::add_column(tempData, Date, Time, .before = "V2")

        # Remove the original datetime column
        tempData[1] <- NULL

        # Rename the columns to appropriate headers
        column_headings <- c(
            'Date', 'Time', 'ch4_wet', 'ch4_sd', 'h2o', 'h2o_sd',
            'co2', 'co2_sd', 'ch4_dry', 'ch4_dry_sd', 'co2_dry', 'co2_dry_sd',
            'gasP_torr', 'GasP_torr_sd', 'gasT_C', 'gasT_C_sd', 'ambT_C', 'ambT_C_sd',
            'rd0_us', 'rd0_us_sd', 'rd1_us', 'rd1_us_sd', 'fit_flag', 'miu_val', 'miu_desc'
        )
        colnames(tempData) <- column_headings

        # Create a new file name for the cleaned data based on the original file's name and data contained
        fn_datePart <- str_split_fixed(basename(raw_lgr_data_files), "_", 3)[, 2]
        fn_time1 <- str_replace_all(tempData$Time[1], ":", "-")
        fn_time2 <- str_replace_all(tail(tempData$Time, 1), ":", "-")
        new_fname <- str_glue("{fn_datePart}_{fn_time1}_{fn_time2}.csv")

        # If no output directory is specified, use the original file's directory
        if (is.null(output_directory)) {
            output_directory <- dirname(raw_lgr_data_files)
        }

        # Formulate the full path for the cleaned data's output
        new_fullpath <- file.path(output_directory, new_fname)

        # # If a file with the new file name already exists, modify the name to avoid overwrite
        # counter <- 1
        # while (file.exists(new_fullpath)) {
        #     new_fullpath <- file.path(
        #         output_directory,
        #         str_glue("{fn_datePart}_{fn_time1}_{fn_time2}_{counter}.csv")
        #     )
        #     counter <- counter + 1
        # }
        #

        # Save the cleaned data as a CSV file
        write.csv(tempData, file = new_fullpath, row.names = FALSE)

        # Notify that the file processing is done
        message(paste0("Done writing file ", new_fname))
    }
}

cleanRawLgrData(directory = "/Users/mah/Downloads/input")
