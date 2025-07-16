# Define file paths
input_file <- "/Users/ronysmac/Desktop/street_tree_project/TG20-01178-2025-04-01T120000.txt"
output_file <- "/Users/ronysmac/Desktop/street_tree_project/licor_street_tree_raw.csv"

# Define the desired column names
desired_cols <- c(
  "time", "date", "seconds", "nanoseconds", "index", "diag", 
  "h2o_ppm", "n2o_ppb", "cavity_pressure_kpa", "cavity_temp_degree_celcius",
  "laser_phase_pressure_kpa", "laser_temp_degree_celcius", "residual",
  "ring_down_time_microsecs", "thermal_enclosure_temp_degree_celcius",
  "phase_error_counts", "laser_t_shift_degree_celcius", "input_voltage_volt", "chk"
)

# Read the data, skipping metadata
# First read the header rows (6 and 7)
header_lines <- readLines(input_file, n = 7)[6:7]

# Combine the two header lines to get units (for reference, though we won't use them)
# We'll use the first line (DATAH) as the main column names
header_parts <- strsplit(header_lines, "\t")
main_cols <- header_parts[[1]]

# Read the actual data, skipping first 7 rows
data <- read.delim(input_file, skip = 7, header = FALSE, sep = "\t")

# Set column names (using the first header line)
colnames(data) <- main_cols

# Reorder and rename columns to match desired output
# Create a mapping between original and desired names
col_mapping <- c(
  "TIME" = "time",
  "DATE" = "date",
  "SECONDS" = "seconds",
  "NANOSECONDS" = "nanoseconds",
  "NDX" = "index",
  "DIAG" = "diag",
  "H2O" = "h2o_ppm",
  "N2O" = "n2o_ppb",
  "CAVITY_P" = "cavity_pressure_kpa",
  "CAVITY_T" = "cavity_temp_degree_celcius",
  "LASER_PHASE_P" = "laser_phase_pressure_kpa",
  "LASER_T" = "laser_temp_degree_celcius",
  "RESIDUAL" = "residual",
  "RING_DOWN_TIME" = "ring_down_time_microsecs",
  "THERMAL_ENCLOSURE_T" = "thermal_enclosure_temp_degree_celcius",
  "PHASE_ERROR" = "phase_error_counts",
  "LASER_T_SHIFT" = "laser_t_shift_degree_celcius",
  "INPUT_VOLTAGE" = "input_voltage_volt",
  "CHK" = "chk"
)

# Select and rename columns
final_data <- data[, names(col_mapping)]
colnames(final_data) <- col_mapping[names(col_mapping)]

# Write to CSV
write.csv(final_data, output_file, row.names = FALSE)

cat("Clean CSV file saved to:", output_file, "\n")