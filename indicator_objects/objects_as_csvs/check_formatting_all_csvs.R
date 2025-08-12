library(tidyverse)

# --- IMPORTANT ---
# Replace 'YOUR_FOLDER_PATH_HERE' with the actual path to your folder.
folder_path <- "indicator_objects/objects_as_csvs/"

# --- Helper function to classify the format of a date string ---
get_date_format <- function(value_string) {
  if (is.na(value_string)) return("Unknown")
  
  if (grepl("^\\d{4}$", value_string)) {
    return("YYYY (%Y)")
  } else if (grepl("^\\d{6}$", value_string)) { 
    return("YYYYMM (%Y%m)")
  } else if (grepl("^[A-Za-z]{3}-\\d{2}$", value_string)) { 
    return("Mon-YY (%b-%y)")
  } else if (grepl("^\\d{1,2}-\\d{4}$", value_string)) {
    return("MM-YYYY (%m-%Y)")
  } else if (grepl("^\\d{4}[A-Za-z]{3}$", value_string)) {
    return("YYYYMon (%Y%b)")
  } else if (grepl("^[A-Za-z]{3}\\d{4}$", value_string)) {
    return("MonYYYY (%b%Y)")
  } else if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", value_string)) {
    return("YYYY-MM-DD (%Y-%m-%d)")
  } else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", value_string)) {
    return("MM/DD/YYYY (%m/%d/%Y)")
  } else if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", value_string)) {
    return("MM-DD-YYYY (%m-%d-%Y)")
  } else if (grepl("^[A-Za-z]{3}-\\d{4}$", value_string)) {
    return("Mon-YYYY (%b-%Y)")
  } else if (grepl("^[A-Za-z]+ \\d{4}$", value_string)) {
    return("Month YYYY (%B %Y)")
  } else {
    return("Other/Non-standard")
  }
}

# Get the full path for all CSV files
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# This function will analyze a single file
analyze_file <- function(file_path) {
  df <- read_csv(file_path, col_types = cols(.default = "c"), show_col_types = FALSE)
  
  # Determine format from the first non-NA value in column 1
  first_val <- first(na.omit(df[[1]]))
  detected_format <- get_date_format(first_val)
  
  # Return a summary row for this file
  tibble(
    file_name = basename(file_path),
    detected_format = detected_format,
    num_columns = ncol(df),
    # UPDATED: Check for NAs only in the last 5 rows of the data frame
    has_NAs = any(is.na(tail(df, 5)))
  )
}

# Run the analysis on all files
if (length(csv_files) > 0) {
  # Create a detailed report for all files
  detailed_report <- map_dfr(csv_files, analyze_file)
  
  # Create the summary count of formats
  format_summary <- detailed_report %>%
    count(detected_format, name = "file_count")
  
  # --- Print the final report ---
  cat("--- Analysis Report ---\n\n")
  
  cat("## Column 1 Format Summary 📊:\n")
  print(format_summary)
  cat("\n---\n\n")
  
  cat("## Detailed File Report:\n")
  print(detailed_report)
  
} else {
  print(paste("No CSV files were found in the folder:", folder_path))
}
