# Code to convert .rds files in the indicator_outputs folder to .csv files, for easier sharing as needed. Should at some point save this function in our IEAnalyzeR package.

# updated by Carissa Gervasi on 3/20/25

convert_rdata_to_csv <- function(input_dir, output_dir) {
  # Ensure input directory exists
  if (!dir.exists(input_dir)) {
    stop("Error: Input directory does not exist.")
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # List all .RData files in the input directory
  rdata_files <- list.files(input_dir, pattern = "\\.RData$", full.names = TRUE)
  
  # Check if any .RData files are found
  if (length(rdata_files) == 0) {
    stop("Error: No .RData files found in the input directory.")
  }
  
  # Loop through each .RData file
  for (rdata_file in rdata_files) {
    # Load the .RData file into an environment
    env <- new.env()
    load(rdata_file, envir = env)
    
    # Check if 'ind' exists in the loaded environment
    if (!exists("ind", envir = env)) {
      cat("Skipping", rdata_file, "- 'ind' object not found.\n")
      next
    }
    
    ind <- env$ind
    
    # Ensure 'ind' contains the expected elements
    if (!all(c("labels", "indicators", "datelist") %in% names(ind))) {
      cat("Skipping", rdata_file, "- Incorrect structure (missing required elements).\n")
      next
    }
    
    # Create labels with blank first column
    labels_df <- data.frame(matrix(nrow = 3, ncol = ncol(ind$labels) + 1))
    labels_df[, 2:ncol(labels_df)] <- ind$labels  # Fill labels, leaving column 1 blank
    
    # Extract main data and format
    formatted_data <- data.frame(datelist = ind$datelist, ind$indicators)
    
    # Define output file name (same as input but with .csv extension)
    output_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(rdata_file)), ".csv"))
    
    # Write labels first (3 rows, first column blank)
    write.table(labels_df, file = output_file, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, na = "")
    
    # Append actual data **without column headers**
    write.table(formatted_data, file = output_file, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE, na = "")
    
    cat("Converted:", rdata_file, "->", output_file, "\n")
  }
  
  cat("All conversions complete! CSV files saved in:", output_dir, "\n")
}



convert_rdata_to_csv("indicator_objects", "indicator_objects/objects_as_csvs")
