# Code to pull all the time series indicator trend analysis results and create a table.

# Install IEAnalyzeR package
#devtools::install_github("https://github.com/Gulf-IEA/IEAnalyzeR")

library(IEAnalyzeR)
library(readxl)
library(tidyverse)
library(plotly)
library(gt)

# First load all the .csv files from the indicator_objects/objects_as_csvs folder

# Set the path to your folder
folder_path <- "indicator_objects/objects_as_csvs/"
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Loop through files: load, process, and save output
for (file in csv_files) {
  file_name <- tools::file_path_sans_ext(basename(file))  # Remove .csv extension
  
  # Load CSV without headers
  data <- read.csv(file, header = FALSE)
  
  # Process with IEAnalyzeR::data_prep()
  processed_data <- tryCatch({
    IEAnalyzeR::data_prep(data)
  }, warning = function(w) {
    message("Warning in file: ", file_name, " - ", conditionMessage(w))  
    return(NULL)  # Return NULL for debugging
  }, error = function(e) {
    message("Error in file: ", file_name, " - ", conditionMessage(e))
    return(NULL)
  })
  
  # Save processed data only if successful
  if (!is.null(processed_data)) {
    assign(paste0(file_name, "_obj"), processed_data, envir = .GlobalEnv)
  }
}

# Check loaded objects
ls()


############# Make the trend analysis summary table #######################

