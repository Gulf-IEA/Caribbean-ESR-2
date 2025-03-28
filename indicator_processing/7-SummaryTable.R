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

# Get the names of all objects in the environment ending with "_obj"
obj_names <- ls(pattern = "_obj$")

# Retrieve the actual list objects and combine them into ind_list
ind_list <- lapply(obj_names, get)

sum_list <- list()

for (i in 1:length(ind_list)) {
  df <- ind_list[[i]]
  
  # Check if there are sub-indicators
  if (ncol(df$labs) > 2) {
    indic_title <- df$labs[1, 2:ncol(df$labs)]
    sub_titles <- df$labs[3, 2:ncol(df$labs)]  # Sub-indicator titles
  } else {
    indic_title <- df$labs[1, 2]  # Overall indicator title
    sub_titles <- NA  # No sub-indicators
  }
  
  trend_sym <- df$vals$mean_sym
  slope_sym <- df$vals$slope_sym
  mean_vals <- round(df$vals$mean, 2)
  sd_vals <- round(df$vals$sd, 2)
  
  # Ensure all extracted values are treated as vectors
  if (length(sub_titles) == 1 && is.na(sub_titles)) {
    sub_titles <- rep(NA, length(mean_vals))  # Handle case where no sub-indicators exist
  }
  
  # Iterate over all sub-indicators
  for (j in seq_along(mean_vals)) {
    all_dat <- c(indic_title[j], sub_titles[j], trend_sym[j], slope_sym[j], mean_vals[j], sd_vals[j])
    all_dat <- unlist(all_dat)
    sum_list[[length(sum_list) + 1]] <- all_dat
  }
}


sum_table<-as.data.frame(do.call("rbind",sum_list))
colnames(sum_table)<-c("indicator", "sub-indicator", "trend_sym", "slope_sym", "mean", "sd")
gt(sum_table)

outreach_obj$labs
