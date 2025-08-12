# Code to pull all the time series indicator trend analysis results and create a table.

# Install IEAnalyzeR package
devtools::install_github("https://github.com/Gulf-IEA/IEAnalyzeR")

library(IEAnalyzeR)
library(readxl)
library(tidyverse)
library(plotly)
library(gt)

# First load all the .csv files from the indicator_objects/objects_as_csvs folder

# Set the path to your folder
#folder_path <- "../../../My Documents/Wind-ESR-Site-main/data/humandimensions/"
folder_path <- "indicator_objects/objects_as_csvs/"
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Loop through files: load, process, and save output
for (file in csv_files) {
  file_name <- tools::file_path_sans_ext(basename(file))  # Remove .csv extension
  
  # Load CSV 
  data <- read.csv(file, check.names = FALSE)
  
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
    sub_titles1 <- df$labs[2, 2:ncol(df$labs)]  # Sub-indicator titles
    sub_titles2 <- df$labs[3, 2:ncol(df$labs)]  # Sub-indicator titles
  } else {
    indic_title <- df$labs[1, 2]  # Overall indicator title
    sub_titles1 <- df$labs[2, 2:ncol(df$labs)]  # No sub-indicators
    sub_titles2 <- NA
  }
  
  trend_sym <- df$vals$mean_sym
  slope_sym <- df$vals$slope_sym
  mean_vals <- round(df$vals$mean, 2)
  sd_vals <- round(df$vals$sd, 2)
  minyear <- round(df$vals$minyear, 0)
  maxyear <- round(df$vals$maxyear, 0)
  
  # Ensure all extracted values are treated as vectors
  if (length(sub_titles2) == 1 && is.na(sub_titles2)) {
    sub_titles2 <- rep("Caribbean", length(mean_vals))  # Handle case where no sub-indicators exist
  }
  
  # Iterate over all sub-indicators
  for (j in seq_along(mean_vals)) {
    all_dat <- c(indic_title[j], sub_titles1[j], sub_titles2[j], trend_sym[j], slope_sym[j], mean_vals[j], sd_vals[j], minyear[j], maxyear[j])
    all_dat <- unlist(all_dat)
    sum_list[[length(sum_list) + 1]] <- all_dat
  }
  
}


sum_table<-as.data.frame(do.call("rbind",sum_list))
colnames(sum_table)<-c("Indicator", "Units", "Extent/sub-indicator", "Trend symbol", "Slope symbol", "Mean", "SD", "Min year", "Max year")

#Remove sub-indicators from the table if the max year is less than 2022 (to focus on recent trends only)
sum_table = sum_table %>% 
  filter("Max year" > 2021)

gt(sum_table)


sum_table %>%
  gt() %>% 
  tab_row_group(
    label = "Recent trend is average",
    rows = `Trend symbol` == "●"
  ) %>%
  tab_row_group(
    label = "Recent trend below average",
    rows = `Trend symbol` == "-"
  ) %>% 
  tab_row_group(
    label = "Recent trend above average",
    rows = `Trend symbol` == "+"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  cols_label(
    Indicator = ""  # removes the column label
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "light gray",
      weight = px(2)
    ),
    locations = cells_body(
      columns = c(Indicator)
    )
  )

