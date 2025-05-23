# Script to pull all the indicators from the indicator_objects folder and merge the data into a matrix that can be used for stoplight plots.

# For the monthly indicators we need to just average those into annual time steps.

rm(list = ls())

# find root project directory 
directory <- rprojroot::find_rstudio_root_file()
setwd(directory)

# Load necessary libraries
library(dplyr)
library(tidyr)

# Function to extract the data from the .RData files
extract_data <- function(file_path) {
  load(file_path)
  if (exists("ind")) {
    list(
      file_name = basename(file_path),
      datelist = ind$datelist,
      indicators = ind$indicators
    )
  } else {
    NULL
  }
}

# Read the descriptive names from the CSV file

#descriptive_names <- read.csv("indicator_data/synthesisFiles/extracted_ind_object_names_REVISED.csv")
descriptive_names <- read.csv("indicator_data/synthesisFiles/extracted_ind_object_names_REVISED_MERGED.csv")

# Set the directory containing the .RData files
indicator_dir <- "indicator_objects"

# Get a list of all .RData files in the directory
rdata_files <- list.files(indicator_dir, pattern = "\\.RData$", full.names = TRUE)

# Extract data from all .RData files
all_data <- lapply(rdata_files, extract_data)
all_data <- Filter(Negate(is.null), all_data)

# Before we go any further, we need to pull out the indicators with monthly data and average them into annual indicators

# the monthly indicators include Carib_Chl, Carib_SST, DegreeHeatingWeeks, OA, turbidity, and unemployment

# Convert monthly data to annual by averaging values per year
convert_to_annual <- function(data_list, date_format) {
  # Extract the date and indicator values
  name <- data_list$file_name
  dates <- data_list$datelist
  values <- data_list$indicators
  
  # Parse the date according to the specified format
  if (date_format == "%m-%Y") {
    years <- as.integer(format(as.Date(paste0("01-", dates), "%d-%m-%Y"), "%Y"))
  } else if (date_format == "%Y%m") {
    years <- as.integer(substr(dates, 1, 4))  # Extract the first 4 characters as year
  } else if (date_format == "%b%Y") {
    years <- as.integer(format(as.Date(paste0("01", dates), "%d%b%Y"), "%Y"))
  } else {
    stop("Unsupported date format")
  }
  
  # Create a dataframe for easier manipulation
  df <- data.frame(year = years, values)
  
  # Group by year and calculate the annual average
  annual_data <- aggregate(. ~ year, data = df, FUN = mean)
  
  # Update the datelist and indicators with the annual data
  data_list$datelist <- annual_data$year
  data_list$indicators <- as.data.frame(annual_data[, -1]) 
  
  return(data_list)
}

# identify monthly datasets
for (i in 1:length(all_data))  {
  if(class(all_data[[i]]$datelist) != "integer") { print(all_data[[i]]$file_name)}
}

# Apply the function to each monthly dataset

all_data[[grep("Chl", all_data)]]$file_name   # ensure grep function will index properly
all_data[[grep("SST", all_data)]]$file_name
all_data[[grep("Weeks", all_data)]]$file_name
all_data[[grep("enforce", all_data)]]$file_name
all_data[[grep("OA", all_data)]]$file_name
all_data[[grep("Sarg", all_data)]]$file_name
all_data[[grep("turbidity", all_data)]]$file_name
all_data[[grep("unemp", all_data)]]$file_name

carib_Chl <- convert_to_annual(all_data[[grep("Chl", all_data)]], "%m-%Y")
Carib_SST <- convert_to_annual(all_data[[grep("SST", all_data)]], "%Y%m")
DegreeHeatingWeeks <- convert_to_annual(all_data[[grep("Weeks", all_data)]], "%Y%m")
enforcement <- convert_to_annual(all_data[[grep("enforce", all_data)]], "%b%Y")
OA <- convert_to_annual(all_data[[grep("OA", all_data)]], "%b%Y")
Sargassum <- convert_to_annual(all_data[[grep("Sarg", all_data)]], "%b%Y")
turbidity <- convert_to_annual(all_data[[grep("turbidity", all_data)]], "%m-%Y")
unemployment <- convert_to_annual(all_data[[grep("unemp", all_data)]], "%Y%m")

# Ok now we have all the monthly indicators as annual indicators. We need to replace these in the all_data list. 

all_data[[grep("Chl", all_data)]] = carib_Chl
all_data[[grep("SST", all_data)]] = Carib_SST
all_data[[grep("Weeks", all_data)]] = DegreeHeatingWeeks
all_data[[grep("enforce", all_data)]] = enforcement
all_data[[grep("OA", all_data)]] = OA
all_data[[grep("Sarg", all_data)]] = Sargassum
all_data[[grep("turbidity", all_data)]] = turbidity
all_data[[grep("unemp", all_data)]] = unemployment


########################################################

#Now back to making the indicator matrix

# Extract the datelist vectors and keep only those that are of class integer (this isn't really necessary now that we've taken the average of the monthly data, but can keep if we want to ignore the monthly data and skip the above code chunk)
datelists <- lapply(all_data, function(x) x$datelist)
datelists <- datelists[sapply(datelists, function(d) class(d) == "integer")]

# Determine the range of years
years <- unlist(datelists)
year_range <- seq(min(years, na.rm = TRUE), max(years, na.rm = TRUE))

# Create an empty matrix with "year" column
matrix_data <- data.frame(year = year_range)


# Function to add indicator columns to the matrix
add_indicator_columns <- function(matrix_data, data, descriptive_names) {
  datelist <- data$datelist
  if (class(datelist) != "integer") {
    return(matrix_data)
  }
  
  years <- datelist
  indicators <- data$indicators
  
  for (col_name in colnames(indicators)) {
    descriptive_name <- descriptive_names %>% 
      filter(file_name == data$file_name & ind_name == col_name) %>% 
      pull(desc_name)
    
    # Handle cases where no match is found
    if (length(descriptive_name) == 0) {
      if (length(colnames(indicators)) == 1) {
        # Use ind_name directly if only one column is present
        descriptive_name <- descriptive_names %>%
          filter(file_name == data$file_name) %>%
          pull(desc_name)
        if (length(descriptive_name) == 0) next
      } else {
        next
      }
    }
    
    indicator_values <- indicators[[col_name]]
    indicator_df <- data.frame(year = years, value = indicator_values)
    indicator_df <- indicator_df %>% complete(year = year_range, fill = list(value = NA))
    
    matrix_data[[descriptive_name]] <- indicator_df$value
  }
  
  return(matrix_data)
}

# Add all indicator columns to the matrix
for (data in all_data) {
  matrix_data <- add_indicator_columns(matrix_data, data, descriptive_names)
}

# Print the resulting matrix
print(matrix_data)
dim(matrix_data)

# re-order the columns

# Get the current column names of the matrix (excluding the first one)
current_colnames <- colnames(matrix_data)[-1]

# Match the current column names with the "desc_name" in the descriptive_names data
reorder_index <- match(current_colnames, descriptive_names$desc_name)

# Check for any unmatched columns
if (any(is.na(reorder_index))) {
  print("Warning: Some columns in the matrix do not match the CSV 'desc_name'.")
}

# Remove NA values from reorder_index and sort according to the 'order' column
valid_columns <- !is.na(reorder_index)
sorted_columns <- current_colnames[valid_columns][order(descriptive_names$order[reorder_index[valid_columns]])]

# Reorganize the matrix, keeping the first column unchanged
matrix_data <- matrix_data[, c(1, match(sorted_columns, colnames(matrix_data)))]
matrix_data
dim(matrix_data) # should be 108 columns

cbind(names(matrix_data)[-1], descriptive_names[, 1])  # check that all indicators are included!

# Save the matrix_data to an .rda file
save(matrix_data, file = "indicator_data/all_indicators_matrix.rda")

