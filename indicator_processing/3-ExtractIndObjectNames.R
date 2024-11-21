# Code to extract the column names from the indicator dataframes within each indicator in the indicator_objects folder. The resulting csv file will be used as a lookup table so we can re-name the indicators.

rm(list = ls())

#  find root directory for project ---------------

directory <- rprojroot::find_rstudio_root_file()
setwd(directory)

# Load necessary libraries
library(dplyr)

# Function to extract column names from the 'indicators' data frame within the 'ind' list
extract_column_names <- function(file_path) {
  load(file_path)
  file_name <- basename(file_path)
  
  # Check if the loaded object contains a list named 'ind' and a data frame named 'indicators' within 'ind'
  if (exists("ind") && is.list(ind) && "indicators" %in% names(ind)) {
    # Extract column names from the 'indicators' data frame within 'ind' list
    col_names <- colnames(ind$indicators)
    if(length(col_names) == 0)  { col_names <- NA  }  # MK added this to deal with error if missing name
    
    # Create a data frame with file name and column names
    data.frame(file_name = file_name, ind_name = col_names)
  } else {
    # Return an empty data frame if 'ind' list or 'indicators' data frame doesn't exist
    data.frame(file_name = character(), ind_name = character())
  }
}

# Set the directory containing the .RData files
indicator_dir <- "indicator_objects"

# Get a list of all .RData files in the directory
rdata_files <- list.files(indicator_dir, pattern = "\\.RData$", full.names = TRUE)
rdata_files

# Initialize an empty data frame to store the results
all_indicators <- data.frame(file_name = character(), ind_name = character())

# Loop through each .RData file and extract the column names
for (file in rdata_files) {
  indicators <- extract_column_names(file)
  all_indicators <- bind_rows(all_indicators, indicators)
}

# Print the resulting data frame
print(all_indicators)

# Save the data frame to a CSV file for manual edits
write.csv(all_indicators, "indicator_data/synthesisFiles/extracted_ind_object_names.csv", row.names = FALSE)












### MANDY'S CODE - PUSHING IT ASIDE FOR THE MOMENT

# merge in revised CSV with manual object names -------------

rm(list = ls())

d1 <- read.csv("indicator_data/synthesisFiles/extracted_ind_object_names.csv")
dm <- read.csv("indicator_data/synthesisFiles/extracted_ind_object_names_REVISED.csv")

head(d1)
head(dm)
dim(d1)
dim(dm)

lis <- names(which(table(d1$file_name) == 1))       # identify indicator files with only one column
d1$ind_name[which(d1$file_name %in% lis)] <- ""     # replace ind_name with blank for those single column files 
dm$ind_name[which(dm$file_name %in% lis)] <- ""

d1$nam <- paste0(d1$file_name, "_", d1$ind_name)    # make identifier with file name and col name for merging
dm$nam <- paste0(dm$file_name, "_", dm$ind_name)

d <- merge(dm, d1, by = "nam", all = TRUE)
head(d)
dim(d)
d <- d[order(d$order), 1:5]
d
apply(is.na(d), 2, summary)  # check no NAs

names(d)
names(d)[2] <- "file_name"
names(d)[2] <- "ind_name"

write.csv(d, "indicator_data/synthesisFiles/extracted_ind_object_names_REVISED_MERGED.csv", row.names = FALSE)

###########  end   ############

