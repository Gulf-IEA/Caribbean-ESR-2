# Code to convert .rds files in the indicator_outputs folder to .csv files, for easier sharing as needed. Need to convert this into a function.

# updated by Carissa Gervasi on 3/20/25

# Load the .RData file
load("indicator_objects/Carib_SST.RData")

# Create labels with blank first column
labels_df <- data.frame(matrix(nrow = 3, ncol = ncol(ind$labels) + 1))
labels_df[, 2:ncol(labels_df)] <- ind$labels  # Fill labels, leaving column 1 blank

# Extract main data and format
formatted_data <- data.frame(datelist = ind$datelist, ind$indicators)

# Define output path
output_file <- "indicator_objects/objects_as_csvs/Carib_SST.csv"

# Create output directory if it doesn't exist
if (!dir.exists("converted_files")) {
  dir.create("converted_files", recursive = TRUE)
}

# Write labels first
write.table(labels_df, file = output_file, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, na = "")

# Append actual data **without column headers**
write.table(formatted_data, file = output_file, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE, na = "")

cat("Conversion complete! Saved as:", output_file)


