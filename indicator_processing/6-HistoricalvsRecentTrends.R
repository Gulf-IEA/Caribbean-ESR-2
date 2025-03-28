# Code to plot historical vs. recent trends and variability for each indicator

# last updated 3/27/25 by Carissa Gervasi

rm(list = ls())

# load libraries ----------------------------
library(plotTimeSeries)
library(dplyr)
library(ggplot2)
library(ggrepel)

# find root project directory 
directory <- rprojroot::find_rstudio_root_file()
setwd(directory)

# load indicator matrix --------------------
load("indicator_data/all_indicators_matrix.rda")
matrix_data
dim(matrix_data)

class(matrix_data$year)
matrix_data$year <- as.numeric(matrix_data$year)

# cut time series to start at 2000 (most time series start in the 2000s) ---------
matrix_data = matrix_data %>% 
  filter(year >= 2000)
dim(matrix_data)

# remove indicators with at least 3 values before or after 2019 (this ensures there are at least 3 values to run a linear model on) -----------------------------------
# Define the two periods
period_1 <- 2000:2019
period_2 <- 2019:2024

# Initialize a logical vector to keep track of which indicators pass the condition
keep_cols <- logical(ncol(matrix_data) - 1)  # excluding the 'year' column

# Loop over the indicator columns (excluding the first column, which is 'year')
for (i in 2:ncol(matrix_data)) {
  # Subset data for each period
  period_1_data <- matrix_data[matrix_data$year %in% period_1, i]
  period_2_data <- matrix_data[matrix_data$year %in% period_2, i]
  
  # Count non-NA values in each period
  count_1 <- sum(!is.na(period_1_data))
  count_2 <- sum(!is.na(period_2_data))
  
  # If both periods have at least 3 non-NA values, keep the column
  if (count_1 >= 3 && count_2 >= 3) {
    keep_cols[i - 1] <- TRUE
  }
}

# Subset the dataframe to keep only the columns that passed the condition
matrix_data_filtered <- matrix_data[, c(1, which(keep_cols) + 1)]

# View the resulting dataframe
head(matrix_data_filtered)

# scale the indicator data
matrix_data_scaled <- matrix_data_filtered
matrix_data_scaled[, -1] <- scale(matrix_data_filtered[, -1])


# run linear models and extract the slopes ---------------------------
# Initialize an empty list to store the slopes
slopes_list <- list()

# Define the periods again
period_1 <- 2000:2019
period_2 <- 2019:2024

# Loop through each indicator column (skipping the 'year' column)
for (i in 2:ncol(matrix_data_scaled)) {
  
  # Extract data for the indicator for each period
  indicator_data <- matrix_data_scaled[, c("year", names(matrix_data_scaled)[i])]
  
  # Subset for period 1 (2000–2019)
  period_1_data <- indicator_data[indicator_data$year %in% period_1, ]
  period_1_model <- lm(period_1_data[[2]] ~ period_1_data$year)
  period_1_slope <- coef(period_1_model)[2]  # Extract slope for period 1
  
  # Subset for period 2 (2019–2024)
  period_2_data <- indicator_data[indicator_data$year %in% period_2, ]
  period_2_model <- lm(period_2_data[[2]] ~ period_2_data$year)
  period_2_slope <- coef(period_2_model)[2]  # Extract slope for period 2
  
  # Store the slopes and indicator name
  slopes_list[[i-1]] <- c(names(matrix_data_scaled)[i], period_1_slope, period_2_slope)
}

# Convert the list into a dataframe
slope_table <- do.call(rbind, slopes_list)
colnames(slope_table) <- c("Indicator", "historic_slope", "recent_slope")

# Convert slope_table to a data frame
slope_table <- as.data.frame(slope_table)

# Print the resulting table
print(slope_table)


# Plot it ###############################################################

# Convert the slopes to numeric (if they are not already)
slope_table$historic_slope <- as.numeric(slope_table$historic_slope)
slope_table$recent_slope <- as.numeric(slope_table$recent_slope)

x_min <- min(slope_table$historic_slope, na.rm = TRUE)
x_max <- max(slope_table$historic_slope, na.rm = TRUE)
y_min <- min(slope_table$recent_slope, na.rm = TRUE)
y_max <- max(slope_table$recent_slope, na.rm = TRUE)


# Define the polygon to shade the area below the 1:1 line (lower triangle)
shade_polygon <- data.frame(
  x = c(-Inf, Inf, Inf),
  y = c(-Inf, y_max, -Inf)
)

# Create ggplot scatter plot
ggplot(slope_table, aes(x = historic_slope, y = recent_slope, label = Indicator)) +
  geom_polygon(data = shade_polygon, aes(x = x, y = y), 
               fill = "lightgray", alpha = 0.5, inherit.aes = FALSE) +
  geom_point(color = "blue", size = 3) + 
  geom_text_repel(size = 2, box.padding = 0.5, max.overlaps = Inf) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) + 
  geom_hline(yintercept = 0, color = "gray50", linetype = "solid") +
  geom_vline(xintercept = 0, color = "gray50", linetype = "solid") +
  labs(title = "Historic vs Recent Trends by Indicator",
       x = "Historic trend (2000-2019)",
       y = "Recent trend (2019-2024)") +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


