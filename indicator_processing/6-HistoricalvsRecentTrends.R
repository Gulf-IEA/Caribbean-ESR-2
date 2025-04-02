# Code to plot historical vs. recent trends and variability for each time series indicator

# last updated 4/2/25 by Carissa Gervasi

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

# scale the indicator data for comparison among indicators
matrix_data_scaled <- matrix_data_filtered
matrix_data_scaled[, -1] <- scale(matrix_data_filtered[, -1])


# run linear models and extract the slopes & calculate variability ---------------------------
# Initialize an empty list 
slopes_list <- list()

# Define the periods again
period_1 <- 2000:2019
period_2 <- 2019:2024

# Loop through each indicator column (skipping the 'year' column)
for (i in 2:ncol(matrix_data_scaled)) {
  
  # Extract data for the indicator for each period
  indicator_data_scaled <- matrix_data_scaled[, c("year", names(matrix_data_scaled)[i])]

  # Subset for period 1 (2000–2019)
  period_1_data_scaled <- indicator_data_scaled[indicator_data_scaled$year %in% period_1, ]
  period_1_model <- lm(period_1_data_scaled[[2]] ~ period_1_data_scaled$year)
  period_1_slope <- coef(period_1_model)[2]  # Extract slope for period 1
  period_1_var <- mean(abs(scale(diff(period_1_data_scaled[[2]]))), na.rm = T)
  
  # Subset for period 2 (2019–2024)
  period_2_data_scaled <- indicator_data_scaled[indicator_data_scaled$year %in% period_2, ]
  period_2_model <- lm(period_2_data_scaled[[2]] ~ period_2_data_scaled$year)
  period_2_slope <- coef(period_2_model)[2]  # Extract slope for period 1
  period_2_var <- mean(abs(scale(diff(period_2_data_scaled[[2]]))), na.rm = T)
  
  # Store the slopes and indicator name
  slopes_list[[i-1]] <- c(names(matrix_data_scaled)[i], period_1_slope, period_2_slope, period_1_var, period_2_var)
}

# Convert the list into a dataframe
slope_table <- do.call(rbind, slopes_list)
colnames(slope_table) <- c("Indicator", "historic_slope", "recent_slope", "historic_var", "recent_var")

# Convert slope_table to a data frame
slope_table <- as.data.frame(slope_table)

# Print the resulting table
print(slope_table)

# Note: some indicators have NaN for current variability because the data points were not consecutive (can't diff without consecutive data points, this mostly applies to the RVC data which has been collected every other year in the recent time period).

# Add a column to slope_table with the category of indicator (risk and performance) so we can plot each group separately -------------------------------------------------

# Read in category assignments from CSV
category_data <- read.csv("indicator_data/synthesisFiles/extracted_ind_object_names_REVISED_MERGED.csv", stringsAsFactors = FALSE)
head(category_data)
# Select only the "desc_name" and "Category" columns
category_data <- category_data %>% select(desc_name, type)

# Merge with slope_table based on the indicator name
slope_table <- slope_table %>%
  left_join(category_data, by = c("Indicator"="desc_name"))

# Add a label based on geographic location --------------------------------
slope_table <- slope_table %>%
  mutate(Location = case_when(
    grepl("_PR$", Indicator)  ~ "Puerto Rico",
    grepl("_PR$", Indicator)  ~ "Puerto Rico",
    grepl("_VI$", Indicator)  ~ "USVI",
    grepl("_STT$", Indicator) ~ "St. Thomas and St. John",
    grepl("_STX$", Indicator) ~ "St. Croix",
    TRUE                      ~ "Caribbean"  # Default for all other cases
  ))


# Plot it ###############################################################
# Convert the slopes and variability to numeric 
slope_table$historic_slope <- as.numeric(slope_table$historic_slope)
slope_table$recent_slope <- as.numeric(slope_table$recent_slope)
slope_table$historic_var <- as.numeric(slope_table$historic_var)
slope_table$recent_var <- as.numeric(slope_table$recent_var)


# Trends

# Define plot limits 
x_min <- -0.9
x_max <- 0.9
y_min <- -0.9
y_max <- 0.9

# Define the polygon to shade below the 1:1 line (y = x)
shade_polygon <- data.frame(
  x = c(x_min-1, x_max+1, x_max+1),  # Covers full x-range
  y = c(x_min-1, x_max+1, y_min-1)   # Defines the lower triangle below y = x
)

# Re-label the categories 
new_labels = c("ccl"="Fishery-dependent indicators",
               "fi"="Fishery-independent indicators",
               "oth"="Other management indicators",
               "risk"="Risks to meeting objectives")

# define colors
location_colors <- c(
  "Puerto Rico" = "#B2182B",
  "USVI" = "#00589C",
  "St. Thomas and St. John" = "#5A9BD4",
  "St. Croix" = "#1FCDC0",
  "Caribbean" = "#882E72"
)

#define shapes
location_shapes <- c(
  "Puerto Rico" = 15,       # Square
  "USVI" = 17,             # Triangle
  "St. Thomas and St. John" = 17,  # Triangle
  "St. Croix" = 17,        # Triangle
  "Caribbean" = 16         # Circle
)

# Create ggplot scatter plot of slopes
P1 = ggplot(slope_table, aes(x = historic_slope, y = recent_slope, label = Indicator, color = Location, shape = Location)) +
  geom_polygon(data = shade_polygon, aes(x = x, y = y), 
               fill = "lightgray", alpha = 0.5, inherit.aes = FALSE) +
  geom_point(size = 3) + 
  geom_text_repel(size = 2, box.padding = 0.5, max.overlaps = Inf) +
  annotate("text", x = x_min, y = y_max, label = "more positive", color = "black", size = 4, fontface = "bold", hjust = 0) +
  annotate("text", x = x_max - 0.5, y = y_min, label = "more negative", color = "black", size = 4, fontface = "bold", hjust = 0) +
  labs(title = "Historic vs Recent Trends by Indicator",
       x = "Historic trend (2000-2019)",
       y = "Recent trend (2019-2024)") +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  facet_wrap(~type, labeller = labeller(type = new_labels)) +
  scale_color_manual(values = location_colors) +
  scale_shape_manual(values = location_shapes) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# Variability

# Define plot limits
x_min <- 0.5
x_max <- 1
y_min <- 0.5
y_max <- 1

# Define the polygon to shade below the 1:1 line (y = x)
shade_polygon <- data.frame(
  x = c(x_min-1, x_max+1, x_max+1),  # Covers full x-range
  y = c(x_min-1, x_max+1, y_min-1)   # Defines the lower triangle below y = x
)

# Create ggplot scatter plot of variability
P2 = ggplot(slope_table, aes(x = historic_var, y = recent_var, label = Indicator, color = Location, shape = Location)) +
  geom_polygon(data = shade_polygon, aes(x = x, y = y), 
               fill = "lightgray", alpha = 0.5, inherit.aes = FALSE) +
  geom_point(size = 3) + 
  geom_text_repel(size = 2, box.padding = 0.5, max.overlaps = Inf) +
  annotate("text", x = x_min, y = y_max, label = "less stable", color = "black", size = 4, fontface = "bold", hjust = 0) +
  annotate("text", x = x_max - 0.1, y = y_min, label = "more stable", color = "black", size = 4, fontface = "bold", hjust = 0) +
  labs(title = "Historic vs Recent Variability by Indicator",
       x = "Historic variability (2000-2019)",
       y = "Recent variability (2019-2024)") +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  facet_wrap(~type, labeller = labeller(type = new_labels)) +
  scale_color_manual(values = location_colors) +
  scale_shape_manual(values = location_shapes) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# Save the plots
ggsave("indicator_plots/histvscurrent_trends.png", plot = P1, width = 11, height = 7, bg = "white")
ggsave("indicator_plots/histvscurrent_var.png", plot = P2, width = 11, height = 7, bg = "white")



### END SCRIPT ###