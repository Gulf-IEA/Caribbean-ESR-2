# Code to develop population indicator using UN API

# Last updated 6/24/2024 by Carissa Gervasi

## R tutorials for using the API can be found here: https://population.un.org/dataportal/about/dataapi
# API kept failing, tried Gemini but all methods failed - seems to be an issue with firewall
# revert to manual download  :( 

# data are housed here:  https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=CSV%20format
# scroll down to "Population" and download "1950-2100, all scenarios (gz)  Total Population on 01 July."
# extract CSV file and put in indicator_data/inputsToBeUpdatedAnnually/ folder. 

rm(list = ls())

plot.new()
dev.off()

library(data.table)

# When code needs to be updated, just need to update the end year here:

load("indicator_processing/spec_file.RData")

# define years  --------------------------------
styear <- 1990
enyear <- terminal_year

# read in data file ---------------
full_data <- fread("indicator_data/inputsToBeUpdatedAnnually/WPP2024_TotalPopulationBySex.csv")

# 2. Filter for the two territories and the "Medium" variant
# This variant contains the full annual time series from 1950 to 2100
subset_data <- full_data[Location %in% c("Puerto Rico", "United States Virgin Islands") & 
                           Variant == "Medium", 
                         .(Location, year = Time, population = PopTotal * 1000)]

# 3. Pivot the data so each territory has its own column
# dcast transforms data from long format to wide format
final_df <- dcast(subset_data, year ~ Location, value.var = "population")

# 4. Rename the columns to your specific requirements
setnames(final_df, 
         old = c("Puerto Rico", "United States Virgin Islands"), 
         new = c("pop_PR", "pop_USVI"))

# 5. Convert to a standard data frame
final_df <- as.data.frame(final_df)
final_df <- final_df[which(final_df$year >= styear & final_df <= enyear), ]

# save as indicator object ----------------------
datdata <- styear:enyear
inddata <- data.frame(cbind(final_df$pop_PR, final_df$pop_USVI))
labs <- c("Total population" , "Thousands of people", "Puerto Rico",
          "Total population" , "Thousands of people", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------
ind <- inddata
plotIndicatorTimeSeries(ind, plotrownum = 2, coltoplot = 1:2, sublabel = TRUE, trendAnalysis = T)

save(ind, file = "indicator_objects/population.RData")

############  END  ################

print("population -- SUCCESSFULLY RUN")




