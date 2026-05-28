# Code to plot number of cruise ship visitors and air passengers Puerto Rico & USVI 

# Last updated 10/17/2024 by Carissa Gervasi

rm(list = ls())

plot.new()
dev.off()


### PUERTO RICO ###

#Originally, PR data came from bde.pr.gov. But as of 2026 those data are no longer on the website. So, I found it at the Puerto Rico Tourism Company website. https://tourism.pr.gov/statistics/. 

# Air passenger movement (CY) is from the Aerostar & Ports Authority
# Cruise ships & visitors (CY) is from the San Juan Cruise Port & City of Ponce

# will need to add up the summary tables on each page... might be a pain to do with pdf scraper


# Puerto Rico - cruise passengers 

#url1<-'https://tourism.pr.gov/wp-content/uploads/2025/09/CY-Cruise-Info.pdf'

# Load necessary libraries
library(pdftools)
library(dplyr)
library(tidyr)

# Specify the URL of the PDF
url <- "https://tourism.pr.gov/wp-content/uploads/2025/09/CY-Cruise-Info.pdf"

# Specify the destination file path
destfile <- "indicator_data/intermediateFiles/CY-Cruise-Info-2025.pdf"

# Download the PDF
download.file(url, destfile, mode = "wb")

# Check if the file has been downloaded
file.exists(destfile)

# Load the PDF
pdf_file <- "indicator_data/intermediateFiles/CY-Cruise-Info-2025.pdf"

# Extract text from the PDF
pdf_text <- pdf_text(pdf_file)

# Extract the relevant page (assuming the table is on the first page)
page_text <- pdf_text[1]

# Split the text into lines
lines <- strsplit(page_text, "\n")[[1]]

# Identify the lines containing the Visitor Arrivals table
# (assuming the table starts with "VISITOR ARRIVALS" and ends before "VISITOR EXPENDITURES")
start_line <- grep("2025", lines)
end_line <- grep("MARKET ", lines)
table_lines <- lines[(start_line + 1):(end_line - 1)]

# Combine lines into a single text block and split by spaces
table_text <- paste(table_lines, collapse = " ")
table_data <- strsplit(table_text, " +")[[1]]

# Create years column
years <- c(2000,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024)

# Extract and clean the total visitors data by removing commas
cruise_passengers <- gsub(",", "", table_data[82:97])
cruise_passengers <- as.numeric(cruise_passengers)

# Extract and clean the total visitors data by removing commas
air_passengers <- gsub(",", "", table_data[62:77])
air_passengers <- as.numeric(air_passengers)    

# Create a data frame
USVI_cruise <- data.frame(Year = years, Cruise_passengers = cruise_passengers)
USVI_air <- data.frame(Year = years, Air_passengers = air_passengers)

# Print the data frame
print(USVI_cruise)
print(USVI_air)




# Puerto Rico - air passengers 

# Load necessary libraries
library(pdftools)
library(dplyr)
library(tidyr)

# Specify the URL of the PDF
url1 <- "https://tourism.pr.gov/wp-content/uploads/2025/09/Passenger-Movement-CY.pdf"

# Specify the destination file path
destfile <- "indicator_data/intermediateFiles/CY-Passenger-Movement-2025.pdf"

# Download the PDF
download.file(url1, destfile, mode = "wb")

# Check if the file has been downloaded
file.exists(destfile)

# Load the PDF
pdf_file <- "indicator_data/intermediateFiles/CY-Passenger-Movement-2025.pdf"

# Extract text from the PDF
pdf_text <- pdf_text(pdf_file)

# Extract the relevant page (assuming the table is on the first page)
page_text <- pdf_text[1]


############################

### USVI ###

# I had to dig around on the website a bit to find the tourism indicator for 2024. I found it here: https://usviber.org/studies/. Under "Tourism Studies". The pdf was called "USVI Tourism Indicator 2024".

# Load necessary libraries
library(pdftools)
library(dplyr)
library(tidyr)

# Specify the URL of the PDF
url2 <- "https://usviber.org/wp-content/uploads/2025/09/TOUR242.pdf"

# Specify the destination file path
destfile <- "indicator_data/intermediateFiles/Tourism-indicator-2024.pdf"

# Download the PDF
download.file(url2, destfile, mode = "wb")

# Check if the file has been downloaded
file.exists(destfile)

# Load the PDF
pdf_file <- "indicator_data/intermediateFiles/Tourism-indicator-2024.pdf"

# Extract text from the PDF
pdf_text <- pdf_text(pdf_file)

# Extract the relevant page (assuming the table is on the first page)
page_text <- pdf_text[1]

# Split the text into lines
lines <- strsplit(page_text, "\n")[[1]]

# Identify the lines containing the Visitor Arrivals table
# (assuming the table starts with "VISITOR ARRIVALS" and ends before "VISITOR EXPENDITURES")
start_line <- grep("VISITOR ARRIVALS", lines)
end_line <- grep("VISITOR EXPENDITURES", lines)
table_lines <- lines[(start_line + 1):(end_line - 1)]

# Combine lines into a single text block and split by spaces
table_text <- paste(table_lines, collapse = " ")
table_data <- strsplit(table_text, " +")[[1]]

# Create years column
years <- c(2000,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024)

# Extract and clean the total visitors data by removing commas
cruise_passengers <- gsub(",", "", table_data[82:97])
cruise_passengers <- as.numeric(cruise_passengers)

# Extract and clean the total visitors data by removing commas
air_passengers <- gsub(",", "", table_data[62:77])
air_passengers <- as.numeric(air_passengers)    

# Create a data frame
USVI_cruise <- data.frame(Year = years, Cruise_passengers = cruise_passengers)
USVI_air <- data.frame(Year = years, Air_passengers = air_passengers)

# Print the data frame
print(USVI_cruise)
print(USVI_air)




##########

all_years <- data.frame(Year = min(PR_cruise$Year, USVI_cruise$Year) : max(PR_cruise$Year, USVI_cruise$Year))

# Merge the data frames with the complete data frame to fill in missing years with NA
combined_df <- merge(all_years, PR_cruise, by = "Year", all.x = TRUE)
combined_df <- merge(combined_df, USVI_cruise, by = "Year", all.x = TRUE)
combined_df <- merge(combined_df, PR_air, by = "Year", all.x = TRUE)
combined_df <- merge(combined_df, USVI_air, by = "Year", all.x = TRUE)

# save as indicator object ----------------------
datdata <- all_years$Year
inddata <- data.frame(cbind(combined_df$Cruise_passengers.x, combined_df$Cruise_passengers.y, combined_df$Air_passengers.x, combined_df$Air_passengers.y))
labs <- c("Cruise passengers" , "Thousands of people", "Puerto Rico",
          "Cruise passengers" , "Thousands of people", "USVI",
          "Air passengers" , "Thousands of people", "Puerto Rico",
          "Air passengers" , "Thousands of people", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------

ind <- inddata
plotIndicatorTimeSeries(ind, coltoplot = 1:4, plotrownum = 2, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.7, anom = "none", yposadj = 1)

save(ind, file = "indicator_objects/cruise_air_visitors.RData")

print("tourism -- SUCCESSFULLY RUN")

###############################  END  #############################