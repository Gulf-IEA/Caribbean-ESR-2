# Code to plot hotel occupancy Puerto Rico & USVI 

# Last updated 1/6/2025 by Carissa Gervasi

rm(list = ls())

plot.new()
dev.off()

# Puerto Rico - hotel occupancy (source: PR Tourism Company)

url1<-'https://www.bde.pr.gov/BDE/PREDDOCS/I_TOUR.XLS'

library(readxl)
library(httr)
packageVersion("readxl")

GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
df <- read_excel(tf, 4L)
str(df)


df2 = df[c(57,58),]

df2t = as.data.frame(t(df2))

df2t = df2t[-c(1),]


yrs_PR = as.integer(df2t$V1)
hotel_PR = as.numeric(df2t$V2)

# Create a data frame
PR_hotel <- data.frame(Year = yrs_PR, Hotel_occupancy = hotel_PR)


############################

#USVI

# Load necessary libraries
library(pdftools)
library(dplyr)
library(tidyr)

# Specify the URL of the PDF
url2 <- "http://usviber.org/wp-content/uploads/2023/12/Tourism-indicator-2022-12-28-23-1.pdf"

# Specify the destination file path
destfile <- "indicator_data/intermediateFiles/Tourism-indicator-2022-12-28-23-1.pdf"

# Download the PDF
download.file(url2, destfile, mode = "wb")

# Check if the file has been downloaded
file.exists(destfile)

# Load the PDF
pdf_file <- "indicator_data/intermediateFiles/Cruise-indicator-2022-12-28-23-1.pdf"

# Extract text from the PDF
pdf_text <- pdf_text(pdf_file)

# Extract the relevant page (assuming the table is on the second page)
page_text <- pdf_text[2]

# Split the text into lines
lines <- strsplit(page_text, "\n")[[1]]

# Identify the lines containing the Visitor Arrivals table
# (assuming the table starts with "VISITOR ARRIVALS" and ends before "VISITOR EXPENDITURES")
start_line <- grep("HOTELS AND OTHER TOURIST ACCOMODATIONS", lines)
end_line <- grep("HOTEL GUESTS BY ORIGIN", lines)
table_lines <- lines[(start_line + 1):(end_line - 1)]

# Combine lines into a single text block and split by spaces
table_text <- paste(table_lines, collapse = " ")
table_data <- strsplit(table_text, " +")[[1]]

# Create years column
years <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)

# Extract and clean the total visitors data by removing commas
hotel_occupancy <- gsub(",", "", table_data[8:22])
hotel_occupancy <- as.numeric(hotel_occupancy)

# Create a data frame
USVI_hotel <- data.frame(Year = years, Hotel_guests = hotel_occupancy)

# Print the data frame
print(USVI_hotel)




##########

all_years <- data.frame(Year = min(PR_hotel$Year, USVI_hotel$Year) : max(PR_hotel$Year, USVI_hotel$Year))

# Merge the data frames with the complete data frame to fill in missing years with NA
combined_df <- merge(all_years, PR_hotel, by = "Year", all.x = TRUE)
combined_df <- merge(combined_df, USVI_hotel, by = "Year", all.x = TRUE)

# save as indicator object ----------------------
datdata <- all_years$Year
inddata <- data.frame(cbind(combined_df$Hotel_occupancy, combined_df$Hotel_guests))
labs <- c("Total non-resident hotel registrations" , "numbers of people", "Puerto Rico",
          "Total hotel guests" , "numbers of people", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------

ind <- inddata
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.7, anom = "none", yposadj = 1)

save(ind, file = "indicator_objects/hotel_occupancy.RData")

print("hotels -- SUCCESSFULLY RUN")

###############################  END  #############################