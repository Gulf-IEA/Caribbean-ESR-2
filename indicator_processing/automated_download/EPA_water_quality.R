# Water quality indicator from the EPA water quality data portal: https://www.epa.gov/waterdata/TADA

# STILL WORKING ON THIS, NOT COMPLETE

if(!"remotes"%in%installed.packages()){
  install.packages("remotes")
}

remotes::install_github("USEPA/EPATADA", ref = "develop", dependencies = TRUE, force = TRUE)

library(EPATADA)

EPATADA::TADA_GetTemplate()

# see here for a list of arguments in the DataRetrieval function: https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.html

# Start by pulling all the data. The function below takes about 10 min to run

dataset_0 = EPATADA::TADA_BigDataRetrieval(
     startDate = "1980-01-01",
     endDate = "2023-12-31",
     countrycode = "US", 
     huc = "null",
     siteid = "null",
     siteType = "Ocean",
     characteristicName = "null",
     characteristicType = c("Microbiological","Nutrient","Physical"),
     sampleMedia = c("water","Water"),
     statecode = c("PR","VI"), 
     applyautoclean = TRUE
   )


# Some data exploration

dat = dataset_0 #rename to something shorter



###### Missing values exploration

# Count missing values in each column
sapply(dat, function(x) sum(is.na(x)))

# Visualize missing values pattern
library(ggplot2)
library(naniar)  # Package for handling missing data in visualizations

gg_miss_var(dat)  # Shows missing values by variable

# Remove columns with more than 300,000 missing values
dat <- dat[, colSums(is.na(dat)) <= 300000]

# Confirm the columns were removed
dim(dat)


# Save this file so we don't have to download it again

saveRDS(dat, "indicator_data/intermediateFiles/EPA_water_quality_raw_11_5_24.rds")

dat = readRDS("indicator_data/intermediateFiles/EPA_water_quality_raw_11_5_24.rds")


#### Filter to only include enterococcus measurements

# Filter rows where TADA.CharacteristicName is "ENTEROCOCCUS"
enterococcus <- dat %>%
  filter(TADA.CharacteristicName == "ENTEROCOCCUS")

head(enterococcus)



# Remove rows with NAs in specified columns
enterococcus <- enterococcus %>%
  filter(!is.na(ResultMeasureValue) & !is.na(ActivityStartDate))

head(enterococcus)



###### Missing values exploration again for entero data

# Count missing values in each column
sapply(enterococcus, function(x) sum(is.na(x)))

# Visualize missing values pattern
library(ggplot2)
library(naniar)  # Package for handling missing data in visualizations

gg_miss_var(enterococcus)  # Shows missing values by variable

# Remove columns with more than 20000 missing values
enterococcus <- enterococcus[, colSums(is.na(enterococcus)) <= 20000]


#Use only routine samples
ent <- enterococcus[which(enterococcus$ActivityTypeCode=="Sample-Routine"), ] 


###################################################

#### Pick back up here



count <- read.table("C://Users/mkarnauskas/Desktop/GOM_ESR_analyses/Gulf_counties.csv", header=T, sep=",")
co <- toupper(count$COUNTY[which(count$STATE=="Florida")])
fl <- fl[which(fl$County %in% co), ]                         # use only Gulf side counties (W FL)
fl <- fl[which(fl$County!="HARDEE"), ]                       # take out non-coastal counties
fl <- fl[which(fl$County!="MARION"), ]
dim(fl)

fl$dep_m <- fl$Activity.Depth
fl$dep_m[which(fl$Activity.Depth.Unit=="ft")] <- fl$Activity.Depth[which(fl$Activity.Depth.Unit=="ft")]/3
fl$dep_m[which(fl$dep_m==9999)] <- NA
fl <- fl[which(fl$dep_m<10), ]                            # use only samples from depths <10 m
dim(fl)
fl <- fl[which(fl$Value.Type=="Actual"), ]                # use only 'actual' samples
dim(fl)
fl <- fl[which(fl$Result.Value.Status=="Final"), ]        # use only 'final' samples
dim(fl)

dates <- fl$date                                          # format dates from Excel
dates <- as.character(dates)
fl$dates <- as.POSIXct(dates, format = "%m/%d/%Y")
fl$yr <- substr(fl$dates, 1, 4)                           # add month and year columns
fl$mo <- substr(fl$dates, 6, 7)
head(fl)

fl$Result.Value.as.Text <- as.vector(as.character(fl$Result.Value.as.Text))
fl$Result.Value.as.Text[which(fl$Result.Value.as.Text=="*Non-detect")] <- NA      # put NAs in data values where not recorded
fl$Result.Value.as.Text[which(fl$Result.Value.as.Text=="*Not Reported")] <- NA
fl$Result.Value.as.Text <- as.numeric(fl$Result.Value.as.Text)

# QAQC of database
table(fl$yr)
table(fl$Units)
tapply(fl$Result.Value.as.Text, fl$Units, mean, na.rm=T)
tapply(fl$Result.Value.as.Text, fl$yr, mean, na.rm=T)
table(fl$Units, fl$Org.Name)

fl <- fl[-which(is.na(fl$Result.Value.as.Text)), ]    # remove NAs from database
tapply(fl$Result.Value.as.Text, fl$Units, mean)
tapply(fl$Result.Value.as.Text, fl$yr, mean)

tab <- table(fl$yr, fl$Station.ID)            # choose only stations with 12 years of monitoring data
tab[which(tab>0)] <- 1
sort(colSums(tab))
lis <- names(which(colSums(tab)==12))         # list of those stations
fl <- fl[which(fl$Station.ID %in% lis), ]
dim(fl)

fl$thresh <- 1
fl$thresh[which(fl$Result.Value.as.Text<35)] <- 0          # group low, medium, high concentration categories
fl$thresh[which(fl$Result.Value.as.Text>104)] <- 2

fl$Station.ID <- as.vector(as.character(fl$Station.ID))    # reformatting columns
fl$County <- as.vector(as.character(fl$County))

res <- table(fl$yr, fl$thresh)                             # results of low, med, high Entero []s per year
colnames(res) <- c("low", "med", "high")
res <- res[1:12,]                                          # remove 2012
res
################################    PLOTS    ###################################
par(mfrow=c(2,1), mex=0.6, mar=c(6,3,1,1)+2, xpd=F)               # plot results
barplot(res[,3]/rowSums(res), ylab="% of samples with high bacterial index")                               # number of hi concentrations
barplot(tapply(fl$Result.Value.as.Text, fl$yr, mean), ylab="average bacterial count (CFU/100 ml)")       # average CPU/100ml

# plot by county and month
par(mfrow=c(2,1), mex=0.6, mar=c(5,4,2,0)+2, xpd=F)
barplot(tapply(fl$Result.Value.as.Text, fl$County, mean, na.rm=T), cex.names=0.8, col=4, las=3)              # plot by county and month
mtext(side=2, line=4, "Enterococcus count (CFU/100ml)"); abline(0,0)
mtext(side=3, line=2, "Averages by county for Florida Gulf coast beach monitoring samples", font=2, las=1)
par(mar=c(2,4,5,0)+2, xpd=F)
barplot(tapply(fl$Result.Value.as.Text, fl$mo, mean), names.arg=mnam, cex.names=0.8, col=4, las=1)
mtext(side=2, line=4, "Enterococcus count (CFU/100ml)"); abline(0,0)
mtext(side=3, line=2, "Averages by month for Florida Gulf coast beach monitoring samples", font=2, las=1)




























































###### Basic stats for numeric variables

# Load necessary library
library(dplyr)
library(tidyr)

# Create a summary table for numeric columns with selected statistics
numeric_summary <- enterococcus %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), list(
    Min = ~ min(., na.rm = TRUE),
    Mean = ~ mean(., na.rm = TRUE),
    Median = ~ median(., na.rm = TRUE),
    Max = ~ max(., na.rm = TRUE)
  ))) %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", "Statistic"), 
               names_sep = "_") %>%
  pivot_wider(names_from = "Statistic", values_from = "value")

# View the summary table
print(numeric_summary)


# Look at the values for each column

# Specify the columns of interest
character_columns <- c("ActivityTypeCode", "ResultValueTypeName") 

# Get unique values for the specified character columns
unique_values_table <- enterococcus %>%
  select(all_of(character_columns)) %>%
  summarise(across(everything(), ~ list(unique(.))))

# Print the table
print(unique_values_table)



# We want to subset to only the routine samples

enterococcus = enterococcus %>% 
  filter(ActivityTypeCode == "Sample-Routine")


#check for duplicates in the date column

enterococcus = enterococcus %>% 
  mutate(ActivityStartDate = as.Date(ActivityStartDate, format = "%Y-%m-%d"))

duplicate_dates = enterococcus %>% 
  group_by(ActivityStartDate) %>% 
  filter(n() > 1) %>%
  ungroup()

ggplot(enterococcus, aes(x = ActivityStartDate, y = ResultMeasureValue)) +
  geom_line()

