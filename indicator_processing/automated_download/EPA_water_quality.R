# Water quality indicator from the EPA water quality data portal: https://www.epa.gov/waterdata/TADA

# STILL WORKING ON THIS, NOT COMPLETE


# Questions:
# different units, how to deal with this? Subset or convert somehow?
# filtering to only sites with long time series... split out USVI into STT, STJ, STX? Or leave as one? Split PR at all? Subset out the inland counties for PR?
# sites where multiple measurements were taken on the same day, how to deal with those? Remove one? Just include both in the annual average?
# Why does PR monitoring end in 2016?


if(!"remotes"%in%installed.packages()){
  install.packages("remotes")
}

remotes::install_github("USEPA/EPATADA", ref = "develop", dependencies = TRUE, force = TRUE)

library(EPATADA)
library(ggplot2)
library(dplyr)
library(naniar)  # Package for handling missing data in visualizations
library(tidyr)
library(ggplot2)

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


gg_miss_var(enterococcus)  # Shows missing values by variable

# Remove columns with more than 20000 missing values
enterococcus <- enterococcus[, colSums(is.na(enterococcus)) <= 20000]


## Do I need to remove non-coastal counties for PR??


# filter so we use only routine samples, samples from depths < 10m, and "actual" samples

ent = enterococcus %>% 
  filter(ActivityTypeCode == "Sample-Routine" & 
           TADA.ActivityDepthHeightMeasure.MeasureValue < 10 &
           ResultValueTypeName == "Actual" 
           )



# format dates and add month, year columns

ent$date2 = ent$ActivityStartDate

ent = ent %>% 
  separate(date2, c("year", "month", "day"))

ent = ent %>% 
  mutate(ActivityStartDate = as.POSIXct(ActivityStartDate, format = "%Y-%m-%d"))

head(ent)

str(ent)

ent$ResultMeasureValue = as.numeric(ent$ResultMeasureValue)


#get rid of NAs in the measured value

ent = ent %>% 
  filter(ResultMeasureValue != "NA")

summary(ent)

table(ent$ResultMeasure.MeasureUnitCode)
table(ent$TADA.ResultMeasure.MeasureUnitCode)


# Problem: there are multiple units of measurement in the data. Keep only #/100ML and MPN/100ML

ent = ent %>% 
  filter(TADA.ResultMeasure.MeasureUnitCode == "#/100ML" | TADA.ResultMeasure.MeasureUnitCode == "MPN/100ML")

table(ent$TADA.ComparableDataIdentifier, ent$TADA.ResultMeasure.MeasureUnitCode)


table(ent$MonitoringLocationName)


test = subset(ent, ent$MonitoringLocationName == "Buccaneer Beach")


table(test$ActivityStartDate)


test2 = as.data.frame(table(ent$MonitoringLocationName, ent$ActivityStartDate))

test3 = subset(ent, ent$MonitoringLocationName == "Condo Row (Princess)")


as.data.frame(table(ent$year, ent$MonitoringLocationName))


# First remove 2001

ent = ent %>% 
  filter(year != 2001)

table(ent$year)



## split PR and USVI 

PR_ent = ent %>% 
  filter(StateCode == "72")

VI_ent = ent %>% 
  filter(StateCode == "78")



tab <- table(PR_ent$year, PR_ent$MonitoringLocationName)            # choose only stations with 7 years of monitoring data for PR
tab[which(tab>0)] <- 1
sort(colSums(tab))
lis <- names(which(colSums(tab)==7))         # list of those stations
PR_ent2 <- PR_ent[which(PR_ent$MonitoringLocationName %in% lis), ]
dim(PR_ent2)
table(PR_ent2$MonitoringLocationName, PR_ent2$year)


tab <- table(VI_ent$year, VI_ent$MonitoringLocationName)            # choose only stations with 18 years of monitoring data for USVI
tab[which(tab>0)] <- 1
sort(colSums(tab))
lis <- names(which(colSums(tab)==18))         # list of those stations
VI_ent2 <- VI_ent[which(VI_ent$MonitoringLocationName %in% lis), ]
dim(VI_ent2)
table(VI_ent2$MonitoringLocationName, VI_ent2$year)
table(VI_ent2$MonitoringLocationName, VI_ent2$CountyCode)



# QAQC of database
table(ent$year)
tapply(ent$ResultMeasureValue, ent$year, mean, na.rm=T)
tapply(PR_ent$ResultMeasureValue, PR_ent$year, mean, na.rm=T)
tapply(VI_ent$ResultMeasureValue, VI_ent$year, mean, na.rm=T)



ent$thresh <- 1
ent$thresh[which(ent$ResultMeasureValue<35)] <- 0          # group low, medium, high concentration categories
ent$thresh[which(ent$ResultMeasureValue>104)] <- 2


res <- table(ent$year, ent$thresh)                             # results of low, med, high Entero []s per year
colnames(res) <- c("low", "med", "high")
res
################################    PLOTS    ###################################
par(mfrow=c(2,1), mex=0.6, mar=c(6,3,1,1)+2, xpd=F)               # plot results
barplot(res[,3]/rowSums(res), ylab="% of samples with high bacterial index")                               # number of hi concentrations
barplot(tapply(ent$TADA.ResultMeasureValue, ent$year, mean), ylab="average bacterial count (#/100 ml)")       # average #/100ml

# plot by county and month
par(mfrow=c(2,1), mex=0.6, mar=c(5,4,2,0)+2, xpd=F)
barplot(tapply(ent$TADA.ResultMeasureValue, ent$CountyCode, mean, na.rm=T), cex.names=0.8, col=4, las=3)              # plot by county and month
mtext(side=2, line=4, "Enterococcus count (#/100ml)"); abline(0,0)
mtext(side=3, line=2, "Averages by county for PR and USVI beach monitoring samples", font=2, las=1)
par(mar=c(2,4,5,0)+2, xpd=F)
barplot(tapply(ent$TADA.ResultMeasureValue, ent$month, mean, na.rm=T), cex.names=0.8, col=4, las=1)
mtext(side=2, line=4, "Enterococcus count (#/100ml)"); abline(0,0)
mtext(side=3, line=2, "Averages by month for PR and USVI beach monitoring samples", font=2, las=1)






# Calculate mean and standard error for each year and CountyCode
summary_data <- ent %>%
  group_by(year, CountyCode) %>%
  summarize(
    MeanValue = mean(TADA.ResultMeasureValue, na.rm = TRUE),
    SE = sd(TADA.ResultMeasureValue, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Create the bar plot with error bars
ggplot(summary_data, aes(x = factor(year), y = MeanValue)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = MeanValue - SE, ymax = MeanValue + SE), width = 0.2) +
  facet_wrap(~ CountyCode, scales = "free_y") +
  labs(
    x = "Year",
    y = "Mean TADA.ResultMeasureValue",
    title = "Mean TADA.ResultMeasureValue by Year and County"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12))




# Calculate mean and standard error for each year and StateCode
summary_data <- ent %>%
  group_by(year, StateCode) %>%
  summarize(
    MeanValue = mean(TADA.ResultMeasureValue, na.rm = TRUE),
    SE = sd(TADA.ResultMeasureValue, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Create the bar plot with error bars
ggplot(summary_data, aes(x = factor(year), y = MeanValue)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = MeanValue - SE, ymax = MeanValue + SE), width = 0.2) +
  facet_wrap(~ StateCode) +
  labs(
    x = "Year",
    y = "Enterococcus count (#/100ml)",
    title = "Mean counts by Year and Area"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12))





























































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

