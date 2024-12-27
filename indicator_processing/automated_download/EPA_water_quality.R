# Water quality indicator from the EPA water quality data portal: https://www.epa.gov/waterdata/TADA

# last updated by C. Gervasi on 11/21/24

# Questions:
# Why does PR monitoring end in 2016? Still trying to figure this out

rm(list = ls())

if(!"remotes"%in%installed.packages()){
  install.packages("remotes")
}

# remotes::install_github("USEPA/EPATADA", ref = "develop", dependencies = TRUE, force = TRUE)

library(EPATADA)
library(ggplot2)
library(dplyr)
library(naniar)  # Package for handling missing data in visualizations
library(tidyr)
library(ggplot2)

EPATADA::TADA_GetTemplate()

# see here for a list of arguments in the DataRetrieval function: https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.html

# Start by pulling all the data. The function below takes about 10 min to run. 

# dataset_0 = EPATADA::TADA_BigDataRetrieval(
#   startDate = "1980-01-01",
#   endDate = "2023-12-31",
#   countrycode = "US", 
#   huc = "null",
#   siteid = "null",
#   siteType = "Ocean",
#   characteristicName = "null",
#   characteristicType = c("Microbiological","Nutrient","Physical"),
#   sampleMedia = c("water","Water"),
#   statecode = c("PR","VI"), 
#   applyautoclean = TRUE
# )

#re-name the file
#dat = dataset_0

#Save this file so we don't have to download it again if we need to make changes
#saveRDS(dat, "indicator_data/intermediateFiles/EPA_water_quality_raw_11_5_24.rds")



####### Start here to work with the intermediate file currently on github
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



###### Missing values exploration 

# Count missing values in each column
sapply(enterococcus, function(x) sum(is.na(x)))

# Visualize missing values pattern
gg_miss_var(enterococcus)  # Shows missing values by variable

# Remove columns with more than 20000 missing values
enterococcus <- enterococcus[, colSums(is.na(enterococcus)) <= 20000]




#Filter so we use only routine samples, samples from depths < 10m, and "actual" samples
ent = enterococcus %>% 
  filter(ActivityTypeCode == "Sample-Routine" & 
           TADA.ActivityDepthHeightMeasure.MeasureValue < 10 &
           ResultValueTypeName == "Actual" 
  )

#Format dates and add month, year columns
ent$date2 = ent$ActivityStartDate

ent = ent %>% 
  separate(date2, c("year", "month", "day"))

ent = ent %>% 
  mutate(ActivityStartDate = as.POSIXct(ActivityStartDate, format = "%Y-%m-%d"))

ent$ResultMeasureValue = as.numeric(ent$ResultMeasureValue)


#Get rid of NAs in the measured value
ent = ent %>% 
  filter(ResultMeasureValue != "NA")

summary(ent)


# Problem: there are multiple units of measurement in the data. Keep only #/100ML and MPN/100ML
ent = ent %>% 
  filter(TADA.ResultMeasure.MeasureUnitCode == "#/100ML" | TADA.ResultMeasure.MeasureUnitCode == "MPN/100ML")


#Remove 2001 since there is no data
ent = ent %>% 
  filter(year != 2001)
table(ent$year)


ent$thresh <- 1
ent$thresh[which(ent$ResultMeasureValue<35)] <- 0          # group low, medium, high concentration categories
ent$thresh[which(ent$ResultMeasureValue>104)] <- 2


res <- table(ent$year, ent$thresh)                         # results of low, med, high Entero []s per year
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


summary_PR = summary_data %>% 
  filter(StateCode == "72")

summary_VI = summary_data %>% 
  filter(StateCode == "78")

alldat = left_join(summary_VI, summary_PR, by = "year")

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


vardata.PR <- data.frame(var = alldat$SE.y)
vardata.VI <- data.frame(var = alldat$SE.x)
ulidata.PR <- data.frame(uli = alldat$MeanValue.y + (vardata.PR$var))
llidata.PR <- data.frame(lli = alldat$MeanValue.y - (vardata.PR$var))
ulidata.VI <- data.frame(uli = alldat$MeanValue.x + (vardata.VI$var))
llidata.VI <- data.frame(lli = alldat$MeanValue.x - (vardata.VI$var))

# save as indicator object ----------------------
datdata <- as.integer(alldat$year)
inddata <- data.frame(cbind(as.numeric(alldat$MeanValue.y), as.numeric(alldat$MeanValue.x)))
ulidata <- data.frame(cbind(as.numeric(ulidata.PR$uli), as.numeric(ulidata.VI$uli)))
llidata <- data.frame(cbind(as.numeric(llidata.PR$lli), as.numeric(llidata.VI$lli)))
labs <- c("Mean Enteroccocus count" , "Number per 100 mL", "Puerto Rico",
          "Mean Enterococcus count" , "Number per 100 mL", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(inddata) <- "indicatordata"


# plot and save ----------------------------------

ind <- inddata
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.7, anom = "none", yposadj = 1)

save(ind, file = "indicator_objects/enterococcus.RData")

print("EPA WATER QUALITY -- SUCCESSFULLY RUN")


