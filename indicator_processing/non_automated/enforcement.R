# Law enforcement incidents data from Carolyn Sramek (sent on 9/10/24) - https://docs.google.com/document/d/1u89fmMIrM8gMUrz6C0KtvJJGt4n5wvAkCfLyILQB0xA/edit?usp=sharing

rm(list = ls())

plot.new()
dev.off()

# The data were manually entered into a csv file. This code makes a side-by-side bar plot with incidents and patrols over time.

# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# Load the dataset
df <- read.csv("indicator_data/inputsToBeUpdatedAnnually/NEIS law enforcement incidents.csv", skip = 3, header = FALSE)

# Rename columns
colnames(df) <- c("Date", "Incidents", "Patrols")

# Convert numeric columns
df <- df %>%
  mutate(Incidents = as.numeric(Incidents),
         Patrols = as.numeric(Patrols)) 

# Convert Date column from "22-Jan" format to a proper Date type
df <- df %>%
  mutate(Date = parse_date_time(Date, orders = "y-b")) %>%
  mutate(Date = as.Date(Date))  # Ensure it's a Date object

# Reshape data for ggplot
df_long <- df %>%
  pivot_longer(cols = c("Incidents", "Patrols"),
               names_to = "Category",
               values_to = "Count")

# Ensure bars appear side by side
p = ggplot(df_long, aes(x = factor(Date), y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +  
  scale_x_discrete(labels = format(df$Date, "%b-%Y")) +  # Format x-axis dates
  labs(title = "Federal Law Enforcement Incidents and Patrols",
       x = "Date",
       y = "Count",
       fill = "Legend") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("indicator_plots/enforcement_barplot.png", plot = p, width = 8, height = 4)


# look at the ratio of incidents:patrols
df$ratio = df$Incidents/df$Patrols

# Ensure bars appear side by side
p2 = ggplot(df, aes(x = factor(Date), y = ratio)) +
  geom_bar(stat = "identity") +  
  scale_x_discrete(labels = format(df$Date, "%b-%Y")) +  # Format x-axis dates
  labs(title = "Federal Law Enforcement Incidents/Patrols",
       x = "Date",
       y = "Ratio") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("indicator_plots/enforcement_barplot_ratio.png", plot = p2, width = 8, height = 4)



print("enforcement -- SUCCESSFULLY RUN")




## Old code


# library(plotTimeSeries)
# 
# dat2 = conv2indicatordata("indicator_data/inputsToBeUpdatedAnnually/NEIS law enforcement incidents.csv", default = T)
# dat2
# 
# dat2$datelist
# class(dat2$datelist)
# dat2$datelist <- as.Date(dat2$datelist, format = "%y-%b")
# #dat2$datelist <- str_replace(dat2$datelist, "-", "20")
# dat2$datelist
# 
# # plot and save ----------------------------------
# 
# ind <- dat2
# plotIndicatorTimeSeries(ind, coltoplot = 2, plotrownum = 1, plotcolnum = 2, trendAnalysis = F, dateformat = "%b%Y", sublabel = FALSE, widadj = 1, hgtadj = 1, anom = "none", yposadj = 1)
# 
# save(ind, file = "indicator_objects/enforcement.RData")
# 
# print("enforcement -- SUCCESSFULLY RUN")
