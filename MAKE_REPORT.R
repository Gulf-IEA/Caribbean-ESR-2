# RUN ENTIRE REPORT

rm(list = ls())

directory <- rprojroot::find_rstudio_root_file()
setwd(directory)
getwd()

dir("indicator_processing/")[1:5]

source("indicator_processing/1-CalculateAllIndicators.R")
source("indicator_processing/2-PlotAllIndicators.R")      
setwd(directory)
source("indicator_processing/3-ExtractIndObjectNames.R")   
source("indicator_processing/4-CreateIndicatorMatrix.R")
dev.off()
source("indicator_processing/5-multivariateSummary.R")

# END 