# Law enforcement incidents data from Carolyn Sramek (sent on 9/10/24) - https://docs.google.com/document/d/1u89fmMIrM8gMUrz6C0KtvJJGt4n5wvAkCfLyILQB0xA/edit?usp=sharing

rm(list = ls())

plot.new()
dev.off()


library(plotTimeSeries)

dat2 = conv2indicatordata("indicator_data/inputsToBeUpdatedAnnually/NEIS law enforcement incidents.csv", default = T)
dat2

dat2$datelist
class(dat2$datelist)
dat2$datelist <- str_replace(dat2$datelist, "-", "20")
dat2$datelist

# plot and save ----------------------------------

ind <- dat2
plotIndicatorTimeSeries(ind, coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = F, dateformat = "%b%Y", sublabel = FALSE, widadj = 1, hgtadj = 1, anom = "none", yposadj = 1)

save(ind, file = "indicator_objects/enforcement.RData")

print("enforcement -- SUCCESSFULLY RUN")
