# fix for dealing with internet timeout error 
# https://stackoverflow.com/questions/68666187/internetopenurl-failed-a-connection-with-the-server-could-not-be-established

rm(list = ls())

plot.new()
dev.off()

library(lubridate)
library(maps)
library(plotTimeSeries)
library(rerddap)

load("indicator_processing/spec_file.RData")

# define years  --------------------------------
styear <- 1982
enyear <- terminal_year

# get ERDDAP info  --------------------------------
sst <- info('ncdcOisst21Agg_LonPM180') # this may work better

# empty data  -------------------------------------------------
dat <- c()

# download by year to avoid timeout errors --------------------
for (yr in styear:enyear) { 
  
  ### BDT rERDDAP fix
  sst_grab <- griddap(sst, fields = 'sst', 
                      time = c(paste0(yr,'-01-01'), paste0(yr,'-12-31')), 
                      longitude = c(min_lon, max_lon), 
                      latitude = c(min_lat, max_lat))
  
  sst_agg <- aggregate(sst_grab$data$sst, 
                       by = list(sst_grab$data$time), 
                       function(x) c(mean(x, na.rm = T), sd(x, na.rm = T)))
  
  if (yr == styear) { dat <- sst_agg  }  else {
    dat <- rbind(dat, sst_agg)  }
}

dat <- data.frame(cbind(dat$Group.1, dat$x))
names(dat) <- c("time", "sst", "sd")

dat$sst <- as.numeric(dat$sst)
dat$sd <- as.numeric(dat$sd)

head(dat)

# add yearmonth column --------------------------
dat$year <- year(dat$time)
dat$mon <- month(dat$time)
dat$yrmon <- paste0(dat$year, sprintf("%02.f", dat$mon))
dat
head(dat)
tail(dat)
barplot(table(dat$year))
barplot(table(dat$mon))
table(dat$year)
table(dat$mon)

# calculate min, mean, max and look at correlations ---------------------

me <- tapply(dat$sst, dat$yrmon, mean, na.rm = T)
mi <- tapply(dat$sst, dat$yrmon, min, na.rm = T)
ma <- tapply(dat$sst, dat$yrmon, max, na.rm = T)

me_sd <- tapply(dat$sd, dat$yrmon, mean, na.rm = T)

inddata <- data.frame(cbind(me, mi, ma))
names(inddata) <- c("mean", "min", "max")
datdata <- names(me)
ulidata <- data.frame(me + me_sd)
llidata <- data.frame(me - me_sd)

cor(inddata)
plot(inddata)
matplot(inddata, type = "l")


# format into indicator object ------------------

labs <- c(rep("U.S. Caribbean sea surface temperature", 3), rep("degrees Celsius", 3), 
          "monthly mean", "monthly minimum", "monthly maximum")

indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) # , ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
s

ind <- s

# save and plot ---------------------------------------

save(ind, file = "indicator_objects/Carib_SST.RData")

plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, dateformat = "%Y%m", sublabel = T, 
                        trendAnalysis = T, widadj = 0.5, anom = "stmon", type = "allLines") #  outtype = "png", hgtadj = 0.8)


print("SST -- SUCCESSFULLY RUN")



