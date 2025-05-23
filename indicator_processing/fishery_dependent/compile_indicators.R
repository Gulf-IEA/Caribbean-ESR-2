# M. Karnauskas 10/19/2023
# compile pelagic:demersal ratio and Lmax indicators
# uses logbook data for PR and USVI 
# need to run first: 
#   INDICATOR_fishery_indicators_PR.R
#   INDICATOR_fishery_indicators_STT.R
#   INDICATOR_fishery_indicators_STX.R
# then compile outputs here


# specification file and libraries -----------------------------
rm(list = ls())


plot.new()
dev.off()

library(maps)
library(plotTimeSeries)

dir("indicator_data/intermediateFiles/fish-dep-indicators/")

load("indicator_data/intermediateFiles/fish-dep-indicators/Lmax_PR.RData")
pr <- findat

load("indicator_data/intermediateFiles/fish-dep-indicators/Lmax_STT.RData")
st <- findat

load("indicator_data/intermediateFiles/fish-dep-indicators/Lmax_STX.RData")
sx <- findat

yrs <- (min(c(pr$V1, st$yrs, sx$yrs))) : (max(c(pr$V1, st$yrs, sx$yrs)))

mat <- data.frame(matrix(data = NA, nrow = length(yrs), ncol = 3))
rownames(mat) <- yrs
mat
mat2 <- mat

mat[match(pr$V1, yrs), 1] <- pr$lmax
mat[match(st$yrs, yrs), 2] <- st$lmax
mat[match(sx$yrs, yrs), 3] <- sx$lmax

mat2[match(pr$V1, yrs), 1] <- pr$lmax_sem
mat2[match(st$yrs, yrs), 2] <- st$lmax_sem
mat2[match(sx$yrs, yrs), 3] <- sx$lmax_sem

datdata <- yrs
inddata <- data.frame(mat)
ulidata <- data.frame(mat + mat2)
llidata <- data.frame(mat - mat2)
labs <- c("Average maximum length of species in demersal landings", "Length (cm)", "Puerto Rico", 
          "Average maximum length of species in demersal landings", "Length (cm)", "St. Thomas and St. John",
          "Average maximum length of species in demersal landings", "Length (cm)", "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
ind <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)

class(ind) <- "indicatordata"
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, sublabel = T, sameYscale = F)

save(ind, file = "indicator_objects/mean_Lmax.RData")

Lmax <- mat


# P:D ratios -----------------------------

rm(list = ls()[-match(c("Lmax"), ls())])

dir("indicator_data/intermediateFiles/fish-dep-indicators/")

load("indicator_data/intermediateFiles/fish-dep-indicators/PDRatioPR.RData")
pr <- pdrat

load("indicator_data/intermediateFiles/fish-dep-indicators/PDRatioSTT.RData")
st <- pdrat

load("indicator_data/intermediateFiles/fish-dep-indicators/PDRatioSTX.RData")
sx <- pdrat

yrspr <- as.numeric(names(pr))
yrsst <- as.numeric(names(st))
yrssx <- as.numeric(names(sx))

yrs <- (min(c(yrspr, yrsst, yrssx))) : (max(c(yrspr, yrsst, yrssx)))

mat <- data.frame(matrix(data = NA, nrow = length(yrs), ncol = 3))
rownames(mat) <- yrs
mat

mat[match(yrspr, yrs), 1] <- pr
mat[match(yrsst, yrs), 2] <- st
mat[match(yrssx, yrs), 3] <- sx

datdata <- yrs
inddata <- data.frame(mat)
labs <- c("Ratio of pelagic to demersal landings", "Landings ratio", "Puerto Rico", 
          "Ratio of pelagic to demersal landings", "Landings ratio", "St. Thomas and St. John",
          "Ratio of pelagic to demersal landings", "Landings ratio", "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
ind <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)

class(ind) <- "indicatordata"
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, sublabel = T, sameYscale = F)

save(ind, file = "indicator_objects/PD_ratio.RData")


# compare indicators --------------------------------

plot(Lmax[, 1], mat[, 1])
cor.test(Lmax[, 1], mat[, 1], na.action=na.omit)

plot(Lmax[, 2], mat[, 2])
cor.test(Lmax[, 2], mat[, 2], na.action=na.omit)

plot(Lmax[, 3], mat[, 3])
cor.test(Lmax[, 3], mat[, 3], na.action=na.omit)

# indicators are no longer highly correlated 
# replaced Lmax with Lmax of demersal species only

print("PD and LMax -- SUCCESSFULLY RUN")
