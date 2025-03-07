
# code for producing multivariate plots -----------
rm(list = ls())

# load libraries ----------------------------
library(plotTimeSeries)

# find root project directory 
directory <- rprojroot::find_rstudio_root_file()
setwd(directory)

# load indicator matrix --------------------
load("indicator_data/all_indicators_matrix.rda")
matrix_data
dim(matrix_data)

class(matrix_data$year)
matrix_data$year <- as.numeric(matrix_data$year)

par(mar = c(2, 7, 1, 1))
trafficLightPlot(matrix_data, noNAs = 0.99)
# 2000+ has some data, 2010+ is data-rich period

par(mfrow = c(7, 5))
for (i in 1:ncol(matrix_data))  { hist(matrix_data[, i], main = names(matrix_data)[i]) }

dev.off()

d <- matrix_data[which(matrix_data$year >= 2000 & matrix_data$year <= 2023), ]

par(mar = c(2, 7, 1, 1))
trafficLightPlot(d, noNAs = 1)

head(d)
tail(d)

names(d)

risks <- c("SST_MEAN", "SST_MIN", "SST_MAX", "DHW_PR", "DHW_VI", "OA", "HURR", "QUAKE", 
           "POLL_PR", "POLL_VI", "TURB_PR", "TURB_STT", "TURB_STX", "WATQUL_PR", "WATQUL_VI", "P_PROD", "SARG",
           "DSTRB_PR", "DSTRB_STT", "DSTRB_STX",
           "POP_PR", "POP_VI", "CRU_PR", "CRU_VI", "AIR_PR", "AIR_VI", "HOTEL_PR", "HOTEL_VI")
risks == names(d)[2:29]
  
food_fi <- c("VETU_PR",  "GUTT_PR",  "ANAL_PR",  "CHRY_PR",  "AURO_PR",  "VIRI_PR", 
             "VETU_STT", "GUTT_STT", "ANAL_STT", "CHRY_STT", "AURO_STT", "VIRI_STT", 
             "VETU_STX", "GUTT_STX", "ANAL_STX", "CHRY_STX", "AURO_STX", "VIRI_STX", 
             "DEN_PR", "DEN_VI", "SLPSIZ_VI")
food_fd <- c("PDR_PR",  "PDR_STT",  "PDR_STX", 
             "LMAX_PR", "LMAX_STT", "LMAX_STX",
             "LOB_PR",  "LOB_STT",  "LOB_STX", 
             "CONC_PR", "CONC_STT", "CONC_STX", 
             "OTHR_PR", "OTHR_STT", "OTHR_STX")
soc <- c("DIV_PR", "DIV_STT", "DIV_STX", 
         "GDP_PR", "GDP_VI", "UNEM_PR", "UNEM_VI",
         "O_GDP_PR", "O_GDP_VI", "ECON_PR", "ECON_VI", 
         "EMPL_PR", "EMPL_VI", "WAGE_PR", "WAGE_VI", 
         "GINI_PR", "GINI_STT", "GINI_STX")
oth <- c("REC_PR", "REC_VI", "BYCAT_PR", "BYCAT_STT", "BYCAT_STX", 
         "MGMT", "ASSES_PR", "ASSES_VI", "EDUC_PR", "EDUC_VI", "ENFORCE", 
         "CRLRCH_PR", "CRLCVR_PR", "CRLRCH_VI", "CRLCVR_VI")
oth == names(d)[97:111]

d_risk <- d[, c(1, which(names(d) %in% risks))]
d_foodfi <- d[, c(1, which(names(d) %in% food_fi))]
d_foodfd <- d[, c(1, which(names(d) %in% food_fd))]
d_soc <- d[, c(1, which(names(d) %in% soc))]
d_oth <- d[, c(1, which(names(d) %in% oth))]

dim(d_risk)
dim(d_foodfi)
dim(d_foodfd)
dim(d_soc)
dim(d_oth)

par(mar = c(0.5, 10, 0, 1), mfrow = c(5, 1))

trafficLightPlot(d_risk, noNAs = 1)
trafficLightPlot(d_foodfi, noNAs = 1)
trafficLightPlot(d_foodfd, noNAs = 1)
trafficLightPlot(d_soc, noNAs = 1)
trafficLightPlot(d_oth, noNAs = 1)

# alternative categorization

risks <- c("SST_MEAN", "SST_MIN", "SST_MAX", "DHW_PR", "DHW_VI", "OA", "HURR", "QUAKE", 
           "POLL_PR", "POLL_VI", "TURB_PR", "TURB_STT", "TURB_STX", "WATQUL_PR", "WATQUL_VI", "P_PROD", "SARG", 
           "DSTRB_PR", "DSTRB_STT", "DSTRB_STX",
           "POP_PR", "POP_VI", "CRU_PR", "CRU_VI", "AIR_PR", "AIR_VI", "HOTEL_PR", "HOTEL_VI")

fi <- c("VETU_PR",  "GUTT_PR",  "ANAL_PR",  "CHRY_PR",  "AURO_PR",  "VIRI_PR", 
        "VETU_STT", "GUTT_STT", "ANAL_STT", "CHRY_STT", "AURO_STT", "VIRI_STT", 
        "VETU_STX", "GUTT_STX", "ANAL_STX", "CHRY_STX", "AURO_STX", "VIRI_STX", 
        "DEN_PR", "DEN_VI", "SLPSIZ_VI")

ccl <- c("PDR_PR",   "PDR_STT",   "PDR_STX", 
         "LMAX_PR", "LMAX_STT", "LMAX_STX", 
         "LOB_PR",   "LOB_STT",   "LOB_STX", 
         "CONC_PR",  "CONC_STT",  "CONC_STX", 
         "OTHR_PR",  "OTHR_STT",  "OTHR_STX", 
         "GINI_PR", "GINI_STT", "GINI_STX", 
         "DIV_PR",   "DIV_STT",   "DIV_STX", 
         "BYCAT_PR", "BYCAT_STT", "BYCAT_STX")

oth <- c("ECON_PR", "ECON_VI", "EMPL_PR", "EMPL_VI", "WAGE_PR", "WAGE_VI", 
         "GDP_PR", "GDP_VI", "UNEM_PR", "UNEM_VI", "O_GDP_PR","O_GDP_VI",
         "REC_PR", "REC_VI", "MGMT", 
         "ASSES_PR", "ASSES_VI", "EDUC_PR", "EDUC_VI", "ENFORCE",
         "CRLRCH_PR", "CRLCVR_PR", "CRLRCH_VI", "CRLCVR_VI")

length(risks) + length(fi) + length(ccl) + length(oth)
ncol(d) - 1 - 12

d_risk <- d[, c(1, which(names(d) %in% risks))]
d_fi <- d[, c(1, which(names(d) %in% fi))]
d_ccl <- d[, c(1, which(names(d) %in% ccl))]
d_oth <- d[, c(1, which(names(d) %in% oth))]

# traffic light plots ----------------------------------

png(filename = "indicator_plots/traffic.png", units="in", width=8, height=7, pointsize=12, res=72*12)

nf <- layout(matrix(c(1:5, 5), 3, 2, byrow = TRUE), widths = c(5, 5), heights = c(4.5, 4.5, 0.8), respect = TRUE)
#layout.show(nf)

par(mar = c(2, 8, 0.5, 0.5))

trafficLightPlot(d_risk, noNAs = 1, cexlabs = 0.8, splits = 5)
mtext(side = 2, "risks to meeting objectives", line = 5.5, cex = 0.9)
trafficLightPlot(d_fi, noNAs = 1, cexlabs = 0.8)
mtext(side = 2, "fishery-independent indicators", line = 5.5, cex = 0.9)
trafficLightPlot(d_oth, noNAs = 1, cexlabs = 0.8)
mtext(side = 2, "other management indicators", line = 5.5, cex = 0.9)
trafficLightPlot(d_ccl, noNAs = 1, cexlabs = 0.8)
mtext(side = 2, "fishery-dependent indicators", line = 5.5, cex = 0.9)

plot.new()
par(mar = c(0, 0, 0, 0)+0.2)
legend("bottom", c("0-20%    ", "20-40%    ", "40-60%    ", "60-80%    ", "80-100%    "), 
       col = c("#FF000039", "#FF900049", "#CCFF0069", "#0090FF89", "#0000FF99"), 
       ncol = 5, pch = 15, pt.cex = 3, x.intersp = 1, cex = 1.3, bty = "n")

dev.off()

# ordination plots ----------------------------

thresh <- 0.1
minyr <- 2011

png(filename = "indicator_plots/pcas.png", units="in", width=8, height=10, pointsize=12, res=72*12)

par(mar = c(4, 6, 1, 1), mfrow = c(3, 2))

pc <- plotOrdScores(d_risk, main = "", mintime = 2011, noNAs = thresh, tim.cex = 1.2) #, method = "nmds", nmdsk = 3)
mtext(side = 2, "risks to meeting objectives", line = 4.5, cex = 0.8)
barplot(sort(pc$rotation[,1]), las = 2, horiz = T, xlim = c(-0.45, 0.45), xlab = "PCA 1 loadings")
abline(v = c(-0.2, 0.2), col = 8, lty = 2)

#pc <- plotOrdScores(d_fi, main = "fishery-independent indicators", mintime = 2011, noNAs = thresh) #, method = "nmds", nmdsk = 3)
#barplot(pc$rotation[,1], las = 2, horiz = T)
#abline(v = c(-0.2, 0.2), col = 8, lty = 2)

pc <- plotOrdScores(d_ccl, main = "", mintime = 2011, noNAs = thresh, tim.cex = 1.2) #, method = "nmds", nmdsk = 3)
mtext(side = 2, "fishery-dependent indicators", line = 4.5, cex = 0.8)
barplot(sort(pc$rotation[,1]), las = 2, horiz = T, xlim = c(-0.45, 0.45), xlab = "PCA 1 loadings")
abline(v = c(-0.2, 0.2), col = 8, lty = 2)

pc <- plotOrdScores(d_oth, main = "", mintime = 2011, noNAs = thresh, tim.cex = 1.2) #, method = "nmds", nmdsk = 3)
mtext(side = 2, "other management indicators", line = 4.5, cex = 0.8)
barplot(sort(pc$rotation[,1]), las = 2, horiz = T, xlim = c(-0.45, 0.45), xlab = "PCA 1 loadings")
abline(v = c(-0.2, 0.2), col = 8, lty = 2)

dev.off()

#### end 
