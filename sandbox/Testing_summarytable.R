# Code to pull all the time series indicator trend analysis results and create a table.

# Install IEAnalyzeR package
devtools::install_github("https://github.com/Gulf-IEA/IEAnalyzeR")

library(IEAnalyzeR)
library(readxl)
library(tidyverse)
library(plotly)
library(gt)

# Need to add the terminal year to vals for the single indicators
# Need to fix cases where the extent is repeated 
# Need to fix plot labels
# Need to add trend analysis to plots



# 3 types of time series data: 
#    1. single indicator, annual time step
#    2. single indicator, monthly time step
#    3. single indicator, standardized monthly anomaly
#    4. multiple indicator, annual time step
#    5. multiple indicator, monthly time step
#    6. multiple indicator, standardized monthly anomaly


# a test version of the data prep function. This now works with multiple indicators where the extent or unit is repeated
df = cruise
i=2
subind = "extent"
trends = T

data_prep_test <-function (df, trends = T, subind = FALSE){
  df_list<-vector("list", 5)
  names(df_list)<-c("data", "pos", "neg", "labs", "vals")
  
  #Data used for everything
  df_dat<-df[4:nrow(df),c(1:ncol(df))]
  
  if (ncol(df_dat)<2.5) {
    colnames(df_dat)<-c("year","value")
    df_dat$value<- as.numeric(df_dat$value)
    
    mean<-mean(as.numeric(df_dat$value), na.rm = T)
    sd<-sd(as.numeric(df_dat$value), na.rm = T)
    
    df_dat$valence[df_dat$value>=mean]<-"pos"
    df_dat$valence[df_dat$value< mean]<-"neg"
    df_dat$min <- ifelse(df_dat$value >= mean, mean, df_dat$value)
    df_dat$max <- ifelse(df_dat$value >= mean, df_dat$value, mean)
    df_dat$year <- as.numeric(df_dat$year)
    df_dat<-df_dat[!is.na(df_dat$value),]
    df_dat} else {
      
      sub_list<-list()
      for (i in 2:ncol(df_dat)){
        sub_df<-df_dat[,c(1,i)]
        df_lab<-df[1:3,] #For example sake cutting to only col I need
        ind<-ifelse(subind=="extent", df_lab[3,i], ifelse(subind=="unit", df_lab[2,i], df_lab[1,i]))
        
        
        colnames(sub_df)<-c("year","value")
        # sub_df$value<- as.numeric(sub_df$value)
        sub_df<-as.data.frame(lapply(sub_df, as.numeric))
        
        mean<-mean(as.numeric(sub_df$value), na.rm = T)
        sd<-sd(as.numeric(sub_df$value), na.rm = T)
        
        sub_df$valence[sub_df$value>=mean]<-"pos"
        sub_df$valence[sub_df$value< mean]<-"neg"
        sub_df$min <- ifelse(sub_df$value >= mean, mean, sub_df$value)
        sub_df$max <- ifelse(sub_df$value >= mean, sub_df$value, mean)
        sub_df$year <- as.numeric(sub_df$year)
        sub_df$subnm<-paste0(ind)
        sub_df$id<-i-1
        #sub_df$subnm<-paste0(ind,"_",i-1) #CG replaced this with above*************
        sub_df<-sub_df[!is.na(sub_df$value),]
        sub_list[[i]]<-sub_df
        
      }
      df_dat<-do.call("rbind",sub_list)
    }
  df_list$data<-df_dat
  
  
  #Pos data set used for main plot
  
  if(ncol(df_dat)<6){
    mean<-mean(as.numeric(df_dat$value), na.rm = T)
    sd<-sd(as.numeric(df_dat$value), na.rm = T)
    pos<-df_dat
    pos$value<-ifelse(pos$valence == "pos",pos$value, mean)
    pos$mean<-mean
    pos$sd<-sd
    pos<-pos[!is.na(pos$value),]
    pos} else {
      sub_list<-list()
      #subs<-unique(df_dat$subnm)
      subs<-unique(df_dat$id) # added this replace above ***************************
      subnm_un<-unique(select(df_dat, subnm, id)) #added this ********************
      for (i in 1:length(subs)){
        sub_df<-df_dat[df_dat$id==subs[i],]
        mean<-mean(as.numeric(sub_df$value), na.rm = T)
        sd<-sd(as.numeric(sub_df$value), na.rm = T)
        pos<-sub_df
        pos$value<-ifelse(pos$valence == "pos",pos$value, mean)
        pos$subnm<-subnm_un[i,1] #changed this *****************************
        pos$mean<-mean
        pos$sd<-sd
        pos<-pos[!is.na(pos$value),]
        sub_list[[i]]<-pos
      }
      pos<-do.call("rbind",sub_list)
    }
  df_list$pos<-pos
  
  
  #Neg data set used for main plot
  if(ncol(df_dat)<6){
    mean<-mean(as.numeric(df_dat$value), na.rm = T)
    sd<-sd(as.numeric(df_dat$value), na.rm = T)
    neg<-df_dat
    neg$value<-ifelse(neg$valence == "neg",neg$value, mean)
    neg$mean<-mean
    neg$sd<-sd
    neg<-neg[!is.na(neg$value),]
    neg} else {
      sub_list<-list()
      #subs<-unique(df_dat$subnm)
      subs<-unique(df_dat$id) # added this replace above ***************************
      subnm_un<-unique(select(df_dat, subnm, id)) #added this ********************
      for (i in 1:length(subs)){
        sub_df<-df_dat[df_dat$id==subs[i],] #changed to id ********************
        mean<-mean(as.numeric(sub_df$value), na.rm = T)
        sd<-sd(as.numeric(sub_df$value), na.rm = T)
        neg<-sub_df
        neg$value<-ifelse(neg$valence == "neg",neg$value, mean)
        neg$subnm<-subnm_un[i,1] #changed this *****************************
        neg$mean<-mean
        neg$sd<-sd
        neg<-neg[!is.na(neg$value),]
        sub_list[[i]]<-neg
      }
      neg<-do.call("rbind",sub_list)
    }
  df_list$neg<-neg
  
  df_list$labs<-df[1:3, c(1:ncol(df))]
  #df_list$labs[3, 2:ncol(df)] <- paste0(df_list$labs[3, 2:ncol(df)], "_", 1:(ncol(df) - 1)) #Carissa added this ****************************************
  
  #Independent values used throughout
  if (trends==T) {
    if(ncol(df_dat)<6){
      mean<-mean(as.numeric(df_dat$value), na.rm = T)
      sd<-sd(as.numeric(df_dat$value), na.rm = T)
      
      #Trend Analysis
      last5<-df_dat[df_dat$year > max(df_dat$year)-5,]
      #Mean Trend
      last5_mean<-mean(last5$value) # mean value last 5 years
      mean_tr<-if_else(last5_mean>mean+sd, "ptPlus", if_else(last5_mean<mean-sd, "ptMinus","ptSolid")) #qualify mean trend
      mean_sym<-if_else(last5_mean>mean+sd, "+", if_else(last5_mean<mean-sd, "-","●")) #qualify mean trend
      mean_word<-if_else(last5_mean>mean+sd, "greater", if_else(last5_mean<mean-sd, "below","within")) #qualify mean trend
      
      #Slope Trend
      lmout<-summary(lm(last5$value~last5$year))
      last5_slope<-coef(lmout)[2,1] * 5 #multiply by years in the trend (slope per year * number of years=rise over 5 years)
      slope_tr<-if_else(last5_slope>sd, "arrowUp", if_else(last5_slope< c(-sd), "arrowDown","arrowRight"))
      slope_sym<-if_else(last5_slope>sd, "↑", if_else(last5_slope< c(-sd), "↓","→"))
      slope_word<-if_else(last5_slope>sd, "an increasing", if_else(last5_slope< c(-sd), "a decreasing","a stable"))
      
      #Dataframe
      vals<-data.frame(mean=mean,
                       sd=sd,
                       mean_tr=mean_tr,
                       slope_tr=slope_tr,
                       mean_sym=mean_sym,
                       slope_sym=slope_sym,
                       mean_word=mean_word,
                       slope_word=slope_word)
      vals} else {
        sub_list<-list()
        subnm_un<-unique(select(df_dat, subnm, id))
        #subs<-unique(df_dat$subnm)
        subs<-unique(df_dat$id) # added this *********************
        print(subs) #CG added as test ************************************
        for (i in 1:length(subs)){
          sub_df<-df_dat[df_dat$id==subs[i],]
          minyear<-min(na.omit(sub_df)$year)
          maxyear<-max(na.omit(sub_df)$year)
          allminyear<-min(df_dat$year)
          allmaxyear<-max(df_dat$year)
          mean<-mean(as.numeric(sub_df$value), na.rm = T)
          sd<-sd(as.numeric(sub_df$value), na.rm = T)
          
          #Trend Analysis
          last5<-sub_df[sub_df$year > max(sub_df$year)-5,]
          #Mean Trend
          last5_mean<-mean(last5$value) # mean value last 5 years
          mean_tr<-if_else(last5_mean>mean+sd, "ptPlus", if_else(last5_mean<mean-sd, "ptMinus","ptSolid")) #qualify mean trend
          mean_sym<-if_else(last5_mean>mean+sd, "+", if_else(last5_mean<mean-sd, "-","●")) #qualify mean trend
          mean_word<-if_else(last5_mean>mean+sd, "greater", if_else(last5_mean<mean-sd, "below","within")) #qualify mean trend
          
          #Slope Trend
          lmout<-summary(lm(last5$value~last5$year))
          last5_slope<-coef(lmout)[2,1] * 5 #multiply by years in the trend (slope per year * number of years=rise over 5 years)
          slope_tr<-if_else(last5_slope>sd, "arrowUp", if_else(last5_slope< c(-sd), "arrowDown","arrowRight"))
          slope_sym<-if_else(last5_slope>sd, "↑", if_else(last5_slope< c(-sd), "↓","→"))
          slope_word<-if_else(last5_slope>sd, "an increasing", if_else(last5_slope< c(-sd), "a decreasing","a stable"))
          
          vals<-data.frame(allminyear=allminyear,
                           allmaxyear=allmaxyear,
                           minyear=minyear,
                           maxyear=maxyear,
                           mean=mean,
                           sd=sd,
                           mean_tr=mean_tr,
                           slope_tr=slope_tr,
                           mean_sym=mean_sym,
                           slope_sym=slope_sym,
                           mean_word=mean_word,
                           slope_word=slope_word,
                           subnm=subnm_un[i,1],
                           id=unique(sub_df$id))
          
          
          sub_list[[i]]<-vals
        }
        vals<-do.call("rbind",sub_list)
        
      }
    df_list$vals<-vals
  }
  df_list
}


###########################################

# First type, single indicators annual time step
EQ = read.csv("indicator_objects/objects_as_csvs/earthquakes.csv", header = F)
Reg = read.csv("indicator_objects/objects_as_csvs/FRsection.csv", header = F)
head(EQ)
head(Reg)

EQ_obj = IEAnalyzeR::data_prep(EQ)
str(EQ_obj)
Reg_obj = IEAnalyzeR::data_prep(Reg)
str(Reg_obj)

IEAnalyzeR::plot_fn_obj(EQ_obj)
IEAnalyzeR::plot_fn_obj(Reg_obj)

# These simple cases work just fine. SUCCESS

##########################################

# Multiple indicator, annual time step, no repeats in extent or unit
AvgL = read.csv("indicator_objects/objects_as_csvs/mean_Lmax.csv", header = F)
dist = read.csv("indicator_objects/objects_as_csvs/disturbance.csv", header = F)
entero = read.csv("indicator_objects/objects_as_csvs/enterococcus.csv", header = F)
gdp = read.csv("indicator_objects/objects_as_csvs/GDP.csv", header = F)
gini = read.csv("indicator_objects/objects_as_csvs/gini_revenue.csv", header = F)
hotel = read.csv("indicator_objects/objects_as_csvs/hotel_occupancy.csv", header = F)
outreach = read.csv("indicator_objects/objects_as_csvs/outreach.csv", header = F)
PD = read.csv("indicator_objects/objects_as_csvs/PD_ratio.csv", header = F)
pollution = read.csv("indicator_objects/objects_as_csvs/pollution.csv", header = F)
pop = read.csv("indicator_objects/objects_as_csvs/population.csv", header = F)
PRLmax = read.csv("indicator_objects/objects_as_csvs/PR_Lmax_classes.csv", header = F)
propdive = read.csv("indicator_objects/objects_as_csvs/prop_diving_trips.csv", header = F)
propbycatch = read.csv("indicator_objects/objects_as_csvs/prop_trips_bycatch.csv", header = F)
STTLmax = read.csv("indicator_objects/objects_as_csvs/STT_Lmax_classes.csv", header = F)
STXLmax = read.csv("indicator_objects/objects_as_csvs/STX_Lmax_classes.csv", header = F)
tier3 = read.csv("indicator_objects/objects_as_csvs/tier3.csv", header = F)


obj = IEAnalyzeR::data_prep(propdive)
str(obj)

# These all seem to work fine too SUCCESS


##########################################

# Multiple indicator, annual time step, repeated extent or unit
Coral = read.csv("indicator_objects/objects_as_csvs/coral_spprichness_cover.csv", header = F)
cruise = read.csv("indicator_objects/objects_as_csvs/cruise_air_visitors.csv", header = F)
density = read.csv("indicator_objects/objects_as_csvs/fish_density.csv", header = F)
oceanNAICS = read.csv("indicator_objects/objects_as_csvs/oceanNAICS.csv", header = F)
RVCPR = read.csv("indicator_objects/objects_as_csvs/RVC_PR.csv", header = F)
RVCSTSJ = read.csv("indicator_objects/objects_as_csvs/RVC_STSJ.csv", header = F)
RVCSTX = read.csv("indicator_objects/objects_as_csvs/RVC_STX.csv", header = F)
totLand = read.csv("indicator_objects/objects_as_csvs/total_landings.csv", header = F)

RVCPR_obj = IEAnalyzeR::data_prep(RVCPR)
str(RVCPR_obj)
IEAnalyzeR::plot_fn_obj(RVCPR_obj, manual_title = "RVC Puerto Rico")


head(AvgL)
head(Coral)
head(cruise)

#Rename row 3
#cruise[3,] = c("NA", "PR cruise", "USVI cruise", "PR air", "USVI air")


AvgL_obj = IEAnalyzeR::data_prep(AvgL)
str(AvgL_obj)

Coral_obj = data_prep_test(Coral)
str(Coral_obj) #this one is messed up

cruise_obj = data_prep_test(cruise)
cruise_obj = IEAnalyzeR::data_prep(cruise)
str(cruise_obj) 
IEAnalyzeR::plot_fn_obj(cruise_obj)
plot_fn_obj_test(cruise_obj, manual_title = "Cruise and air passengers")



########### test plot


plot_fn_obj_test<-function (df_obj, interactive = FALSE, sep_yaxis=F, manual_ylab=NULL, manual_xlab=NULL, manual_title=NULL) {
  
  #Main Plot
  plot <- ggplot(data = df_obj$data, aes(x = year, y = value)) +
    geom_ribbon(data = df_obj$pos, aes(ymax = max, ymin = mean), fill = "#7FFF7F") +
    geom_ribbon(data = df_obj$neg, aes(ymax = mean, ymin = min), fill = "#FF7F7F") +
    geom_rect(data = merge(df_obj$data, df_obj$vals), aes(xmin = min(year), xmax = max(year), ymin = mean - sd, ymax = mean + sd), fill = "white") +
    geom_hline(aes(yintercept = mean), lty = "dashed", data = df_obj$vals) +
    geom_hline(aes(yintercept = mean + sd), data = df_obj$vals) +
    geom_hline(aes(yintercept = mean -sd), data = df_obj$vals) +
    geom_line(aes(group = 1), lwd = 0.75) +
    xlab("Year") + ylab(df_obj$labs[2,2]) + ggtitle (df_obj$labs[1, 2])+
    theme_bw() + theme(strip.background = element_blank(),
                       strip.text = element_text(face = "bold"),
                       title = element_text(size = 14, face = "bold"))
  
  #Altering scales
  if (max(df_obj$data$year) - min(df_obj$data$year) > 20) {
    plot <- plot + scale_x_continuous(breaks = seq(min(df_obj$data$year),
                                                   max(df_obj$data$year), 5))
  } else {
    plot <- plot + scale_x_continuous(breaks = seq(min(df_obj$data$year),
                                                   max(df_obj$data$year), 2))
  }
  
  
  #Add facetting for sub indicators
  if (ncol(df_obj$data) > 5.5) {
    
    if (sep_yaxis==T) {
      plot<-plot+
        facet_wrap(~subnm, ncol = ifelse(length(unique(df_obj$data$subnm)) < 4, 1, 2), scales = "free_y", strip.position = "left")+
        theme(strip.placement = "outside",
              strip.background = element_blank(),
              strip.text = element_text(face="bold"),
              axis.title.y = element_blank())
      
      if (!is.null(manual_ylab)) {
        plot<-plot+
          facet_wrap(~subnm,ncol = ifelse(length(unique(df_obj$data$subnm)) < 4, 1, 2), scales = "free_y" , strip.position = "left",
                     labeller = as_labeller(setNames(manual_ylab, sort(unique(df_obj$data$subnm)))))
      }
    } else {
      plot<-plot+
        facet_wrap(~id, ncol = ifelse(length(unique(df_obj$data$id)) < 4, 1, 2), scales = "free_y",labeller = as_labeller(setNames(df_obj$vals$subnm,df_obj$vals$id))) #Changed this to ID (CG) **************************************************
    }
    
  }
  
  
  #Adjusting Labels
  if (!is.null(manual_ylab)) {
    plot<-plot+
      ylab(manual_ylab)
  }
  
  if (!is.null(manual_xlab)) {
    plot<-plot+
      xlab(manual_xlab)
  }
  
  if (!is.null(manual_title)) {
    plot<-plot+
      ggtitle(manual_title)
  }
  
  #Toggling Interactivity (LAST IN ORDER)
  if (!interactive == F) {
    plot = ggplotly(plot)
  }
  
  
  plot
}






#################################################

# Making a table

EQ_obj$vals
com_sing<-comfish[,1:2]
com_df<-data_prep(com_sing)

ind_list<-list(EQ_obj, Reg_obj)

sum_list<-list()
for (i in 1:length(ind_list)) {
  df<-ind_list[[i]]
  indic_title<-df$labs[1,2]
  trend_sym<-df$vals$mean_sym
  slope_sym<-df$vals$slope_sym
  mean<-round(df$vals$mean, 2)
  sd<-round(df$vals$sd, 2)
  all_dat<-c(indic_title, trend_sym, slope_sym, mean, sd)
  all_dat<-unlist(all_dat)
  sum_list[[i]]<-all_dat
}

sum_table<-as.data.frame(do.call("rbind",sum_list))
colnames(sum_table)<-c("indicator", "trend_sym", "slope_sym", "mean", "sd")
gt(sum_table)



