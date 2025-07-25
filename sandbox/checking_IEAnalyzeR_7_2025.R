

data <- read.csv(csv_files[2])

data_prep_test(data, trends = T, anomaly = "stmon")

df = data
anomaly = "stdmonthly"
i=2

data_prep_test<-function (df, trends = T, subind = FALSE, anomaly=NULL)
{
  
  # system.file("images", paste0(,".png") , package = "IEAnalyzeR")
  
  ### helper function to interpolate across mean for pos/neg ribbon plotting
  poly_fix <- function(x, y, mean){
    d <- data.frame(x = x, y = y)
    rx <- do.call("rbind",
                  lapply(1:(nrow(d)-1), function(i){
                    f <- lm(x ~ y, d[i:(i+1), ])
                    if (f$qr$rank < 2) return(NULL)
                    r <- predict(f, newdata = data.frame(y=mean))
                    if(d[i, ]$x < r & r < d[i+1, ]$x)
                      return(data.frame(x = r, y = mean))
                    else return(NULL)
                  }))
    d2 <- cbind(rx, mean, mean)
    names(d2) <- c("year", "value",'min', 'max')
    return(d2)
  }
  ### end helper function
  
  
  #Name for df
  df_nm<-paste0(deparse(substitute(df)))
  
  if(class(df)=="character") {
    #Name for df
    df_nm<-sub("^.*/(.*)\\.[^.]*$", "\\1", df)
    #Read CSV
    df<-read.csv(df, check.names = F, )
  }
  
  if (!is.null(anomaly)) {
    df_nm<-paste(df_nm,anomaly,"anom",sep = "_")
  }
  
  
  df_list <- list()
  df_dat <- df[3:nrow(df), c(1:ncol(df))]
  options(scipen = 999)
  
  ### Helper function for converting dates
  clean_dates_to_partial_year <- function(date_strings) {
    date_formats = c("m-Y", "bY", "Ym", "Yb", "b-y", "y-b")
    parsed_dates = parse_date_time(date_strings, orders = date_formats, quiet = TRUE)
    partial_years = year(parsed_dates) + (month(parsed_dates) - 1) / 12
    return(partial_years)
  }
  ### end helper function
  
  
  if (!all(grepl("^[0-9]{4}$", df_dat[, 1]) | is.na(df_dat[, 1]))) {
    df_dat[, 1] <- clean_dates_to_partial_year(df_dat[, 1])
  }
  
  
  #Change df_dat to be anomaly if chose
  if (!is.null(anomaly)) {
    get_month <- function(pyear) {
      round((pyear - floor(pyear)) * 12 + 1)
    }
    
    #Check if in partial year (monthly data)
    if(any(na.omit(as.numeric(df_dat[,1])) %% 1 != 0)) {
      
      sub_list <- list()
      for (i in 2:ncol(df_dat)) {
        sub_df <- df_dat[, c(1, i)]
        colnames(sub_df) <- c("year", "value")
        sub_df$value<-as.numeric(sub_df$value)
        sub_df$month <- get_month(sub_df$year)
        mon_means_sd<-sub_df %>% group_by(month) %>%
          summarise(mon_mean=mean(value, na.rm=T), mon_sd=sd(value, na.rm=T))
        sub_df<-left_join(sub_df, mon_means_sd, by="month")
        sub_df<-sub_df %>%
          mutate(anom_value=value-mon_mean, sd_anom_value= anom_value/mon_sd)
        sub_df$col_nm <- colnames(df)[i]
        sub_df$id <- i - 1
        
        if (anomaly=="monthly") {
          sub_df<-dplyr::select(sub_df, year, value=anom_value, col_nm, id)
          sub_list[[i-1]] <- sub_df
        }
        if (anomaly=="stdmonthly") {
          sub_df<-dplyr::select(sub_df, year, value=sd_anom_value, col_nm, id)
          sub_list[[i-1]] <- sub_df
        }
        
        
      }
      df_dat <- do.call("rbind", sub_list)
      df_dat<-df_dat %>%  pivot_wider(id_cols = c("year"), names_from = c("col_nm", "id"), values_from = "value")
      colnames(df_dat)<-sub("_(?!.*_).*", "", colnames(df_dat), perl = TRUE)
    }
  }
  
  if (ncol(df_dat)<3) {
    colnames(df_dat)<-c("year","value")
    df_dat$value<- as.numeric(df_dat$value)
    df_dat$year <- as.numeric(df_dat$year)
    df_dat<-df_dat[!is.na(df_dat$value),]
  } else {
    sub_list<-list()
    for (i in 2:ncol(df_dat)){
      sub_df<-df_dat[,c(1,i)]
      df_lab<-rbind(colnames(df),df[1:2,])
      ind<-ifelse(subind=="extent", df_lab[3,i], ifelse(subind=="unit", df_lab[2,i], df_lab[1,i]))
      colnames(sub_df)<-c("year","value")
      sub_df<-as.data.frame(lapply(sub_df, as.numeric))
      sub_df$year <- as.numeric(sub_df$year)
      sub_df$subnm<-paste0(ind)
      sub_df$id<- i-1
      sub_df<-sub_df[!is.na(sub_df$value),]
      sub_list[[i]]<-sub_df
    }
    df_dat<-do.call("rbind",sub_list)
  }
  df_dat <- df_dat %>% arrange(year)
  df_list$data <- df_dat
  
  
  ### create dataframes for ribbon plotting
  # inputs: dataframe with year and value
  # outputs: positive ribbon dataframe with year, min=mean, max=value
  #         negative ribbon dataframe with year, min=value, max=mean
  
  if(ncol(df_dat)<3){
    ribbon <- df_dat
    mean <- mean(as.numeric(ribbon$value), na.rm = T)
    ribbon$min <- ifelse(ribbon$value >= mean, mean, ribbon$value)
    ribbon$max <- ifelse(ribbon$value >= mean, ribbon$value, mean)
    poly_o <- poly_fix(ribbon$year, ribbon$value, mean)
    ribbon <- rbind(ribbon, poly_o)
    ribbon$mean <- mean
    ribbon <- ribbon[!is.na(ribbon$value),]
  } else {
    sub_list <- list()
    subs <- unique(df_dat$id)
    subnm_un <- unique(select(df_dat, subnm, id))
    for (i in 1:length(subs)){
      ribbon <- df_dat[df_dat$id==subs[i], ]
      mean <- mean(as.numeric(ribbon$value), na.rm = T)
      ribbon$min <- ifelse(ribbon$value >= mean, mean, ribbon$value)
      ribbon$max <- ifelse(ribbon$value >= mean, ribbon$value, mean)
      poly_o <- poly_fix(ribbon$year, ribbon$value, mean)
      poly_o$subnm <- ribbon$subnm[1]
      poly_o$id <- ribbon$id[1]
      ribbon <- rbind(ribbon, poly_o)
      ribbon$mean <- mean
      ribbon <- ribbon[!is.na(ribbon$value),]
      sub_list[[i]] <- ribbon
    }
    ribbon<-do.call("rbind",sub_list)
  }
  df_list$ribbon <- ribbon
  
  df_list$labs <- rbind(colnames(df),df[1:2,])
  if (trends == T) {
    if (ncol(df_dat) < 3) {
      mean <- mean(as.numeric(df_dat$value), na.rm = T)
      sd <- sd(as.numeric(df_dat$value), na.rm = T)
      minyear <- min(df_dat$year)
      maxyear <-  max(df_dat$year)
      allminyear <- min(df_dat$year)
      allmaxyear <- max(df_dat$year)
      last5 <- df_dat[df_dat$year > max(df_dat$year) -
                        5, ]
      last5_mean <- mean(last5$value)
      mean_tr <- if_else(last5_mean > mean + sd, "circle_plus",
                         if_else(last5_mean < mean - sd, "circle_minus", "circle_fill"))
      mean_sym <- if_else(last5_mean > mean + sd, "+",
                          if_else(last5_mean < mean - sd, "-", "●"))
      mean_word <- if_else(last5_mean > mean + sd, "greater",
                           if_else(last5_mean < mean - sd, "below", "within"))
      lmout <- summary(lm(last5$value ~ last5$year))
      last5_slope <- coef(lmout)[2, 1] * 5
      slope_tr <- if_else(last5_slope > sd, "arrow_up",
                          if_else(last5_slope < c(-sd), "arrow_down", "arrow_leftright"))
      slope_sym <- if_else(last5_slope > sd, "↑", if_else(last5_slope <
                                                            c(-sd), "↓", "→"))
      slope_word <- if_else(last5_slope > sd, "an increasing",
                            if_else(last5_slope < c(-sd), "a decreasing",
                                    "a stable"))
      # mean_img<- paste0(img_dir, mean_tr, ".png")
      mean_img<- system.file("images/trend_symb", paste0(mean_tr,".png") , package = "IEAnalyzeR")
      # slope_img<-paste0(img_dir, slope_tr, ".png")
      slope_img<- system.file("images/trend_symb", paste0(slope_tr,".png") , package = "IEAnalyzeR")
      mid_y<-(diff(range(na.omit(df_dat)$value))/2)+min(na.omit(df_dat)$value)
      min_y<-min(na.omit(df_dat)$value)
      max_y<-max(na.omit(df_dat)$value)
      vals <- data.frame(mean = mean, sd = sd, minyear=minyear,maxyear=maxyear,allminyear=allminyear,
                         allmaxyear=allmaxyear,mean_tr = mean_tr, mean_img=mean_img, slope_tr = slope_tr,
                         slope_img=slope_img, mean_sym = mean_sym, slope_sym = slope_sym,
                         mean_word = mean_word, slope_word = slope_word, mid_y=mid_y, min_y=min_y, max_y=max_y, df_nm=df_nm)
      if (nrow(last5)<5) {
        trend_vars<-c("mean_tr", "mean_img", "slope_tr", "slope_img", "mean_sym", "slope_sym", "mean_word", "slope_word")
        
        vals[trend_vars]<-NA
      }
      vals
    } else {
      sub_list <- list()
      subnm_un <- unique(select(df_dat, subnm, id))
      subs <- unique(df_dat$id)
      for (i in 1:length(subs)) {
        sub_df <- df_dat[df_dat$id == subs[i], ]
        minyear <- min(na.omit(sub_df)$year)
        maxyear <- max(na.omit(sub_df)$year)
        allminyear <- min(df_dat$year)
        allmaxyear <- max(df_dat$year)
        mean <- mean(as.numeric(sub_df$value), na.rm = T)
        sd <- sd(as.numeric(sub_df$value), na.rm = T)
        last5 <- sub_df[sub_df$year > max(sub_df$year) -
                          5, ]
        last5_mean <- mean(last5$value)
        mean_tr <- if_else(last5_mean > mean + sd, "circle_plus",
                           if_else(last5_mean < mean - sd, "circle_minus",
                                   "circle_fill"))
        mean_sym <- if_else(last5_mean > mean + sd, "+",
                            if_else(last5_mean < mean - sd, "-", "●"))
        mean_word <- if_else(last5_mean > mean + sd,
                             "greater", if_else(last5_mean < mean - sd,
                                                "below", "within"))
        lmout <- summary(lm(last5$value ~ last5$year))
        last5_slope <- coef(lmout)[2, 1] * 5
        slope_tr <- if_else(last5_slope > sd, "arrow_up",
                            if_else(last5_slope < c(-sd), "arrow_down",
                                    "arrow_leftright"))
        slope_sym <- if_else(last5_slope > sd, "↑",
                             if_else(last5_slope < c(-sd), "↓", "→"))
        slope_word <- if_else(last5_slope > sd, "an increasing",
                              if_else(last5_slope < c(-sd), "a decreasing",
                                      "a stable"))
        # mean_img<- paste0(img_dir, mean_tr, ".png")
        mean_img<- system.file("images/trend_symb", paste0(mean_tr,".png") , package = "IEAnalyzeR")
        # slope_img<-paste0(img_dir, slope_tr, ".png")
        slope_img<- system.file("images/trend_symb", paste0(slope_tr,".png") , package = "IEAnalyzeR")
        mid_y<-(diff(range(na.omit(sub_df)$value))/2)+min(na.omit(sub_df)$value)
        min_y<-min(na.omit(sub_df)$value)
        max_y<-max(na.omit(sub_df)$value)
        vals <- data.frame(allminyear = allminyear, allmaxyear = allmaxyear,
                           minyear = minyear, maxyear = maxyear, mean = mean,
                           sd = sd, mean_tr = mean_tr, mean_img=mean_img, slope_tr = slope_tr,
                           slope_img=slope_img, mean_sym = mean_sym, slope_sym = slope_sym,
                           mean_word = mean_word, slope_word = slope_word,
                           subnm = subnm_un[i, 1], id = unique(sub_df$id), mid_y=mid_y, min_y=min_y, max_y=max_y, df_nm=df_nm)
        if (nrow(last5)<5) {
          trend_vars<-c("mean_tr", "mean_img", "slope_tr", "slope_img", "mean_sym", "slope_sym", "mean_word", "slope_word")
          
          vals[trend_vars]<-NA
        }
        sub_list[[i]] <- vals
      }
      vals <- do.call("rbind", sub_list)
      
    }
    df_list$vals <- vals
  } else {
    if (ncol(df_dat) < 3) {
      mean <- mean(as.numeric(df_dat$value), na.rm = T)
      sd <- sd(as.numeric(df_dat$value), na.rm = T)
      minyear <- min(df_dat$year)
      maxyear <-  max(df_dat$year)
      allminyear <- min(df_dat$year)
      allmaxyear <- max(df_dat$year)
      vals <- data.frame(mean = mean, sd = sd, minyear=minyear,maxyear=maxyear,allminyear=allminyear,
                         allmaxyear=allmaxyear, df_nm=df_nm)
      vals
    } else {
      sub_list <- list()
      subnm_un <- unique(select(df_dat, subnm, id))
      subs <- unique(df_dat$id)
      for (i in 1:length(subs)) {
        sub_df <- df_dat[df_dat$id == subs[i], ]
        minyear <- min(na.omit(sub_df)$year)
        maxyear <- max(na.omit(sub_df)$year)
        allminyear <- min(df_dat$year)
        allmaxyear <- max(df_dat$year)
        mean <- mean(as.numeric(sub_df$value), na.rm = T)
        sd <- sd(as.numeric(sub_df$value), na.rm = T)
        vals <- data.frame(allminyear = allminyear, allmaxyear = allmaxyear,
                           minyear = minyear, maxyear = maxyear, mean = mean,
                           sd = sd, subnm = subnm_un[i, 1], id = unique(sub_df$id),df_nm=df_nm)
        sub_list[[i]] <- vals
      }
      vals <- do.call("rbind", sub_list)
      
    }
    df_list$vals <- vals
  }
  
  
  df_list
}
