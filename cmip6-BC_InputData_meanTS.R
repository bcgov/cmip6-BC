
# Objective: summarize the gridded time series into average time series for BC and each of the 9 ecoprovinces. 
#

library(scales)
library(raster)
library(rworldmap)
library(rworldxtra)
library(maps)
library(mapdata)
library(maptools)
library(sp)
library(colorRamps)
library(rgeos)
library(rgdal)


monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
seasons <- c("wt", "sp", "sm", "at")
seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)],4, byrow=T)

dem.pts <- read.csv("./outputs/dem_cmip6eval.csv")
ecoprovs <- c("BC", sort(as.character(unique(dem.pts$id2))))
ecoprov.names <- c("British Columbia", "Boreal Plains", "Central Interior", "Coast and Mountains", "Georgia Depression", "Northern Boreal Mountains", "Sub-Boreal Interior", "Southern Interior Mountains", "Southern Interior", "Taiga Plains")
# elements <- c("Tave", "Tmax", "Tmin", "PPT", "NFFD")
elements <- c("Tave", "Tmax", "Tmin", "PPT")


# Helpers
compute_seasonal <- function(cvar_name, cvar_monthly_df, func, sep="") {
  element_winter <- cvar_monthly_df[paste(cvar_name, c("12","01","02"), sep=sep)]
  element_winter[2:length(element_winter)] <- element_winter[1:(length(element_winter)-1)] #advance december by one year (doesn't account for first year in series, but not a big deal)
  element_winter <- apply(element_winter, 1, func)

  element_spring <- apply(cvar_monthly_df[paste(cvar_name, c("03","04","05"), sep=sep)],  1, func)
  element_summer <- apply(cvar_monthly_df[paste(cvar_name, c("06","07","08"), sep=sep)], 1, func)
  element_autum <- apply(cvar_monthly_df[paste(cvar_name, c("09","10","11"), sep=sep)], 1, func)

  # Include annual cvar
  element_annual <- apply(t(cvar_monthly_df), 2, func)

  cvar_monthly_df[cvar_name] <- element_annual

  cvar_seasonal_df <- data.frame(element_annual, element_winter, element_spring, element_summer, element_autum)
  names(cvar_seasonal_df) <- c(cvar_name, paste(cvar_name, seasons, sep="_"))

  return(cvar_seasonal_df)
}

# Climate Variable Functions (source these)
 
# DD_0, degree days below 0

dd_0 <- function(tm) {
  
  optimized_params <- read.csv(file = "./optimizedParameterTables/param_DD_S1.csv", sep=',', header = TRUE)

  #optimized_params <- dd_param[dd_param$Month == m,]
  #dd_param <- dd_param_above_5[dd_param_above_5$Region == "All"]

    
  k <- optimized_params$k
  a <- optimized_params$a
  b <- optimized_params$b
  t0 <- optimized_params$T0
  c <- optimized_params$c
  beta <- optimized_params$beta

  dd <- apply(tm, 2, function(temp) ifelse(temp > k , a/(1 + exp(-(temp - t0)/b)) , c + (beta * temp)))
  dd[is.na(dd)] <- 0
  rownames(dd) <- paste("DD_0", monthcodes, sep="_")
  
  
  return(dd)

}

# DD5, degree days above 5

dd5 <- function(tm) {
  
  optimized_params <- read.csv(file = "./optimizedParameterTables/param_DD_S2.csv", sep=',', header = TRUE)
  optimized_params <- optimized_params[optimized_params$Region == "All",]

  k <- optimized_params$k
  a <- optimized_params$a
  b <- optimized_params$b
  t0 <- optimized_params$T0
  c <- optimized_params$c
  beta <- optimized_params$beta

  dd <- apply(tm, 2, function(temp) ifelse(temp > k , a/(1 + exp(-(temp - t0)/b)) , c + (beta * temp)))
  rownames(dd) <- paste("DD5", monthcodes, sep="_")
  
  
  return(dd)

}


# DD_18, degree days below 18

dd_18 <- function(tm) {
  
  optimized_params <- read.csv(file = "./optimizedParameterTables/param_DD_S3.csv", sep=',', header = TRUE)
    
  k <- optimized_params$k
  a <- optimized_params$a
  b <- optimized_params$b
  t0 <- optimized_params$T0
  c <- optimized_params$c
  beta <- optimized_params$beta

  dd <- apply(tm, 2, function(temp) ifelse(temp > k , a/(1 + exp(-(temp - t0)/b)) , c + (beta * temp)))
  rownames(dd) <- paste("DD_18", monthcodes, sep="_")
  
  return(dd)

}

# DD18, degree days above 18

dd18 <- function(tm) {
  
  optimized_params <- read.csv(file = "./optimizedParameterTables/param_DD_S4.csv", sep=',', header = TRUE)
  optimized_params <- optimized_params[optimized_params$Region == "All",]

    
  k <- optimized_params$k
  a <- optimized_params$a
  b <- optimized_params$b
  t0 <- optimized_params$T0
  c <- optimized_params$c
  beta <- optimized_params$beta

  dd <- apply(tm, 2, function(temp) ifelse(temp > k , a/(1 + exp(-(temp - t0)/b)) , c + (beta * temp)))
  rownames(dd) <- paste("DD18", monthcodes, sep="_")
  
  return(dd)

}

# NFFD
compute_nffd <- function(t_min) {
  nffd_param <- read.csv(file = "./optimizedParameterTables/param_NFFD.csv", sep=',', header = TRUE)

  a <- nffd_param$a
  b <- nffd_param$b
  t0 <- nffd_param$T0

  nffd <- ( a/(1 + exp(-(t_min - t0)/b)))
  rownames(nffd) <- paste("NFFD", monthcodes, sep="")

  nffd_df <- data.frame(t(nffd))

  return(nffd_df)
}

# td: difference between the mean warmest monthly temperature and the mean coldest monthly temperature
td <- function(t_ave) {
  
  warmest_temp_of_year <- apply(t_ave, 2, max)
  coldest_temp__of_year <- apply(t_ave, 2, min)

  return(warmest_temp_of_year - coldest_temp__of_year)
}

# bffp: the day of the year on which FFP begins
# td: difference between the mean warmest monthly temperature and the mean coldest monthly temperature
# nffd: number of frost-free days.
bffp <- function(t_min_04, t_min_06, td, nffd) {

  bffp_value <- 352.1358994 + -0.021715653 * t_min_04^2 + -3.542187618 * t_min_06 + 0.020359471 * t_min_06^2 - 4.897998097 * td + 0.033521327 * td^2 - 2.164862277 * nffd + 0.006767633 * nffd^2 - 0.00000929 * nffd^3 + 0.043516586 * (td * nffd) - 0.00000253 * (td * nffd)^2

  return(bffp_value)
}

# effp: the day of the year on which FFP ends
# t_min_list: named list of monthly minimum temperature for each month
# td: difference between the mean warmest monthly temperature and the mean coldest monthly temperature
effp <- function(t_min_09, t_min_10, t_min_11, nffd)  {

  effp_value <- 243.7752209 + 4.134210825 * t_min_09 - 0.162876448 * t_min_09^2 + 1.248649021 * t_min_10 + 0.145073612 * t_min_10^2 + 0.004319892 * t_min_10 + -0.005753127 * t_min_10^2 - 0.06296471 * nffd + 0.000399177 * nffd^2

  return(effp_value)

}

# ffp: frost free period
ffp <-function(effp,bffp) {

  return(effp- bffp)

}

# pas: precipitation as snow
# tm: min temperature for that month
pas <- function(t_min_monthly, ppt_monthly) {

  pas_param <- read.csv(file = "./optimizedParameterTables/param_PAS.csv", sep=',', header = TRUE)

  b <- pas_param$b
  t0 <- pas_param$T0

  pas_monthly <- (1/(1 + exp(-(t_min_monthly - t0)/b))) * ppt_monthly
  rownames(pas_monthly) <- paste("PAS", monthcodes, sep="")

  return(pas_monthly)

}

# emt: extreme minimum temperature
# td: difference between the mean warmest monthly temperature and the mean coldest monthly temperature
emt <- function(td, t_min_monthly, t_min_01, t_min_12) {

  tminx <- apply(t_min_monthly, 2, min) # tminx: minimum min monthly temp over the year

  emt_value <- -23.02164 + 0.77908 * t_min_01 + 0.67048 * t_min_12 + 0.01075 * tminx^2 + 0.11565 * td

  return(emt_value)
}

# ext: extreme maximum temperature
# t_max_list: named list of monthly maximum temperature for each month
# td: difference between the mean warmest monthly temperature and the mean coldest monthly temperature
ext <- function(td, t_max_monthly, t_max_07, t_max_08) {

  tmaxx <- apply(t_max_monthly, 2, max)  # tmaxx: maximum max monthly temp over the year

  ext_value <- 10.64245 -1.92005 * t_max_07 + 0.04816 * t_max_07^2 + 2.51176 * t_max_08 - 0.03088 * t_max_08^2 -0.01311 * tmaxx^2 + 0.33167 * td - 0.001 * td^2

  return(ext_value)
}

# es: saturated vapour pressure at a temperature t
# t: air temperature
es <- function(t) {

  svp <- function(t) {
    return(0.6105 * exp((17.273*t)/(t+237.3)))
  }

  es_value <- apply(t, 2, function(temp) ifelse(temp < 0 , svp(temp)*(1 + (temp*0.01)) ,svp(temp)))

  return(es_value)
}

# rh: relative humidity
# tmin_mean: monthly mean minimum air temperature
# tmax_mean: monthly mean maximum air temperature
rh <- function(t_min_monthly, t_max_monthly) {
  es_avg = (es(t_min_monthly)+ es(t_max_monthly))/2
  rh_monthly <- (100 * es(t_min_monthly)/es_avg)
  rownames(rh_monthly) <- paste("RH", monthcodes, sep="")

  return(rh_monthly)
}


# ==========================================
#step 2d: create mean observational time series for province/ecoregion

obs.ts <- read.csv("./outputs/obs.ts.csv")

## station observations
ecoprov=ecoprovs[2]
for(ecoprov in ecoprovs){
  if(ecoprov=="BC") s <- 1:dim(obs.ts)[1] else {
    for(i in 1:length(unique(obs.ts$Year))){ if(i==1) s <- which(dem.pts$id2==ecoprov) else s <- c(s,which(dem.pts$id2==ecoprov)+length(dem.pts$id2)*(i-1))  }
  }

  # Filter for ecoprov
  gridded_data <- obs.ts[s,]

  ##########################
  # Define base variables (Tmin, Tmax, Tave) for each month
  ##########################

  # Years
  years <- gridded_data$Year # Might not need this

  # Tmin
  t_max_monthly <- t(as.matrix(gridded_data[,2:13]))

  # Tmax
  t_min_monthly <- t(as.matrix(gridded_data[,14:25]))

  # PPT
  ppt_monthly <- t(as.matrix(gridded_data[,26:37]))

  ##########################
  # Derived Climate Variables
  ##########################

  # PPT Seasonal
  ppt_df <- data.frame(t(ppt_monthly))
  ppt_seasonal <- compute_seasonal("PPT",ppt_df ,  sum)
  gridded_data <- cbind(gridded_data, ppt_seasonal)

  # Tave
  t_ave_monthly <- (t_min_monthly+t_max_monthly)/2
  rownames(t_ave_monthly) <- paste("Tave", monthcodes, sep="")
  t_ave_df <- data.frame(t(t_ave_monthly))
  t_ave_seasonal_df <- compute_seasonal("Tave", t_ave_df, mean)

  gridded_data <- cbind(gridded_data,t_ave_df) # Add monthly Tave
  gridded_data <- cbind(gridded_data, t_ave_seasonal_df)
  

  # NFFD
  nffd_monthly_df <- compute_nffd(t_min_monthly)
  nffd_seasonal_df <- compute_seasonal("NFFD", nffd_monthly_df,  sum)
  gridded_data <- cbind(gridded_data, nffd_seasonal_df)


  ## FFP, bFFP and eFFP
  temp_diff <- td(t_ave_monthly)
  begin_frost_free <- bffp(t_min_monthly["Tmin04",], t_min_monthly["Tmin06",], temp_diff, nffd_seasonal_df$NFFD)
  end_frost_free <- effp(t_min_monthly["Tmin09",], t_min_monthly["Tmin10",], t_min_monthly["Tmin11",], nffd_seasonal_df$NFFD)
  frost_free_period <- ffp(end_frost_free, begin_frost_free)

  gridded_data$FFP <- frost_free_period

  # PAS
  pas_monthly <- pas(t_min_monthly, ppt_monthly)

  pas_df <- data.frame(t(pas_monthly))
  pas_seasonal_df <- compute_seasonal("PAS", pas_df, sum)
  gridded_data <- cbind(gridded_data, pas_seasonal_df)

  # EMT, EXT
  extreme_min_temp <- emt(temp_diff, t_min_monthly, t_min_monthly["Tmin01",] ,t_min_monthly["Tmin12",])
  extreme_max_temp <- ext(temp_diff, t_min_monthly, t_min_monthly["Tmin07",] ,t_min_monthly["Tmin08",])

  gridded_data$EMT <- extreme_min_temp
  gridded_data$EXT <- extreme_max_temp

  # RH
  rh_monthly <- rh(t_min_monthly, t_max_monthly)
  rh_monthly_df <- data.frame(t(rh_monthly))
  rh_seasonal_df <- compute_seasonal("RH",rh_monthly_df, mean)

  gridded_data <- cbind(gridded_data, rh_seasonal_df)

  # DD_0

  dd_0_monthly <- dd_0(t_min_monthly)
  dd_0_monthly_df <- data.frame(t(dd_0_monthly))
  
  dd_0_seasonal_df <- compute_seasonal("DD_0", dd_0_monthly_df, mean, sep="_")

  gridded_data <- cbind(gridded_data, dd_0_seasonal_df)

  # DD5

  dd5_monthly <- dd5(t_min_monthly)
  dd5_monthly_df <- data.frame(t(dd5_monthly))
  dd5_seasonal_df <- compute_seasonal("DD5", dd5_monthly_df, mean, sep="_")

  gridded_data <- cbind(gridded_data, dd5_seasonal_df)

  # DD_18

  dd_18_monthly <- dd_18(t_min_monthly)
  dd_18_monthly_df <- data.frame(t(dd_18_monthly))
  dd_18_seasonal_df <- compute_seasonal("DD_18", dd_18_monthly_df, mean, sep="_")

  gridded_data <- cbind(gridded_data, dd_18_seasonal_df)

  # DD18

  dd18_monthly <- dd18(t_min_monthly)
  dd18_monthly_df <- data.frame(t(dd18_monthly))
  dd18_seasonal_df <- compute_seasonal("DD18", dd18_monthly_df, mean, sep="_")

  gridded_data <- cbind(gridded_data, dd18_seasonal_df)

  # TD

  td_yearly <- td(t_ave_monthly)
  gridded_data$TD <- td_yearly

  ##########################
  # Aggregate all years together
  ##########################
  ts <- aggregate(gridded_data, by=list(gridded_data$Year), FUN = mean, na.rm=T)[,-1]

  write.csv(ts,paste("./gridded_output/ts.obs.mean.", ecoprov, ".csv", sep=""), row.names=FALSE)
  print(ecoprov)
}


## ERA5
era5.ts <- read.csv("./outputs/era5.ts.csv")

ecoprov=ecoprovs[2]
for(ecoprov in ecoprovs){
  if(ecoprov=="BC") s <- 1:dim(era5.ts)[1] else {
    for(i in 1:length(unique(era5.ts$Year))){ if(i==1) s <- which(dem.pts$id2==ecoprov) else s <- c(s,which(dem.pts$id2==ecoprov)+length(dem.pts$id2)*(i-1))  }
  }


  # Filter for ecoprov
  gridded_data <- obs.ts[s,]

  ##########################
  # Define base variables (Tmin, Tmax, Tave) for each month
  ##########################

  # Years
  years <- gridded_data$Year # Might not need this

  # Tmin
  t_max_monthly <- t(as.matrix(gridded_data[,2:13]))

  # Tmax
  t_min_monthly <- t(as.matrix(gridded_data[,14:25]))

  # PPT
  ppt_monthly <- t(as.matrix(gridded_data[,26:37]))

  ##########################
  # Derived Climate Variables
  ##########################

  # PPT Seasonal
  ppt_df <- data.frame(t(ppt_monthly))
  ppt_seasonal <- compute_seasonal("PPT",ppt_df ,  sum)
  gridded_data <- cbind(gridded_data, ppt_seasonal)

  # Tave
  t_ave_monthly <- (t_min_monthly+t_max_monthly)/2
  rownames(t_ave_monthly) <- paste("Tave", monthcodes, sep="")
  t_ave_df <- data.frame(t(t_ave_monthly))
  t_ave_seasonal_df <- compute_seasonal("Tave", t_ave_df, mean)

  gridded_data <- cbind(gridded_data,t_ave_df) # Add monthly Tave
  gridded_data <- cbind(gridded_data, t_ave_seasonal_df)


  # NFFD
  nffd_monthly_df <- compute_nffd(t_min_monthly)
  nffd_seasonal_df <- compute_seasonal("NFFD", nffd_monthly_df,  sum)
  gridded_data <- cbind(gridded_data, nffd_seasonal_df)


  ## FFP, bFFP and eFFP
  temp_diff <- td(t_ave_monthly)
  begin_frost_free <- bffp(t_min_monthly["Tmin04",], t_min_monthly["Tmin06",], temp_diff, nffd_seasonal_df$NFFD)
  end_frost_free <- effp(t_min_monthly["Tmin09",], t_min_monthly["Tmin10",], t_min_monthly["Tmin11",], nffd_seasonal_df$NFFD)
  frost_free_period <- ffp(end_frost_free, begin_frost_free)

  gridded_data$FFP <- frost_free_period

  # PAS
  pas_monthly <- pas(t_min_monthly, ppt_monthly)

  pas_df <- data.frame(t(pas_monthly))
  pas_seasonal_df <- compute_seasonal("PAS", pas_df, sum)
  gridded_data <- cbind(gridded_data, pas_seasonal_df)

  # EMT, EXT
  extreme_min_temp <- emt(temp_diff, t_min_monthly, t_min_monthly["Tmin01",] ,t_min_monthly["Tmin12",])
  extreme_max_temp <- ext(temp_diff, t_min_monthly, t_min_monthly["Tmin07",] ,t_min_monthly["Tmin08",])

  gridded_data$EMT <- extreme_min_temp
  gridded_data$EXT <- extreme_max_temp

  # RH
  rh_monthly <- rh(t_min_monthly, t_max_monthly)
  rh_monthly_df <- data.frame(t(rh_monthly))
  rh_seasonal_df <- compute_seasonal("RH",rh_monthly_df, mean)

  gridded_data <- cbind(gridded_data, rh_seasonal_df)

  # DD_0
  
  dd_0_monthly <- dd_0(t_min_monthly)
  dd_0_monthly_df <- data.frame(t(dd_0_monthly))
  dd_0_seasonal_df <- compute_seasonal("DD_0", dd_0_monthly_df, mean, sep="_")
  
  gridded_data <- cbind(gridded_data, dd_0_seasonal_df)
  
  # DD5
  
  dd5_monthly <- dd5(t_min_monthly)
  dd5_monthly_df <- data.frame(t(dd5_monthly))
  dd5_seasonal_df <- compute_seasonal("DD5", dd5_monthly_df, mean, sep="_")
  
  gridded_data <- cbind(gridded_data, dd5_seasonal_df)
  
  # DD_18
  
  dd_18_monthly <- dd_18(t_min_monthly)
  dd_18_monthly_df <- data.frame(t(dd_18_monthly))
  dd_18_seasonal_df <- compute_seasonal("DD_18", dd_18_monthly_df, mean, sep="_")
  
  gridded_data <- cbind(gridded_data, dd_18_seasonal_df)
  
  # DD18
  
  dd18_monthly <- dd18(t_min_monthly)
  dd18_monthly_df <- data.frame(t(dd18_monthly))
  dd18_seasonal_df <- compute_seasonal("DD18", dd18_monthly_df, mean, sep="_")
  
  gridded_data <- cbind(gridded_data, dd18_seasonal_df)

  # TD
  
  td_yearly <- td(t_ave_monthly)
  gridded_data$TD <- td_yearly

  ##########################
  # Aggregate all years together
  ##########################
  ts <- aggregate(gridded_data, by=list(gridded_data$Year), FUN = mean, na.rm=T)[,-1]
  write.csv(ts,paste("./gridded_output/ts.era5.mean.", ecoprov, ".csv", sep=""), row.names=FALSE)
  print(ecoprov)
}




# ==========================================
# step 3: GCM Files
# ==========================================

files <- list.files("outputs/", pattern=paste("^ts.*", sep="."))
gcms <- unique(sapply(strsplit(files, "[.]"), "[", 2))
gcms <- gcms[-grep("obs|era", gcms)]

i=1
for(i in 1:length(gcms)){
  gcm <- gcms[i]


  # ==========================================
  # step 3b: calculate average time series across BC/ecoprovince for each gcm and scenario

  files <- list.files("outputs/")
  files <- files[grep(paste("ts", gcm, sep="."), files)] #these ts (time series) files have one record for each grid cell for each year.
  run.list <- sapply(strsplit(files, "[.]"), "[", 3)
  scenario.list <- sapply(strsplit(run.list, "_"), "[", 1)
  ripf.list <- sapply(strsplit(run.list, "_"), "[", 2)
  scenarios <- unique(scenario.list)


  scenario <- scenarios[1]
  for(scenario in scenarios){
    ripfs <- unique(ripf.list[which(scenario.list==scenario)])
    ripf <- ripfs
    for(ripf in ripfs){
      data.full <- read.csv(paste("./outputs/ts.", gcm, ".", scenario, "_", ripf, ".csv", sep=""))
      ecoprov <- ecoprovs[1]
      for(ecoprov in ecoprovs){
        if(ecoprov=="BC") s <- 1:dim(data.full)[1] else {
          for(k in 1:length(unique(data.full$Year))){ if(k==1) s <- which(dem.pts$id2==ecoprov) else s <- c(s,which(dem.pts$id2==ecoprov)+length(dem.pts$id2)*(k-1))  }
        }
        data <- data.full[s,]
        Year <- data$Year
        x <- unique(data$Year)

        gridded_data <- data


        ##########################
        # Define base variables (Tmin, Tmax, Tave) for each month
        ##########################

        # Years
        years <- gridded_data$Year # Might not need this

        # Tmin
        t_max_monthly <- t(as.matrix(gridded_data[,2:13]))

        # Tmax
        t_min_monthly <- t(as.matrix(gridded_data[,14:25]))

        # PPT
        ppt_monthly <- t(as.matrix(gridded_data[,26:37]))

        ##########################
        # Derived Climate Variables
        ##########################

        # PPT Seasonal
        ppt_df <- data.frame(t(ppt_monthly))
        ppt_seasonal <- compute_seasonal("PPT",ppt_df ,  sum)
        gridded_data <- cbind(gridded_data, ppt_seasonal)

        # Tave
        t_ave_monthly <- (t_min_monthly+t_max_monthly)/2
        rownames(t_ave_monthly) <- paste("Tave", monthcodes, sep="")
        t_ave_df <- data.frame(t(t_ave_monthly))
        t_ave_seasonal_df <- compute_seasonal("Tave", t_ave_df, mean)

        gridded_data <- cbind(gridded_data, t_ave_seasonal_df)


        # NFFD
        nffd_monthly_df <- compute_nffd(t_min_monthly)
        nffd_seasonal_df <- compute_seasonal("NFFD", nffd_monthly_df,  sum)
        gridded_data <- cbind(gridded_data, nffd_seasonal_df)


        ## FFP, bFFP and eFFP
        temp_diff <- td(t_ave_monthly)
        begin_frost_free <- bffp(t_min_monthly["Tmin04",], t_min_monthly["Tmin06",], temp_diff, nffd_seasonal_df$NFFD)
        end_frost_free <- effp(t_min_monthly["Tmin09",], t_min_monthly["Tmin10",], t_min_monthly["Tmin11",], nffd_seasonal_df$NFFD)
        frost_free_period <- ffp(end_frost_free, begin_frost_free)

        gridded_data$FFP <- frost_free_period

        # PAS
        pas_monthly <- pas(t_min_monthly, ppt_monthly)

        pas_df <- data.frame(t(pas_monthly))
        pas_seasonal_df <- compute_seasonal("PAS", pas_df, sum)
        gridded_data <- cbind(gridded_data, pas_seasonal_df)

        # EMT, EXT
        extreme_min_temp <- emt(temp_diff, t_min_monthly, t_min_monthly["Tmin01",] ,t_min_monthly["Tmin12",])
        extreme_max_temp <- ext(temp_diff, t_min_monthly, t_min_monthly["Tmin07",] ,t_min_monthly["Tmin08",])

        gridded_data$EMT <- extreme_min_temp
        gridded_data$EXT <- extreme_max_temp

        # RH
        rh_monthly <- rh(t_min_monthly, t_max_monthly)
        rh_monthly_df <- data.frame(t(rh_monthly))
        rh_seasonal_df <- compute_seasonal("RH",rh_monthly_df, mean)

        gridded_data <- cbind(gridded_data, rh_seasonal_df)

        # DD_0
        
        dd_0_monthly <- dd_0(t_min_monthly)
        dd_0_monthly_df <- data.frame(t(dd_0_monthly))
        dd_0_seasonal_df <- compute_seasonal("DD_0", dd_0_monthly_df, mean, sep="_")
        
        gridded_data <- cbind(gridded_data, dd_0_seasonal_df)
        
        # DD5
        
        dd5_monthly <- dd5(t_min_monthly)
        dd5_monthly_df <- data.frame(t(dd5_monthly))
        dd5_seasonal_df <- compute_seasonal("DD5", dd5_monthly_df, mean, sep="_")
        
        gridded_data <- cbind(gridded_data, dd5_seasonal_df)
        
        # DD_18
        
        dd_18_monthly <- dd_18(t_min_monthly)
        dd_18_monthly_df <- data.frame(t(dd_18_monthly))
        dd_18_seasonal_df <- compute_seasonal("DD_18", dd_18_monthly_df, mean, sep="_")
        
        gridded_data <- cbind(gridded_data, dd_18_seasonal_df)
        
        # DD18
        
        dd18_monthly <- dd18(t_min_monthly)
        dd18_monthly_df <- data.frame(t(dd18_monthly))
        dd18_seasonal_df <- compute_seasonal("DD18", dd18_monthly_df, mean, sep="_")
        
        gridded_data <- cbind(gridded_data, dd18_seasonal_df)
        
        # TD
        
        td_yearly <- td(t_ave_monthly)
        gridded_data$TD <- td_yearly

        ##########################
        # Aggregate all years together
        ##########################
        ts <- aggregate(gridded_data, by=list(gridded_data$Year), FUN = mean, na.rm=T)[,-1]

        
        variables <- names(ts)[-1]
        assign(paste("ts.mean",ecoprov, ripf, sep="."), round(ts,1))
      }
      print(ripf)
    }
    ecoprov=ecoprovs[1]
    for(ecoprov in ecoprovs){
      for(variable in variables){
        ensemble <- data.frame()
        for(ripf in ripfs){
          ts <- get(paste("ts.mean",ecoprov, ripf, sep="."))

          ensemble[1:length(x),which(ripfs==ripf)] <- ts[,which(names(ts)==variable)]
          # print(ripf)
        }
        names(ensemble) <- ripfs
        ensemble <- data.frame(Year=x, ensemble)
        write.csv(ensemble,paste("./gridded_output/ensemble", gcm, ecoprov, variable, scenario, "csv", sep="."), row.names=FALSE)
        # print(variable)
      }
      # print(ecoprov)
    }
    print(scenario)
  }

  print(gcm)
}


# ==========================================
# step 4: Ensemble Files.
# min, max, and mean for each model and for whole ensemble
# ==========================================

variables <-names(read.csv("./gridded_output/ts.era5.mean.SIM.csv"))[-1] 
print(variables)

files <- list.files("./gridded_output/", pattern=paste("^ensemble.*", sep="."))
gcms.all <- unique(sapply(strsplit(files, "[.]"), "[", 2))
scenarios <- unique(sapply(strsplit(files, "[.]"), "[", 5))
funs <- c("min", "max", "mean")
fun <- funs[1]
for(fun in funs){
  scenario <- scenarios[1]
  for(scenario in scenarios){
    ecoprov <- ecoprovs[1]
    for(ecoprov in ecoprovs){
      variable <- variables[1]
      for(variable in variables){
        files <- list.files("./gridded_output/", pattern=paste("^ensemble.*", ecoprov, variable, scenario,"*", sep="."))
        gcms <- unique(sapply(strsplit(files, "[.]"), "[", 2))
        gcm <- gcms[1]
        data <- read.csv(paste("./gridded_output/ensemble", gcm, ecoprov, variable, scenario, "csv", sep="."))
        temp <- data.frame(data[,1], matrix(NA, dim(data)[1],length(gcms.all)))
        names(temp) <- c("Year", gcms.all)
        for(gcm in gcms){
          data <- read.csv(paste("./gridded_output/ensemble", gcm, ecoprov, variable, scenario, "csv", sep="."))
          stat <- if(dim(data)[2]==2) data[,2] else round(apply(data[,-1], 1, fun),1)
          temp[match(data$Year, temp$Year),which(names(temp)==gcm)] <- stat
          # print(gcm)
        }
        temp <- cbind(temp, round(apply(temp[,-1], 1, fun, na.rm=T),1))
        names(temp) <- c("Year", gcms.all, "ensemble")
        write.csv(temp,paste(paste("./gridded_output/ens", fun, sep=""), ecoprov, variable, scenario, "csv", sep="."), row.names=FALSE)
        print(variable)
      }
      print(ecoprov)
    }
    print(scenario)
  }
  print(fun)
}

## rbind the scenarios together and write out.
files <- list.files("./gridded_output/", pattern=paste("^ensemble.*", sep="."))
scenarios <- unique(sapply(strsplit(files, "[.]"), "[", 5))
variables <- unique(sapply(strsplit(files, "[.]"), "[", 4))
ecoprovs <- unique(sapply(strsplit(files, "[.]"), "[", 3))
funs <- c("min", "max", "mean")
for(fun in funs){
  for(ecoprov in ecoprovs){
    for(variable in variables){
      for(scenario in scenarios){
        temp <- read.csv(paste(paste("./gridded_output/ens", fun, sep=""), ecoprov, variable, scenario, "csv", sep="."))
        data <- if(scenario==scenarios[1]) data.frame(scenario=rep(scenario, dim(temp)[1]), temp) else rbind(data, data.frame(scenario=rep(scenario, dim(temp)[1]), temp))
        # print(scenario)
      }
      write.csv(data,paste(paste("./gridded_output/ens", fun, sep=""), ecoprov, variable, "csv", sep="."), row.names=FALSE)
      # print(variable)
    }
    print(ecoprov)
  }
  print(fun)
}



