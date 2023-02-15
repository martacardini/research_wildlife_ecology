# R code for monitoring lynx Home Range

# research questions:#
# HOW DOES THE HR CHANGES IN RESPECTTO THE TIME OF THE DAY?
# is it bigger IN THE NIGHT? 
#--> IF WE HAVE TIME: compare the HR size of lynx in respect to the distance from anthropized areas. 

# setting the working directory
setwd("uniBO/research wildlife ecology/R Material - WG3 HR & WG4 HS")

# loading the necessary packages
library(foreign)
library(tidyverse)
library(dplyr)
library(lubridate)
library(raster)
library(mapview)
library(sf)
library(amt)
library(ggplot2) 
library(ctmm)



# import the data of the lynx, extract the needed columns, add the ID column, add columns with date and time together, create timestamp column ---- 
Ctirad <- read.dbf("data/gps_data_lynx_bfnp/Luchs_Ctirad_2011_010020_NpSumava_GPS.dbf")[,c("LMT_DATE", "LMT_TIME", "LATITUDE", "LONGITUDE", "HEIGHT", "TEMP")]
Ctirad$ID <- "Ctirad"
Ctirad$lmt <- ymd_hms(paste(Ctirad$LMT_DATE, Ctirad$LMT_TIME), tz= "Europe/Berlin") # local time zone

Emanuel <- read.dbf("data/gps_data_lynx_bfnp/Luchs_Emanuel_2010_007987_NpSumava_GPS.dbf")[,c("LMT_DATE", "LMT_TIME", "LATITUDE", "LONGITUDE", "HEIGHT", "TEMP")]
Emanuel$ID <- "Emanuel"
Emanuel$lmt <- ymd_hms(paste(Emanuel$LMT_DATE, Emanuel$LMT_TIME), tz= "Europe/Berlin")

Kika <- read.dbf("data/gps_data_lynx_bfnp/Luchs_Kika_2011_008899_NpBayWa_GPS.dbf")[,c("LMT_DATE", "LMT_TIME", "LATITUDE", "LONGITUDE", "HEIGHT", "TEMP")]
Kika$ID <- "Kika"
Kika$lmt <- ymd_hms(paste(Kika$LMT_DATE, Kika$LMT_TIME), tz= "Europe/Berlin")

Kubicka <- read.dbf("data/gps_data_lynx_bfnp/Luchs_Kubicka_2011_005324_NpSumava_GPS.dbf")[,c("LMT_DATE", "LMT_TIME", "LATITUDE", "LONGITUDE", "HEIGHT", "TEMP")]
Kubicka$ID <- "Kubicka" 
Kubicka$lmt <- ymd_hms(paste(Kubicka$LMT_DATE, Kubicka$LMT_TIME), tz= "Europe/Berlin")

Matilda <- read.dbf("data/gps_data_lynx_bfnp/Luchs_Matilda_2011_010067_NpSumava_GPS.dbf")[,c("LMT_DATE", "LMT_TIME", "LATITUDE", "LONGITUDE", "HEIGHT", "TEMP")]
Matilda$ID <- "Matilda"
Matilda$lmt <- ymd_hms(paste(Matilda$LMT_DATE, Matilda$LMT_TIME), tz= "Europe/Berlin")

Milan <- read.dbf("data/gps_data_lynx_bfnp/Luchs_Milan_2008_005327_NpBayWa_GPS.dbf")[,c("LMT_DATE", "LMT_TIME", "LATITUDE", "LONGITUDE", "HEIGHT", "TEMP")]
Milan$ID <- "Milan"
Milan$lmt <- ymd_hms(paste(Milan$LMT_DATE, Milan$LMT_TIME), tz= "Europe/Berlin")

Nimo <- read.dbf("data/gps_data_lynx_bfnp/Luchs_Nimo_2012_007986_NpSumava_GPS.dbf")[,c("LMT_DATE", "LMT_TIME", "LATITUDE", "LONGITUDE", "HEIGHT", "TEMP")]
Nimo$ID <- "Nimo"
Nimo$lmt <- ymd_hms(paste(Nimo$LMT_DATE, Nimo$LMT_TIME), tz= "Europe/Berlin")

Nora <- read.dbf("data/gps_data_lynx_bfnp/Luchs_Nora_2007_002848_NpBayWa_GPS.dbf")[,c("LMT_DATE", "LMT_TIME", "LATITUDE", "LONGITUDE", "HEIGHT", "TEMP")]
Nora$ID <- "Nora"
Nora$lmt <- ymd_hms(paste(Nora$LMT_DATE, Nora$LMT_TIME), tz= "Europe/Berlin")

Patrik <- read.dbf("data/gps_data_lynx_bfnp/Luchs_Patrik_2010_007984_NpBayWa_GPS.dbf")[,c("UTC_DATE", "UTC_TIME", "LATITUDE", "LONGITUDE", "HEIGHT", "TEMP")]
Patrik$ID <- "Patrick"
Patrik$lmt <- ymd_hms(paste(Patrik$UTC_DATE, Patrik$UTC_TIME), tz= "UTC")
#Patrik$UTC_TIME <- ymd_hms(Patrik$UTC_TIME, tz="UTC")
# transform Patrik utc times into lmt
Patrik$lmt <- with_tz(Patrik$lmt, tzone = "Europe/Berlin")
# Patrick$LMT_DATE<- with_tz(Patrik$lmt, tzone = "Europe/Berlin")

Tessa <- read.dbf("data/gps_data_lynx_bfnp/Luchs_Tessa_2011_010068_NpBayWa_GPS.dbf")[,c("LMT_DATE", "LMT_TIME", "LATITUDE", "LONGITUDE", "HEIGHT", "TEMP")]
Tessa$ID <- "Tessa"
Tessa$lmt <- ymd_hms(paste(Tessa$LMT_DATE, Tessa$LMT_TIME), tz= "Europe/Berlin")

# delete the columns with the separated time and date 
Ctirad <- Ctirad[,3:8]
Emanuel <- Emanuel[,3:8]
Kika <- Kika[,3:8]
Kubicka <- Kubicka[,3:8]
Matilda <- Matilda[,3:8]
Milan <- Milan[,3:8]
Nora <- Nora[,3:8]
Nimo <- Nimo[,3:8]
Patrik <- Patrik[,3:8]
Tessa <- Tessa[,3:8]


# create a data frame with all the individuals together
all_lynx_raw <- rbind(Tessa,Patrik,Nora,Nimo,Milan,Matilda,Kubicka,Kika,Emanuel,Ctirad)

#remove NAs
all_lynx_clean <- na.omit(all_lynx_raw)

# write.csv2(all_lynx_clean, "All_lynx_CLEAN.csv")

# dev.off()


#--- REMOVE SPACIAL OUTLIERS ----

# visualise latitude and longitude in the map and the histograms 
mapview(all_lynx_clean[,1],all_lynx_clean[,2])
hist(all_lynx_clean$LATITUDE)
hist(all_lynx_clean$LONGITUDE)

range(all_lynx_clean$LATITUDE)
range(all_lynx_clean$LONGITUDE)
summary(all_lynx_clean)

# clean the dataset by removing the outliers
lynx_filtered <- all_lynx_clean %>% filter(LATITUDE < 50, LATITUDE > 48, LONGITUDE < 14, LONGITUDE > 12)
hist(lynx_filtered$LATITUDE)
hist(lynx_filtered$LONGITUDE)
mapview(lynx_filtered[,1],lynx_filtered[,2])


# ---- REMOVE TEMPORAL OUTLIERS ----

#new column with the date
lynx_filtered$days <- as.Date(lynx_filtered$lmt)


# histogram for the lynx data, by day --> identify the outliers
for(i in unique(lynx_filtered$ID)){
  hist(filter(lynx_filtered,ID==i)$lmt, breaks = "days", main = i)
}

# add new column to identify the rows
lynx_filtered$record <- 1:nrow(lynx_filtered)

# remove outliers 
lynx_filtered_new <-  lynx_filtered %>% filter(!record %in% c(8623:8638), !record %in% c(6006:6008))


# plot the cleaned data, to verify if it worked
for(i in unique(lynx_filtered_new$ID)){
  hist(filter(lynx_filtered_new,ID==i)$lmt, breaks = "days", main = i)
}


# clean!!!!
# write.csv2(lynx_filtered_new, "lynx_no_outliers.csv")

dev.off()





# --- MAKE A TRAK ----
str(lynx_filtered_new)

# nest the data by individual ID
nestedlynx <- nest(lynx_filtered_new, by= -"ID")

# make a trak
# mutate() to add new column trk in the nested data
# make_track() to create the track 
# transform coordinates from 4326 system to 3035 system
# time_of_day() to identify day and night

lynx_track <- nestedlynx %>%
  mutate(trk = map(by, function(x) {
    make_track(x, LONGITUDE, LATITUDE, lmt, crs = st_crs(4326)) %>%
      transform_coords(st_crs(3035)) %>% 
      time_of_day(include.crepuscule=F)
  }))
str(lynx_track)
# database separated by day and night
lynx_trak_unnest <- lynx_track %>% dplyr::select(-by) %>% unnest(cols=trk)

track_day <- subset(lynx_trak_unnest, tod_=="day")
track_night <- subset(lynx_trak_unnest, tod_=="night")
str(track_day)
str(track_night)

# nest the data separated by day and night
nest_track_day <- nest(track_day, by=(-"ID"))
nest_track_night <- nest(track_night, by=(-"ID"))

# make another track with the data divided by day and night
lynx_track_day <- nest_track_day %>%
  mutate(trk = map(by, function(x) {
    make_track(x, x_, y_, t_, crs = st_crs(3035))}))

lynx_track_night <- nest_track_night %>%
  mutate(trk = map(by, function(x) {
    make_track(x, x_, y_, t_, crs = st_crs(3035))}))


# --- NSD ----

# --DAY NSD
lynx_track_day_nsd <- lynx_track_day %>% mutate(trk=map(trk,~ add_nsd(.)))
#day plot
nsd_plot_day <- map(lynx_track_day_nsd$trk, function(X)
  ggplot(X, aes(x = t_, y = nsd_)) + geom_path())
# add id
for (i in 1:length(lynx_track_day_nsd$ID)) { nsd_plot_day[[i]] <- nsd_plot_day[[i]] + ggtitle(lynx_track_day_nsd$ID[i])  }

nsd_plot_day

# milan e emanuel have few data --> maybe remove them in data analysis (function subset)


# --NIGHT NSD
lynx_track_night_nsd <- lynx_track_night %>% mutate(trk=map(trk,~ add_nsd(.)))
#night plot
nsd_plot_night <- map(lynx_track_night_nsd$trk, function(X)
  ggplot(X, aes(x = t_, y = nsd_)) + geom_path())
# add id
for (i in 1:length(lynx_track_night_nsd$ID)) { nsd_plot_night[[i]] <- nsd_plot_night[[i]] + ggtitle(lynx_track_night_nsd$ID[i])  }

nsd_plot_night


#### Variograms

# ---->DAY VARIOGRAMS

# same as: 'lynx_data <- lynx_data[order(lynx_data$id, lynx_data$utc),]'
# we need to use the raw data separated by time of the day
# transform the coordination system back to latitude and longitude (from 3035 to 4326)

lynx_data_ctmm_day <- track_day %>% dplyr::select(ID, timestamp=t_, longitude=x_, latitude=y_) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=3035) %>% st_transform(4326) %>%
  mutate(longitude=st_coordinates(.)[,1], latitude=st_coordinates(.)[,2]) %>% st_drop_geometry() %>% 
  ctmm::as.telemetry(.)

# plot all
map(lynx_data_ctmm_day, function(X) plot(ctmm::variogram(X), level=c(0.5, 0.95)) + title(X@info$identity))


# ---->NIGHT VARIOGRAMS

#same as the day

lynx_data_ctmm_night <- track_night %>% dplyr::select(ID, timestamp=t_, longitude=x_, latitude=y_) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=3035) %>% st_transform(4326) %>%
  mutate(longitude=st_coordinates(.)[,1], latitude=st_coordinates(.)[,2]) %>% st_drop_geometry() %>% 
  ctmm::as.telemetry(.)

# plot all
map(lynx_data_ctmm_night, function(X) plot(ctmm::variogram(X), level=c(0.5, 0.95)) + title(X@info$identity))


# ---- HOME RANGE ESTIMATION ----


# DAY HOME RANGE ()
hr_day <- lynx_track_day %>% 
  mutate(
    hr_akde=map(trk, ~ hr_akde(.,fit_ctmm(.,"ou")))
  )

hr_day <- filter(lynx_track_day, ID %in% c("Tessa", "Patrick")) %>% 
  mutate(
    hr_akde=map(trk, ~ hr_akde(.,fit_ctmm(.,"ou")))
  )

# calculate the home range area 
# pivot_longer(), hr_area()

# get the isopleth
# 95% akde: line that includes points with 95% probability of finding the lynx in this area
# 50% adke: line including points that have 50% of the porbability of finding the lynx there

# which gives the home range area in m² and the polygon to plot. 
# transform the area to km²
hr_isopleths()

# plot it together with the spatial data
mapview()




# NIGHT HOME RANGE




# ---  import the corine land cover map to analyse the proportion of anthropised area---- 
raster("data/covariates/rasters/NPBW_corine_LAEA.tif")





# ---- COVARIATE CALCULATION
# ---- MODELLING ----
library(lme4) #GLM(M)s
library(fitdistrplus) # to create distributions
library(car) #qq-plots
library(AICcmodavg) #model selection
library(DHARMa) #model diagnostics
library(effects) #plots of effects of variables of a model
library(mgcv) #GAM(M)s
library(itsadug) #functions for evaluation and plot of GAM(M)s
