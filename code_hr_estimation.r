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
Patrik$ID <- "Patrik"
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

# visualise the cleaned dataset
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
lynx_track_day_nest <- nest(track_day, by=(-"ID"))
lynx_track_night_nest <- nest(track_night, by=(-"ID"))

# make another track with the data divided by day and night
lynx_track_day <- lynx_track_day_nest %>%
  mutate(trk = map(by, function(x) {
    make_track(x, x_, y_, t_, crs = st_crs(3035))}))

lynx_track_night <- lynx_track_night_nest %>%
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
    hr_akde=map(trk, ~ st_as_sf(., coords=c("x_", "y_"), crs=3035) %>% st_transform(., 4326) %>%
                  mutate(x_=st_coordinates(.)[,1], y_=st_coordinates(.)[,2]) %>%
                  st_drop_geometry(.))# %>% hr_akde(.,fit_ctmm(.,"ou")))
  )

#lynx_track <- lynx_track %>% mutate(trk = map(trk, function(x) {
#  x %>% time_of_day(include.crepuscule=FALSE) # use 'include.crepuscule=TRUE' to get also dusk and dawn
#} ))

#lynx_track_day <- lynx_track %>% dplyr::select(c(id, trk)) %>% unnest(cols = trk) %>% filter(tod_=="day")
#lynx_track_night <- lynx_track %>% dplyr::select(c(id, trk)) %>% unnest(cols = trk) %>% filter(tod_=="night")

#lynx_track_day_nest <- lynx_track_day %>% nest(data=-"id")
#lynx_track_night_nest <- lynx_track_night %>% nest(data=-"id")


# make another track with the data divided by day and night

#### day

lynx_track_day_trk <- lynx_track_day_nest %>%
  mutate(trk = map(by, function(d) {
    make_track(d, x_, y_, t_, crs = st_crs(3035))
  }))

# calculate how much time takes the function to work
system.time(
  hr_akde_ou_day <- lynx_track_day_trk %>%
    mutate(
      hr_akde = map(trk, ~ hr_akde(., fit_ctmm(., "ou")))
    )
)

# get the isopleth
# 95% akde: line that includes points with 95% probability of finding the lynx in this area
# transform the area from m² to km²
# add sex column

iso_hr_akde_ou_day <- hr_akde_ou_day %>%
  mutate(
    iso_hr_akde = map(hr_akde, ~ hr_isopleths(.))
  )

iso_hr_akde_ou_day_unn <- iso_hr_akde_ou_day %>% dplyr::select(c(id,iso_hr_akde)) %>%
  unnest(cols = iso_hr_akde) %>%
  filter(what=="estimate") %>%
  mutate(area_km2 = as.numeric(area)/1000000) %>%
  mutate(sex = ifelse(id %in% c("Tessa", "Kubicka", "Nora", "Matilda"), "f", "m") %>% factor())

# plot the day home ranges with the visual data
mapview(st_as_sf(iso_hr_akde_ou_day_unn), zcol="id", burst=TRUE)


#### night

# repeat same processes as before

lynx_track_night_trk <- lynx_track_night_nest %>%
  mutate(trk = map(data, function(d) {
    make_track(d, x_, y_, t_, crs = st_crs(3035)) 
  }))

system.time(
  hr_akde_ou_night <- lynx_track_night_trk %>%
    mutate(
      hr_akde = map(trk, ~ hr_akde(., fit_ctmm(., "ou")))
    )
)

iso_hr_akde_ou_night <- hr_akde_ou_night %>%
  mutate(
    iso_hr_akde = map(hr_akde, ~ hr_isopleths(.))
  )

iso_hr_akde_ou_night_unn <- iso_hr_akde_ou_night %>% dplyr::select(c(id,iso_hr_akde)) %>%
  unnest(cols = iso_hr_akde) %>%
  filter(what=="estimate") %>%
  mutate(area_km2 = as.numeric(area)/1000000) %>%
  mutate(sex = ifelse(id %in% c("Tessa", "Kubicka", "Nora", "Matilda"), "f", "m") %>% factor())


# plot the night home ranges with visual data
mapview(st_as_sf(iso_hr_akde_ou_night_unn), zcol="id", burst=TRUE)


# export the datasets
lynx_hr_day <- iso_hr_akde_ou_day_unn %>% dplyr::select(-geometry)
# write.csv2(lynx_hr_day, "lynx_hr_day.csv")
lynx_hr_night<- iso_hr_akde_ou_night_unn %>% dplyr::select(-geometry)
# write.csv2(lynx_hr_night, "lynx_hr_night.csv")










# ---- MODELLING ----
library(lme4) #GLM(M)s
library(fitdistrplus) # to create distributions
library(car) #qq-plots
library(AICcmodavg) #model selection
library(DHARMa) #model diagnostics
library(effects) #plots of effects of variables of a model
library(mgcv) #GAM(M)s
library(itsadug) #functions for evaluation and plot of GAM(M)s



# import the data from the HR obtained with the isopleth

lynx_hr_day <- read.csv2("lynx_hr_day.csv")     #day
lynx_hr_night <- read.csv2("lynx_hr_night.csv") #night

# combine the 2 data frames together to have a unique data frame with the time of the day included
# add a column with the time of the day ("day" or "night")

lynx_hr_day2 <- lynx_hr_day %>%
  add_column(time = "day")

lynx_hr_night2 <- lynx_hr_night %>%
  add_column(time = "night")

total_lynx_hr <- rbind(lynx_hr_day2,lynx_hr_night2)
total_lynx_hr # database with all the 


############ --- BOXPLOTS --- ################----

# day home range by sex
boxplot_day_sex <- ggplot(lynx_hr_day, aes(x=sex, y=area_km2, color =sex)) + 
 geom_boxplot() + ggtitle("Home range size during the day")
boxplot_day_sex

# night home range by sex
boxplot_night_sex <- ggplot(lynx_hr_night, aes(x=sex, y=area_km2, fill=sex)) + 
  geom_boxplot() +  ggtitle("Home range size during the night")
boxplot_night_sex

 
# boxplot HR size by time of the day and by sex
boxplot_hr_time_sex<- ggplot(total_lynx_hr, aes(x=time, y=area_km2, fill=sex)) +
  geom_boxplot()+  ggtitle("Home range size by time of the day")
boxplot_hr_time_sex

# export the boxplot
# png("boxplot_hr_time_sex.png", width = 600, height = 600)
# boxplot_hr_time_sex
# dev.off()

# riga nera = mediana


#### ---MODELLING --- ###

#plot the histograms to check for the shape of data distribution
hist(total_lynx_hr$area_km2)
hist(log(total_lynx_hr$area_km2))

# not normal distribution --> glm
help(Gamma)

# lm(log(area_km2) ~ sex) --> NO
# possible family to use: 
# ---> glm(area_km2 ~ sex, family=gaussian(link="log"), data = lynx_hr_total)
# ---> glm(area_km2 ~ sex, family=gaussian(link="identity"), data = lynx_hr_total)
# ---> glm(area_km2 ~ sex, family=Gamma(link="identity"), data = lynx_hr_total)
# using family = gaussian(link="log")


# is there a significant difference in the HR size from m to f?

# area_km2 ~ sex
sexglm <- glm(area_km2 ~ sex, family=gaussian(link="log"), data = total_lynx_hr)
# DHARMa residuals
residualssexglm <- simulateResiduals(sexglm)
plot(residualssexglm)

# area_km2 ~ time
timeglm <- glm(area_km2 ~ time, family=gaussian(link="log"), data = total_lynx_hr)
# DHARMa residuals
residualstimeglm <- simulateResiduals(timeglm)
plot(residualstimeglm)

# null model area_km2 ~ 1
nullglm <- glm(area_km2 ~ 1, family=gaussian(link="log"), data = total_lynx_hr)
# DHARMa residuals
residualsnullglm <- simulateResiduals(nullglm)
plot(residualsnullglm)

# area_km2 ~ sex + time
sextimeglm <- glm(area_km2 ~ sex + time, family=gaussian(link="log"), data = total_lynx_hr)
# DHARMa residuals
residualssextimeglm <- simulateResiduals(sextimeglm)
plot(residualssextimeglm)

# area_km2 ~ sex + time + sex:time
intsextimeglm <- glm(area_km2 ~ sex + time + sex:time, family=gaussian(link="log"), data = total_lynx_hr)
# identical to area_km2 ~ sex*time
# DHARMa residuals
residualsintsextimeglm <- simulateResiduals(intsextimeglm)
plot(residualsintsextimeglm)

## family = Gamma(link = "identity")
# testglm <- glm(formula = area_km2 ~ sex + time + sex:time, family = Gamma(link = "identity"), 
#                data = total_lynx_hr)
# residualstestglm <- simulateResiduals(testglm)
# plot(residualstestglm)

# summary of the models
summary(sextimeglm)
summary(intsextimeglm)
# summary(testglm)


# comparing the models to find the best one
# AIC()
# install.packages("AICcmodavg")
library(AICcmodavg)

# create a list of the models
modelshr <- list(sexglm, timeglm, nullglm, sextimeglm, intsextimeglm)

names_modelshr <- c("sex", "time", "null", "sex+time", "sex*time")

aictab(modelshr, second.ord = T, modnames = names_modelshr)
# second.ord = T --> small sample size, uses AICc
# delta_AICc<2 = to take into consideration


# plot the models
library(effects)
plot(allEffects(lthr))
