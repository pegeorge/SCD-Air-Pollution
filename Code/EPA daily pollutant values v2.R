#########          OTHER POLLUTANTS DATABASE            ###############
#
#    This code creates a database with daily pollutant data, 
#    using downloads from the EPA website 
#    https://www.epa.gov/outdoor-air-quality-data/download-daily-data 
#
#


library(psych)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggfortify)
library(zoo)                 # for rollmean


rm(list=ls())

#####           Set the working directory where the original dataset is located
setwd("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/")
list.files()


####    Carbon Monoxide - CO  #######

CO_2010_002 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2010_002.csv")
CO_2011_002 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2011_002.csv")
CO_2012_002 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2012_002.csv")
CO_2013_002 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2013_002.csv")
CO_2014_002 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2014_002.csv")
CO_2015_002 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2015_002.csv")
CO_2016_002 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2016_002.csv")
CO_2017_002 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2017_002.csv")
CO_2018_002 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2018_002.csv")
CO_2014_056 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2014_056.csv")
CO_2015_056 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2015_056.csv")   
CO_2016_056 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2016_056.csv")
CO_2017_056 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2017_056.csv")   
CO_2018_056 <- read.csv("Raw databases/EPA Air Pollution Databases/CO_2018_056.csv")  

typeof(CO_2010$Date)

#again, use lubridate to change the date column to data format
CO_2010_002 <- CO_2010_002 %>% mutate(Date = mdy(Date))
CO_2011_002 <- CO_2011_002 %>% mutate(Date = mdy(Date))
CO_2012_002 <- CO_2012_002 %>% mutate(Date = mdy(Date))
CO_2013_002 <- CO_2013_002 %>% mutate(Date = mdy(Date))
CO_2014_002 <- CO_2014_002 %>% mutate(Date = mdy(Date))
CO_2015_002 <- CO_2015_002 %>% mutate(Date = mdy(Date))
CO_2016_002 <- CO_2016_002 %>% mutate(Date = mdy(Date))
CO_2017_002 <- CO_2017_002 %>% mutate(Date = mdy(Date))
CO_2018_002 <- CO_2018_002 %>% mutate(Date = mdy(Date))
CO_2014_056 <- CO_2014_056 %>% mutate(Date = mdy(Date))
CO_2015_056 <- CO_2015_056 %>% mutate(Date = mdy(Date))
CO_2016_056 <- CO_2016_056 %>% mutate(Date = mdy(Date))
CO_2017_056 <- CO_2017_056 %>% mutate(Date = mdy(Date))
CO_2018_056 <- CO_2018_056 %>% mutate(Date = mdy(Date))

CO_EPA_2010_2018 <- bind_rows(CO_2010_002, CO_2011_002, CO_2012_002, CO_2013_002, 
                              CO_2014_002, CO_2015_002, CO_2016_002, CO_2017_002, CO_2018_002, 
                              CO_2014_056, CO_2015_056, CO_2016_056, CO_2017_056, CO_2018_056)


#the code below will add missing dates (With NA)
ts <- seq.POSIXt(as.POSIXct("2010-01-01", '%y-%m-%d'),as.POSIXct("2018-12-31", '%y-%m-%d'), by = "day")
df <- data.frame(Date = ts)

CO_full <- full_join(df, CO_EPA_2010_2018)

CO_full_agr <-aggregate(Daily.Max.8.hour.CO.Concentration ~ Date, data = CO_full, mean, na.action = na.pass) 



####    Nitrogen Dioxide - NO2  #######


NO2_2010_002 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2010_002.csv")
NO2_2011_002 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2011_002.csv")
NO2_2012_002 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2012_002.csv")
NO2_2013_002 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2013_002.csv")
NO2_2014_002 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2014_002.csv")
NO2_2015_002 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2015_002.csv")
NO2_2016_002 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2016_002.csv")
NO2_2017_002 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2017_002.csv")
NO2_2018_002 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2018_002.csv")
NO2_2014_056 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2014_056.csv")
NO2_2015_056 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2015_056.csv")   
NO2_2016_056 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2016_056.csv")
NO2_2017_056 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2017_056.csv")   
NO2_2018_056 <- read.csv("Raw databases/EPA Air Pollution Databases/NO2_2018_056.csv")  


#again, use lubridate to change the date column to data format
NO2_2010_002 <- NO2_2010_002 %>% mutate(Date = mdy(Date))
NO2_2011_002 <- NO2_2011_002 %>% mutate(Date = mdy(Date))
NO2_2012_002 <- NO2_2012_002 %>% mutate(Date = mdy(Date))
NO2_2013_002 <- NO2_2013_002 %>% mutate(Date = mdy(Date))
NO2_2014_002 <- NO2_2014_002 %>% mutate(Date = mdy(Date))
NO2_2015_002 <- NO2_2015_002 %>% mutate(Date = mdy(Date))
NO2_2016_002 <- NO2_2016_002 %>% mutate(Date = mdy(Date))
NO2_2017_002 <- NO2_2017_002 %>% mutate(Date = mdy(Date))
NO2_2018_002 <- NO2_2018_002 %>% mutate(Date = mdy(Date))
NO2_2014_056 <- NO2_2014_056 %>% mutate(Date = mdy(Date))
NO2_2015_056 <- NO2_2015_056 %>% mutate(Date = mdy(Date))
NO2_2016_056 <- NO2_2016_056 %>% mutate(Date = mdy(Date))
NO2_2017_056 <- NO2_2017_056 %>% mutate(Date = mdy(Date))
NO2_2018_056 <- NO2_2018_056 %>% mutate(Date = mdy(Date))

NO2_EPA_2010_2018 <- bind_rows(NO2_2010_002, NO2_2011_002, NO2_2012_002, NO2_2013_002, 
                               NO2_2014_002, NO2_2015_002, NO2_2016_002, NO2_2017_002, NO2_2018_002, 
                               NO2_2014_056, NO2_2015_056, NO2_2016_056, NO2_2017_056, NO2_2018_056)


#the code below will add missing dates (With NA)
ts <- seq.POSIXt(as.POSIXct("2010-01-01", '%y-%m-%d'),as.POSIXct("2018-12-31", '%y-%m-%d'), by = "day")
df <- data.frame(Date = ts)

NO2_full <- full_join(df, NO2_EPA_2010_2018)

NO2_full_agr <-aggregate(Daily.Max.1.hour.NO2.Concentration ~ Date, data = NO2_full, mean, na.action = na.pass) 





########       Ozone - Ozone       ##############

Ozone_2010_002 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2010_002.csv")
Ozone_2011_002 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2011_002.csv")
Ozone_2012_002 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2012_002.csv")
Ozone_2013_002 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2013_002.csv")
Ozone_2014_002 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2014_002.csv")
Ozone_2015_002 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2015_002.csv")
Ozone_2016_002 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2016_002.csv")
Ozone_2017_002 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2017_002.csv")
Ozone_2018_002 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2018_002.csv")
Ozone_2010_055 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2010_055.csv")
Ozone_2011_055 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2011_055.csv")
Ozone_2012_055 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2012_055.csv")
Ozone_2013_055 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2013_055.csv")
Ozone_2014_055 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2014_055.csv")
Ozone_2015_055 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2015_055.csv")
Ozone_2016_055 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2016_055.csv")
Ozone_2017_055 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2017_055.csv")
Ozone_2018_055 <- read.csv("Raw databases/EPA Air Pollution Databases/Ozone_2018_055.csv") 


#again, use lubridate to change the date column to data format
Ozone_2010_002 <- Ozone_2010_002 %>% mutate(Date = mdy(Date))
Ozone_2011_002 <- Ozone_2011_002 %>% mutate(Date = mdy(Date))
Ozone_2012_002 <- Ozone_2012_002 %>% mutate(Date = mdy(Date))
Ozone_2013_002 <- Ozone_2013_002 %>% mutate(Date = mdy(Date))
Ozone_2014_002 <- Ozone_2014_002 %>% mutate(Date = mdy(Date))
Ozone_2015_002 <- Ozone_2015_002 %>% mutate(Date = mdy(Date))
Ozone_2016_002 <- Ozone_2016_002 %>% mutate(Date = mdy(Date))
Ozone_2017_002 <- Ozone_2017_002 %>% mutate(Date = mdy(Date))
Ozone_2018_002 <- Ozone_2018_002 %>% mutate(Date = mdy(Date))
Ozone_2010_055 <- Ozone_2010_055 %>% mutate(Date = mdy(Date))
Ozone_2011_055 <- Ozone_2011_055 %>% mutate(Date = mdy(Date))
Ozone_2012_055 <- Ozone_2012_055 %>% mutate(Date = mdy(Date))
Ozone_2013_055 <- Ozone_2013_055 %>% mutate(Date = mdy(Date))
Ozone_2014_055 <- Ozone_2014_055 %>% mutate(Date = mdy(Date))
Ozone_2015_055 <- Ozone_2015_055 %>% mutate(Date = mdy(Date))
Ozone_2016_055 <- Ozone_2016_055 %>% mutate(Date = mdy(Date))
Ozone_2017_055 <- Ozone_2017_055 %>% mutate(Date = mdy(Date))
Ozone_2018_055 <- Ozone_2018_055 %>% mutate(Date = mdy(Date))

Ozone_EPA_2010_2018 <- bind_rows(Ozone_2010_002, Ozone_2011_002, Ozone_2012_002, Ozone_2013_002, 
                                 Ozone_2014_002, Ozone_2015_002, Ozone_2016_002, Ozone_2017_002, Ozone_2018_002, 
                                 Ozone_2010_055, Ozone_2011_055, Ozone_2012_055, Ozone_2013_055,
                                 Ozone_2014_055, Ozone_2015_055, Ozone_2016_055, Ozone_2017_055, Ozone_2018_055)


#the code below will add missing dates (With NA)
ts <- seq.POSIXt(as.POSIXct("2010-01-01", '%y-%m-%d'),as.POSIXct("2018-12-31", '%y-%m-%d'), by = "day")
df <- data.frame(Date = ts)

Ozone_full <- full_join(df, Ozone_EPA_2010_2018)

Ozone_full_agr <-aggregate(Daily.Max.8.hour.Ozone.Concentration ~ Date, data = Ozone_full, mean, na.action = na.pass) 




########       PM2.5       ##############

PM25_2010_002 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2010_002.csv")
PM25_2011_002 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2011_002.csv")
PM25_2012_002 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2012_002.csv")
PM25_2013_002 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2013_002.csv")
PM25_2014_002 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2014_002.csv")
PM25_2015_002 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2015_002.csv")
PM25_2016_002 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2016_002.csv")
PM25_2017_002 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2017_002.csv")
PM25_2018_002 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2018_002.csv")
PM25_2010_039 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2010_039.csv")
PM25_2011_039 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2011_039.csv")
PM25_2012_039 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2012_039.csv")
PM25_2013_039 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2013_039.csv")
PM25_2014_039 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2014_039.csv")
PM25_2015_039 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2015_039.csv")
PM25_2016_039 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2016_039.csv")
PM25_2017_039 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2017_039.csv")
PM25_2018_039 <- read.csv("Raw databases/EPA Air Pollution Databases/PM25_2018_039.csv") 


#again, use lubridate to change the date column to data format
PM25_2010_002 <- PM25_2010_002 %>% mutate(Date = mdy(Date))
PM25_2011_002 <- PM25_2011_002 %>% mutate(Date = mdy(Date))
PM25_2012_002 <- PM25_2012_002 %>% mutate(Date = mdy(Date))
PM25_2013_002 <- PM25_2013_002 %>% mutate(Date = mdy(Date))
PM25_2014_002 <- PM25_2014_002 %>% mutate(Date = mdy(Date))
PM25_2015_002 <- PM25_2015_002 %>% mutate(Date = mdy(Date))
PM25_2016_002 <- PM25_2016_002 %>% mutate(Date = mdy(Date))
PM25_2017_002 <- PM25_2017_002 %>% mutate(Date = mdy(Date))
PM25_2018_002 <- PM25_2018_002 %>% mutate(Date = mdy(Date))
PM25_2010_039 <- PM25_2010_039 %>% mutate(Date = mdy(Date))
PM25_2011_039 <- PM25_2011_039 %>% mutate(Date = mdy(Date))
PM25_2012_039 <- PM25_2012_039 %>% mutate(Date = mdy(Date))
PM25_2013_039 <- PM25_2013_039 %>% mutate(Date = mdy(Date))
PM25_2014_039 <- PM25_2014_039 %>% mutate(Date = mdy(Date))
PM25_2015_039 <- PM25_2015_039 %>% mutate(Date = mdy(Date))
PM25_2016_039 <- PM25_2016_039 %>% mutate(Date = mdy(Date))
PM25_2017_039 <- PM25_2017_039 %>% mutate(Date = mdy(Date))
PM25_2018_039 <- PM25_2018_039 %>% mutate(Date = mdy(Date))

PM25_EPA_2010_2018 <- bind_rows(PM25_2010_002, PM25_2011_002, PM25_2012_002, PM25_2013_002, 
                                PM25_2014_002, PM25_2015_002, PM25_2016_002, PM25_2017_002, PM25_2018_002, 
                                PM25_2010_039, PM25_2011_039, PM25_2012_039, PM25_2013_039,
                                PM25_2014_039, PM25_2015_039, PM25_2016_039, PM25_2017_039, PM25_2018_039)


#the code below will add missing dates (With NA)
ts <- seq.POSIXt(as.POSIXct("2010-01-01", '%y-%m-%d'),as.POSIXct("2018-12-31", '%y-%m-%d'), by = "day")
df <- data.frame(Date = ts)

PM25_EPA_full <- full_join(df, PM25_EPA_2010_2018)

PM25_EPA_full_agr <-aggregate(Daily.Mean.PM2.5.Concentration ~ Date, data = PM25_EPA_full, mean, na.action = na.pass) 



EPA_pollut_agr <- left_join(PM25_EPA_full_agr, Ozone_full_agr, by='Date')
EPA_pollut_agr <- left_join(EPA_pollut_agr, CO_full_agr, by='Date')
EPA_pollut_agr <- left_join(EPA_pollut_agr, NO2_full_agr, by='Date')


EPA_pollut_agr <- EPA_pollut_agr %>% 
  rename(PM25 = Daily.Mean.PM2.5.Concentration) %>% 
  rename(Ozone = Daily.Max.8.hour.Ozone.Concentration) %>% 
  rename(NO2 = Daily.Max.1.hour.NO2.Concentration) %>% 
  rename(CO = Daily.Max.8.hour.CO.Concentration)



# creating lag and rolling mean values
EPA_pollut_agr <- EPA_pollut_agr %>% 
  mutate(PM25_lag_1day = lag(PM25, n=1)) %>% 
  mutate(PM25_lag_2day = lag(PM25, n=2)) %>% 
  mutate(PM25_lag_3day = lag(PM25, n=3)) %>%
  mutate(PM25_lag_4day = lag(PM25, n=4)) %>% 
  mutate(PM25_lead_1day = lead(PM25, n=1)) %>%
  mutate(PM25_lead_2day = lead(PM25, n=2)) %>%  
  mutate(NO2_lag_1day = lag(NO2, n=1)) %>% 
  mutate(NO2_lag_2day = lag(NO2, n=2)) %>% 
  mutate(NO2_lag_3day = lag(NO2, n=3)) %>%
  mutate(NO2_lag_4day = lag(NO2, n=4)) %>%
  mutate(Ozone_lag_1day = lag(Ozone, n=1)) %>% 
  mutate(Ozone_lag_2day = lag(Ozone, n=2)) %>% 
  mutate(Ozone_lag_3day = lag(Ozone, n=3)) %>%
  mutate(Ozone_lag_4day = lag(Ozone, n=4)) %>%
  mutate(CO_lag_1day = lag(CO, n=1)) %>% 
  mutate(CO_lag_2day = lag(CO, n=2)) %>% 
  mutate(CO_lag_3day = lag(CO, n=3)) %>% 
  mutate(CO_lag_4day = lag(CO, n=4)) %>% 
  mutate(CO_lead_1day = lead(CO, n = 1)) %>% 
  mutate(CO_lead_2day = lead(CO, n = 2))

EPA_pollut_agr <- EPA_pollut_agr %>% 
  mutate(PM25_rollmean_3day = rollmeanr(x = PM25, k = 3, fill = NA)) %>% 
  mutate(PM25_rollmean_2day = rollmeanr(x = PM25, k = 2, fill = NA)) %>%
  mutate(PM25_rollmean_prev2day = lag(PM25_rollmean_2day)) %>% 
  mutate(PM25_rollmean_prev3day = lag(PM25_rollmean_3day)) %>% 
  mutate(NO2_rollmean_3day = rollmeanr(x = NO2, k = 3, fill = NA)) %>% 
  mutate(NO2_rollmean_2day = rollmeanr(x = NO2, k = 2, fill = NA)) %>%
  mutate(NO2_rollmean_prev2day = lag(NO2_rollmean_2day)) %>% 
  mutate(NO2_rollmean_prev3day = lag(NO2_rollmean_3day)) %>%
  mutate(Ozone_rollmean_3day = rollmeanr(x = Ozone, k = 3, fill = NA)) %>% 
  mutate(Ozone_rollmean_2day = rollmeanr(x = Ozone, k = 2, fill = NA)) %>%
  mutate(Ozone_rollmean_prev2day = lag(Ozone_rollmean_2day)) %>% 
  mutate(Ozone_rollmean_prev3day = lag(Ozone_rollmean_3day)) %>% 
  mutate(CO_rollmean_3day = rollmeanr(x = CO, k = 3, fill = NA)) %>% 
  mutate(CO_rollmean_2day = rollmeanr(x = CO, k = 2, fill = NA)) %>%
  mutate(CO_rollmean_prev2day = lag(CO_rollmean_2day)) %>% 
  mutate(CO_rollmean_prev3day = lag(CO_rollmean_3day)) 


#Save dataframe 
save(EPA_pollut_agr, file = "C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/EPA_pollut_agr.Rda")


