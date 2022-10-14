##############            WEATHER AND DAY OF WEEK (Is business day or not?) DATA

#         here - I am making a new dataset to be used, which is taken from 
#         Atlanta-Hartfield monitoring station
#         I will trim the database to only include variables I need

library(ggplot2)
library(lubridate)
library(dplyr)
library(bizdays)
library(RQuantLib)
library(zoo)

rm(list=ls())

#####           Set the working directory where the original dataset is located
setwd("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/")

#MAC
#setwd('~/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/')
list.files()

atl_weather_raw <- read.csv("Raw databases/Atlanta_temp_wind_rain.csv")

typeof(atl_weather_raw$DATE)


#again, use lubridate to change the date column to data format
atl_weather_raw <- atl_weather_raw %>% 
  mutate(DATE = mdy(DATE))

typeof(atl_weather_raw$DATE)

#save only the columns I'm interested in 
atl_weather <- select(atl_weather_raw, c(DATE, PRCP, SNOW, TMAX, TMIN))

atl_weather <- atl_weather %>% 
  mutate(PRCP_dichot = ifelse(PRCP > 0.3, 1, 0))

atl_weather <- atl_weather %>% 
  filter(DATE >= "2010-01-01" & DATE < "2019-01-01")

ls(atl_weather)

atl_weather = atl_weather %>% 
  mutate(TMIN_rollmean_3day = rollmeanr(x = TMIN, k = 3, fill = NA), 
         TMIN_rollmean_2day = rollmeanr(x = TMIN, k = 2, fill = NA))


#Now, to add business day indicator to calendar, using packages bizdays and RQuantLib https://cran.r-project.org/web/packages/bizdays/bizdays.pdf 
# note, this is TRUE if business day, FALSE if not business day 

load_builtin_calendars()
load_quantlib_calendars("UnitedStates", from = "2010-01-01", to = "2019-01-01")
calendars()

is.bizday(atl_weather$DATE[4], "QuantLib/UnitedStates")

atl_weather_day <- atl_weather %>% 
  mutate(BusinessDay = is.bizday(DATE, "QuantLib/UnitedStates"))


####          adding time variables to the dataframe

#first, start with numbering the rows
atl_weather_day <- atl_weather_day %>% 
  mutate(day_number = seq.int(nrow(atl_weather_day)))

#next, add seasons
atl_weather_day <- atl_weather_day %>%
  mutate(
    season = case_when(
      month(DATE) %in%  9:11 ~ "Fall",
      month(DATE) %in%  c(12, 1, 2)  ~ "Winter",
      month(DATE) %in%  3:5  ~ "Spring",
      TRUE ~ "Summer"))







#save the dataset 
save(atl_weather_day, file = "C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/atl_weather_day.Rda")
