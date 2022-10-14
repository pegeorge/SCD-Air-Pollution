##############            CREATE DAILY MEANS OF PM2.5 VALUES ACROSS ATLANTA (5, 10, 20 miles from CHOA)

#         here - I am combining the PM2.5 1km x 1km values into one, daily mean
#         over the relevant areas (5m, 10m, and 20m from CHOA facilities) in Atlanta



library(gt)
library(dplyr)
library(lubridate)
library(ggplot2)
library(psych)
library(zoo)                 # for rollmean


rm(list=ls())

#####           Set the working directory where the original dataset is located
setwd("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/PM25 Combined and cropped dataframes/")
list.files()



#####               start with 10 miles from CHOA 



#####           Load the datasets            #######
CHOA_2010_10_m <- read.csv("combined_2010_choa_10_miles.csv")
CHOA_2011_10_m <- read.csv("combined_2011_choa_10_miles.csv")
CHOA_2012_10_m <- read.csv("combined_2012_choa_10_miles.csv")
CHOA_2013_10_m <- read.csv("combined_2013_choa_10_miles.csv")
CHOA_2014_10_m <- read.csv("combined_2014_choa_10_miles.csv")
CHOA_2015_10_m <- read.csv("combined_2015_choa_10_miles.csv")
CHOA_2016_10_m <- read.csv("combined_2016_choa_10_miles.csv")

head(CHOA_2010_10_m)


####           Combining the datasets        #######

CHOA_10_m_combined <- bind_cols(CHOA_2010_10_m, CHOA_2011_10_m, CHOA_2012_10_m, 
                                CHOA_2013_10_m, CHOA_2014_10_m, CHOA_2015_10_m, CHOA_2016_10_m)

head(CHOA_2010_10_m)
str(CHOA_2010_10_m)

CHOA_10_m_combined_tidy <- CHOA_10_m_combined %>% select(starts_with(c("Lon", "Lat", "pm25")))
head(CHOA_10_m_combined_tidy, 5)           
CHOA_10_m_combined_tidy <- CHOA_10_m_combined_tidy %>% select(-c(2:7))
CHOA_10_m_combined_tidy <- CHOA_10_m_combined_tidy %>% select(-c(3:8))

daily_mean_df_10m <- as.data.frame(colMeans(CHOA_10_m_combined_tidy))
daily_mean_df_10m <- daily_mean_df_10m[-c(1, 2),, drop = FALSE]


# add list of dates to the dataframe by this code_  seq.Date(from, to, by, length.out = NULL, along.with = NULL, ...)
dates_list <- seq.Date(as.Date("2010/1/1"), as.Date("2016/12/31"), "days")

daily_mean_df_10m$date <- dates_list
daily_mean_df_10m <- daily_mean_df_10m %>% 
  rename(daily_PM_mean = `colMeans(CHOA_10_m_combined_tidy)`)

typeof(daily_mean_df_10m$date)

Daily_average_PM_histogram_10m <- 
  ggplot(daily_mean_df_10m) + 
  aes(x = daily_PM_mean) +
  geom_histogram(binwidth = 0.5) + 
  theme_classic() +
  labs ( title = "Histogram of daily average PM2.5 values (2010 - 2016)", 
         subtitle = "10 mile radius from Egleston, Hughes Spalding, and Scottish Rite",
         caption = "This histogram shows the distribution of daily mean PM2.5 values
        median 10.3 ug/m^3 with IQR 7.6 - 12.2 and range 1.0 - 82.4") +
  xlab("Average PM2.5 Level (ug/m^3)") + ylab("Number of Days")

Daily_average_PM_histogram_10m

describe(daily_mean_df_10m$daily_PM_mean)
summary(daily_mean_df_10m$daily_PM_mean)



#now, to graph using ggplot and the date function of ggplot
daily_avg_PM25_time_graph_10m <- 
  ggplot(daily_mean_df_10m) +
  aes(x = date, y = daily_PM_mean) +
  geom_line( size = 0.1) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "6 months") + 
  theme_classic() +
  labs(title = "Daily PM2.5 Levels (2010 - 2016) Across Atlanta", 
       subtitle = "10 mile radius from Egleston, Hughes Spalding, and Scottish Rite",
       caption = "This histogram shows daily mean PM2.5 values over time during the study period") +
  xlab("Date") + ylab("Daily PM2.5 Level (ug/m^3)")

daily_avg_PM25_time_graph_10m



# creating rolling mean
daily_mean_df_10m <- daily_mean_df_10m %>% 
  mutate(PM_lag_1day = lag(daily_mean_df_10m$daily_PM_mean, n=1)) %>% 
  mutate(PM_lag_2day = lag(daily_mean_df_10m$daily_PM_mean, n=2)) %>%
  mutate(PM_lag_3day = lag(daily_mean_df_10m$daily_PM_mean, n=3)) %>%
  mutate(PM_lag_4day = lag(daily_mean_df_10m$daily_PM_mean, n=4)) %>%
  mutate(PM_rollmean_4day = rollmeanr(x = daily_PM_mean, k = 4, fill = NA)) %>% 
  mutate(PM_rollmean_3day = rollmeanr(x = daily_PM_mean, k = 3, fill = NA)) %>% 
  mutate(PM_rollmean_2day = rollmeanr(x = daily_PM_mean, k = 2, fill = NA)) %>% 
  mutate(PM_rollmean_prev2day = lag(PM_rollmean_2day)) %>% 
  mutate(PM_rollmean_prev3day = lag(PM_rollmean_3day)) %>% 
  mutate(PM_rollmean_prev4day = lag(PM_rollmean_4day)) %>% 
  mutate(PM_lead_1day = lead(daily_PM_mean, n = 1)) %>% 
  mutate(PM_lead_2day = lead(daily_PM_mean, n = 2))



  



#save dataframe
save(daily_mean_df_10m, file = "C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/PM25 Combined and cropped dataframes/daily_mean_df_10m.Rda")



#####               now with 20 miles from CHOA 



#####           Load the datasets            #######
CHOA_2010_20_m <- read.csv("combined_2010_choa_20_miles.csv")
CHOA_2011_20_m <- read.csv("combined_2011_choa_20_miles.csv")
CHOA_2012_20_m <- read.csv("combined_2012_choa_20_miles.csv")
CHOA_2013_20_m <- read.csv("combined_2013_choa_20_miles.csv")
CHOA_2014_20_m <- read.csv("combined_2014_choa_20_miles.csv")
CHOA_2015_20_m <- read.csv("combined_2015_choa_20_miles.csv")
CHOA_2016_20_m <- read.csv("combined_2016_choa_20_miles.csv")


####           Combining the datasets        #######

CHOA_20_m_combined <- bind_cols(CHOA_2010_20_m, CHOA_2011_20_m, CHOA_2012_20_m, 
                                CHOA_2013_20_m, CHOA_2014_20_m, CHOA_2015_20_m, CHOA_2016_20_m)

head(CHOA_2010_20_m)
str(CHOA_2010_20_m)

CHOA_20_m_combined_tidy <- CHOA_20_m_combined %>% select(starts_with(c("Lon", "Lat", "pm25")))
head(CHOA_20_m_combined_tidy, 5)           
CHOA_20_m_combined_tidy <- CHOA_20_m_combined_tidy %>% select(-c(2:7))
CHOA_20_m_combined_tidy <- CHOA_20_m_combined_tidy %>% select(-c(3:8))




# create means from the columns which creates daily means (and then drop the lon / lat means)
daily_mean_df_20m <- as.data.frame(colMeans(CHOA_20_m_combined_tidy))
daily_mean_df_20m <- daily_mean_df_20m[-c(1, 2),, drop = FALSE]



# add list of dates to the dataframe by this code_  seq.Date(from, to, by, length.out = NULL, along.with = NULL, ...)
dates_list <- seq.Date(as.Date("2010/1/1"), as.Date("2016/12/31"), "days")


daily_mean_df_20m$date <- dates_list
daily_mean_df_20m <- daily_mean_df_20m %>% 
  rename(daily_PM_mean = `colMeans(CHOA_20_m_combined_tidy)`)

typeof(daily_mean_df_20m$date)

Daily_average_PM_histogram_20m <- 
  ggplot(daily_mean_df_20m) + 
  aes(x = daily_PM_mean) +
  geom_histogram(binwidth = 0.5) + 
  theme_classic() +
  labs ( title = "Histogram of daily average PM2.5 values (2010 - 2016)", 
         subtitle = "20 mile radius from Egleston, Hughes Spalding, and Scottish Rite",
         caption = "This histogram shows the distribution of daily mean PM2.5 values
        median 10.1 ug/m^3 with IQR 7.4 - 13 and range 0.9 - 76.8") +
  xlab("Average PM2.5 Level (ug/m^3)") + ylab("Number of Days")

Daily_average_PM_histogram_20m

describe(daily_mean_df_20m$daily_PM_mean)
summary(daily_mean_df_20m$daily_PM_mean)


#now, to graph using ggplot and the date function of ggplot
daily_avg_PM25_time_graph_20m <- 
  ggplot(daily_mean_df_20m) +
  aes(x = date, y = daily_PM_mean) +
  geom_line( size = 0.1) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "6 months") + 
  theme_classic() + 
  labs(title = "Daily PM2.5 Levels (2010 - 2016) Across Atlanta", 
       subtitle = "20 mile radius from Egleston, Hughes Spalding, and Scottish Rite",
       caption = "This histogram shows daily mean PM2.5 values over time during the study period") +
  xlab("Date") + ylab("Daily PM2.5 Level (ug/m^3)")

daily_avg_PM25_time_graph_20m


mean(daily_mean_df_20m$PM_rollmean_3day, na.rm = TRUE)
sd(daily_mean_df_20m$PM_rollmean_3day, na.rm = TRUE)

mean_PM_rollmean = mean(daily_mean_df_20m$PM_rollmean_3day, na.rm = TRUE)
sd_up_PM_rollmean = mean(daily_mean_df_20m$PM_rollmean_3day, na.rm = TRUE) + sd(daily_mean_df_20m$PM_rollmean_3day, na.rm = TRUE)
sd_down_PM_rollmean = mean(daily_mean_df_20m$PM_rollmean_3day, na.rm = TRUE) - sd(daily_mean_df_20m$PM_rollmean_3day, na.rm = TRUE)


# creating rolling mean
daily_mean_df_20m <- daily_mean_df_20m %>% 
  mutate(PM_lag_1day = lag(daily_mean_df_20m$daily_PM_mean, n=1)) %>% 
  mutate(PM_lag_2day = lag(daily_mean_df_20m$daily_PM_mean, n=2)) %>%
  mutate(PM_lag_3day = lag(daily_mean_df_20m$daily_PM_mean, n=3)) %>%
  mutate(PM_lag_4day = lag(daily_mean_df_20m$daily_PM_mean, n=4)) %>%
  mutate(PM_rollmean_4day = zoo::rollmeanr(x = daily_PM_mean, k = 4, fill = NA)) %>% 
  mutate(PM_rollmean_3day = zoo::rollmeanr(x = daily_PM_mean, k = 3, fill = NA)) %>% 
  mutate(PM_rollmean_2day = zoo::rollmeanr(x = daily_PM_mean, k = 2, fill = NA)) %>% 
  mutate(PM_rollmean_prev2day = lag(PM_rollmean_2day)) %>% 
  mutate(PM_rollmean_prev3day = lag(PM_rollmean_3day)) %>% 
  mutate(PM_rollmean_prev4day = lag(PM_rollmean_4day)) %>% 
  mutate(PM_lead_1day = lead(daily_PM_mean, n = 1)) %>% 
  mutate(PM_lead_2day = lead(daily_PM_mean, n = 2))


#now, to graph using ggplot and the date function of ggplot
rollmean_3d_PM25_time_graph_20m <- 
  ggplot(daily_mean_df_20m) +
  aes(x = date, y = PM_rollmean_3day) +
  geom_line( size = 0.1) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "6 months") + 
  theme_classic() + 
  labs(title = "Daily PM2.5 Levels (2010 - 2016) Across Atlanta", 
       subtitle = "20 mile radius from Egleston, Hughes Spalding, and Scottish Rite",
       caption = "This histogram shows 3 day rolling mean PM2.5 values during the study period, 
Solid line is mean value, dashed lines are plus and minus one standard deviation") +
  xlab("Date") + ylab("Rolling Mean, 24 hour PM2.5 Level (ug/m^3)") + 
  geom_hline(yintercept = mean_PM_rollmean) + 
  geom_hline(yintercept = sd_up_PM_rollmean, linetype = 'dashed' ) + 
  geom_hline(yintercept = sd_down_PM_rollmean, linetype = 'dashed') + 
  theme(text = element_text(size = 18), 
        axis.text.x = element_text(angle = 45, size = 12, vjust = 0.6), 
        plot.caption = element_text(hjust = 0, size = 13))

rollmean_3d_PM25_time_graph_20m

#save dataframe
save(daily_mean_df_20m, file = "C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/PM25 Combined and cropped dataframes/daily_mean_df_20m.Rda")




#####               now with 5 miles from CHOA 



#####           Load the datasets            #######
CHOA_2010_5_m <- read.csv("combined_2010_choa_5_miles.csv")
CHOA_2011_5_m <- read.csv("combined_2011_choa_5_miles.csv")
CHOA_2012_5_m <- read.csv("combined_2012_choa_5_miles.csv")
CHOA_2013_5_m <- read.csv("combined_2013_choa_5_miles.csv")
CHOA_2014_5_m <- read.csv("combined_2014_choa_5_miles.csv")
CHOA_2015_5_m <- read.csv("combined_2015_choa_5_miles.csv")
CHOA_2016_5_m <- read.csv("combined_2016_choa_5_miles.csv")


####           Combining the datasets        #######

CHOA_5_m_combined <- bind_cols(CHOA_2010_5_m, CHOA_2011_5_m, CHOA_2012_5_m, 
                                CHOA_2013_5_m, CHOA_2014_5_m, CHOA_2015_5_m, CHOA_2016_5_m)


CHOA_5_m_combined_tidy <- CHOA_5_m_combined %>% select(starts_with(c("Lon", "Lat", "pm25")))
head(CHOA_5_m_combined_tidy, 5)           
CHOA_5_m_combined_tidy <- CHOA_5_m_combined_tidy %>% select(-c(2:7))
CHOA_5_m_combined_tidy <- CHOA_5_m_combined_tidy %>% select(-c(3:8))



# create means from the columns which creates daily means (and then drop the lon / lat means)
daily_mean_df_5m <- as.data.frame(colMeans(CHOA_5_m_combined_tidy))
daily_mean_df_5m <- daily_mean_df_5m[-c(1, 2),, drop = FALSE]



# add list of dates to the dataframe by this code_  seq.Date(from, to, by, length.out = NULL, along.with = NULL, ...)
dates_list <- seq.Date(as.Date("2010/1/1"), as.Date("2016/12/31"), "days")


daily_mean_df_5m$date <- dates_list
daily_mean_df_5m <- daily_mean_df_5m %>% 
  rename(daily_PM_mean = `colMeans(CHOA_5_m_combined_tidy)`)


Daily_average_PM_histogram_5m <- 
  ggplot(daily_mean_df_5m) + 
  aes(x = daily_PM_mean) +
  geom_histogram(binwidth = 0.5) + 
  theme_classic() +
  labs ( title = "Histogram of daily average PM2.5 values (2010 - 2016)", 
         subtitle = "5 mile radius from Egleston, Hughes Spalding, and Scottish Rite",
         caption = "This histogram shows the distribution of daily mean PM2.5 values
        median 10.4 ug/m^3 with IQR 7.8 - 13.4 and range 1.2 - 93") +
  xlab("Average PM2.5 Level (ug/m^3)") + ylab("Number of Days")

Daily_average_PM_histogram_5m

describe(daily_mean_df_5m$daily_PM_mean)
summary(daily_mean_df_5m$daily_PM_mean)


#now, to graph using ggplot and the date function of ggplot
daily_avg_PM25_time_graph_5m <- 
  ggplot(daily_mean_df_5m) +
  aes(x = date, y = daily_PM_mean) +
  geom_line( size = 0.1) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "6 months") + 
  theme_classic() +
  labs(title = "Daily PM2.5 Levels (2010 - 2016) Across Atlanta", 
       subtitle = "5 mile radius from Egleston, Hughes Spalding, and Scottish Rite",
       caption = "This histogram shows daily mean PM2.5 values over time during the study period") +
  xlab("Date") + ylab("Daily PM2.5 Level (ug/m^3)")

daily_avg_PM25_time_graph_5m

# creating rolling mean
daily_mean_df_5m <- daily_mean_df_5m %>% 
  mutate(PM_lag_1day = lag(daily_PM_mean, n=1)) %>% 
  mutate(PM_lag_2day = lag(daily_PM_mean, n=2)) %>%
  mutate(PM_lag_3day = lag(daily_PM_mean, n=3)) %>%
  mutate(PM_lag_4day = lag(daily_PM_mean, n=4)) %>%
  mutate(PM_rollmean_4day = rollmeanr(x = daily_PM_mean, k = 4, fill = NA)) %>% 
  mutate(PM_rollmean_3day = rollmeanr(x = daily_PM_mean, k = 3, fill = NA)) %>% 
  mutate(PM_rollmean_2day = rollmeanr(x = daily_PM_mean, k = 2, fill = NA)) %>% 
  mutate(PM_rollmean_prev2day = lag(PM_rollmean_2day)) %>% 
  mutate(PM_rollmean_prev3day = lag(PM_rollmean_3day)) %>% 
  mutate(PM_rollmean_prev4day = lag(PM_rollmean_4day)) %>% 
  mutate(PM_lead_1day = lead(daily_PM_mean, n = 1)) %>% 
  mutate(PM_lead_2day = lead(daily_PM_mean, n = 2))

#save dataframe
save(daily_mean_df_5m, file = "C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/PM25 Combined and cropped dataframes/daily_mean_df_5m.Rda")

