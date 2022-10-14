#########                   LAG DAY ANALYSIS            ###############

# Here, I look at which lag (day prior to) and lead (day after) pollutant levels affect SCD visits
#    note, day 1 lead might be associated with ED visits due to correlation between day 1 and day 0, 
#    but day 2 lead really shouldn't have much effect.... 


library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggfortify)
library(zoo)                 # for rollmean
library(splines)
library(lubridate)
library(MASS)                # to run negative binomial regression 
library(AER)                 # to check for overdispersion 
library(jtools)              # standardized coeff  https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
library(broom)               # for creating dataframe out of coefficients

rm(list=ls())


#####           Set the working directory where the original dataset is located
setwd("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/")

#MAC
#setwd('~/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/')
list.files()



########################                       20 miles                          ###########################


####            first, load the main outcome variable, which is total number of ED visits per day
####            note, this db comes from raw to final dataframe, and excludes those who need to be excluded
load("Saved R Dataframes/ED_DF_Finalized/SCD_ER_count_20m_df_2018.rda")
SCD_ER_count_20m_df_date <- SCD_ER_count_20m_df_date %>% rename(date = Date)
SCD_ER_count_20m_df_date <- SCD_ER_count_20m_df_date %>% rename(ED_visits_count = Frequency)


####           here, load the PM2.5 (5, 10, or 20 m from CHOA) daily averages 
####           note, this db comes from 'Daily Mean PM Values 5 10 20...' 
load("PM25 Combined and cropped dataframes/daily_mean_df_20m.Rda")

#              (only need to load weather and EPA agr once, as they are the same for all df)
####           here, load the weather data and the day of week (is business day or not) data
####           note, this db comes from 'weather data
load("Saved R Dataframes/atl_weather_day.Rda")
atl_weather_day <- atl_weather_day %>% rename(date = DATE)

#              (only need to load weather and EPA agr once, as they are the same for all df)
#####         here, load the EPA pollutant data     ######
#####         note, this comes from EPA daily pollutant values, and I use the same df for 5m, 10m, and 20m analyses
#####         note, this data is only from several monitoring stations across Atlanta averaged out 
#####               by taking from all the Atlanta-area monitoring stations per pollutant 
load("Saved R Dataframes/EPA_pollut_agr.Rda")
EPA_pollut_agr <- EPA_pollut_agr %>% rename(date = Date)

ls(EPA_pollut_agr)

####          create new dataframe from which to run the model

SCD_20m_daily_ED_df <- left_join(SCD_ER_count_20m_df_date, daily_mean_df_20m, by = 'date')
SCD_20m_daily_ED_df <- left_join(SCD_20m_daily_ED_df, EPA_pollut_agr, by = 'date')
SCD_20m_daily_ED_df <- left_join(SCD_20m_daily_ED_df, atl_weather_day, by = 'date')





knots_season = c(60, 244, 425, 609, 791, 975, 1156, 1340, 
                 1521, 1705, 1886, 2070, 2252, 2436, 2617, 2801, 2982, 3166)

# knots_year = c(365, 730, 1095, 1461, 1826, 2191)

ls(SCD_20m_daily_ED_df)


summary(SCD_20m_daily_ED_df$TMAX)
summary(SCD_20m_daily_ED_df$TMIN_rollmean_3day)

ls(atl_weather_day)



#below, I scale the coefficients (I use my own, multiplying by 2*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM_rollmean_3day_scale = (PM_rollmean_3day - mean(PM_rollmean_3day, na.rm = TRUE))/(sd(PM_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM_rollmean_3day_scale2 = (PM_rollmean_3day - mean(PM_rollmean_3day, na.rm = TRUE))/(2*sd(PM_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(CO_rollmean_3day_scale2 = (CO_rollmean_3day - mean(CO_rollmean_3day, na.rm = TRUE))/(2*sd(CO_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(NO2_rollmean_3day_scale2 = (NO2_rollmean_3day - mean(NO2_rollmean_3day, na.rm = TRUE))/(2*sd(NO2_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(Ozone_rollmean_3day_scale2 = (Ozone_rollmean_3day - mean(Ozone_rollmean_3day, na.rm = TRUE))/(2*sd(Ozone_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_20m_daily_ED_df$CO_rollmean_3day_scale2, na.rm = TRUE)
sd(SCD_20m_daily_ED_df$NO2_rollmean_3day_scale2, na.rm = TRUE)
mean(SCD_20m_daily_ED_df$Ozone_rollmean_3day_scale2, na.rm = TRUE)



m20_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                         PM_rollmean_3day_scale2 + 
                         BusinessDay + bs(day_number, knots = knots_season) + 
                         PRCP_dichot   + 
                         bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20_scale_PM.NB)


m20_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_CO.NB)

m20_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_rollmean_3day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot   + 
                             bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_NO2.NB)

m20_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_rollmean_3day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot   + 
                               bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_Ozone.NB)



### Dataframe with results


NB_20m_model_results_rollmean_3day.df <- tidy(m20_scale_PM.NB)[2, ]
NB_20m_model_results_rollmean_3day.df <- rbind(NB_20m_model_results_rollmean_3day.df, tidy(m20_scale_CO.NB)[2,])
NB_20m_model_results_rollmean_3day.df <- rbind(NB_20m_model_results_rollmean_3day.df, tidy(m20_scale_NO2.NB)[2,])
NB_20m_model_results_rollmean_3day.df <- rbind(NB_20m_model_results_rollmean_3day.df, tidy(m20_scale_Ozone.NB)[2,])

NB_20m_model_results_rollmean_3day.df$timeframe = '3 day rolling mean'









#lead_1day   ####################################



#below, I scale the coefficients (I use my own, multiplying by 2*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM_lead_1day_scale2 = (PM_lead_1day - mean(PM_lead_1day, na.rm = TRUE))/(2*sd(PM_lead_1day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(CO_lead_1day_scale2 = (CO_lead_1day - mean(CO_lead_1day, na.rm = TRUE))/(2*sd(CO_lead_1day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(NO2_lead_1day_scale2 = (NO2_lead_1day - mean(NO2_lead_1day, na.rm = TRUE))/(2*sd(NO2_lead_1day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(Ozone_lead_1day_scale2 = (Ozone_lead_1day - mean(Ozone_lead_1day, na.rm = TRUE))/(2*sd(Ozone_lead_1day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_20m_daily_ED_df$CO_lead_1day, na.rm = TRUE)
sd(SCD_20m_daily_ED_df$NO2_lead_1day, na.rm = TRUE)
mean(SCD_20m_daily_ED_df$Ozone_lead_1day, na.rm = TRUE)



m20_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                            PM_lead_1day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20_scale_PM.NB)


m20_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_lead_1day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_CO.NB)

m20_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_lead_1day + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot   + 
                             bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_NO2.NB)

m20_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_lead_1day + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot   + 
                               bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_Ozone.NB)



### Dataframe with results


NB_20m_model_results_lead_1day.df <- tidy(m20_scale_PM.NB)[2, ]
NB_20m_model_results_lead_1day.df <- rbind(NB_20m_model_results_lead_1day.df, tidy(m20_scale_CO.NB)[2,])
NB_20m_model_results_lead_1day.df <- rbind(NB_20m_model_results_lead_1day.df, tidy(m20_scale_NO2.NB)[2,])
NB_20m_model_results_lead_1day.df <- rbind(NB_20m_model_results_lead_1day.df, tidy(m20_scale_Ozone.NB)[2,])

NB_20m_model_results_lead_1day.df$timeframe = '1 day lead'











#lead_2day   ####################################



#below, I scale the coefficients (I use my own, multiplying by 2*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM_lead_2day_scale2 = (PM_lead_2day - mean(PM_lead_2day, na.rm = TRUE))/(2*sd(PM_lead_2day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(CO_lead_2day_scale2 = (CO_lead_2day - mean(CO_lead_2day, na.rm = TRUE))/(2*sd(CO_lead_2day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(NO2_lead_2day_scale2 = (NO2_lead_2day - mean(NO2_lead_2day, na.rm = TRUE))/(2*sd(NO2_lead_2day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(Ozone_lead_2day_scale2 = (Ozone_lead_2day - mean(Ozone_lead_2day, na.rm = TRUE))/(2*sd(Ozone_lead_2day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_20m_daily_ED_df$CO_lead_2day, na.rm = TRUE)
sd(SCD_20m_daily_ED_df$NO2_lead_2day, na.rm = TRUE)
mean(SCD_20m_daily_ED_df$Ozone_lead_2day, na.rm = TRUE)



m20_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                            PM_lead_2day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20_scale_PM.NB)


m20_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_lead_2day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_CO.NB)

m20_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_lead_2day + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot   + 
                             bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_NO2.NB)

m20_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_lead_2day + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot   + 
                               bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_Ozone.NB)



### Dataframe with results


NB_20m_model_results_lead_2day.df <- tidy(m20_scale_PM.NB)[2, ]
NB_20m_model_results_lead_2day.df <- rbind(NB_20m_model_results_lead_2day.df, tidy(m20_scale_CO.NB)[2,])
NB_20m_model_results_lead_2day.df <- rbind(NB_20m_model_results_lead_2day.df, tidy(m20_scale_NO2.NB)[2,])
NB_20m_model_results_lead_2day.df <- rbind(NB_20m_model_results_lead_2day.df, tidy(m20_scale_Ozone.NB)[2,])

NB_20m_model_results_lead_2day.df$timeframe = '2 day lead'


NB_20m_model_results_lead_2day.df













#lag_4day   ##########################################################



#below, I scale the coefficients (I use my own, multiplying by 2*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM_lag_4day_scale = (PM_lag_4day - mean(PM_lag_4day, na.rm = TRUE))/(sd(PM_lag_4day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM_lag_4day_scale2 = (PM_lag_4day - mean(PM_lag_4day, na.rm = TRUE))/(2*sd(PM_lag_4day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(CO_lag_4day_scale2 = (CO_lag_4day - mean(CO_lag_4day, na.rm = TRUE))/(2*sd(CO_lag_4day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(NO2_lag_4day_scale2 = (NO2_lag_4day - mean(NO2_lag_4day, na.rm = TRUE))/(2*sd(NO2_lag_4day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(Ozone_lag_4day_scale2 = (Ozone_lag_4day - mean(Ozone_lag_4day, na.rm = TRUE))/(2*sd(Ozone_lag_4day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_20m_daily_ED_df$CO_lag_4day_scale2, na.rm = TRUE)
sd(SCD_20m_daily_ED_df$NO2_lag_4day_scale2, na.rm = TRUE)
mean(SCD_20m_daily_ED_df$Ozone_lag_4day_scale2, na.rm = TRUE)

ls(SCD_20m_daily_ED_df)

m20_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                            PM_lag_4day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20_scale_PM.NB)


m20_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_lag_4day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_CO.NB)

m20_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_lag_4day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot   + 
                             bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_NO2.NB)

m20_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_lag_4day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot   + 
                               bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_Ozone.NB)



### Dataframe with results


NB_20m_model_results_lag_4day.df <- tidy(m20_scale_PM.NB)[2, ]
NB_20m_model_results_lag_4day.df <- rbind(NB_20m_model_results_lag_4day.df, tidy(m20_scale_CO.NB)[2,])
NB_20m_model_results_lag_4day.df <- rbind(NB_20m_model_results_lag_4day.df, tidy(m20_scale_NO2.NB)[2,])
NB_20m_model_results_lag_4day.df <- rbind(NB_20m_model_results_lag_4day.df, tidy(m20_scale_Ozone.NB)[2,])

NB_20m_model_results_lag_4day.df$timeframe = '4 day lag'









#lag_3day   ##########################################################



#below, I scale the coefficients (I use my own, multiplying by 2*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM_lag_3day_scale = (PM_lag_3day - mean(PM_lag_3day, na.rm = TRUE))/(sd(PM_lag_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM_lag_3day_scale2 = (PM_lag_3day - mean(PM_lag_3day, na.rm = TRUE))/(2*sd(PM_lag_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(CO_lag_3day_scale2 = (CO_lag_3day - mean(CO_lag_3day, na.rm = TRUE))/(2*sd(CO_lag_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(NO2_lag_3day_scale2 = (NO2_lag_3day - mean(NO2_lag_3day, na.rm = TRUE))/(2*sd(NO2_lag_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(Ozone_lag_3day_scale2 = (Ozone_lag_3day - mean(Ozone_lag_3day, na.rm = TRUE))/(2*sd(Ozone_lag_3day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_20m_daily_ED_df$CO_lag_3day_scale2, na.rm = TRUE)
sd(SCD_20m_daily_ED_df$NO2_lag_3day_scale2, na.rm = TRUE)
mean(SCD_20m_daily_ED_df$Ozone_lag_3day_scale2, na.rm = TRUE)



m20_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                            PM_lag_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20_scale_PM.NB)


m20_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_lag_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_CO.NB)

m20_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_lag_3day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot   + 
                             bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_NO2.NB)

m20_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_lag_3day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot   + 
                               bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_Ozone.NB)



### Dataframe with results


NB_20m_model_results_lag_3day.df <- tidy(m20_scale_PM.NB)[2, ]
NB_20m_model_results_lag_3day.df <- rbind(NB_20m_model_results_lag_3day.df, tidy(m20_scale_CO.NB)[2,])
NB_20m_model_results_lag_3day.df <- rbind(NB_20m_model_results_lag_3day.df, tidy(m20_scale_NO2.NB)[2,])
NB_20m_model_results_lag_3day.df <- rbind(NB_20m_model_results_lag_3day.df, tidy(m20_scale_Ozone.NB)[2,])

NB_20m_model_results_lag_3day.df$timeframe = '3 day lag'








#lag_2day   ##########################################################



#below, I scale the coefficients (I use my own, multiplying by 2*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM_lag_2day_scale = (PM_lag_2day - mean(PM_lag_2day, na.rm = TRUE))/(sd(PM_lag_2day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM_lag_2day_scale2 = (PM_lag_2day - mean(PM_lag_2day, na.rm = TRUE))/(2*sd(PM_lag_2day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(CO_lag_2day_scale2 = (CO_lag_2day - mean(CO_lag_2day, na.rm = TRUE))/(2*sd(CO_lag_2day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(NO2_lag_2day_scale2 = (NO2_lag_2day - mean(NO2_lag_2day, na.rm = TRUE))/(2*sd(NO2_lag_2day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(Ozone_lag_2day_scale2 = (Ozone_lag_2day - mean(Ozone_lag_2day, na.rm = TRUE))/(2*sd(Ozone_lag_2day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_20m_daily_ED_df$CO_lag_2day_scale2, na.rm = TRUE)
sd(SCD_20m_daily_ED_df$NO2_lag_2day_scale2, na.rm = TRUE)
mean(SCD_20m_daily_ED_df$Ozone_lag_2day_scale2, na.rm = TRUE)



m20_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                            PM_lag_2day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20_scale_PM.NB)


m20_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_lag_2day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_CO.NB)

m20_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_lag_2day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot   + 
                             bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_NO2.NB)

m20_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_lag_2day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot   + 
                               bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_Ozone.NB)



### Dataframe with results


NB_20m_model_results_lag_2day.df <- tidy(m20_scale_PM.NB)[2, ]
NB_20m_model_results_lag_2day.df <- rbind(NB_20m_model_results_lag_2day.df, tidy(m20_scale_CO.NB)[2,])
NB_20m_model_results_lag_2day.df <- rbind(NB_20m_model_results_lag_2day.df, tidy(m20_scale_NO2.NB)[2,])
NB_20m_model_results_lag_2day.df <- rbind(NB_20m_model_results_lag_2day.df, tidy(m20_scale_Ozone.NB)[2,])

NB_20m_model_results_lag_2day.df$timeframe = '2 day lag'














#lag_1day   ##########################################################



#below, I scale the coefficients (I use my own, multiplying by 2*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM_lag_1day_scale = (PM_lag_1day - mean(PM_lag_1day, na.rm = TRUE))/(sd(PM_lag_1day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM_lag_1day_scale2 = (PM_lag_1day - mean(PM_lag_1day, na.rm = TRUE))/(2*sd(PM_lag_1day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(CO_lag_1day_scale2 = (CO_lag_1day - mean(CO_lag_1day, na.rm = TRUE))/(2*sd(CO_lag_1day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(NO2_lag_1day_scale2 = (NO2_lag_1day - mean(NO2_lag_1day, na.rm = TRUE))/(2*sd(NO2_lag_1day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(Ozone_lag_1day_scale2 = (Ozone_lag_1day - mean(Ozone_lag_1day, na.rm = TRUE))/(2*sd(Ozone_lag_1day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_20m_daily_ED_df$CO_lag_1day_scale2, na.rm = TRUE)
sd(SCD_20m_daily_ED_df$NO2_lag_1day_scale2, na.rm = TRUE)
mean(SCD_20m_daily_ED_df$Ozone_lag_1day_scale2, na.rm = TRUE)



m20_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                            PM_lag_1day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20_scale_PM.NB)


m20_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_lag_1day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_CO.NB)

m20_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_lag_1day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot   + 
                             bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_NO2.NB)

m20_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_lag_1day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot   + 
                               bs( TMIN_rollmean_2day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_Ozone.NB)



### Dataframe with results


NB_20m_model_results_lag_1day.df <- tidy(m20_scale_PM.NB)[2, ]
NB_20m_model_results_lag_1day.df <- rbind(NB_20m_model_results_lag_1day.df, tidy(m20_scale_CO.NB)[2,])
NB_20m_model_results_lag_1day.df <- rbind(NB_20m_model_results_lag_1day.df, tidy(m20_scale_NO2.NB)[2,])
NB_20m_model_results_lag_1day.df <- rbind(NB_20m_model_results_lag_1day.df, tidy(m20_scale_Ozone.NB)[2,])

NB_20m_model_results_lag_1day.df$timeframe = '1 day lag'

NB_20m_model_results_lag_1day.df








#day of   ##########################################################



#below, I scale the coefficients (I use my own, multiplying by 2*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(daily_PM_mean_scale2 = (daily_PM_mean - mean(daily_PM_mean, na.rm = TRUE))/(2*sd(daily_PM_mean, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(CO_scale2 = (CO - mean(CO, na.rm = TRUE))/(2*sd(CO, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(NO2_scale2 = (NO2 - mean(NO2, na.rm = TRUE))/(2*sd(NO2, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(Ozone_scale2 = (Ozone - mean(Ozone, na.rm = TRUE))/(2*sd(Ozone, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_20m_daily_ED_df$CO, na.rm = TRUE)
sd(SCD_20m_daily_ED_df$NO2, na.rm = TRUE)
mean(SCD_20m_daily_ED_df$Ozone, na.rm = TRUE)



m20_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                            daily_PM_mean_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20_scale_PM.NB)


m20_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot   + 
                            bs( TMIN, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_CO.NB)

m20_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot   + 
                             bs( TMIN, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_NO2.NB)

m20_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot   + 
                               bs( TMIN, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_Ozone.NB)



### Dataframe with results


NB_20m_model_results_dayof.df <- tidy(m20_scale_PM.NB)[2, ]
NB_20m_model_results_dayof.df <- rbind(NB_20m_model_results_dayof.df, tidy(m20_scale_CO.NB)[2,])
NB_20m_model_results_dayof.df <- rbind(NB_20m_model_results_dayof.df, tidy(m20_scale_NO2.NB)[2,])
NB_20m_model_results_dayof.df <- rbind(NB_20m_model_results_dayof.df, tidy(m20_scale_Ozone.NB)[2,])

NB_20m_model_results_dayof.df$timeframe = 'Day of'

NB_20m_model_results_dayof.df




















timeframe.df <- rbind(NB_20m_model_results_dayof.df, NB_20m_model_results_lag_1day.df, NB_20m_model_results_lag_2day.df, 
                      NB_20m_model_results_lag_3day.df, NB_20m_model_results_lag_4day.df, NB_20m_model_results_lead_1day.df, 
                      NB_20m_model_results_lead_2day.df)

timeframe.df

timeframe.df <- timeframe.df %>% 
  mutate(IRR = exp(estimate), 
         lb = exp(estimate - 1.96*std.error), 
         ub = exp(estimate + 1.96*std.error)) %>% 
  mutate(Pollutant = ifelse(term == 'daily_PM_mean_scale2', 'PM2.5', 
                          ifelse(term == 'PM_lag_1day_scale2', 'PM2.5', 
                          ifelse(term == 'PM_lag_2day_scale2', 'PM2.5', 
                          ifelse(term == 'PM_lag_3day_scale2', 'PM2.5', 
                          ifelse(term == 'PM_lag_4day_scale2', 'PM2.5', 
                          ifelse(term == 'PM_lead_1day_scale2', 'PM2.5', 
                          ifelse(term == 'PM_lead_2day_scale2', 'PM2.5',
                          ifelse(term == 'PM_rollmean_3day_scale2', 'PM2.5',
                          ifelse(term == 'CO_lag_1day_scale2', 'CO', 
                          ifelse(term == 'CO_lag_2day_scale2', 'CO', 
                          ifelse(term == 'CO_lag_3day_scale2', 'CO', 
                          ifelse(term == 'CO_lag_4day_scale2', 'CO', 
                          ifelse(term == 'CO_lead_1day_scale2', 'CO',
                          ifelse(term == 'CO_lead_2day_scale2', 'CO',
                          ifelse(term == 'CO_rollmean_3day_scale2', 'CO',
                          ifelse(term == 'CO_scale2', 'CO',
                          ifelse(term == 'NO2_lag_1day_scale2', 'NO2', 
                          ifelse(term == 'NO2_lag_2day_scale2', 'NO2', 
                          ifelse(term == 'NO2_lag_3day_scale2', 'NO2', 
                          ifelse(term == 'NO2_lag_4day_scale2', 'NO2', 
                          ifelse(term == 'NO2_lead_1day', 'NO2', 
                          ifelse(term == 'NO2_rollmean_3day_scale2', 'NO2',
                          ifelse(term == 'NO2_scale2', 'NO2',
                          ifelse(term == 'Ozone_lag_1day_scale2', 'Ozone', 
                          ifelse(term == 'Ozone_lag_2day_scale2', 'Ozone', 
                          ifelse(term == 'Ozone_lag_3day_scale2', 'Ozone', 
                          ifelse(term == 'Ozone_lag_4day_scale2', 'Ozone', 
                          ifelse(term == 'Ozone_lead_1day', 'Ozone', 
                          ifelse(term == 'Ozone_rollmean_3day_scale2', 'Ozone', 'Ozone'))))))))))))))))))))))))))))))


table(timeframe.df$timeframe)
timeframe.df

# https://konstantinkashin.com/2013/using-ggplot2-to-plot-regression-coefficients-with-confidence-intervals/ 

str(timeframe.df)
levels(timeframe.df$timeframe)
timeframe.df$timeframe <- factor(timeframe.df$timeframe, 
                                          levels = c('4 day lag', '3 day lag', '2 day lag', '1 day lag', 
                                                     'Day of', '1 day lead', '2 day lead'))
timeframe.df$Pollutant <- factor(timeframe.df$Pollutant,
                                        levels = c('PM2.5', 'CO', 'NO2', 'Ozone'))
timeframe.df



write.csv(timeframe.df, file = '../Analyses/timeframe.df.csv')


timeframe_short.df = timeframe.df %>% 
  filter(timeframe != '2 day rolling mean' & timeframe != '3 day rolling mean') %>% 
  filter(Pollutant == 'PM2.5' | Pollutant == 'CO')

write.csv(timeframe_short.df, file = '../Analyses/timeframe_short.df.csv')

ggplot(timeframe_short.df, aes(Pollutant , IRR , color = timeframe)) +
  geom_point(aes(shape=timeframe ),size=4, position=position_dodge(width=0.4)) +
  theme_classic() +
  scale_y_continuous("Incidence Rate Ratio with 95% CI") +
  geom_errorbar(aes(ymin=lb,ymax=ub), size = 0.8, width=0.4, position=position_dodge(width=0.4)) + 
  theme(legend.position = c(0.9, 0.8)) +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 1.0) +
  labs(title = 'Figure 3. Effect of Pollutants on ED visits, by pollution level on day relative to visit', 
       subtitle = 'Main exposure is individual pollutant by day relative to ED visit, main outcome is daily number of emergency department visits', 
       caption = 'Plot above shows the effect of two PM2.5 and CO on daily emergency department visits. Lag day means day prior to encounter. 
For example, 2 day lag refers to the pollution levels 2 days prior to encounter. Solid vertical lines represent day of encounter.') +
  theme(text = element_text(size = 18)) +
  theme(plot.caption = element_text(hjust = 0, size = 12.5), 
        plot.subtitle = element_text(size = 14)) +
  geom_vline(xintercept = 1.055, linetype = 'solid')+
  geom_vline(xintercept = 2.055, linetype = 'solid')


#note the good correlation!! 
hist(SCD_20m_daily_ED_df$daily_PM25_mean)
cor.test(SCD_20m_daily_ED_df$PM25, SCD_20m_daily_ED_df$daily_PM25_mean)
