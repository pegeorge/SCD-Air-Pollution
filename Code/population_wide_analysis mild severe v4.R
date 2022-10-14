#####                     Population-wide analysis - mild v severe
#
#
#   I take two separate datasets, one which contains only patients with severe SCD (HbSS or HbSbeta0)
#   the other with all other genotypes, and compare how pollution affects them
#   hypothesizing that kids with more severe forms of SCD will be more affected by pollution 



library(ggplot2)
library(tidyr)
library(dplyr)
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
list.files()



############################################################################################################



####                                SEVERE (SCA ONLY)



########################                       20 miles                          ###########################


####            first, load the main outcome variable, which is total number of ED visits per day
####            note, this db comes from raw to final dataframe, and excludes those who need to be excluded
load("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/ED_DF_Finalized/SSonly_ER_count_20m_df_date.rda")
SCD_ER_count_20m_df_date <- SSonly_ER_count_20m_df_date %>% rename(date = Date)
SCD_ER_count_20m_df_date <- SCD_ER_count_20m_df_date %>% rename(ED_visits_count = Frequency)


####           here, load the PM2.5 (5, 10, or 20 m from CHOA) daily averages 
####           note, this db comes from 'Daily Mean PM25 Values 5 10 20...' 
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



####          create new dataframe from which to run the model

SCD_20m_daily_ED_df <- left_join(SCD_ER_count_20m_df_date, daily_mean_df_20m, by = 'date')
SCD_20m_daily_ED_df <- left_join(SCD_20m_daily_ED_df, EPA_pollut_agr, by = 'date')
SCD_20m_daily_ED_df <- left_join(SCD_20m_daily_ED_df, atl_weather_day, by = 'date')




knots_season = c(60, 244, 425, 609, 791, 975, 1156, 1340, 
                 1521, 1705, 1886, 2070, 2252, 2436, 2617, 2801, 2982, 3166)

#knots_season = c(365, 730, 1095, 1461, 1826, 2191)

ls(SCD_20m_daily_ED_df)



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
                         PRCP_dichot +   
                         bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20_scale_PM.NB)


m20_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot +   
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_CO.NB)

m20_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_rollmean_3day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot +   
                             bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_NO2.NB)

m20_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_rollmean_3day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot +   
                               bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_Ozone.NB)

tidy(m20_scale_Ozone.NB)



### Dataframe with results


NB_20m_model_results.df <- tidy(m20_scale_PM.NB)[2, ]
NB_20m_model_results.df <- rbind(NB_20m_model_results.df, tidy(m20_scale_CO.NB)[2,])
NB_20m_model_results.df <- rbind(NB_20m_model_results.df, tidy(m20_scale_NO2.NB)[2,])
NB_20m_model_results.df <- rbind(NB_20m_model_results.df, tidy(m20_scale_Ozone.NB)[2,])

NB_20m_model_results.df






########################                       10 miles                          ###########################




####            first, load the main outcome variable, which is total number of ED visits per day
####            note, this db comes from raw to final dataframe, and excludes those who need to be excluded
load("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/ED_DF_Finalized/SSonly_ER_count_10m_df_date.rda")
SCD_ER_count_10m_df_date <- SSonly_ER_count_10m_df_date %>% rename(date = Date)
SCD_ER_count_10m_df_date <- SCD_ER_count_10m_df_date %>% rename(ED_visits_count = Frequency)


####           here, load the PM2.5 (5, 10, or 20 m from CHOA) daily averages 
####           note, this db comes from 'Daily Mean PM25 Values 5 10 20...' 
load("PM25 Combined and cropped dataframes/daily_mean_df_10m.Rda")

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



####          create new dataframe from which to run the model

SCD_10m_daily_ED_df <- left_join(SCD_ER_count_10m_df_date, daily_mean_df_10m, by = 'date')
SCD_10m_daily_ED_df <- left_join(SCD_10m_daily_ED_df, EPA_pollut_agr, by = 'date')
SCD_10m_daily_ED_df <- left_join(SCD_10m_daily_ED_df, atl_weather_day, by = 'date')




knots_season = c(60, 244, 425, 609, 791, 975, 1156, 1340, 
                 1521, 1705, 1886, 2070, 2252, 2436)

knots_season = c(365, 730, 1095, 1461, 1826, 2191)

ls(SCD_10m_daily_ED_df)



#below, I scale the coefficients (I use my own, multiplying by 2*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(PM_rollmean_3day_scale = (PM_rollmean_3day - mean(PM_rollmean_3day, na.rm = TRUE))/(sd(PM_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(PM_rollmean_3day_scale2 = (PM_rollmean_3day - mean(PM_rollmean_3day, na.rm = TRUE))/(2*sd(PM_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(CO_rollmean_3day_scale2 = (CO_rollmean_3day - mean(CO_rollmean_3day, na.rm = TRUE))/(2*sd(CO_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(NO2_rollmean_3day_scale2 = (NO2_rollmean_3day - mean(NO2_rollmean_3day, na.rm = TRUE))/(2*sd(NO2_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(Ozone_rollmean_3day_scale2 = (Ozone_rollmean_3day - mean(Ozone_rollmean_3day, na.rm = TRUE))/(2*sd(Ozone_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_10m_daily_ED_df$CO_rollmean_3day_scale2, na.rm = TRUE)
sd(SCD_10m_daily_ED_df$NO2_rollmean_3day_scale2, na.rm = TRUE)
mean(SCD_10m_daily_ED_df$Ozone_rollmean_3day_scale2, na.rm = TRUE)



m10_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                            PM_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot +   
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_10m_daily_ED_df)

summary(m10_scale_PM.NB)


m10_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot +   
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_10m_daily_ED_df)
summary(m10_scale_CO.NB)

m10_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_rollmean_3day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot +   
                             bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_10m_daily_ED_df)
summary(m10_scale_NO2.NB)

m10_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_rollmean_3day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot +   
                               bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_10m_daily_ED_df)
summary(m10_scale_Ozone.NB)


### Dataframe with results


NB_10m_model_results.df <- tidy(m10_scale_PM.NB)[2, ]
NB_10m_model_results.df <- rbind(NB_10m_model_results.df, tidy(m10_scale_CO.NB)[2,])
NB_10m_model_results.df <- rbind(NB_10m_model_results.df, tidy(m10_scale_NO2.NB)[2,])
NB_10m_model_results.df <- rbind(NB_10m_model_results.df, tidy(m10_scale_Ozone.NB)[2,])

NB_10m_model_results.df






########################                       5 miles                          ###########################



####            first, load the main outcome variable, which is total number of ED visits per day
####            note, this db comes from raw to final dataframe, and excludes those who need to be excluded
load("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/ED_DF_Finalized/SSonly_ER_count_5m_df_date.rda")
SCD_ER_count_5m_df_date <- SSonly_ER_count_5m_df_date %>% rename(date = Date)
SCD_ER_count_5m_df_date <- SCD_ER_count_5m_df_date %>% rename(ED_visits_count = Frequency)


####           here, load the PM2.5 (5, 10, or 20 m from CHOA) daily averages 
####           note, this db comes from 'Daily Mean PM25 Values 5 10 20...' 
load("PM25 Combined and cropped dataframes/daily_mean_df_5m.Rda")

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



####          create new dataframe from which to run the model

SCD_5m_daily_ED_df <- left_join(SCD_ER_count_5m_df_date, daily_mean_df_5m, by = 'date')
SCD_5m_daily_ED_df <- left_join(SCD_5m_daily_ED_df, EPA_pollut_agr, by = 'date')
SCD_5m_daily_ED_df <- left_join(SCD_5m_daily_ED_df, atl_weather_day, by = 'date')




knots_season = c(60, 244, 425, 609, 791, 975, 1156, 1340, 
                 1521, 1705, 1886, 2070, 2252, 2436)



#below, I scale the coefficients (I use my own, multiplying by 2*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(PM_rollmean_3day_scale = (PM_rollmean_3day - mean(PM_rollmean_3day, na.rm = TRUE))/(sd(PM_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(PM_rollmean_3day_scale2 = (PM_rollmean_3day - mean(PM_rollmean_3day, na.rm = TRUE))/(2*sd(PM_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(CO_rollmean_3day_scale2 = (CO_rollmean_3day - mean(CO_rollmean_3day, na.rm = TRUE))/(2*sd(CO_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(NO2_rollmean_3day_scale2 = (NO2_rollmean_3day - mean(NO2_rollmean_3day, na.rm = TRUE))/(2*sd(NO2_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(Ozone_rollmean_3day_scale2 = (Ozone_rollmean_3day - mean(Ozone_rollmean_3day, na.rm = TRUE))/(2*sd(Ozone_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_5m_daily_ED_df$CO_rollmean_3day_scale2, na.rm = TRUE)
sd(SCD_5m_daily_ED_df$NO2_rollmean_3day_scale2, na.rm = TRUE)
mean(SCD_5m_daily_ED_df$Ozone_rollmean_3day_scale2, na.rm = TRUE)



m5_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                            PM_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot +   
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_5m_daily_ED_df)

summary(m5_scale_PM.NB)


m5_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot +   
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_5m_daily_ED_df)
summary(m5_scale_CO.NB)

m5_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_rollmean_3day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot +   
                             bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_5m_daily_ED_df)
summary(m5_scale_NO2.NB)

m5_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_rollmean_3day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot +   
                               bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_5m_daily_ED_df)
summary(m5_scale_Ozone.NB)


### Dataframe with results


NB_5m_model_results.df <- tidy(m5_scale_PM.NB)[2, ]
NB_5m_model_results.df <- rbind(NB_5m_model_results.df, tidy(m5_scale_CO.NB)[2,])
NB_5m_model_results.df <- rbind(NB_5m_model_results.df, tidy(m5_scale_NO2.NB)[2,])
NB_5m_model_results.df <- rbind(NB_5m_model_results.df, tidy(m5_scale_Ozone.NB)[2,])

NB_5m_model_results.df


NB_5m_model_results.df <- NB_5m_model_results.df %>% 
  mutate(distance = '5 mile radius')

NB_10m_model_results.df <- NB_10m_model_results.df %>% 
  mutate(distance = '10 mile radius')

NB_20m_model_results.df <- NB_20m_model_results.df %>% 
  mutate(distance = '20 mile radius')

ED_model_results.df <- rbind(NB_5m_model_results.df, NB_10m_model_results.df, NB_20m_model_results.df)

ED_model_results_severe.df <- ED_model_results.df %>% 
  mutate(IRR = exp(estimate), 
         lb = exp(estimate - 1.96*std.error), 
         ub = exp(estimate + 1.96*std.error)) %>% 
  mutate(Pollutant = ifelse(term == 'PM_rollmean_3day_scale2', 'PM2.5', 
                            ifelse(term == 'CO_rollmean_3day_scale2', 'CO', 
                                   ifelse(term == 'NO2_rollmean_3day_scale2', 'NO2', 'Ozone'))))



ED_model_results_severe.df








############################################################################################################



####                                'MILD' (ONLY)




#############################################################################################################









########################                       20 miles                          ###########################


####            first, load the main outcome variable, which is total number of ED visits per day
####            note, this db comes from raw to final dataframe, and excludes those who need to be excluded
load("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/ED_DF_Finalized/SCDmild_ER_count_20m_df_date.rda")
SCD_ER_count_20m_df_date <- SCDmild_ER_count_20m_df_date %>% rename(date = Date)
SCD_ER_count_20m_df_date <- SCD_ER_count_20m_df_date %>% rename(ED_visits_count = Frequency)


####           here, load the PM2.5 (5, 10, or 20 m from CHOA) daily averages 
####           note, this db comes from 'Daily Mean PM25 Values 5 10 20...' 
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



####          create new dataframe from which to run the model

SCD_20m_daily_ED_df <- left_join(SCD_ER_count_20m_df_date, daily_mean_df_20m, by = 'date')
SCD_20m_daily_ED_df <- left_join(SCD_20m_daily_ED_df, EPA_pollut_agr, by = 'date')
SCD_20m_daily_ED_df <- left_join(SCD_20m_daily_ED_df, atl_weather_day, by = 'date')




knots_season = c(60, 244, 425, 609, 791, 975, 1156, 1340, 
                 1521, 1705, 1886, 2070, 2252, 2436)

knots_season = c(365, 730, 1095, 1461, 1826, 2191)

ls(SCD_20m_daily_ED_df)




#poisson model
m20.Poisson <- glm(formula = ED_visits_count ~ PM_rollmean_3day + BusinessDay + 
                     PRCP_dichot + bs(day_number, knots = knots_season) +   
                     bs( TMIN_rollmean_3day, knots = c(42, 69)), family = 'poisson', data = SCD_20m_daily_ED_df)

dispersiontest(m20.Poisson)             # rule of thumb, >1.10 is considered over-dispersion https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/count_data_and_overdispersion.html#:~:text=Over%20dispersion%20can%20be%20detected,or%20greater%20is%20considered%20large.



#negative binomial model 
m20.NB <- glm.nb(formula = ED_visits_count ~ PM_rollmean_3day + CO_rollmean_3day + NO2_rollmean_3day + Ozone_rollmean_3day + BusinessDay + 
                   PRCP_dichot + bs(day_number, knots = knots_season) +   
                   bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20.NB)
summ(m20.NB)




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
                            PRCP_dichot +   
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20_scale_PM.NB)


m20_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot +   
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_CO.NB)

m20_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_rollmean_3day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot +   
                             bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_NO2.NB)

m20_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_rollmean_3day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot +   
                               bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_Ozone.NB)

tidy(m20_scale_Ozone.NB)



### Dataframe with results


NB_20m_model_results.df <- tidy(m20_scale_PM.NB)[2, ]
NB_20m_model_results.df <- rbind(NB_20m_model_results.df, tidy(m20_scale_CO.NB)[2,])
NB_20m_model_results.df <- rbind(NB_20m_model_results.df, tidy(m20_scale_NO2.NB)[2,])
NB_20m_model_results.df <- rbind(NB_20m_model_results.df, tidy(m20_scale_Ozone.NB)[2,])

NB_20m_model_results.df






########################                       10 miles                          ###########################




####            first, load the main outcome variable, which is total number of ED visits per day
####            note, this db comes from raw to final dataframe, and excludes those who need to be excluded
load("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/ED_DF_Finalized/SCDmild_ER_count_10m_df_date.rda")
SCD_ER_count_10m_df_date <- SCDmild_ER_count_10m_df_date %>% rename(date = Date)
SCD_ER_count_10m_df_date <- SCD_ER_count_10m_df_date %>% rename(ED_visits_count = Frequency)


####           here, load the PM2.5 (5, 10, or 20 m from CHOA) daily averages 
####           note, this db comes from 'Daily Mean PM25 Values 5 10 20...' 
load("PM25 Combined and cropped dataframes/daily_mean_df_10m.Rda")

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



####          create new dataframe from which to run the model

SCD_10m_daily_ED_df <- left_join(SCD_ER_count_10m_df_date, daily_mean_df_10m, by = 'date')
SCD_10m_daily_ED_df <- left_join(SCD_10m_daily_ED_df, EPA_pollut_agr, by = 'date')
SCD_10m_daily_ED_df <- left_join(SCD_10m_daily_ED_df, atl_weather_day, by = 'date')




knots_season = c(60, 244, 425, 609, 791, 975, 1156, 1340, 
                 1521, 1705, 1886, 2070, 2252, 2436)

knots_season = c(365, 730, 1095, 1461, 1826, 2191)

ls(SCD_10m_daily_ED_df)




#poisson model
m10.Poisson <- glm(formula = ED_visits_count ~ PM_rollmean_3day + BusinessDay + 
                     PRCP_dichot + bs(day_number, knots = knots_season) +   
                     bs( TMIN_rollmean_3day, knots = c(42, 69)), family = 'poisson', data = SCD_10m_daily_ED_df)

dispersiontest(m10.Poisson)             # rule of thumb, >1.10 is considered over-dispersion https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/count_data_and_overdispersion.html#:~:text=Over%20dispersion%20can%20be%20detected,or%20greater%20is%20considered%20large.



#negative binomial model 
m10.NB <- glm.nb(formula = ED_visits_count ~ PM_rollmean_3day + CO_rollmean_3day + NO2_rollmean_3day + Ozone_rollmean_3day + BusinessDay + 
                   PRCP_dichot + bs(day_number, knots = knots_season) +   
                   bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_10m_daily_ED_df)

summary(m10.NB)


#below, I scale the coefficients (I use my own, multiplying by 2*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(PM_rollmean_3day_scale = (PM_rollmean_3day - mean(PM_rollmean_3day, na.rm = TRUE))/(sd(PM_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(PM_rollmean_3day_scale2 = (PM_rollmean_3day - mean(PM_rollmean_3day, na.rm = TRUE))/(2*sd(PM_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(CO_rollmean_3day_scale2 = (CO_rollmean_3day - mean(CO_rollmean_3day, na.rm = TRUE))/(2*sd(CO_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(NO2_rollmean_3day_scale2 = (NO2_rollmean_3day - mean(NO2_rollmean_3day, na.rm = TRUE))/(2*sd(NO2_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(Ozone_rollmean_3day_scale2 = (Ozone_rollmean_3day - mean(Ozone_rollmean_3day, na.rm = TRUE))/(2*sd(Ozone_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_10m_daily_ED_df$CO_rollmean_3day_scale2, na.rm = TRUE)
sd(SCD_10m_daily_ED_df$NO2_rollmean_3day_scale2, na.rm = TRUE)
mean(SCD_10m_daily_ED_df$Ozone_rollmean_3day_scale2, na.rm = TRUE)



m10_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                            PM_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot +   
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_10m_daily_ED_df)

summary(m10_scale_PM.NB)


m10_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot +   
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_10m_daily_ED_df)
summary(m10_scale_CO.NB)

m10_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_rollmean_3day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_season) + 
                             PRCP_dichot +   
                             bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_10m_daily_ED_df)
summary(m10_scale_NO2.NB)

m10_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_rollmean_3day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_season) + 
                               PRCP_dichot +   
                               bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_10m_daily_ED_df)
summary(m10_scale_Ozone.NB)


### Dataframe with results


NB_10m_model_results.df <- tidy(m10_scale_PM.NB)[2, ]
NB_10m_model_results.df <- rbind(NB_10m_model_results.df, tidy(m10_scale_CO.NB)[2,])
NB_10m_model_results.df <- rbind(NB_10m_model_results.df, tidy(m10_scale_NO2.NB)[2,])
NB_10m_model_results.df <- rbind(NB_10m_model_results.df, tidy(m10_scale_Ozone.NB)[2,])

NB_10m_model_results.df






########################                       5 miles                          ###########################



####            first, load the main outcome variable, which is total number of ED visits per day
####            note, this db comes from raw to final dataframe, and excludes those who need to be excluded
load("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/ED_DF_Finalized/SCDmild_ER_count_5m_df_date.rda")
SCD_ER_count_5m_df_date <- SCDmild_ER_count_5m_df_date %>% rename(date = Date)
SCD_ER_count_5m_df_date <- SCD_ER_count_5m_df_date %>% rename(ED_visits_count = Frequency)


####           here, load the PM2.5 (5, 10, or 20 m from CHOA) daily averages 
####           note, this db comes from 'Daily Mean PM25 Values 5 10 20...' 
load("PM25 Combined and cropped dataframes/daily_mean_df_5m.Rda")

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



####          create new dataframe from which to run the model

SCD_5m_daily_ED_df <- left_join(SCD_ER_count_5m_df_date, daily_mean_df_5m, by = 'date')
SCD_5m_daily_ED_df <- left_join(SCD_5m_daily_ED_df, EPA_pollut_agr, by = 'date')
SCD_5m_daily_ED_df <- left_join(SCD_5m_daily_ED_df, atl_weather_day, by = 'date')




knots_season = c(60, 244, 425, 609, 791, 975, 1156, 1340, 
                 1521, 1705, 1886, 2070, 2252, 2436)

knots_season = c(365, 730, 1095, 1461, 1826, 2191)






#poisson model
m5.Poisson <- glm(formula = ED_visits_count ~ PM_rollmean_3day + BusinessDay + 
                    PRCP_dichot + bs(day_number, knots = knots_season) +   
                    bs( TMIN_rollmean_3day, knots = c(42, 69)), family = 'poisson', data = SCD_5m_daily_ED_df)

dispersiontest(m5.Poisson)             

#negative binomial model 
m5.NB <- glm.nb(formula = ED_visits_count ~ PM_rollmean_3day + CO_rollmean_3day + NO2_rollmean_3day + Ozone_rollmean_3day + BusinessDay + 
                  PRCP_dichot + bs(day_number, knots = knots_season) +   
                  bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_5m_daily_ED_df)

summary(m5.NB)


#below, I scale the coefficients (I use my own, multiplying by 2*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(PM_rollmean_3day_scale = (PM_rollmean_3day - mean(PM_rollmean_3day, na.rm = TRUE))/(sd(PM_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(PM_rollmean_3day_scale2 = (PM_rollmean_3day - mean(PM_rollmean_3day, na.rm = TRUE))/(2*sd(PM_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(CO_rollmean_3day_scale2 = (CO_rollmean_3day - mean(CO_rollmean_3day, na.rm = TRUE))/(2*sd(CO_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(NO2_rollmean_3day_scale2 = (NO2_rollmean_3day - mean(NO2_rollmean_3day, na.rm = TRUE))/(2*sd(NO2_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(Ozone_rollmean_3day_scale2 = (Ozone_rollmean_3day - mean(Ozone_rollmean_3day, na.rm = TRUE))/(2*sd(Ozone_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_5m_daily_ED_df$CO_rollmean_3day_scale2, na.rm = TRUE)
sd(SCD_5m_daily_ED_df$NO2_rollmean_3day_scale2, na.rm = TRUE)
mean(SCD_5m_daily_ED_df$Ozone_rollmean_3day_scale2, na.rm = TRUE)



m5_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                           PM_rollmean_3day_scale2 + 
                           BusinessDay + bs(day_number, knots = knots_season) + 
                           PRCP_dichot +   
                           bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_5m_daily_ED_df)

summary(m5_scale_PM.NB)


m5_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                           CO_rollmean_3day_scale2 + 
                           BusinessDay + bs(day_number, knots = knots_season) + 
                           PRCP_dichot +   
                           bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_5m_daily_ED_df)
summary(m5_scale_CO.NB)

m5_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                            NO2_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_season) + 
                            PRCP_dichot +   
                            bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_5m_daily_ED_df)
summary(m5_scale_NO2.NB)

m5_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                              Ozone_rollmean_3day_scale2 + 
                              BusinessDay + bs(day_number, knots = knots_season) + 
                              PRCP_dichot +   
                              bs( TMIN_rollmean_3day, knots = c(42, 69)), link = 'log', data = SCD_5m_daily_ED_df)
summary(m5_scale_Ozone.NB)


### Dataframe with results


NB_5m_model_results.df <- tidy(m5_scale_PM.NB)[2, ]
NB_5m_model_results.df <- rbind(NB_5m_model_results.df, tidy(m5_scale_CO.NB)[2,])
NB_5m_model_results.df <- rbind(NB_5m_model_results.df, tidy(m5_scale_NO2.NB)[2,])
NB_5m_model_results.df <- rbind(NB_5m_model_results.df, tidy(m5_scale_Ozone.NB)[2,])

NB_5m_model_results.df


NB_5m_model_results.df <- NB_5m_model_results.df %>% 
  mutate(distance = '5 mile radius')

NB_10m_model_results.df <- NB_10m_model_results.df %>% 
  mutate(distance = '10 mile radius')

NB_20m_model_results.df <- NB_20m_model_results.df %>% 
  mutate(distance = '20 mile radius')

ED_model_results.df <- rbind(NB_5m_model_results.df, NB_10m_model_results.df, NB_20m_model_results.df)

ED_model_results_mild.df <- ED_model_results.df %>% 
  mutate(IRR = exp(estimate), 
         lb = exp(estimate - 1.96*std.error), 
         ub = exp(estimate + 1.96*std.error)) %>% 
  mutate(Pollutant = ifelse(term == 'PM_rollmean_3day_scale2', 'PM2.5', 
                            ifelse(term == 'CO_rollmean_3day_scale2', 'CO', 
                                   ifelse(term == 'NO2_rollmean_3day_scale2', 'NO2', 'Ozone'))))



ED_model_results_mild.df
ED_model_results_mild.df = data.frame(lapply(ED_model_results_mild.df, function(x) {
  gsub('radius', ', moderate', x)
}))
ED_model_results_mild.df

ED_model_results_severe.df
ED_model_results_severe.df = data.frame(lapply(ED_model_results_severe.df, function(x) {
  gsub('radius', ', severe', x)
}))
ED_model_results_severe.df

ED_model_results_mild_severe.df = rbind(ED_model_results_mild.df, ED_model_results_severe.df)

# https://konstantinkashin.com/2013/using-ggplot2-to-plot-regression-coefficients-with-confidence-intervals/ 

str(ED_model_results_mild_severe.df)
levels(ED_model_results_mild_severe.df$distance)
ED_model_results_mild_severe.df$distance <- factor(ED_model_results_mild_severe.df$distance, 
                                       levels = c('5 mile , moderate', '10 mile , moderate', '20 mile , moderate', 
                                                  '5 mile , severe', '10 mile , severe', '20 mile , severe' ))
ED_model_results_mild_severe.df$Pollutant <- factor(ED_model_results_mild_severe.df$Pollutant,
                                        levels = c('PM2.5', 'CO', 'NO2', 'Ozone'))

str(ED_model_results_mild_severe.df)
ED_model_results_mild_severe.df$estimate = as.numeric(ED_model_results_mild_severe.df$estimate)
ED_model_results_mild_severe.df$lb = as.numeric(ED_model_results_mild_severe.df$lb)
ED_model_results_mild_severe.df$ub = as.numeric(ED_model_results_mild_severe.df$ub)
ED_model_results_mild_severe.df$IRR = as.numeric(ED_model_results_mild_severe.df$IRR)

ED_model_results_mild_severe_short.df = ED_model_results_mild_severe.df %>% 
  filter(distance != '10 mile , moderate' & distance != '10 mile , severe')

ggplot(ED_model_results_mild_severe_short.df, aes(Pollutant , IRR , color = distance)) +
  geom_point(aes(shape=distance ),size=4, position=position_dodge(width=0.3)) +
  theme_classic() + 
  scale_color_manual(name="Distance from CHOA",values=c("blue", 'darkblue', 'red2', 'darkred')) +
  scale_shape_manual(name="Distance from CHOA",values=c(16,17, 16, 17)) +
  scale_y_continuous("Incidence Rate Ratio with 95% CI") +
  geom_errorbar(aes(ymin=lb,ymax=ub), size = 0.8, width=0.4, position=position_dodge(width=0.3)) + 
  theme(legend.position = c(0.85, 0.85)) +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 1.0) +
  labs(title = 'Figure 4. Effect of pollutants on ED Visits, by hemoglobin type', 
       subtitle = 'Main exposure is individual pollutant, main outcome is daily number of emergency department visits', 
       caption = 'Severe includes patients with HbSS, HbSbeta0; Moderate includes all other sickle cell variants, does not include sickle cell trait.') +
  theme(text = element_text(size = 18)) +
  theme(plot.caption = element_text(hjust = 0, size = 12.5), 
        plot.subtitle = element_text(size = 14))









