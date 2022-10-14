#########                   creates code for the HU (Quarmyne et al 2016) comparison           ###############

#### here, I create a graph that compares ED visit IRR for stopping HU (using Q et al 2016 data AJH) 
####    as compared to going from 98th percentile pollution day to 2nd %ile pollution day 
####    ie. change of 4 standard deviations 

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



########################                       20 miles                          ###########################


####            first, load the main outcome variable, which is total number of ED visits per day
####            note, this db comes from raw to final dataframe, and excludes those who need to be excluded
load("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/ED_DF_Finalized/SCD_ER_count_20m_df_date.rda")
SCD_ER_count_20m_df_date <- SCD_ER_count_20m_df_date %>% rename(date = Date)
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

knots_year = c(365, 730, 1095, 1461, 1826, 2191)

ls(SCD_20m_daily_ED_df)




#negative binomial model 
m20.NB <- glm.nb(formula = ED_visits_count ~ PM25_rollmean_3day + CO_rollmean_3day + NO2_rollmean_3day + Ozone_rollmean_3day + BusinessDay + 
                   PRCP_dichot + bs(day_number, knots = knots_year) + bs(TMAX, knots = c(40, 80)) + 
                   bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20.NB)
summ(m20.NB)




#below, I scale the coefficients (I use my own, multiplying by 4*sd (to match with above - typically use 2*sd in other analyses))


SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(PM25_rollmean_3day_scale2 = (PM25_rollmean_3day - mean(PM25_rollmean_3day, na.rm = TRUE))/(4*sd(PM25_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(CO_rollmean_3day_scale2 = (CO_rollmean_3day - mean(CO_rollmean_3day, na.rm = TRUE))/(4*sd(CO_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(NO2_rollmean_3day_scale2 = (NO2_rollmean_3day - mean(NO2_rollmean_3day, na.rm = TRUE))/(4*sd(NO2_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_20m_daily_ED_df = SCD_20m_daily_ED_df %>% 
  mutate(Ozone_rollmean_3day_scale2 = (Ozone_rollmean_3day - mean(Ozone_rollmean_3day, na.rm = TRUE))/(4*sd(Ozone_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_20m_daily_ED_df$CO_rollmean_3day_scale2, na.rm = TRUE)
sd(SCD_20m_daily_ED_df$NO2_rollmean_3day_scale2, na.rm = TRUE)
mean(SCD_20m_daily_ED_df$Ozone_rollmean_3day_scale2, na.rm = TRUE)



m20_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                         PM25_rollmean_3day_scale2 + 
                         BusinessDay + bs(day_number, knots = knots_year) + 
                         PRCP_dichot + bs(TMAX, knots = c(40, 80)) + 
                         bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_20m_daily_ED_df)

summary(m20_scale_PM.NB)


m20_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_year) + 
                            PRCP_dichot + bs(TMAX, knots = c(40, 80)) + 
                            bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_CO.NB)

m20_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_rollmean_3day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_year) + 
                             PRCP_dichot + bs(TMAX, knots = c(40, 80)) + 
                             bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_20m_daily_ED_df)
summary(m20_scale_NO2.NB)

m20_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_rollmean_3day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_year) + 
                               PRCP_dichot + bs(TMAX, knots = c(40, 80)) + 
                               bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_20m_daily_ED_df)
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
load("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/ED_DF_Finalized/SCD_ER_count_10m_df_date.rda")
SCD_ER_count_10m_df_date <- SCD_ER_count_10m_df_date %>% rename(date = Date)
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

knots_year = c(365, 730, 1095, 1461, 1826, 2191)

ls(SCD_10m_daily_ED_df)




#poisson model
m10.Poisson <- glm(formula = ED_visits_count ~ PM25_rollmean_3day + BusinessDay + 
                     PRCP_dichot + bs(day_number, knots = knots_season) + bs(TMAX, knots = c(40, 80)) + 
                     bs(TMIN, knots = c(30, 60)), family = 'poisson', data = SCD_10m_daily_ED_df)

dispersiontest(m10.Poisson)             # rule of thumb, >1.10 is considered over-dispersion https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/count_data_and_overdispersion.html#:~:text=Over%20dispersion%20can%20be%20detected,or%20greater%20is%20considered%20large.



#negative binomial model 
m10.NB <- glm.nb(formula = ED_visits_count ~ PM25_rollmean_3day + CO_rollmean_3day + NO2_rollmean_3day + Ozone_rollmean_3day + BusinessDay + 
                   PRCP_dichot + bs(day_number, knots = knots_year) + bs(TMAX, knots = c(40, 80)) + 
                   bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_10m_daily_ED_df)

summary(m10.NB)


#below, I scale the coefficients (I use my own, multiplying by 4*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(PM25_rollmean_3day_scale = (PM25_rollmean_3day - mean(PM25_rollmean_3day, na.rm = TRUE))/(sd(PM25_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(PM25_rollmean_3day_scale2 = (PM25_rollmean_3day - mean(PM25_rollmean_3day, na.rm = TRUE))/(4*sd(PM25_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(CO_rollmean_3day_scale2 = (CO_rollmean_3day - mean(CO_rollmean_3day, na.rm = TRUE))/(4*sd(CO_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(NO2_rollmean_3day_scale2 = (NO2_rollmean_3day - mean(NO2_rollmean_3day, na.rm = TRUE))/(4*sd(NO2_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_10m_daily_ED_df = SCD_10m_daily_ED_df %>% 
  mutate(Ozone_rollmean_3day_scale2 = (Ozone_rollmean_3day - mean(Ozone_rollmean_3day, na.rm = TRUE))/(4*sd(Ozone_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_10m_daily_ED_df$CO_rollmean_3day_scale2, na.rm = TRUE)
sd(SCD_10m_daily_ED_df$NO2_rollmean_3day_scale2, na.rm = TRUE)
mean(SCD_10m_daily_ED_df$Ozone_rollmean_3day_scale2, na.rm = TRUE)



m10_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                            PM25_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_year) + 
                            PRCP_dichot + bs(TMAX, knots = c(40, 80)) + 
                            bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_10m_daily_ED_df)

summary(m10_scale_PM.NB)


m10_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_year) + 
                            PRCP_dichot + bs(TMAX, knots = c(40, 80)) + 
                            bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_10m_daily_ED_df)
summary(m10_scale_CO.NB)

m10_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_rollmean_3day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_year) + 
                             PRCP_dichot + bs(TMAX, knots = c(40, 80)) + 
                             bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_10m_daily_ED_df)
summary(m10_scale_NO2.NB)

m10_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_rollmean_3day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_year) + 
                               PRCP_dichot + bs(TMAX, knots = c(40, 80)) + 
                               bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_10m_daily_ED_df)
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
load("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/ED_DF_Finalized/SCD_ER_count_5m_df_date.rda")
SCD_ER_count_5m_df_date <- SCD_ER_count_5m_df_date %>% rename(date = Date)
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

knots_year = c(365, 730, 1095, 1461, 1826, 2191)






#poisson model
m5.Poisson <- glm(formula = ED_visits_count ~ PM25_rollmean_3day + BusinessDay + 
                     PRCP_dichot + bs(day_number, knots = knots_season) + bs(TMAX, knots = c(40, 80)) + 
                     bs(TMIN, knots = c(30, 60)), family = 'poisson', data = SCD_5m_daily_ED_df)

dispersiontest(m5.Poisson)             

#negative binomial model 
m5.NB <- glm.nb(formula = ED_visits_count ~ PM25_rollmean_3day + CO_rollmean_3day + NO2_rollmean_3day + Ozone_rollmean_3day + BusinessDay + 
                   PRCP_dichot + bs(day_number, knots = knots_year) + bs(TMAX, knots = c(40, 80)) + 
                   bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_5m_daily_ED_df)

summary(m5.NB)


#below, I scale the coefficients (I use my own, multiplying by 4*sd per Andrew Gelman recommendations http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(PM25_rollmean_3day_scale = (PM25_rollmean_3day - mean(PM25_rollmean_3day, na.rm = TRUE))/(sd(PM25_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(PM25_rollmean_3day_scale2 = (PM25_rollmean_3day - mean(PM25_rollmean_3day, na.rm = TRUE))/(4*sd(PM25_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(CO_rollmean_3day_scale2 = (CO_rollmean_3day - mean(CO_rollmean_3day, na.rm = TRUE))/(4*sd(CO_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(NO2_rollmean_3day_scale2 = (NO2_rollmean_3day - mean(NO2_rollmean_3day, na.rm = TRUE))/(4*sd(NO2_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)

SCD_5m_daily_ED_df = SCD_5m_daily_ED_df %>% 
  mutate(Ozone_rollmean_3day_scale2 = (Ozone_rollmean_3day - mean(Ozone_rollmean_3day, na.rm = TRUE))/(4*sd(Ozone_rollmean_3day, na.rm = TRUE)), na.rm = TRUE)


mean(SCD_5m_daily_ED_df$CO_rollmean_3day_scale2, na.rm = TRUE)
sd(SCD_5m_daily_ED_df$NO2_rollmean_3day_scale2, na.rm = TRUE)
mean(SCD_5m_daily_ED_df$Ozone_rollmean_3day_scale2, na.rm = TRUE)



m5_scale_PM.NB <- glm.nb(formula = ED_visits_count ~ 
                            PM25_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_year) + 
                            PRCP_dichot + bs(TMAX, knots = c(40, 80)) + 
                            bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_5m_daily_ED_df)

summary(m5_scale_PM.NB)


m5_scale_CO.NB <- glm.nb(formula = ED_visits_count ~ 
                            CO_rollmean_3day_scale2 + 
                            BusinessDay + bs(day_number, knots = knots_year) + 
                            PRCP_dichot + bs(TMAX, knots = c(40, 80)) + 
                            bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_5m_daily_ED_df)
summary(m5_scale_CO.NB)

m5_scale_NO2.NB <- glm.nb(formula = ED_visits_count ~ 
                             NO2_rollmean_3day_scale2 + 
                             BusinessDay + bs(day_number, knots = knots_year) + 
                             PRCP_dichot + bs(TMAX, knots = c(40, 80)) + 
                             bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_5m_daily_ED_df)
summary(m5_scale_NO2.NB)

m5_scale_Ozone.NB <- glm.nb(formula = ED_visits_count ~ 
                               Ozone_rollmean_3day_scale2 + 
                               BusinessDay + bs(day_number, knots = knots_year) + 
                               PRCP_dichot + bs(TMAX, knots = c(40, 80)) + 
                               bs(TMIN, knots = c(30, 60)), link = 'log', data = SCD_5m_daily_ED_df)
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

ED_model_results.df <- ED_model_results.df %>% 
  mutate(IRR = exp(-estimate), 
         lb = exp(-estimate - 1.96*std.error), 
         ub = exp(-estimate + 1.96*std.error)) %>% 
  mutate(Pollutant = ifelse(term == 'PM25_rollmean_3day_scale2', 'PM2.5', 
                            ifelse(term == 'CO_rollmean_3day_scale2', 'CO', 
                                   ifelse(term == 'NO2_rollmean_3day_scale2', 'NO2', 'Ozone'))))

ED_model_results.df[nrow(ED_model_results.df) + 1,] = list('Hydroxyurea', 0, 0, 0, 0, 'HU', 0.57, 0.49, 0.67, 'Hydroxyurea')

ED_model_results.df

pd <- position_dodge(width=0.2,height=NULL)

# https://konstantinkashin.com/2013/using-ggplot2-to-plot-regression-coefficients-with-confidence-intervals/ 

str(ED_model_results.df)
levels(ED_model_results.df$distance)
ED_model_results.df$distance <- factor(ED_model_results.df$distance, 
                                          levels = c('5 mile radius', '10 mile radius', '20 mile radius', 'HU'))
ED_model_results.df$Pollutant <- factor(ED_model_results.df$Pollutant,
                                        levels = c('PM2.5', 'CO', 'NO2', 'Ozone', 'Hydroxyurea'))




ggplot(ED_model_results.df, aes(Pollutant , IRR , color = distance)) +
  geom_point(aes(shape=distance ),size=4, position=position_dodge(width=0.3)) +
  scale_color_manual(name="Distance from CHOA",values=c("coral","steelblue", '#009E73', 'purple')) +
  scale_shape_manual(name="Distance from CHOA",values=c(17,19, 18, 16)) +
  theme_classic() +
  scale_y_continuous("Incidence Rate Ratio with 95% CI") +
  geom_errorbar(aes(ymin=lb,ymax=ub), size = 0.8, width=0.4, position=position_dodge(width=0.3)) + 
  theme(legend.position = c(0.87, 0.85)) +
  geom_hline(yintercept = 1, linetype = 'dashed') +
  labs(title = 'Incidence Rate Ratios', 
       subtitle = 'Main exposure is individual pollutant, main outcome is daily number of emergency department visits', 
       caption = 'Plot above shows results of 12 separate models (4 pollutants x 3 areas), and hydroxyurea data from Quarmyne et al 2016 AJH
       with data from a similar patient population. Note that here, the pollutants have been mean centered and divided by 4*sd 
       (which would equal the effect of going from 2.1 to 97.9 percentile).') +
  theme(text = element_text(size = 18)) +
  theme(plot.caption = element_text(hjust = 0, size = 11), 
        plot.subtitle = element_text(size = 14))

