
# Time series graphs of pollution values 

library(dplyr)
library(tidyverse)
library(ggplot2)
library(psych)

rm(list=ls())


#####           Set the working directory where the original dataset is located
setwd("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/")


load("Saved R Dataframes/EPA_pollut_agr.Rda")
EPA_pollut_agr <- EPA_pollut_agr %>% rename(date = Date)

load("PM25 Combined and cropped dataframes/daily_mean_df_20m.Rda")

pollutant_df_wide = left_join(EPA_pollut_agr, daily_mean_df_20m,  by = 'date')

describe(pollutant_df_wide$daily_PM_mean)
describe(pollutant_df_wide$CO)
describe(pollutant_df_wide$NO2)
describe(pollutant_df_wide$Ozone)

pollutant_df = pollutant_df_wide %>% 
  dplyr::select(date, daily_PM_mean, CO, NO2, Ozone) %>% 
  gather(key = 'pollutant', value = "value", -date)
  

ls(pollutant_df)

class(pollutant_df$date)

#now, to graph using ggplot and the date function of ggplot
ggplot(pollutant_df) +
  aes(x = date, y = value) +
  geom_line(aes(color = pollutant), size = 0.01) +
  theme_classic() + 
  labs(title = "Figure 1: Daily Pollutant Levels, Atlanta, GA (2010-2018)", 
       subtitle = "Area of interest includes a 20 mile radius from nearest CHOA facility",
       caption = "This time series graph shows daily pollutant values during the study period.
PM2.5 (SEDAC derived data) mean 10.6 (sd 4.7) ug/m3;  CO (EPA data) mean 0.57 (sd 0.25) ppm; 
NO2 (EPA) mean 26.7 (sd 11.4) ppb;  Ozone (EPA) mean 0.04 (sd 0.01) ppb.") +
  xlab("Date") + ylab("Daily Pollutant Levels (log scale)") +
  scale_color_discrete(name = 'Pollutant', 
                       labels = c('CO (Daily maximum, ppm)', 'PM2.5 (Daily mean, ug/m3)', 
                                  'NO2 (Daily maxiumum, ppb)', 'Ozone (Daily maximum, ppb)')) +
  theme(legend.position = c(0.85, 0.995)) + 
  scale_y_continuous(trans = 'log10', breaks = c(0.01, 0.1, 1, 1.0, 10)) + 
  theme(text = element_text(size = 18)) +
  theme(plot.caption = element_text(hjust = 0, size = 14), 
        plot.subtitle = element_text(size = 14))
