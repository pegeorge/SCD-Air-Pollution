# 
##    FROM RAW (ALMOST) TO FINAL DATABASE TO BE USED
# 
#    this is the code that creates the final dataframe to be used in the air pollution population wide analysis
#         starts with raw dataframes, then creates variables, then does exclusion criteria 


#    update (not exactly from the raw databases, I do include the code for reference, but I start with 
#         databases that added FIPS and geoAddress )


#    includes 2018



# ---------------------------------------

library(gt)
library(pander)
library(lubridate)
library(ggplot2)
library(psych)
library(formattable)
library(tidyverse)


rm(list=ls())

#####           Set the working directory where the original dataset is located
setwd("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/")
list.files()

#####           Load the datasets
SCD_raw_2010_2014 <- read.csv("DR_77_2010_2014.csv")
SCD_raw_2015_2018 <- read.csv("DR_77_2015_2018.csv")


####            Create new datasets to be manipulated            ---------------------------------------
#df with everything 
SCD_all <- bind_rows(SCD_raw_2010_2014, SCD_raw_2015_2018)
head(SCD_all, 3)

 
#   first, change date format to date and times                  ---------------------------------------
SCD_all <- SCD_all %>% 
  mutate(adm_date = mdy(adm_date), 
         bmt_date = mdy(bmt_date), 
         Splenectomy_date = mdy(Splenectomy_date), 
         deceased_date = mdy(deceased_date),
         dsch_date = mdy(dsch_date), 
         dob = mdy(dob))

typeof(SCD_all$age_at_death)
head(SCD_all$bmt_date)


#CREATE NEW VARIABLES     ---------------------------------------------------------------------------------------------------------------------


#######        create age_at_visit and age_at_death variable 
SCD_all <- SCD_all %>% 
  mutate(age_at_visit = (adm_date - dob)) 

SCD_all$age_at_visit <- format(round(as.numeric(SCD_all$age_at_visit / 365.25), 1), nsmall = 1)
SCD_all$age_at_death <- format(round(as.numeric(SCD_all$age_at_death), 1), nsmall = 1)

head(SCD_all$age_at_death)
typeof(SCD_all$age_at_death)

SCD_all$age_at_visit <- as.numeric(SCD_all$age_at_visit)
SCD_all$age_at_death <- as.numeric(SCD_all$age_at_death)

summary(SCD_all$age_at_death)





#######      next, create deceased (yes vs no) which is needed for properly excluding patients later on
SCD_all <- SCD_all %>% 
  mutate(deceased_yn = ifelse(is.na(deceased_date) == TRUE, 0, 1))

table(SCD_all$deceased_yn)




#######         next, create a loss to follow up (LTFU) variable    


#create variable of time between visits
SCD_all <- SCD_all %>% 
  group_by(corp_id) %>% 
  arrange(corp_id, adm_date) %>% 
  mutate(visit_number = seq_along(adm_date), 
         last_visit=last(adm_date), 
         age_at_last_visit = last(adm_date) - dob)

SCD_all$age_at_last_visit <- format(round(as.numeric(SCD_all$age_at_last_visit / 365.25), 1), nsmall = 1)
typeof(SCD_all$age_at_last_visit)
SCD_all$age_at_last_visit = as.numeric(SCD_all$age_at_last_visit)


checkdf <- SCD_all %>% select(adm_date, dob, age_at_visit, age_at_death, corp_id, visit_number, age_at_last_visit, visit_number_max)
checkdf <- checkdf[1:10000,]


#create variable that shows max visit_number
SCD_all <- SCD_all %>% 
  group_by(corp_id) %>% 
  mutate(visit_number_max = max(visit_number))


#create a time_since and time_since_max variable
placeholder_date <- as.Date("2019-01-01")


SCD_all <- SCD_all %>% 
  group_by(corp_id) %>% 
  arrange(corp_id, adm_date) %>% 
  mutate(time_since = ifelse(visit_number_max == 1, placeholder_date - adm_date, 
                             ifelse(visit_number < visit_number_max, adm_date-lag(adm_date), placeholder_date - adm_date))) 

#replace NA with -999 (for use later in coding, can always move back to NA if needed)

SCD_all <- SCD_all %>% 
  replace_na(list(time_since = -999))

SCD_all <- SCD_all %>% 
  mutate(time_since = ifelse((time_since != -999), time_since /365, time_since ))
SCD_all$time_since <- as.numeric(SCD_all$time_since)

typeof(SCD_all$time_since)


SCD_all <- SCD_all %>% 
  group_by(corp_id) %>% 
  mutate(time_since_max = max(time_since))
SCD_all$time_since_max <- as.numeric(SCD_all$time_since_max)

SCD_all$time_since <- formattable(SCD_all$time_since, digits = 1, format = "f")
SCD_all$time_since_max <- formattable(SCD_all$time_since_max, digits = 1, format = "f")

typeof(SCD_all$time_since)
typeof(SCD_all$visit_number_max)



#####          create LTFU variable        ########

#create a time_since and time_since_max variable
placeholder_date <- as.Date("2019-01-01")
placeholder_date_2 <- as.Date("2018-01-01")

SCD_all <- SCD_all %>% 
  mutate(LTFU = ifelse(time_since > 1.1, 1, 
                       ifelse((visit_number == visit_number_max) & 
                                (age_at_last_visit < 17) & 
                                (placeholder_date_2 > last_visit) & 
                                (deceased_yn == 0), 1, 0))) 

SCD_all <- SCD_all %>% replace_na(list(LTFU = 0))

checkdf <- SCD_all %>% select(adm_date, dob, age_at_visit, age_at_death, corp_id, visit_number, age_at_last_visit, visit_number_max, LTFU)
checkdf <- checkdf[1:10000,]


table(SCD_all$LTFU)




########        create an unknown address variable              ####



SCD_all <- SCD_all %>% 
  mutate(unknown_addr = ifelse(address_at_visit == "Unknown", 1, 
                               ifelse(address_at_visit == "unknown", 1,
                                      ifelse(address_at_visit == "NULL", 1,
                                             ifelse(address_at_visit == "", 1, 
                                                    ifelse(str_detect(address_at_visit, "Georgia Crime Victims") == 1, 1, 
                                                           ifelse(str_detect(address_at_visit, "COBB COUNTY DFCS") == 1, 1, 
                                                                  ifelse(str_detect(address_at_visit, "unknown") == 1, 1, 0))))))))

SCD_all <- SCD_all %>% 
  mutate(PO_box_addr =  ifelse(str_detect(address_at_visit, "PO BOX") == 1, 1, 
                               ifelse(str_detect(address_at_visit, "PO Box") == 1, 1, 
                                      ifelse(str_detect(address_at_visit, "PO box") == 1, 1,
                                             ifelse(str_detect(address_at_visit, "Po Box") == 1, 1, 
                                                    ifelse(str_detect(address_at_visit, "po box") == 1, 1,
                                                           ifelse(str_detect(address_at_visit, "P O BOX") == 1, 1,  
                                                                  ifelse(str_detect(address_at_visit, "P O Box") == 1, 1,
                                                                         ifelse(str_detect(address_at_visit, "P.O. BOX") == 1, 1,
                                                                                ifelse(str_detect(address_at_visit, "p o box") == 1, 1,
                                                                                       ifelse(str_detect(address_at_visit, "p.o box") == 1, 1,
                                                                                              ifelse(str_detect(address_at_visit, "P. O. Box") == 1, 1, 
                                                                                                     ifelse(str_detect(address_at_visit, "P.O. Box") == 1, 1,
                                                                                                            ifelse(str_detect(address_at_visit, "P.O Box") == 1, 1, 0))))))))))))))




rm(SCD_raw_2010_2014, SCD_raw_2015_2018)

###################################################################################################################################



# note, the above code is for reference, it creates the same dataframe as the one I'm about to load below
# but the one I load below actually includes the geocoded API (which takes time and $ to create, so can't be constantly re-doing it)



###################################################################################################################################


setwd("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/")

rm(list = ls())

load('Saved R Dataframes/SCD_all_3.Rda')

#create a time_since and time_since_max variable
placeholder_date <- as.Date("2019-01-01")
placeholder_date_2 <- as.Date("2018-01-01")

ls(SCD_all_3)
table(SCD_all_3$LTFU)


#below is in case I need to adjust the LTFU variable 
SCD_all_3 <- SCD_all_3 %>% 
  mutate(LTFU = ifelse(time_since > 1.1, 1,  
                       ifelse((visit_number == visit_number_max) & 
                                (age_at_last_visit < 17) & 
                                (placeholder_date_2 > last_visit) & 
                                (deceased_yn == 0), 1, 0))) 



#  NOW, TO EXCLUDE PATIENTS WHO DO NOT FIT INCLUSION/EXCLUSION CRITERIA      -----------------------------------------------------------------------------------


#focused df, removes a lot of variables I don't need at this time (see below)
SCD_focused <- SCD_all_3 %>% select(corp_id, dob, sex, genotype, deceased_date, age_at_death,
                                  Race, Ethnic_Group, primary_campus, facility, 
                                  pat_class, ed, ICU_YN, adm_date, dsch_date, 
                                  age_at_visit, age_at_last_visit, LTFU,
                                  visit_number, visit_number_max, time_since, time_since_max,  
                                  Insurance_at_visit, address_at_visit, address, 
                                  PO_box_addr, unknown_addr, geoAddress, fips,
                                  city, state, zip, HEMOGLOBIN) %>% ungroup() 




#####  filters out dates that are not within my time frame (note, because I am looking at ER visits, I care about adm_date (admission))

SCD_focused <- SCD_focused %>% filter(adm_date > "2009-12-31" & adm_date < "2019-01-01")
SCD_focused %>%  summarize(Count = n())
n_distinct(SCD_focused$corp_id)


summary(SCD_focused$age_at_death)
head(SCD_focused$age_at_visit)

SCD_focused$age_at_visit <- as.numeric(SCD_focused$age_at_visit)
SCD_focused$age_at_death <- as.numeric(SCD_focused$age_at_death)




#### now, filters out only ED visits


SCD_focused_ED <- SCD_focused %>% filter(ed == 1)
SCD_focused_ED %>%  summarize(Count = n())
n_distinct(SCD_focused_ED$corp_id)


######   filter out based on age (age < 1 at visit vs age > 18 at visit)


summary(SCD_focused_ED$age_at_visit)


SCD_focused_ED <- SCD_focused_ED %>% 
  filter(age_at_visit > 0.9 & age_at_visit < 18.1)

SCD_focused_ED %>%  summarize(Count = n())
n_distinct(SCD_focused_ED$corp_id)



#####  next, filter out people with addresses that are unknown or outside 20 (or 10 or 5) miles  


#remove dataframes I dont need bc next part can be slow
rm(SCD_focused, SCD_all, SCD_all_2, SCD_all_3)

table(SCD_focused_ED$PO_box_addr)
table(SCD_focused_ED$unknown_addr)

unknown_addr_df = SCD_focused_ED %>% filter(unknown_addr == 1)
PO_boxx_addr_df = SCD_focused_ED %>% filter(PO_box_addr == 1)

n_distinct(unknown_addr_df$corp_id)
n_distinct(PO_boxx_addr_df$corp_id)

# load the census tract dataset
FIPS_20_mile <- read.csv('FIPS databases/CHOA_20_mile_census_tract.csv')
FIPS_10_mile <- read.csv('FIPS databases/CHOA_10_mile_census_tract.csv')
FIPS_5_mile <- read.csv('FIPS databases/CHOA_5_mile_census_tract.csv')

# make sure both are numeric 
typeof(FIPS_20_mile$GEOID)
typeof(SCD_focused_ED$fips)

SCD_focused_ED <- SCD_focused_ED %>% 
  mutate(fips_num = as.numeric(fips))

typeof(SCD_focused_ED$fips_num)

# drop NA columns 
SCD_focused_ED <- SCD_focused_ED %>% drop_na(fips_num)

#create new empty databases to be filled in below
SCD_focused_ED_20_mile <- SCD_focused_ED[0,]
SCD_focused_ED_5_mile <- SCD_focused_ED[0,]
SCD_focused_ED_10_mile <- SCD_focused_ED[0,]



i <- 1
j <- 1

for (i in 1:nrow(FIPS_5_mile)) {
  out <- paste0("working on i = ", i)
  print(out)
  for (j in 1:nrow(SCD_focused_ED)) {
    if (FIPS_5_mile$GEOID[i] == SCD_focused_ED$fips_num[j]) 
    {
      SCD_focused_ED_5_mile <- rbind(SCD_focused_ED_5_mile, SCD_focused_ED[j,])
    } 
    else {NULL}
  }
}


for (i in 1:nrow(FIPS_10_mile)) {
  out <- paste0("working on i = ", i)
  print(out)
  for (j in 1:nrow(SCD_focused_ED)) {
    if (FIPS_10_mile$GEOID[i] == SCD_focused_ED$fips_num[j]) 
    {
      SCD_focused_ED_10_mile <- rbind(SCD_focused_ED_10_mile, SCD_focused_ED[j,])
    } 
    else {NULL}
  }
}


for (i in 1:nrow(FIPS_20_mile)) {
  out <- paste0("working on i = ", i)
  print(out)
  for (j in 1:nrow(SCD_focused_ED)) {
    if (FIPS_20_mile$GEOID[i] == SCD_focused_ED$fips_num[j]) 
    {
      SCD_focused_ED_20_mile <- rbind(SCD_focused_ED_20_mile, SCD_focused_ED[j,])
    } 
    else {NULL}
  }
}




SCD_focused_ED_20_mile %>%  summarize(Count = n())
n_distinct(SCD_focused_ED_20_mile$corp_id)




#####    filter out based on LTFU

SCD_focused_ED_20_mile <- SCD_focused_ED_20_mile %>% 
  filter(LTFU == 0)

SCD_focused_ED_20_mile %>%  summarize(Count = n())
n_distinct(SCD_focused_ED_20_mile$corp_id)


SCD_focused_ED_10_mile <- SCD_focused_ED_10_mile %>% 
  filter(LTFU == 0)

SCD_focused_ED_5_mile <- SCD_focused_ED_5_mile %>% 
  filter(LTFU == 0)



save(SCD_focused_ED_20_mile, file = 'Saved R Dataframes/ED_DF_Finalized/SCD_focused_ED_20_mile_2018.rda')
save(SCD_focused_ED_10_mile, file = 'Saved R Dataframes/ED_DF_Finalized/SCD_focused_ED_10_mile_2018.rda')
save(SCD_focused_ED_5_mile, file = 'Saved R Dataframes/ED_DF_Finalized/SCD_focused_ED_5_mile_2018.rda')




#####    NEXT, WE WILL CONVERT TO ED VISITS/DAY (which is the outcome variable of the first analysis)    -------------------------------

rm(list = ls())

load("Saved R Dataframes/ED_DF_Finalized/SCD_focused_ED_20_mile_2018.rda")
load("Saved R Dataframes/ED_DF_Finalized/SCD_focused_ED_10_mile_2018.rda")
load("Saved R Dataframes/ED_DF_Finalized/SCD_focused_ED_5_mile_2018.rda")



#here, we will count the number of events per day, 20 mile
tab_20m <- table(cut(SCD_focused_ED_20_mile$adm_date, 'day'))
head(tab_20m)

SCD_ER_count_20m_df <- data.frame(Date=format(as.Date(names(tab_20m)), '%d/%m/%Y'), 
                                  Frequency = as.vector(tab_20m))

head(SCD_ER_count_20m_df)
typeof(SCD_ER_count_20m_df$Date)

#again, use lubridate to change the date column to data format
SCD_ER_count_20m_df_date <- SCD_ER_count_20m_df %>% 
  mutate(Date = dmy(Date))

typeof(SCD_ER_count_20m_df_date$Date)
head(SCD_ER_count_20m_df_date)

#initial stats
describe(SCD_ER_count_20m_df_date$Frequency)
summary(SCD_ER_count_20m_df$Frequency)



#here, we will count the number of events per day
tab_10m <- table(cut(SCD_focused_ED_10_mile$adm_date, 'day'))
head(tab_10m)

SCD_ER_count_10m_df <- data.frame(Date=format(as.Date(names(tab_10m)), '%d/%m/%Y'), 
                                  Frequency = as.vector(tab_10m))

head(SCD_ER_count_10m_df)
typeof(SCD_ER_count_10m_df$Date)

#again, use lubridate to change the date column to data format
SCD_ER_count_10m_df_date <- SCD_ER_count_10m_df %>% 
  mutate(Date = dmy(Date))

typeof(SCD_ER_count_10m_df_date$Date)
head(SCD_ER_count_10m_df_date)

#initial stats
describe(SCD_ER_count_10m_df_date$Frequency)
summary(SCD_ER_count_10m_df$Frequency)





#######    #here, we will count the number of events per day, 5 mile


tab_5m <- table(cut(SCD_focused_ED_5_mile$adm_date, 'day'))
head(tab_5m)

SCD_ER_count_5m_df <- data.frame(Date=format(as.Date(names(tab_5m)), '%d/%m/%Y'), 
                                  Frequency = as.vector(tab_5m))

head(SCD_ER_count_5m_df)
typeof(SCD_ER_count_5m_df$Date)

#again, use lubridate to change the date column to data format
SCD_ER_count_5m_df_date <- SCD_ER_count_5m_df %>% 
  mutate(Date = dmy(Date))

typeof(SCD_ER_count_5m_df_date$Date)
head(SCD_ER_count_5m_df_date)

#initial stats
describe(SCD_ER_count_5m_df_date$Frequency)
summary(SCD_ER_count_5m_df$Frequency)


save(SCD_ER_count_5m_df_date, file = "C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/ED_DF_Finalized/SCD_ER_count_5m_df_2018.rda")
save(SCD_ER_count_10m_df_date, file = "C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/ED_DF_Finalized/SCD_ER_count_10m_df_2018.rda")
save(SCD_ER_count_20m_df_date, file = "C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/Saved R Dataframes/ED_DF_Finalized/SCD_ER_count_20m_df_2018.rda")


