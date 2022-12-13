#Takes Prepared GPS data from previous Script and creates tour stop characteristics in 
#correct format (time bin dummies)


# Setup -------------------------------------------------------------------

source("./dev/init_dev.R")
library(readxl)
library(tidyverse)
library(rbin)
SYSTEM_DEV_DATA_PATH <- file.path(SYSTEM_DEV_PATH, 'DATA_Processed')

file <- file.path(SYSTEM_DEV_PATH, 'Data_Processed', 'CVGPS', 'ToursTripsCharacteristics_Centroid.csv')
GPS <- read_csv(file) %>% 
  filter(!is.na(currentStopTime_min))


# adding binned stop time dummy
GPS2 <- GPS %>% 
  mutate(av_15 = ifelse(currentStopTime_min >= 0 & currentStopTime_min <= 15, 1, if_else(is.na(currentStopTime_min), NA_character_, '0')),
         av_30 = ifelse(currentStopTime_min > 15 & currentStopTime_min <= 30, 1, if_else(is.na(currentStopTime_min), NA_character_, '0')),
         av_45 = ifelse(currentStopTime_min > 30 & currentStopTime_min <= 45, 1, if_else(is.na(currentStopTime_min), NA_character_, '0')),
         av_60 = ifelse(currentStopTime_min > 45 & currentStopTime_min <= 60, 1, if_else(is.na(currentStopTime_min), NA_character_, '0')),
         av_75 = ifelse(currentStopTime_min > 60 & currentStopTime_min <= 75, 1, if_else(is.na(currentStopTime_min), NA_character_, '0')),
         av_90 = ifelse(currentStopTime_min > 75 & currentStopTime_min <= 90, 1, if_else(is.na(currentStopTime_min), NA_character_, '0')),
         av_150 = ifelse(currentStopTime_min > 90 & currentStopTime_min <= 150, 1, if_else(is.na(currentStopTime_min), NA_character_, '0')),
         av_210 = ifelse(currentStopTime_min > 150 & currentStopTime_min <= 210, 1, if_else(is.na(currentStopTime_min), NA_character_, '0')),
         av_270 = ifelse(currentStopTime_min > 210 & currentStopTime_min <= 270, 1, if_else(is.na(currentStopTime_min), NA_character_, '0')),
         av_390 = ifelse(currentStopTime_min> 270 & currentStopTime_min <= 390, 1, if_else(is.na(currentStopTime_min), NA_character_, '0')),
         av_600 = ifelse(currentStopTime_min > 390 & currentStopTime_min <= 600, 1, if_else(is.na(currentStopTime_min), NA_character_, '0')),
         av_600p = ifelse(currentStopTime_min > 600, 1, if_else(is.na(currentStopTime_min), NA_character_, '0'))
         )

GPS3 <- GPS2 %>% 
  mutate(lv_15 = ifelse(av_15 == 1 & VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.', 1, 0),
         lv_30 = ifelse(av_30 == 1 & VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.', 1, 0),
         lv_45 = ifelse(av_45 == 1 & VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.', 1, 0),
         lv_60 = ifelse(av_60 == 1 & VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.', 1, 0),
         lv_75 = ifelse(av_75 == 1 & VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.', 1, 0),
         lv_90 = ifelse(av_90 == 1 & VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.', 1, 0),
         lv_150 = ifelse(av_150 == 1 & VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.', 1, 0),
         lv_210 = ifelse(av_210 == 1 & VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.', 1, 0),
         lv_270 = ifelse(av_270 == 1 & VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.', 1, 0),
         lv_390 = ifelse(av_390 == 1 & VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.', 1, 0),
         lv_600 = ifelse(av_600 == 1 & VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.', 1, 0),
         lv_600p = ifelse(av_600p == 1 & VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.', 1, 0),
         
         mv_15 = ifelse(av_15 == 1 & VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.', 1, 0),
         mv_30 = ifelse(av_30 == 1 & VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.', 1, 0),
         mv_45 = ifelse(av_45 == 1 & VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.', 1, 0),
         mv_60 = ifelse(av_60 == 1 & VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.', 1, 0),
         mv_75 = ifelse(av_75 == 1 & VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.', 1, 0),
         mv_90 = ifelse(av_90 == 1 & VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.', 1, 0),
         mv_150 = ifelse(av_150 == 1 & VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.', 1, 0),
         mv_210 = ifelse(av_210 == 1 & VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.', 1, 0),
         mv_270 = ifelse(av_270 == 1 & VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.', 1, 0),
         mv_390 = ifelse(av_390 == 1 & VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.', 1, 0),
         mv_600 = ifelse(av_600 == 1 & VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.', 1, 0),
         mv_600p = ifelse(av_600p == 1 & VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.', 1, 0)
         )



# New Summaries -----------------------------------------------------------
colsums_av <- GPS3 %>% 
  summarise(across(av_15:av_600, ~sum(as.numeric(.x)))) %>% 
  mutate(total = sum(across(av_15:av_600, na.rm = T)))

colsums_lv <- GPS3 %>% 
  summarise(across(lv_15:lv_600, ~sum(as.numeric(.x)))) %>% 
  mutate(total = sum(across(lv_15:lv_600, na.rm = T)))

colsums_mv <- GPS3 %>% 
  summarise(across(mv_15:mv_600, ~sum(as.numeric(.x)))) %>% 
  mutate(total = sum(across(mv_15:mv_600, na.rm = T)))


