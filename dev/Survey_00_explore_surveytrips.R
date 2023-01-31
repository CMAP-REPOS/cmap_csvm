#Explores Survey data to evaluate its utility in Stops estimation

#Trips x Activity 
#Activity x Vehicle Type
#Industry of Operator x Activity 


# Setup -------------------------------------------------------------------

("./dev/init_dev.R")
THIS_MODEL_PATH <- file.path("./dev/Estimation/cv_stops")
library(readxl)
library(tidyverse)



# Load Data ---------------------------------------------------------------

trips <- read_excel('dev/Data_Processed/Survey/CMAP data for RSG 20220328 v1.0.xlsx', sheet = 3)
driver <- read_excel('dev/Data_Processed/Survey/CMAP data for RSG 20220328 v1.0.xlsx', sheet = 2)
est <- read_excel('dev/Data_Processed/Survey/CMAP data for RSG 20220328 v1.0.xlsx', sheet = 1)




# Selecting Needed Data -----------------------------------------------------
##These are the trip characteristics

trips_2 <- trips %>% 
  select(DRVR_telkey ,
         Activity = QPLACE_6,
         Activity_other = AQPLACE_6_DD)

###Joining to Employer via Driver Data
driver <- read_excel('dev/Data_Processed/Survey/CMAP data for RSG 20220328 v1.0.xlsx', sheet = 2)
driver_2 <- driver %>% 
  select(DRVR_telkey, 
         EMP_telkey, 
         Vehicle = Q9_NEW_DROP, 
         Vehicle_Other = AQ9_NEW_OTHER,
         OvernightFacility = Q04)

###joins driver to trips
trips_driver <- trips_2 %>% 
  left_join(driver_2, by = 'DRVR_telkey')





# Summarizing the Data ----------------------------------------------------
#Trips x Activity - 
#Activity x Vehicle Type
#Industry x Activity 

trips_driver_2 <- trips_driver %>% 
  mutate(activity_cat = case_when(
    Activity %in% c(1,2,3,4,11) ~ 'Mainetenance_Driver_BusinessActivity',
    Activity %in% c(5,6) ~ 'Goods_DropoffPickup',
    Activity %in% c(7,8,9,10,11) ~ 'Services_GettingProviding', #'Getting Government Provided Services maybe goes in business activities?
    Activity == 18 ~ 'Other'
  )) %>% 
  mutate(vehicle_cat = case_when(
    Vehicle %in% c(1,2,3,4) ~ 'Light',
    Vehicle %in% c(5,6,7) ~ 'Single Unit',
    Vehicle == 8 ~ 'Semi',
    Vehicle == 9 ~ 'Other',
    !is.na(Vehicle_Other) ~ 'Other'
  )) %>% 
  mutate(vehicle_cat = case_when(
    grepl('Single Unit', Vehicle_Other) ~  'Single Unit',
    T ~ vehicle_cat
    )) %>% 
  mutate(VehicleStationed_cat = case_when(
    OvernightFacility %in% c(1,2) ~ 'Office_Gov',
    OvernightFacility %in% c(3,12,14) ~ 'Retail_GasStation_Food',
    OvernightFacility %in% c(4,8,9,10,11,13,17) ~ 'Industry_Transport_Logistics', #includes truck stop and mining
    OvernightFacility %in% c(5,6) ~ 'Health_Education',
    OvernightFacility ==16 ~ 'Agriculture',
    OvernightFacility == 15 ~ 'ConstructionSite',
    OvernightFacility == 7 ~ 'Residence/Home',
    OvernightFacility == 18 ~ 'Other',
    OvernightFacility == 99 ~ 'DontKnow'
  ))





# Crosstabs ---------------------------------------------------------------
#Trips by Activity (based on Trips:QPLACE_6)
#Could also calculate via Driver:Q6 (vehicle primary business use)
trips_driver_2 %>% 
  group_by(activity_cat) %>%
  count() %>% 
  as.tibble()
#most in other are service trips i would say 


#Trips by Vehicle Type
trips_driver_2 %>% 
  group_by(vehicle_cat) %>%
  count() %>% 
  as.tibble() 



#Trips by Vehcile Overnight Facility
trips_driver_2 %>% 
  group_by(VehicleStationed_cat) %>%
  count() %>% 
  as.tibble() 







