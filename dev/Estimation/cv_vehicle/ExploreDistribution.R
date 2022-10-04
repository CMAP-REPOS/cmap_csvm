
#Tablutates results of vehicle choice model


# setup -------------------------------------------------------------------
setwd("./dev/Estimation/cv_vehicle")

data <- readRDS('cv_vehicle_processed_data_test.rds')


# tabluation --------------------------------------------------------------

# 1=Passenger Car
# 2=Pick-up Truck (4 wheels)
# 3=Van (Cargo or Minivan) (4 wheels)
# 4=Sport Utility Vehicle (SUV) (4 wheels)
# 5=Single Unit 2-axle (6 wheels)
# 6=Single Unit 3-axle (10 wheels)
# 7=Single Unit 4-axle (14 wheels)
# 8=Semi (all Tractor-Trailer combinations)
# 888=Other


# 1=Returning to Base Location
# 2=Vehicle Maintenance (fuel, oil, etc)
# 3=Driver Needs (lunch, restroom, etc)
# 4=Deadhead/Drop Trailer/Bobtail
# 5=Delivering cargo
# 6=Picking up cargo
# 7=Getting Government Related Services
# 8=Providing Installation / Maintenance / Repair Services
# 9=Making a sales call
# 10=Providing professional services (legal, medical, financial)
# 11=Shopping for Business
# 888=Other"

data1 <- data %>% 
  select(SITE_TAZID, VEHNUM, TOUR_NUM, STOP_SEQ, OTAZ, DTAZ, STOP_ACTIVITY, VEH_CLASS, veh_choice, model_emp_cat, activity_return:activity_other) %>% 
  arrange(SITE_TAZID, VEHNUM, TOUR_NUM, STOP_SEQ)


tab <- data1 %>% 
  group_by(model_emp_cat, STOP_ACTIVITY, veh_choice) %>% 
  tally() %>% 
  mutate(veh_choice = case_when(veh_choice == 1 ~ 'Light',
                                veh_choice == 2 ~ 'Medium',
                                veh_choice == 3 ~ 'Heavy')) %>% 
  mutate(STOP_ACTIVITY = case_when(STOP_ACTIVITY %in% c(1, 2, 3, 4, 11) ~ 'Driver & Firm Needs/Return',
                                   STOP_ACTIVITY %in% c(5, 6) ~ 'Goods Delivery/Pickup',
                                   STOP_ACTIVITY %in% c(7) ~ 'Getting Gov. Services',
                                   STOP_ACTIVITY %in% c(8, 9 , 10) ~ 'Providing Services',
                                   STOP_ACTIVITY %in% c(888) ~ 'Other'))
tab2 <- tab %>% 
  group_by(model_emp_cat, STOP_ACTIVITY, veh_choice) %>% 
  summarise(n = sum(n)) %>% 
  group_by(model_emp_cat, STOP_ACTIVITY) %>% 
  summarise(veh_choice, n, p = n/sum(n)) %>% 
  arrange(STOP_ACTIVITY)



