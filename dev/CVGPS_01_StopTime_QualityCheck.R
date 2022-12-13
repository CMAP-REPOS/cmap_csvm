source("./dev/init_dev.R")
library(readxl)
library(tidyverse)
library(lubridate)
gps <- read_xlsx('dev/Data_Processed/CVGPS/TripsMediumLight_OD_ISPE83_SZPlusCounties_ValuesOnly_forRSG.xlsx')
SYSTEM_DEV_DATA_PATH <- file.path(SYSTEM_DEV_PATH, 'DATA_Processed')

#tours with only one trip are going to break things, instead
#trying different ways of summarizing stop time and tour time in CVGPS to avoid including last stop time
gps2 <- gps %>% 
  arrange(`Tour ID`,`Begin Date Time`) %>% 
  group_by(`Tour ID`) %>% 
  mutate(tour_leg = row_number()) %>% 
  ungroup() %>% 
  select(OBJECTID,DeviceId, `New Device?`, `Begin Tour - After Error Checking`, `Tour ID`, `Begin Date Time`, `end Date Time`,`Trip Duration`, tour_leg, 
         `Detention or Layover Time at Start Location from Previous Trip`
         ) %>%
  
  mutate(prevStop_duration = format(`Detention or Layover Time at Start Location from Previous Trip`, '%H:%M:%S'),
         trip_duration = format(`Trip Duration`, '%H:%M:%S')
  ) %>%
  mutate(hms_prevStopDur = lubridate::hms(prevStop_duration),
         hms_tripDuration = lubridate::hms(trip_duration)) %>% 
  mutate(prevstopDur_sec = period_to_seconds(hms_prevStopDur),
         traveltime_sec = period_to_seconds(hms_tripDuration)
  ) %>% 
  select(-c(prevStop_duration:hms_tripDuration)) %>%
  mutate(across(prevstopDur_sec:traveltime_sec, ~./60)) %>% 
  mutate(across(prevstopDur_sec:traveltime_sec, round, 1)) %>% 
  mutate(stopTime = lead(prevstopDur_sec)) %>% #compute a leading time to make it so that the stop time reflects the time spent after travel time
  mutate() %>% 
  mutate(beginTime = format(`Begin Date Time`, '%H'),
         endTime = format(`end Date Time`, '%H')
  )

gps2_1 <- gps2 %>% 
  mutate(TripDurationBins = if_else(stopTime > 720, 'Over 12h', 
                                    if_else(stopTime > 600, 'Over 10h',
                                            if_else(stopTime > 480, 'Over 8h',
                                                    if_else(stopTime > 300, 'Over 5h', 'Good')
                                                    )
                                            )
                                    )
  )

#for each tour id, chooses the highest value for tour_leg as the number of trips on that tour
TripsOnTour <- gps2_1 %>% 
  group_by(`Tour ID`) %>% 
  slice(which.max(tour_leg)) %>%
  ungroup() %>% 
  select(`Tour ID`, TripsOnTour = tour_leg)

gps2_1 <- gps2_1 %>% 
  left_join(TripsOnTour, by = 'Tour ID')

#creating some rules to create NA stop times where likely an overnight stop
gps3 <- gps2_1 %>% 
  mutate(BeginTime = as.integer(beginTime)) %>% 
  mutate(endTime = as.integer(endTime)) %>% 
  
  mutate(stopTime = if_else((TripDurationBins %in% c('Over 5h', 'Over 8h', 'Over 10h', 'Over 12h') & 
                                endTime %in% c(22, 23, 0, 1, 2, 3, 4)) | 
                              #if the "trip began at night & is over 5 hours
                              
                              (TripDurationBins %in% c('Over 8h', 'Over 10h', 'Over 12h') & 
                                 TripsOnTour == 1) & (!endTime %in% c(6,7,8,9,10, 11, 12, 13, 14)) | 
                              #if a trip on a 1-trip tour & over 8 hours & didnt start during working during a reasonable time for an 8hour shift
                              
                              (TripDurationBins %in% c('Over 5h', 'Over 8h', 'Over 10h', 'Over 12h') & 
                                 tour_leg == TripsOnTour),
                            
                            NA_real_,
                            stopTime)) %>% 
                            #if the last trip of the tour and is long
  
  rename(StopDepartHour = beginTime, StopArrivalHour = endTime) %>% 
  select(`New Device?`, `Begin Tour - After Error Checking`, `Tour ID`, tour_leg,prevstopDur_sec:TripsOnTour)


gps3_2 <- gps3 %>% 
  group_by(`Tour ID`) %>% 
  mutate(TTravTime = sum(traveltime_sec),
         TStopTime = sum(stopTime, na.rm = T),
         TTourTIme = TTravTime + TStopTime) %>% 
  ungroup()

# gps_GB_4 <- gps_GB_3 %>% 
#   left_join(TourTime4)
# 
# 
# 
# 
# #appending arrival time at first stop
# tour_firsttripstart <- gps_GB_4 %>% 
#   group_by(`Tour ID`) %>% 
#   slice(which.min(tour_leg)) %>% 
#   select(`Tour ID`, 
#          FirstArrivalTime = `end Date Time`)
# 
# tour_firsttripstart2 <- tour_firsttripstart %>% 
#   distinct() %>% 
#   mutate(HoursAfterMidnight = format(FirstArrivalTime, format = '%H:%M:%S')) %>% 
#   ungroup() %>% 
#   mutate(minutesAfterMidnight = lubridate::hms(HoursAfterMidnight)) %>% 
#   ungroup() %>% 
#   mutate(FirstStop_MaM = round((period_to_seconds(minutesAfterMidnight)/60), 0)) %>% 
#   select(`Tour ID`, FirstStop_MaM)