

# Setup -------------------------------------------------------------------
source("./dev/init_dev.R")
library(readxl)
library(tidyverse)
gps <- read_xlsx('dev/Data_Processed/CVGPS/TripsMediumLight_OD_ISPE83_SZPlusCounties_ValuesOnly_forRSG.xlsx')
SYSTEM_DEV_DATA_PATH <- file.path(SYSTEM_DEV_PATH, 'DATA_Processed')


# Classifying Vehicle Base ------------------------------------------------
#for trips that are tour start, count subzone tally
OriginStop_byID <- gps %>%
  filter(`Begin Tour - After Error Checking` == TRUE) %>% 
  group_by(DeviceId, Origin_SZ17PlusCounties) %>% 
  summarise(n = n())

#for that tally, choose the subzone with the highest tally
Home_sTAZ_origin <- OriginStop_byID %>% 
  group_by(DeviceId) %>% 
  slice(which.max(n))


#If tally is greater than 1, it is the vehicle base
ClearHome <- Home_sTAZ_origin %>% 
  filter(n > 1) %>% 
  select(DeviceId, Origin_SZ17PlusCounties)



#if tally is not greater than one, then no clear base
noClearHome <- Home_sTAZ_origin %>% 
  filter(n == 1)

noClearHome_full <- gps %>% 
  filter(DeviceId %in% noClearHome$DeviceId) %>%
  filter()



#if only one tour then the starting zone of the tour is the home subzone
oneTour <- noClearHome_full %>% 
  group_by(DeviceId) %>% 
  summarise(n = n_distinct(`Tour ID`)) %>% 
  filter(n == 1)

oneTour_full <- gps %>% 
  filter(DeviceId %in% oneTour$DeviceId)

OneTour_home <- oneTour_full %>% 
  filter(`Begin Tour - After Error Checking` == TRUE) %>% 
  select(DeviceId, Origin_SZ17PlusCounties)



#All the records with a base subzone
HomeIds_goodBase <- OneTour_home %>% 
  rbind(ClearHome) %>% 
  select(DeviceId,
         BaseSubzone = Origin_SZ17PlusCounties)


#all the records without a clear home subzone
MultipleTours_noClearHome <- noClearHome_full %>% 
  filter(!(DeviceId %in% HomeIds_goodBase$DeviceId))


#creating the gps data that will be used for further classification, only those with a base
GPS_goodBase <- gps %>% 
  right_join(HomeIds_goodBase, by = 'DeviceId')

rm(gps)
















# Trip/Tour Characteristics and Classification ---------------------------


#home subzone and destination subzone type- internal or external
gps_GB_1 <- GPS_goodBase %>% 
  mutate(HOMESubzone_TYPE = case_when(as.integer(substr(BaseSubzone,1,1)) == 9 & nchar(BaseSubzone) == 6 ~ 'COUNTY',
                                  is.na(BaseSubzone) ~ NA_character_,
                                  T ~ 'TAZ')) %>%
  mutate(DSubzone_TYPE = case_when(as.integer(substr(Destination_SZ17PlusCounties,1,1)) == 9 & nchar(Destination_SZ17PlusCounties) == 6 ~ 'COUNTY',
                               is.na(Destination_SZ17PlusCounties) ~ NA_character_,
                               T ~ 'TAZ')) %>% 
  mutate(HOMESubzone_type = case_when(HOMESubzone_TYPE == 'TAZ' ~ 'I',
                               is.na(HOMESubzone_TYPE) ~ NA_character_,
                               T ~ 'X')) %>% 
  mutate(DESTSubzone_type = case_when(DSubzone_TYPE == 'TAZ' ~ 'I',
                               is.na(DSubzone_TYPE) ~ NA_character_,
                               T ~ 'X')) %>% 
  mutate(InternalExternal = paste0(HOMESubzone_type, DESTSubzone_type))



#appending home subzone county
##reading in subzone data

SYSTEM_DEV_DATA_PATH <- file.path(SYSTEM_DEV_PATH, 'DATA_Processed')
sz <- read_sf(file.path(SYSTEM_DEV_PATH,'Data_Processed','TAZ', 'subzones17.shp'))

zone_dict2 <- sz %>% 
  select(subzone17, zone17, county_nam, state) %>% 
  st_drop_geometry() %>% 
  mutate(county_state = paste0(county_nam, ', ', state))

gps_GB_1 <- gps_GB_1 %>% 
  left_join(zone_dict2, by = c('BaseSubzone' = 'subzone17'))

gps_GB_1 <- gps_GB_1 %>% 
  rename(base_Zone = zone17,
         base_county = county_state)



#classifying as single or multistop

TourID_count <- gps_GB_1 %>% 
  group_by(`Tour ID`) %>% 
  count() %>% 
  mutate(single_multi = case_when(n == 1 ~ 'single',
                                  n > 1 ~ 'multi',
                                  T ~ NA_character_)) %>% 
  select(`Tour ID`, single_multi)


gps_GB_1 <- gps_GB_1 %>% 
  left_join(TourID_count, by = 'Tour ID')




#appending a tour leg

gps_GB_1 <- gps_GB_1 %>% 
  arrange(`Tour ID`,`Begin Date Time`) %>% 
  group_by(`Tour ID`) %>% 
  mutate(tour_leg = row_number())




#Classifying: Is Tour Start/Tour End == BaseSubzone

ClassifyStartEnd <- gps_GB_1 %>% 
  select(BaseSubzone, `Tour ID`, tour_leg, Origin_SZ17PlusCounties, Destination_SZ17PlusCounties)

ClassifyStart <- ClassifyStartEnd %>% 
  group_by(`Tour ID`) %>% 
  slice(which.min(tour_leg)) %>% 
  mutate(TourStartisBase = case_when(Origin_SZ17PlusCounties == BaseSubzone ~ '1',
                                     (Origin_SZ17PlusCounties != BaseSubzone &
                                       !is.na(Origin_SZ17PlusCounties)) ~ '0',
                                     is.na(Origin_SZ17PlusCounties) ~ 'Missing_Origin')
         )

ClassifyEnd <- ClassifyStartEnd %>% 
  group_by(`Tour ID`) %>% 
  slice(which.max(tour_leg)) %>% 
  mutate(TourEndisBase = case_when(Destination_SZ17PlusCounties == BaseSubzone ~ '1',
                                   (Destination_SZ17PlusCounties != BaseSubzone & 
                                     !is.na(Destination_SZ17PlusCounties)) ~ '0',
                                   is.na(Destination_SZ17PlusCounties) ~ 'Missing_dest')
         )

TourStartEndClass <- ClassifyStart %>% 
  full_join(ClassifyEnd, by = 'Tour ID') %>% 
  select(`Tour ID`, TourStartisBase, TourEndisBase)

gps_GB_2 <- gps_GB_1 %>% 
  left_join(TourStartEndClass, by = 'Tour ID')



#classifying: tour type

gps_GB_2 <- gps_GB_2 %>% 
  mutate(TourType = case_when(single_multi == 'single' & TourStartisBase == '1' & TourEndisBase == '1' ~ 'BBS',
                              single_multi == 'single' & TourStartisBase == '1' & TourEndisBase == '0' ~ 'BNS',
                              single_multi == 'single' & TourStartisBase == '0' & TourEndisBase == '1' ~ 'NBS',
                              single_multi == 'single' & TourStartisBase == '0' & TourEndisBase == '0' ~ 'NNS',
                              single_multi == 'multi' & TourStartisBase == '1' & TourEndisBase == '1' ~ 'BBM',
                              single_multi == 'multi' & TourStartisBase == '1' & TourEndisBase == '0' ~ 'BNM',
                              single_multi == 'multi' & TourStartisBase == '0' & TourEndisBase == '1' ~ 'NBM',
                              single_multi == 'multi' & TourStartisBase == '0' & TourEndisBase == '0' ~ 'NNM',
                              T ~ NA_character_)
  )






#Appending number of stops on tour

Tour_numstops <- gps_GB_2 %>% 
  group_by(`Tour ID`) %>% 
  slice(which.max(tour_leg)) %>% 
  select(`Tour ID`, Trips = tour_leg)


gps_GB_2 <- gps_GB_2 %>% 
  left_join(Tour_numstops, by = 'Tour ID')


#creating a rule to count non return trips
#previously:
gps_GB_3 <- gps_GB_2 %>% 
  mutate(Trips_notReturn = case_when(TourEndisBase == 1 & Trips != 1 ~ (as.numeric(Trips)-1)))


#alternative:
# gps_GB_3 <- gps_GB_2 %>% 
#   mutate(Trips_notReturn = case_when(TourEndisBase == 1 & Trips != 1 ~ (as.numeric(Trips)-1),
#                                      TourEndisBase == 0 ~ as.numeric(Trips),
#                                      T ~ as.numeric(Trips))
#          )


#Classify: Has External Stop
p <- function(v) {
  Reduce(f=paste0, x = v)
}

forDetectExternal <- gps_GB_3 %>% 
  group_by(`Tour ID`) %>% 
  summarise(tripTypes = p(as.character(InternalExternal)))

forDetectExternal2 <- forDetectExternal %>% 
  mutate(hasExternal = case_when(str_detect(tripTypes, 'X') ~ 1,
                                 T ~ 0)) %>% 
  select(`Tour ID`, hasExternal)

gps_GB_3 <- gps_GB_3 %>% 
  left_join(forDetectExternal2, by = 'Tour ID')


















# Time Functions ----------------------------------------------------------

gps_GB_4 <- gps_GB_3 %>% 
  ungroup() %>% 
  mutate(prevStop_duration = format(`Detention or Layover Time at Start Location from Previous Trip`, '%H:%M:%S'),
         trip_duration = format(`Trip Duration`, '%H:%M:%S')
  ) %>%
  ungroup() %>% 
  mutate(hms_prevStopDur = lubridate::hms(prevStop_duration),
         hms_tripDuration = lubridate::hms(trip_duration)) %>% 
  ungroup() %>% 
  mutate(prevstopDur_sec = period_to_seconds(hms_prevStopDur),
         traveltime_sec = period_to_seconds(hms_tripDuration)
  ) %>% 
  ungroup() %>% 
  select(-c(prevStop_duration:hms_tripDuration)) %>%
  ungroup() %>% 
  mutate(across(prevstopDur_sec:traveltime_sec, ~./60)) %>% 
  ungroup() %>% 
  mutate(across(prevstopDur_sec:traveltime_sec, round, 1)) %>% 
  ungroup() %>% 
  mutate(stopTime = lead(prevstopDur_sec)) %>% #compute a leading time to make it so that the stop time reflects the time spent after travel time
  ungroup() %>% 
  mutate(beginTime = format(`Begin Date Time`, '%H'),
         endTime = format(`end Date Time`, '%H')
  )

gps_GB_4 <- gps_GB_4 %>% 
  mutate(TripDurationBins = if_else(stopTime > 720, 'Over 12h', 
                                    if_else(stopTime > 600, 'Over 10h',
                                            if_else(stopTime > 480, 'Over 8h',
                                                    if_else(stopTime > 300, 'Over 5h', 'Good')
                                            )
                                    )
  )
  )


#creating some rules to create NA stop times where likely an overnight stop
gps_GB_4 <- gps_GB_4 %>% 
  mutate(BeginTime = as.integer(beginTime)) %>% 
  mutate(endTime = as.integer(endTime)) %>% 
  
  mutate(stopTime = if_else((TripDurationBins %in% c('Over 5h', 'Over 8h', 'Over 10h', 'Over 12h') & 
                               endTime %in% c(22, 23, 0, 1, 2, 3, 4)) | 
                              #if the "trip began at night & is over 5 hours
                              
                              (TripDurationBins %in% c('Over 8h', 'Over 10h', 'Over 12h') & 
                                 Trips == 1) & (!endTime %in% c(6,7,8,9,10, 11, 12, 13, 14)) | 
                              #if a trip on a 1-trip tour & over 8 hours & didnt start during working during a reasonable time for an 8hour shift
                              
                              (TripDurationBins %in% c('Over 5h', 'Over 8h', 'Over 10h', 'Over 12h') & 
                                tour_leg == Trips),
                              NA_real_,
                              stopTime)) %>% 
  #if the last trip of the tour and is long
  
  rename(StopDepartHour = beginTime, StopArrivalHour = endTime)



gps_GB_4 <- gps_GB_4 %>% 
  filter(!is.na(stopTime) | stopTime != 0)

gps_GB_4 <- gps_GB_4 %>% 
  group_by(`Tour ID`) %>% 
  mutate(TTravTime = sum(traveltime_sec),
         TStopTime = sum(stopTime),
         TTourTime = TTravTime + TStopTime)





#appending arrival time at first stop
tour_firsttripstart <- gps_GB_4 %>% 
  group_by(`Tour ID`) %>% 
  slice(which.min(tour_leg)) %>% 
  select(`Tour ID`, 
         FirstArrivalTime = `end Date Time`)

tour_firsttripstart2 <- tour_firsttripstart %>% 
  distinct() %>% 
  mutate(HoursAfterMidnight = format(FirstArrivalTime, format = '%H:%M:%S')) %>% 
  ungroup() %>% 
  mutate(minutesAfterMidnight = lubridate::hms(HoursAfterMidnight)) %>% 
  ungroup() %>% 
  mutate(FirstStop_MaM = round((period_to_seconds(minutesAfterMidnight)/60), 0)) %>% 
  select(`Tour ID`, FirstStop_MaM)


gps_GB_4 <- gps_GB_4 %>%
  left_join(tour_firsttripstart2, by = 'Tour ID')









# Geomtery Functions ------------------------------------------------------
#getting distances between every unique pair of subzones (faster than computing for every row)

#calculate centroid for each subzone
sz_centroid <- sz %>% 
  mutate(geometry_centroid = st_centroid(st_geometry(.)))

sz_simple <- sz_centroid %>% 
  select(subzone17, geometry_centroid, geometry)


#get all the unique subzone Base-Destination Pairs
gps_unique_OD <- unique(gps_GB_4[,c('BaseSubzone', 'Destination_SZ17PlusCounties')])

gps_unique_distances <- gps_unique_OD %>% 
  left_join(sz_simple, by = c('BaseSubzone' = 'subzone17')) %>% #join subzone geometry by base
  left_join(sz_simple, by = c('Destination_SZ17PlusCounties' = 'subzone17')) %>%  #join subzone geometry by destination
  rename(., c(geometry_base = geometry.x,
              geometry_dest = geometry.y,
              geometry_centroid_base = geometry_centroid.x,
              geometry_centroid_dest = geometry_centroid.y
  ))
#calculate the distnaces between centroids for all unique pairs of base to dest
gps_unique_distances <- gps_unique_distances %>% 
  mutate(distance = st_distance(geometry_centroid_base, geometry_centroid_dest, by_element = T))


gps_unique_distances <- gps_unique_distances %>% 
  mutate(distance_miles = (round(as.vector(distance)/5280, 3))) %>% 
  select(BaseSubzone, Destination_SZ17PlusCounties, distance, distance_miles)


#joining unique pair distances to main dataframe
gps_GB_4 <- gps_GB_4 %>% 
  left_join(gps_unique_distances, by = c('BaseSubzone', 'Destination_SZ17PlusCounties'))

#creating the full dataset
gps_CharsDist_full <- gps_GB_4 %>% 
  select(-distance, -prevstopDur_sec, -trip_duration, -hms_prevStopDur, -hms_tripDuration, -prevStop_duration)



gps_CharsDist_full %>% 
  select(`Tour ID`, Trips) %>% 
  distinct() %>% 
  group_by(Trips) %>% 
  count()




# Defining subsets for various calculations -------------------------------

#Temporarily Excluding tours with only 1 stop until clarify on what to do with them
ToursNot1Stop <- gps_CharsDist_full %>% 
  filter(Trips > 1 ) %>% 
  select(`Tour ID`) %>% 
  distinct()


gps_full_multiple <- gps_CharsDist_full %>% 
  ungroup() %>% 
  filter(`Tour ID` %in% ToursNot1Stop$`Tour ID`)



gps_full_noexternalbase <- gps_full_multiple %>% 
  filter(!is.na(base_county))


#no external trips for distance calculations
gps_full_noexternal <- gps_full_noexternalbase %>% 
  filter(InternalExternal == 'II')


#all trips besides return to base trip
gps_full_noreturn <- gps_full_noexternalbase %>% 
  filter(distance_miles != 0)


#for counting tour types
gps_full_noexternal
  
  
  
  

# Calculating Statistics --------------------------------------------------
#number of trips on tours with more than one trip
Tours_TypeCount <- gps_full_multiple %>% 
  select(`Tour ID`, TourType, hasExternal) %>% 
  distinct() %>% 
  group_by(TourType) %>% 
  tally()

Tours_ExternalCount <- gps_full_multiple %>% 
  select(`Tour ID`, TourType, hasExternal) %>% 
  distinct() %>% 
  group_by(hasExternal) %>% 
  tally()

Tours_CountyCount <- gps_full_multiple %>% 
  select(`Tour ID`, base_county) %>% 
  distinct() %>% 
  group_by(base_county) %>% 
  tally()




p = c(0, .2, .4, .6,.8, 1.0)

Tours_TripsQuintile <- gps_full_noreturn %>% 
  select(`Tour ID`, Trips_notReturn) %>% 
  distinct() %>%
  ungroup() %>% 
  summarise(min = quantile(Trips_notReturn, probs = p[1], na.rm = T),
            quint20 = quantile(Trips_notReturn, probs = p[2], na.rm = T),
            quint40 = quantile(Trips_notReturn, probs = p[3], na.rm = T),
            quint60 = quantile(Trips_notReturn, probs = p[4], na.rm = T),
            quint80 = quantile(Trips_notReturn, probs = p[5], na.rm = T),
            max = quantile(Trips_notReturn, probs = p[6], na.rm = T))
            
            
            
            
tourTimes <- gps_full_noreturn %>%
  select(`Tour ID`,traveltime_min, currentStopTime_min, FirstStop_MaM) %>%
  group_by(`Tour ID`) %>% 
  mutate(totalTravTime = sum(traveltime_min),
         totalStopTime = sum(currentStopTime_min)) %>%
  select(`Tour ID`, totalTravTime, totalStopTime) %>% 
  distinct() %>% 
  mutate(totalTourTime = sum(totalTravTime + totalStopTime)) %>% 
  select(`Tour ID`, totalTravTime, totalStopTime, totalTourTime) %>% 
  distinct() %>% 
  filter(totalStopTime != 0)

#sum of tour times
tripTimes <- gps_full_noreturn %>% 
  select(DeviceId,`Tour ID`, tour_leg, traveltime_min, currentStopTime_min) %>% 
  filter(currentStopTime_min != 0) %>% 
  distinct()

tourTimes <- gps_full_noreturn %>%
  select(`Tour ID`,traveltime_min, currentStopTime_min, FirstStop_MaM) %>%
  group_by(`Tour ID`) %>% 
  mutate(totalTravTime = sum(traveltime_min),
         totalStopTime = sum(currentStopTime_min)) %>%
  select(`Tour ID`, totalTravTime, totalStopTime) %>% 
  distinct() %>% 
  mutate(totalTourTime = sum(totalTravTime + totalStopTime)) %>% 
  select(`Tour ID`, totalTravTime, totalStopTime, totalTourTime) %>% 
  distinct() %>% 
  filter(totalStopTime != 0)

gps_CharsDist_full <- gps_CharsDist_full %>% 
  left_join(tourTimes, by = 'Tour ID')

write_csv(gps_CharsDist_full, 'dev/Data_Processed/CVGPS/ToursTripsCharacteristics_Centroid.csv')






# Initial Summaries -------------------------------------------------------


FirstStopTimes <- gps_full_noreturn %>% 
  select(`Tour ID`, FirstStop_MaM) %>% 
  distinct() %>% 
  ungroup()

Trips_StopTimes <- tripTimes %>% 
  ungroup() %>% 
  summarise(min = quantile(currentStopTime_min, probs = p[1], na.rm = T),
            quint20 = quantile(currentStopTime_min, probs = p[2], na.rm = T),
            quint40 = quantile(currentStopTime_min, probs = p[3], na.rm = T),
            quint60 = quantile(currentStopTime_min, probs = p[4], na.rm = T),
            quint80 = quantile(currentStopTime_min, probs = p[5], na.rm = T),
            max = quantile(currentStopTime_min, probs = p[6], na.rm = T))





Tours_TravTimeQuint <- tourTimes %>%
  ungroup() %>%
  summarise(min = quantile(totalTravTime, probs = p[1], na.rm = T),
            quint20 = quantile(totalTravTime, probs = p[2], na.rm = T),
            quint40 = quantile(totalTravTime, probs = p[3], na.rm = T),
            quint60 = quantile(totalTravTime, probs = p[4], na.rm = T),
            quint80 = quantile(totalTravTime, probs = p[5], na.rm = T),
            max = quantile(totalTravTime, probs = p[6], na.rm = T))

Tours_StopTimeQuint <- tourTimes %>%
  ungroup() %>%
  summarise(min = quantile(totalStopTime, probs = p[1], na.rm = T),
            quint20 = quantile(totalStopTime, probs = p[2], na.rm = T),
            quint40 = quantile(totalStopTime, probs = p[3], na.rm = T),
            quint60 = quantile(totalStopTime, probs = p[4], na.rm = T),
            quint80 = quantile(totalStopTime, probs = p[5], na.rm = T),
            max = quantile(totalStopTime, probs = p[6], na.rm = T))


Tours_TourTimeQuint <- tourTimes %>% 
  ungroup() %>% 
  summarise(min = quantile(totalTourTime, probs = p[1], na.rm = T),
            quint20 = quantile(totalTourTime, probs = p[2], na.rm = T),
            quint40 = quantile(totalTourTime, probs = p[3], na.rm = T),
            quint60 = quantile(totalTourTime, probs = p[4], na.rm = T),
            quint80 = quantile(totalTourTime, probs = p[5], na.rm = T),
            max = quantile(totalTourTime, probs = p[6], na.rm = T))


Tours_FirstStopTime <- FirstStopTimes %>% 
  ungroup() %>% 
  summarise(min = quantile(FirstStop_MaM, probs = p[1], na.rm = T),
            quint20 = quantile(FirstStop_MaM, probs = p[2], na.rm = T),
            quint40 = quantile(FirstStop_MaM, probs = p[3], na.rm = T),
            quint60 = quantile(FirstStop_MaM, probs = p[4], na.rm = T),
            quint80 = quantile(FirstStop_MaM, probs = p[5], na.rm = T),
            max = quantile(FirstStop_MaM, probs = p[6], na.rm = T))



#stoptime for unqiue vehicle
Device_MeanStopTime <- tripTimes %>%
  group_by(DeviceId) %>% 
  summarise(meanstop = mean(currentStopTime_min)) %>% 
  ungroup() %>% 
  summarise(min = quantile(meanstop, probs = p[1], na.rm = T),
            quint20 = quantile(meanstop, probs = p[2], na.rm = T),
            quint40 = quantile(meanstop, probs = p[3], na.rm = T),
            quint60 = quantile(meanstop, probs = p[4], na.rm = T),
            quint80 = quantile(meanstop, probs = p[5], na.rm = T),
            max = quantile(meanstop, probs = p[6], na.rm = T))
  
  
Vehicle_tripTimes <- gps_full_noreturn %>% 
  select(DeviceId,VehicleWeightClass_1, currentStopTime_min) %>% 
  filter(currentStopTime_min != 0) %>%
  group_by(VehicleWeightClass_1) %>% 
  summarise(min = quantile(currentStopTime_min, probs = p[1], na.rm = T),
            quint20 = quantile(currentStopTime_min, probs = p[2], na.rm = T),
            quint40 = quantile(currentStopTime_min, probs = p[3], na.rm = T),
            quint60 = quantile(currentStopTime_min, probs = p[4], na.rm = T),
            quint80 = quantile(currentStopTime_min, probs = p[5], na.rm = T),
            max = quantile(currentStopTime_min, probs = p[6], na.rm = T))


TourType_StopTimes <- gps_full_noreturn %>% 
  select(`Tour ID`, TourType, currentStopTime_min) %>% 
  filter(currentStopTime_min != 0 & !is.na(TourType)) %>%
  group_by(TourType) %>% 
  summarise(min = quantile(currentStopTime_min, probs = p[1], na.rm = T),
            quint20  = quantile(currentStopTime_min, probs = p[2], na.rm = T),
            quint40 = quantile(currentStopTime_min, probs = p[3], na.rm = T),
            quint60 = quantile(currentStopTime_min, probs = p[4], na.rm = T),
            quint80 = quantile(currentStopTime_min, probs = p[5], na.rm = T),
            max = quantile(currentStopTime_min, probs = p[6], na.rm = T))


BaseCounty_StopTimes <- gps_full_noreturn %>% 
  select(`Tour ID`, base_county, currentStopTime_min) %>% 
  filter(currentStopTime_min != 0) %>%
  group_by(base_county) %>% 
  summarise(min = quantile(currentStopTime_min, probs = p[1], na.rm = T),
            quint20 = quantile(currentStopTime_min, probs = p[2], na.rm = T),
            quint40 = quantile(currentStopTime_min, probs = p[3], na.rm = T),
            quint60 = quantile(currentStopTime_min, probs = p[4], na.rm = T),
            quint80 = quantile(currentStopTime_min, probs = p[5], na.rm = T),
            max = quantile(currentStopTime_min, probs = p[6], na.rm = T))


BaseCounty_nTours <- gps_full_noreturn %>% 
  select(`Tour ID`, base_county) %>% 
  distinct() %>% 
  group_by(base_county) %>% 
  tally()

write_csv(BaseCounty_StopTimes, file.path('dev','Data_Processed', 'CVGPS', 'Stops Model - GPS Summaries', 'BaseCounty_stopTimes.csv'))
write_csv(BaseCounty_nTours, file.path('dev','Data_Processed', 'CVGPS', 'Stops Model - GPS Summaries', 'BaseCounty_nTours.csv'))
