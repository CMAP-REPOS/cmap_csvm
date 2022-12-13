

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

zone_dict3 <- zone_dict2 %>% 
  select(subzone17, zone17)

gps_GB_1 <- gps_GB_1 %>% 
  left_join(zone_dict3, by = c('Destination_SZ17PlusCounties' = 'subzone17')) %>% 
  rename(dest_zone = zone17)

gps_GB_1 <- gps_GB_1 %>% 
left_join(zone_dict3, by = c('Origin_SZ17PlusCounties' = 'subzone17')) %>% 
  rename(origin_zone = zone17)



#classifying as single or multistop
#this needs attention -----
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
  mutate(Trips_notReturn = case_when(TourEndisBase == 1 & Trips != 1 ~ (as.numeric(Trips)-1)),
         NStops = Trips-1) %>%
  mutate(single_multi = if_else(NStops == 1,
                                'single',
                                if_else(NStops >= 2,
                                        'multi',
                                        NA_character_)))

gps_GB_3 <- gps_GB_3 %>% 
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
library(lubridate)

#computing trip and stop durations
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
  ungroup()

#

gps_GB_4 <- gps_GB_4 %>% 
  mutate(BeginDateTimeCDT = with_tz(`Begin Date Time`, tz = 'America/Chicago'),
         EndDateTimeCDT = with_tz(`end Date Time`, tz = 'America/Chicago')) %>% 
  mutate(BeginTimeCDT = format(BeginDateTimeCDT, '%H'),
         EndTimeCDT = format(EndDateTimeCDT, '%H')
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
  mutate(BeginTimeCDT = as.integer(BeginTimeCDT)) %>% 
  mutate(EndTimeCDT = as.integer(EndTimeCDT)) %>% 
  
  mutate(stopTime = if_else((TripDurationBins %in% c('Over 5h', 'Over 8h', 'Over 10h', 'Over 12h') & 
                               EndTimeCDT %in% c(22, 23, 0, 1, 2, 3, 4)) | 
                              #if the "trip began at night & is over 5 hours
                              
                              (TripDurationBins %in% c('Over 8h', 'Over 10h', 'Over 12h') & 
                                 Trips == 1) & (!EndTimeCDT %in% c(6,7,8,9,10, 11, 12, 13, 14)) | 
                              #if a trip on a 1-trip tour & over 8 hours & didnt start during working during a reasonable time for an 8hour shift
                              
                              (TripDurationBins %in% c('Over 5h', 'Over 8h', 'Over 10h', 'Over 12h') & 
                                 tour_leg == Trips),
                            NA_real_,
                            stopTime)) %>% 
  #if the last trip of the tour and is long
  
  rename(StopDepartHour = BeginTimeCDT, StopArrivalHour = EndTimeCDT)



gps_GB_4 <- gps_GB_4 #%>% 
  #filter(!is.na(stopTime) | stopTime != 0)

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
         FirstArrivalTime = EndDateTimeCDT)

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








# Skims Distances ---------------------------------------------------------

#read in skims
skims_tod <- readRDS('E:/Projects/Clients/CMAP/cmap_csvm/scenarios/base/outputs/skims_tod.rds')

#selecting just dist.avg, average distance across TOD
#joining skims dist for base-stops distances
skims_tod <- skims_tod %>% 
  select(OTAZ, DTAZ, base_stop_dist = dist.avg)


gps_CharsDist_full <- gps_GB_4 %>% 
  left_join(skims_tod, by = c('base_Zone' = 'OTAZ', 'dest_zone' = 'DTAZ'))


#joining skims dist for each stop distance
skims_tod <- skims_tod %>% 
  select(OTAZ, DTAZ, trip_dist = base_stop_dist)

gps_CharsDist_full <- gps_CharsDist_full %>% 
  left_join(skims_tod, by = c('origin_zone' = 'OTAZ', 'dest_zone' = 'DTAZ'))

# Defining subsets for various calculations -------------------------------
gps_CharsDist_full <- gps_CharsDist_full %>% 
  filter(Trips > 1)





write_csv(gps_CharsDist_full, 'dev/Data_Processed/CVGPS/ToursTripsCharacteristics_SKIMS.csv')
