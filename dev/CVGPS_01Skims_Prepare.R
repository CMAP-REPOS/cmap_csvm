

# Setup -------------------------------------------------------------------
##Configure Environment
source("./dev/init_dev.R")
library(readxl)
library(tidyverse)
library(lubridate)

SYSTEM_DEV_DATA_PATH <- file.path(SYSTEM_DEV_PATH, 'DATA_Processed')


##Read CVGPS Data
gps <- read_xlsx('dev/Data_Processed/CVGPS/TripsMediumLight_OD_ISPE83_SZPlusCounties_ValuesOnly_forRSG.xlsx')



# Classifying Vehicle Base ------------------------------------------------

# Section Description: 
# Necessary for various calibration targets, we make a decision about 
# what subzone is a vehicle's home base. We tally tour start subzones 
# and choose the most prevalent, otherwise if we only have one tour, 
# that first subzone is the home base, otherwise we won't use for calibration targets


## For unique vehicle, tally the origin subzones on the first trip of each tour
OriginStop_byID <- gps %>%
  filter(`Begin Tour - After Error Checking` == TRUE) %>% 
  group_by(DeviceId, Origin_SZ17PlusCounties) %>% 
  summarise(n = n())


## For that tally, choose the subzone with the highest tally
Home_sTAZ_origin <- OriginStop_byID %>% 
  group_by(DeviceId) %>% 
  slice(which.max(n))


## For that subzone, if tally is greater than 1, we call that subzone that vehicle's home base 
ClearHome <- Home_sTAZ_origin %>% 
  filter(n > 1) %>% 
  select(DeviceId, Origin_SZ17PlusCounties)



## If tally is not greater than one, we say there is no clear vehicle base
noClearHome <- Home_sTAZ_origin %>% 
  filter(n == 1)

noClearHome_full <- gps %>% 
  filter(DeviceId %in% noClearHome$DeviceId) %>%
  filter()



## If a unique vehicle has only one tour, the origin zone of that tour is the vehicle base
oneTour <- noClearHome_full %>% 
  group_by(DeviceId) %>% 
  summarise(n = n_distinct(`Tour ID`)) %>% 
  filter(n == 1)


## Subset those records for devices with one tour
oneTour_full <- gps %>% 
  filter(DeviceId %in% oneTour$DeviceId)

OneTour_home <- oneTour_full %>% 
  filter(`Begin Tour - After Error Checking` == TRUE) %>% 
  select(DeviceId, Origin_SZ17PlusCounties)



## We bind together the list of ID's and assigned base subzones (where tally was greater than one or where there was only one tour)
HomeIds_goodBase <- OneTour_home %>% 
  rbind(ClearHome) %>% 
  select(DeviceId,
         BaseSubzone = Origin_SZ17PlusCounties)


## We join that list of vehicle base's onto the working dataset using DeviceId
GPS_goodBase <- gps %>% 
  right_join(HomeIds_goodBase, by = 'DeviceId')

rm(gps)


## Subsetting all the records not in GPS_goodBase - the records we wont use
MultipleTours_noClearHome <- noClearHome_full %>% 
  filter(!(DeviceId %in% HomeIds_goodBase$DeviceId))





# Trip/Tour Characteristics and Classification ---------------------------

## Section Description:
# Using the subset of records for which we could assign a vehicle base subzone,
# we compute some characteristics including: 
# - Are Base and Destination Internal or External
# - Zone and County for Vehicle Base, Trip Origin, and Trip Destination 
# - Single vs Multi-stop Tours
# - Number of trips
# - Number of stops
# - Single- vs Muli-stop tour
# - Tour with External Stop
# - Base Start/End + Single/Multi Classification



# Classify Home (Vehicle Base) and Destination as Internal or External
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





#Appending County Data

## Read subzone data
sz <- read_sf(file.path(SYSTEM_DEV_PATH,'Data_Processed','TAZ', 'subzones17.shp'))


## Select relavent columns from subzone data
zone_dict2 <- sz %>% 
  select(subzone17, zone17, county_nam, state) %>% 
  st_drop_geometry() %>% 
  #Combine County and State to avoid conflating similarly named counties across states
  mutate(county_state = paste0(county_nam, ', ', state)) %>% 
  select(-county_nam, -state)



## Append Vehicle Base County and Zone onto Main Dataset using Base Subzone
gps_GB_1 <- gps_GB_1 %>% 
  left_join(zone_dict2, by = c('BaseSubzone' = 'subzone17'))

gps_GB_1 <- gps_GB_1 %>% 
  rename(base_Zone = zone17,
         base_county = county_state)




## Append Zone & County for Trip Destination and Origin using Subzone
### Origin
gps_GB_1 <- gps_GB_1 %>% 
  left_join(zone_dict2, by = c('Origin_SZ17PlusCounties' = 'subzone17')) %>% 
  rename(origin_zone = zone17,
         origin_countyState = county_state)


### Destination
gps_GB_1 <- gps_GB_1 %>% 
  left_join(zone_dict2, by = c('Destination_SZ17PlusCounties' = 'subzone17')) %>% 
  rename(dest_zone = zone17,
         dest_countyState = county_state)



# Append a Tour Leg Variable 
gps_GB_1 <- gps_GB_1 %>% 
  arrange(`Tour ID`,`Begin Date Time`) %>% 
  group_by(`Tour ID`) %>% 
  mutate(tour_leg = row_number())




# Classifying: Is Tour Start/Tour End == BaseSubzone

## Creating a temporary Dataframe with Base, Origin, and Destination Variables
ClassifyStartEnd <- gps_GB_1 %>% 
  select(BaseSubzone, `Tour ID`, tour_leg, Origin_SZ17PlusCounties, Destination_SZ17PlusCounties)


## Classify Start:
### Take the first trip on every tour
### If the origin subzone is the assigned vehicle base subzone, tour start is base. 
### Otherwise, not. Or 'Mising Origin' if origin subzone is missing.
ClassifyStart <- ClassifyStartEnd %>% 
  group_by(`Tour ID`) %>% 
  slice(which.min(tour_leg)) %>% 
  mutate(TourStartisBase = case_when(Origin_SZ17PlusCounties == BaseSubzone ~ '1',
                                     (Origin_SZ17PlusCounties != BaseSubzone &
                                        !is.na(Origin_SZ17PlusCounties)) ~ '0',
                                     is.na(Origin_SZ17PlusCounties) ~ 'Missing_Origin')
  )

## Classify End:
### Take last trip on every tour
### If the destination subzone is the assigned vehicle base subzone, tour end is base. 
### Otherwise, not. Or NA if destination subzone is missing.
ClassifyEnd <- ClassifyStartEnd %>% 
  group_by(`Tour ID`) %>% 
  slice(which.max(tour_leg)) %>% 
  mutate(TourEndisBase = case_when(Destination_SZ17PlusCounties == BaseSubzone ~ '1',
                                   (Destination_SZ17PlusCounties != BaseSubzone & 
                                      !is.na(Destination_SZ17PlusCounties)) ~ '0',
                                   is.na(Destination_SZ17PlusCounties) ~ 'Missing_dest')
  )


## Create a table with both start and end classification for each Tour ID
TourStartEndClass <- ClassifyStart %>% 
  full_join(ClassifyEnd, by = 'Tour ID') %>% 
  select(`Tour ID`, TourStartisBase, TourEndisBase)



## Join Tour Start/End Classification onto working dataset
gps_GB_2 <- gps_GB_1 %>% 
  left_join(TourStartEndClass, by = 'Tour ID')



# Classification: Tour Type

## In a temporary dataframe, calculating a variable called 'Trips', which is equal to the highest tour_leg for each Tour_ID
Tour_numstops <- gps_GB_2 %>% 
  group_by(`Tour ID`) %>% 
  slice(which.max(tour_leg)) %>% 
  select(`Tour ID`, Trips = tour_leg)


## Joining the 'Trips' Variable back onto the working dataset.
gps_GB_2 <- gps_GB_2 %>% 
  left_join(Tour_numstops, by = 'Tour ID')




## Creating a variable called 'Trips_notReturn' which calculates the number 
## of trips on a tour that are not a return-to-base trip

### FIRST: If tourEndisBase is true, then Trip_notReturn is equal to (total trips - 1), 
### Otherwise, Trips_notReturn is the total stops on that tour

### SECOND: We create a variable 'NStops' which = Trips_notReturn for clarity

### Last: We classify tours as either single- or multi-stop using NStops

gps_GB_3 <- gps_GB_2 %>% 
  
  mutate(Trips_notReturn = if_else(TourEndisBase == 1,
                                   as.numeric(Trips)-1,
                                   as.numeric(Trips)),
         NStops = Trips_notReturn) %>%
  
  mutate(single_multi = if_else(NStops == 1,
                                'single',
                                if_else(NStops >= 2,
                                        'multi',
                                        NA_character_)))




# Classify tour as Single vs Multi & Tour Start/End is base vs not base

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






 # Classify: Does the tour have External Stops
p <- function(v) {
  Reduce(f=paste0, x = v)
}

## Create a temporary table and run function 'p'
forDetectExternal <- gps_GB_3 %>% 
  group_by(`Tour ID`) %>% 
  summarise(tripTypes = p(as.character(InternalExternal)))

## The output of function p are the Internal/External classification for every trip on a tour, pasted together
## If that output contains an X, that tour has an external trip

forDetectExternal2 <- forDetectExternal %>% 
  mutate(hasExternal = case_when(str_detect(tripTypes, 'X') ~ 1,
                                 T ~ 0)) %>% 
  select(`Tour ID`, hasExternal)


## Join this classification back onto our working dataset
gps_GB_3 <- gps_GB_3 %>% 
  left_join(forDetectExternal2, by = 'Tour ID')











# Time Functions ----------------------------------------------------------

## Section Description: 
## The Code Blocks Below take the included timestamp data and use them to create temoral 
## characteristics that will be used for model calibration and sanity checks
## These Include:
## - Trip - Stop Duration
## - Trip - Travel Time
## - Trip - Departure Time from Origin 
## - Trip - Arrival Time at Destination 
## - Tour - Arrival Time at first Stop


#computing trip and stop durations
gps_GB_4 <- gps_GB_3 %>% 
  ungroup() %>% 
  
  ## Format Stop Time and Trip Time timestamps as Hour:Minute:Second 
  mutate(prevStop_duration = format(`Detention or Layover Time at Start Location from Previous Trip`, '%H:%M:%S'),
         trip_duration = format(`Trip Duration`, '%H:%M:%S')
  ) %>%
  ungroup() %>% 
  
  ##Extract Just Hour Minute Seconds
  mutate(hms_prevStopDur = lubridate::hms(prevStop_duration),
         hms_tripDuration = lubridate::hms(trip_duration)) %>% 
  ungroup() %>% 
  
  
  ##Convert HourMinuteSeconds to Seconds
  mutate(prevstopDur_sec = period_to_seconds(hms_prevStopDur),
         traveltime_sec = period_to_seconds(hms_tripDuration)
  ) %>% 
  ungroup() %>% 
  
  ## Remove variables from intermediate steps
  select(-c(prevStop_duration:hms_tripDuration)) %>%
  ungroup() %>% 
  
  ## Convert Stop Time and Travel Time from seconds to Minutes
  mutate(across(prevstopDur_sec:traveltime_sec, ~./60)) %>% 
  ungroup() %>% 
  
  ## Round to 1 significant digit
  mutate(across(prevstopDur_sec:traveltime_sec, round, 1)) %>% 
  ungroup() %>% 
  
  ## Compute a leading stop time
  ## Existing stop duration is the duration of the stop at the destination of the previous trip
  mutate(stopTime = lead(prevstopDur_sec)) %>%
  ungroup()


# Pull out trip departure and arrival times
## Cast Trip Departure and Arrival timestamps from GMT to Central Time
gps_GB_4 <- gps_GB_4 %>% 
  mutate(BeginDateTimeCDT = with_tz(`Begin Date Time`, tz = 'America/Chicago'),
         EndDateTimeCDT = with_tz(`end Date Time`, tz = 'America/Chicago')) %>% 
  mutate(BeginTimeCDT = format(BeginDateTimeCDT, '%H'),
         EndTimeCDT = format(EndDateTimeCDT, '%H')
  )










## Calculate some Stop Duration Bins to assess the reasonableness of certain trip durations
gps_GB_4 <- gps_GB_4 %>% 
  mutate(TripDurationBins = if_else(stopTime > 720, 'Over 12h', 
                                    if_else(stopTime > 600, 'Over 10h',
                                            if_else(stopTime > 480, 'Over 8h',
                                                    if_else(stopTime > 300, 'Over 5h', 'Good')
                                            )
                                    )
  )
  )


## Creating some rules to assign NA stop times where likely an overnight stop
gps_GB_4 <- gps_GB_4 %>% 
  mutate(BeginTimeCDT = as.integer(BeginTimeCDT)) %>% 
  mutate(EndTimeCDT = as.integer(EndTimeCDT)) %>% 
  

  mutate(stopTime = if_else((TripDurationBins %in% c('Over 5h', 'Over 8h', 'Over 10h', 'Over 12h') & 
                               EndTimeCDT %in% c(22, 23, 0, 1, 2, 3, 4)) | 
                              #if the "trip began at night & is over 5 hours
                              
                              
                              (TripDurationBins %in% c('Over 8h', 'Over 10h', 'Over 12h') & 
                                 Trips == 1) & (!EndTimeCDT %in% c(6,7,8,9,10, 11, 12, 13, 14)) | 
                              #if is a trip on a 1-trip tour & over 8 hours & didnt start during working during a reasonable time for an 8hour stop
                              # May be a bit restrictive at the moment
                              
                              
                              (TripDurationBins %in% c('Over 5h', 'Over 8h', 'Over 10h', 'Over 12h') & 
                                 tour_leg == Trips),
                            NA_real_,
                            stopTime)) %>% 
                            #if is the last trip of the tour and is a long stop
  
  rename(StopDepartHour = BeginTimeCDT, StopArrivalHour = EndTimeCDT)





## For each tour, sum travel time to get total tour travel time
## For each tour, sum stop times to get total tour stop time
## For each tour, sum total travel time and total stop time to get total tour time
gps_GB_4 <- gps_GB_4 %>% 
  group_by(`Tour ID`) %>% 
  mutate(TTravTime = sum(traveltime_sec),
         TStopTime = sum(stopTime),
         TTourTime = TTravTime + TStopTime)





## Append Tour arrival time at first stop to working dataset

### Identify first trip of each tour and pull out arrival time
tour_firsttripstart <- gps_GB_4 %>% 
  group_by(`Tour ID`) %>% 
  slice(which.min(tour_leg)) %>% 
  select(`Tour ID`, 
         FirstArrivalTime = EndDateTimeCDT)

### Format arrival time as Minutes after Midnight and as hour (24 Hour Time)

## First format as HMS
tour_firsttripstart2 <- tour_firsttripstart %>% 
  distinct() %>% 
  mutate(FirstArrival_HMS = format(FirstArrivalTime, format = '%H:%M:%S')) %>% 
  ungroup()

## Convert to lubridate HMS object
tour_firsttripstart2 <- tour_firsttripstart2 %>% 
  mutate(minutesAfterMidnight = lubridate::hms(FirstArrival_HMS)) %>% 
  ungroup()


# Convert lubridate HMS object to seconds and divide by 60 to get Minutes after Midngight
tour_firsttripstart2 <- tour_firsttripstart2 %>% 
  mutate(FirstStop_MaM = floor(period_to_seconds(minutesAfterMidnight)/60))

tour_firsttripstart2 <- tour_firsttripstart2 %>% 
  select(`Tour ID`, FirstStop_MaM)

### Append First Stop Arrival Time Information to Working Dataset
gps_GB_4 <- gps_GB_4 %>%
  left_join(tour_firsttripstart2, by = 'Tour ID')




 
# Skims Distances ---------------------------------------------------------
## Section Description:
### Using Skims data from CMAP:
### Calculate Distances from Base to Trip Destinations
### Calculate Trip Distances

## Read in Skims
skims_tod <- readRDS('E:/Projects/Clients/CMAP/cmap_csvm/scenarios/base/outputs/skims_tod.rds')

## Pull out Origin, Destination, and dist.avg and call it base_stop_dist (average distance across all time periods)
skims_tod <- skims_tod %>% 
  select(OTAZ, DTAZ, base_stop_dist = dist.avg)

## Join Skim Distance for Vehicle Base to Trip Destination to the Working Dataset
gps_CharsDist_full <- gps_GB_4 %>% 
  left_join(skims_tod, by = c('base_Zone' = 'OTAZ', 'dest_zone' = 'DTAZ'))


## Rename Skims dist to trip_dist
skims_tod <- skims_tod %>% 
  select(OTAZ, DTAZ, trip_dist = base_stop_dist)

## Join Skim Distance for Trip Origin to Trip Destination to the Working Dataset
gps_CharsDist_full <- gps_CharsDist_full %>% 
  left_join(skims_tod, by = c('origin_zone' = 'OTAZ', 'dest_zone' = 'DTAZ'))





# Attach District Information ---------------------------------------------
## Attach District for Vehicle Base, Origin, and Destination

## Read Zone System Data
district_dict <- read_csv('lib/data/TAZ_System.csv') %>% 
  st_drop_geometry() %>% 
  select(zone17 = TAZ, DistrictName, DistrictNum)

## Create Temporary Files for each of Base, Origin, Destination
Origin_district <- district_dict %>% 
  select(zone17, ODistrict = DistrictName, ODistrict_num = DistrictNum)

Destination_district <- district_dict %>% 
  select(zone17, DDistrict = DistrictName, DDistrict_num = DistrictNum)

Base_district <- district_dict %>% 
  select(zone17, BaseDistrict = DistrictName, BaseDistrict_num = DistrictNum)


## Append to the working dataset
gps_CharsDist_full <- gps_CharsDist_full %>% 
  left_join(Base_district, by = c('base_Zone' = 'zone17')) %>% 
  left_join(Origin_district, by = c('origin_zone' = 'zone17')) %>%
  left_join(Destination_district, by = c('dest_zone' = 'zone17')) 




# Defining subsets for Calibration Summaries ------------------------------
## Exclude tours that had only one trip
gps_CharsDist_full <- gps_CharsDist_full %>% 
  filter(Trips > 1)






# Identify tours with discontinuities
## Dataset had some information but not all
gps_CharsDist_full <- gps_CharsDist_full %>% 
  mutate(lead_origin = lead(origin_zone)) %>% 
  mutate(tour_discont = ifelse(dest_zone != lead_origin & tour_leg != Trips,
                               1,
                               0))

discont <- gps_CharsDist_full %>% 
  filter(tour_discont == 1) %>% 
  select(`Tour ID`, tour_leg, Trips, dest_zone, lead_origin, tour_discont) %>% 
  left_join(skims_tod, by= c('dest_zone' = 'OTAZ', 'lead_origin' = 'DTAZ')) %>% 
  rename(discontinuity_dist = trip_dist) %>% 
  mutate(missingDist_over4 = ifelse(discontinuity_dist > 4,
                                    1,
                                    0))
discont2 <- discont %>% 
  select(`Tour ID`, missingDist_over4) %>% 
  filter(missingDist_over4 == 1) %>% 
  distinct()

gps_final <- gps_CharsDist_full %>% 
  left_join(discont2, by = 'Tour ID')


# Write GPS File with Relavent Characteristics ----------------------------
write_csv(gps_final, 'dev/Data_Processed/CVGPS/ToursTripsCharacteristics_SKIMS.csv')









