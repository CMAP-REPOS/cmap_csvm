#Takes Prepared GPS data from previous Script and creates summary calibration targets


# Setup -------------------------------------------------------------------

##Configure Environment
source("./dev/init_dev.R")
library(readxl)
library(tidyverse)
library(rbin)

SYSTEM_DEV_DATA_PATH <- file.path(SYSTEM_DEV_PATH, 'DATA_Processed')


##Read the CVGPS Data
file <- file.path(SYSTEM_DEV_PATH, 'Data_Processed', 'CVGPS', 'ToursTripsCharacteristics_SKIMS.csv')
GPS <- fread(file)



# Specify two subsets of the Data

## Make a copy of the original
GPS2 <- GPS


## Identify tour_id's for which any of the following are missing for any trip:
## - stopTime, base_stop_dist, TourType, trip_dist, have a 'Reporting Concern', or missing Origin Zone or Destination Zone. 
ToursWithNA <- GPS2 %>% 
  select(Tour_ID = `Tour ID`, OTAZ = origin_zone, DTAZ = dest_zone, trip_dist, stopTime, base_stop_dist, TourType, 
         missingDist_over4, `Reporting  Concern Indicated by ""1""`) %>% 
  filter(is.na(stopTime) | is.na(base_stop_dist) | is.na(TourType) | is.na(trip_dist) | is.na(OTAZ) | 
           is.na(DTAZ) | missingDist_over4 == 1 | `Reporting  Concern Indicated by ""1""` == 1) %>% 
  distinct()






## We create one dataset that removes tours with the characteristics above 
## as well as remove the final trip on tours where there is a return to base trip
GPS <- GPS %>% 
  filter(!`Tour ID` %in% c(ToursWithNA$Tour_ID)) %>% 
  filter(!(Trips_notReturn < Trips & tour_leg == Trips))


## This copy also excludes tours with missing data but keeps the return to base stop for the travelling salesman analysis
GPS2 <- GPS2 %>% 
  filter(!`Tour ID` %in% c(ToursWithNA$Tour_ID))







# Tour N StopsDistribution -----

## Tally tours by the number of stops on that tour (NStops is the number of trips except for return to base)
NStopsDistr <- GPS %>% 
  select(`Tour ID`, NStops) %>%
  distinct() %>% 
  group_by(NStops) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(NStopsDistr, 'dev/Data_Processed/CVGPS/Calibration Targets/Tour_NStops_CVGPS.csv')


## Tally and Calculate Share of Single Vs Multi-Stop Tours
Tours_SingleVsMulti <- GPS %>% 
  select(`Tour ID`, Trips, NStops, single_multi, TourType) %>% 
  distinct() %>% 
  group_by(single_multi) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(Tours_SingleVsMulti, 'dev/Data_Processed/CVGPS/Calibration Targets/TourSingleMulti_CVGPS.csv')


## Tally and calculate share of tour types (Single/Multi & Base/Not Base)
TourType_CVGPS <- GPS2 %>% 
  select(`Tour ID`,TourType, single_multi) %>% 
  distinct() %>% 
  group_by(single_multi, TourType) %>% 
  tally() %>% 
  filter(!is.na(TourType)) %>% 
  ungroup() %>% 
  mutate(Target = n/sum(n))

write_csv(TourType_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/TourType_CVGPS.csv')





# Stop Durations -----
## Using the Subset of Stops that are not the return to base stop to calculate distribution of stop durations
## Also, recoding trip time = 0, to blank
  ### Computed stop time as lead of 'previous layover' so if the trip was the last trip for that device, 
  ### it would be assigned the 'previous layover time' of the first trip of the next device which would be 0

GPS <- GPS %>% 
  as.data.table()

GPS <- GPS %>% 
  mutate(stopTime = ifelse(stopTime == 0,
                           NA,
                           stopTime))


newDevice <- GPS %>% 
  filter(is.na(stopTime))

## Renaming vehicle type for Coniceness
GPS <- GPS %>% 
  mutate(Vehicle = case_when(VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.' ~ 'Light',
                             VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.' ~ 'Medium'))


##Creating Duration Bins
GPS[, duration_15min := ceiling(stopTime/15) * 15]
GPS[, 
                     duration_group := 
                       fcase(
                         duration_15min %in% 15, 1,
                         duration_15min %in% 30, 2,
                         duration_15min %in% 45, 3,
                         duration_15min %in% 60, 4,
                         duration_15min %in% 75, 5,
                         duration_15min %in% 90, 6,
                         duration_15min %in% c(90 + 1:4 * 15), 7,
                         duration_15min %in% c(150 + 1:4 * 15), 8,
                         duration_15min %in% c(210 + 1:4 * 15), 9,
                         duration_15min %in% c(270 + 1:8 * 15), 10 #The longest stop in the sample is 360 minutes
                         #duration_15min %in% c(390 + 1:14 * 15), 11
                         )]


## Classifying Stop Times by those bins 
GPS[, duration_group := factor(duration_group, 
                               labels = c("1-15", "16-30", "31-45", "46-60", "61-75", "76-90", "91-150", "151-210", "211-270", "271-360"))]


## Calculating Share of Stops in each stop duration bin
## Will produce NAs, those are the trips for which the next trip was the start of a new device so would have been assigned
duration_stops_CVGPS <- GPS %>% 
  group_by(duration_group) %>% 
  tally() %>% 
  filter(!is.na(duration_group)) %>% 
  mutate(Target = n/sum(n))

write_csv(duration_stops_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/duration_stops_CVGPS.csv')


## Calculating share of stops in each stop duration bin by vehicle
duration_stops_vehicle_CVGPS <- GPS %>% 
  group_by(Vehicle, duration_group) %>% 
  tally() %>%
  filter(!is.na(duration_group)) %>% 
  group_by(Vehicle) %>% 
  mutate(Target = n/sum(n)) %>% 
  ungroup()

write_csv(duration_stops_vehicle_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/duration_stops_vehicle_CVGPS.csv')

duration_stops_mean <- GPS %>% 
  filter(!is.na(stopTime)) %>% 
  summarise(Target = mean(stopTime))

write_csv(duration_stops_mean, 'dev/Data_Processed/CVGPS/Calibration Targets/duration_stops_Mean_CVGPS.csv')
  




# Base-Stop Distances -----
## Using the subset of records that are not the return to base trip, compute distribution of base to stop destination distances

#Overall
## Create Bins
dist_bins <- c(0, 2, 5, 10, 20)

## Assign Base-Stop distances to those bins
GPS[, dist_bin := factor(findInterval(x = base_stop_dist, vec = dist_bins),
                                     labels = c("dist_00_02", "dist_02_05", "dist_05_10", "dist_10_20", "dist_20_p"))]

## Calcualte Distribution of trips in each distance bin
BaseStopDist_CVGPS <- GPS %>% 
  group_by(dist_bin) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(BaseStopDist_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/BaseStopDist_CVGPS.csv')



## Calculate mean of Base-Stop Distance for all non-return-to-base trips
BaseStopMean_CVGPS <- GPS %>% 
  summarise(mean(base_stop_dist)) %>% 
  rename(mean = `mean(base_stop_dist)`)

write_csv(BaseStopMean_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/BaseStopMean_CVGPS.csv')
  


## Calcualte Distribution of trips in each distance bin by vehicle type
BaseStopDist_vehicle_CVGPS <- GPS %>% 
  group_by(Vehicle, dist_bin) %>% 
  tally() %>% 
  group_by(Vehicle) %>% 
  mutate(Target = n/sum(n)) %>% 
  ungroup()

write_csv(BaseStopDist_vehicle_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/BaseStopDist_vehicle_CVGPS.csv')



## Calcualte Distribution of trips in each distance bin by Vehicle Base District
BaseStopDist_district_CVGPS <- GPS %>% 
  group_by(BaseDistrict, dist_bin) %>% 
  tally() %>% 
  mutate(Target = n/sum(n)) %>% 
  as.data.table() %>% 
  pivot_longer(n:Target, names_to = 'statistic', values_to = 'value') %>% 
  pivot_wider(names_from = dist_bin) 



## Calculate overall mean of Base-Stop Distances by District
BaseStopMean_district_CVGPS <- GPS %>% 
  group_by(BaseDistrict) %>% 
  summarise(mean_dist = mean(base_stop_dist))



## Write one table with the distributions and overall means for each district
BaseStop_district <- BaseStopDist_district_CVGPS %>% 
  left_join(BaseStopMean_district_CVGPS, by = 'BaseDistrict')%>% 
  mutate(across(c(dist_00_02:mean_dist), ~round(., 4)))
  
write_csv(BaseStop_district, 'dev/Data_Processed/CVGPS/Calibration Targets/BaseStopDist_district_CVGPS.csv', na = '')





# Trip/Tour Distances -----
## GPS2 has return to base
## GPS does not have return to base

# Distribution of Total Tour Distances

## Calculate total tour distance for each tour (including return to base trip)
TourDistDistr <- GPS2 %>% 
  group_by(`Tour ID`) %>% 
  mutate(TourDist = sum(trip_dist)) %>% 
  select(`Tour ID`, TourDist) %>% 
  distinct() %>% 
  as.data.table()

TourDist <- GPS2 %>% 
  group_by(`Tour ID`) %>% 
  mutate(TourDist = sum(trip_dist)) %>% 
  select(`Tour ID`, TourDist) %>% 
  distinct()

GPS2 <- GPS2 %>% 
  left_join(TourDist, by = 'Tour ID')
  



## Set Distance Bins
tour_dist_bins <- c(0, 30, 60, 120, 180, 240, 360, 480)

## Assign total tour distance to bins
TourDistDistr[, dist_bin := factor(findInterval(x = TourDist, vec = tour_dist_bins),
                         labels = c("dist_00_30", "dist_30_60", "dist_60_120", "dist_120_180", "dist_180_240", "dist_240_360", "dist_360_480", "dist_480_p"))]

## Calculate distribution of Total Tour Distances
TourDistDistr <- TourDistDistr %>% 
  group_by(dist_bin) %>% 
  tally() %>% 
  mutate(Target = n/sum(n)) %>% 
  rename(TourDist = dist_bin) %>% 
  as.data.table()

write_csv(TourDistDistr, 'dev/Data_Processed/CVGPS/Calibration Targets/TotalTourDistance.csv')


## Mean Tour Distance
tourdist_mean <- GPS2 %>%
  group_by(`Tour ID`) %>% 
  mutate(TourDist = sum(trip_dist)) %>% 
  select(`Tour ID`, TourDist) %>% 
  distinct() %>% 
  ungroup() %>% 
  summarise(mean_tour = mean(TourDist))

write_csv(tourdist_mean, 'dev/Data_Processed/CVGPS/Calibration Targets/TotalTourDistance_mean.csv')


# Distribution of Trip Distances (not including return to base trip)

## Overall
GPS2[, TripDist1Mile := ceiling(trip_dist/ 1) * 1]

TripDistances1Mile <- GPS2 %>% 
  group_by(TripDist1Mile) %>% 
  tally() %>% 
  mutate(Target = n/sum(n)) %>% 
  select(Trip_dist = TripDist1Mile, n, Target)

write_csv(TripDistances1Mile, 'dev/Data_Processed/CVGPS/Calibration Targets/TripDistanceDistribution.csv')



## By Base District
TripDistances1Mile_baseDistrict <- GPS2 %>% 
  group_by(BaseDistrict, TripDist1Mile) %>% 
  tally() %>% 
  mutate(Target = n/sum(n)) %>% 
  select(vehicleBase_District = BaseDistrict, Trip_dist = TripDist1Mile, n, Target)

write_csv(TripDistances1Mile_baseDistrict, 'dev/Data_Processed/CVGPS/Calibration Targets/TripDistanceDistribution_baseDistrict.csv')

## By Trip Origin District
TripDistances1Mile_tripOriginDistrict <- GPS2 %>% 
  group_by(ODistrict, TripDist1Mile) %>% 
  tally() %>% 
  mutate(Target = n/sum(n)) %>% 
  select(tripOrigin_District = ODistrict, Trip_dist = TripDist1Mile, n, Target)

write_csv(TripDistances1Mile_tripOriginDistrict, 'dev/Data_Processed/CVGPS/Calibration Targets/TripDistanceDistribution_tripOriginDistrict.csv')

## for dashboard table
dash_MeanTripDist <- GPS2 %>% 
  summarise(Field = 'Trips: Distance', Statistic = 'Mean (Miles)', N = n(), value = mean(trip_dist))





# Valid Tours per Vehicle Weight Class -----
GPS %>% 
  select(Vehicle, `Tour ID`) %>% 
  distinct() %>% 
  group_by(Vehicle) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))



# Distribution of total tour duration

## Get total tour time (Not including return to base trip)
GPS2 <- GPS2 %>% 
  group_by(`Tour ID`) %>% 
  mutate(TTravTime = sum(traveltime_sec),
         TStopTime = sum(stopTime),
         TTourTime = TTravTime + TStopTime)

GPS2 <- GPS2 %>% 
  as.data.table()

## Set Duration Bins
duration_bins <- c(0, 30, 60, 120, 180, 240, 360, 480)

## Assign Total tour times to bins amd label
GPS2[, duration_bin := findInterval(TTourTime, duration_bins)]
GPS2[, duration_bin := factor(duration_bin, labels = c("< 30 mins", "30-59 mins", "1-2 hours", 
                                                                "2-3 hours", "3-4 hours", "4-6 hours", 
                                                                "6-8 hours", "8+ hours"))]


## Calculate Distribution of total duration time
tourDuration_CVGPS <- GPS2 %>%
  select(`Tour ID`, duration_bin) %>% 
  distinct() %>% 
  filter(!is.na(duration_bin)) %>% 
  group_by(duration_bin) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(tourDuration_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/tourDuration_CVGPS.csv')


## Mean Tour Duration Time
tourDuration_mean <- GPS2 %>% 
  select(`Tour ID`, TTourTime) %>% 
  distinct() %>% 
  summarise(meanDur = mean(TTourTime))

write_csv(tourDuration_mean, 'dev/Data_Processed/CVGPS/Calibration Targets/tourDuration_mean_CVGPS.csv')





## Calculate Distribution of total duration time by vehicle type
tourDuration_Vehicle_CVPGS <- GPS2 %>%
  select(`Tour ID`, Vehicle = VehicleWeightClass_1, duration_bin) %>% 
  distinct() %>% 
  filter(!is.na(duration_bin)) %>% 
  group_by(Vehicle, duration_bin) %>% 
  tally() %>% 
  group_by(Vehicle) %>% 
  mutate(Target = n/sum(n)) %>% 
  ungroup()

write_csv(tourDuration_Vehicle_CVPGS, 'dev/Data_Processed/CVGPS/Calibration Targets/tourDuration_vehicle_CVGPS.csv')





# Tour First Arrival Time Distribution --------------------------------------

## Convert First Arrival Minutes after Midnight to half-hour time bins (0 - 23.5)
GPS[, startTime30 := floor(FirstStop_MaM / 30) * 30]
GPS <- GPS %>%
  mutate(startHour = startTime30/60)


## Calculate Distribution of First Stop Arrival Times
TourFirstArrival_CVGPS <- GPS %>%
  select(`Tour ID`, startHour) %>% 
  distinct() %>% 
  group_by(startHour) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(TourFirstArrival_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/TourFirstArrival_CVGPS.csv')


## Calculate Distribution of First Stop Arrival Times by vehicle type
TourFirstArrival_vehicle_CVGPS <- GPS %>%
  select(`Tour ID`, Vehicle, startHour) %>% 
  distinct() %>% 
  group_by(Vehicle, startHour) %>% 
  tally() %>% 
  group_by(Vehicle) %>% 
  mutate(Target = n/sum(n)) %>% 
  ungroup()

write_csv(TourFirstArrival_vehicle_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/TourFirstArrival_vehicle_CVGPS.csv')





# Trip Start Hour Distribution ---------------------------------------------

## Calculate distribution of trip start hours (except return to base)
TripDepartHour_CVGPS <- GPS %>% 
  group_by(StopDepartHour) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(TripDepartHour_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/TripDepartHour_CVGPS.csv')



## Calculate distribution of trip start hours (except return to base) by Vehicle Base District
TripDepartHour_baseDistrict_CVGPS <- GPS %>% 
  group_by(BaseDistrict, StopDepartHour) %>% 
  tally() %>% 
  mutate(Target = n/sum(n)) %>% 
  rename(vehicleBase_district = BaseDistrict)

write_csv(TripDepartHour_baseDistrict_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/TripDepartHour_baseDistrict_CVGPS.csv')



## Calculate distribution of trip start hours (except return to base) by Trip Origin District
TripDepartHour_tripOriginDistrict_CVGPS <- GPS %>% 
  group_by(ODistrict, StopDepartHour) %>% 
  tally() %>% 
  mutate(Target = n/sum(n)) %>% 
  rename(tripOrigin_district = ODistrict)

write_csv(TripDepartHour_tripOriginDistrict_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/TripDepartHour_tripOriginDistrict_CVGPS.csv')



## Calculate distribution of trip start hours (except return to base) by vehicle type
TripDepartHour_vehicle_CVGPS <- GPS %>% 
  group_by(VehicleWeightClass_1, StopDepartHour) %>% 
  tally() %>% 
  mutate(Target =n/sum(n))

write_csv(TripDepartHour_vehicle_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/TripDepartHour_vehicle_CVGPS.csv')




# County-County OD Distribution -----------------------------------------

#county share all trips
library(sf)
## Read zone data to attach County FIPS
zone_dict <- read_sf('dev/Data_Processed/TAZ/subzones17.shp') %>% 
  st_drop_geometry() %>% 
  select(SZ = subzone17,county_fip)


Origin <- zone_dict %>% 
  select(SZ, OCounty_FIPS = county_fip)

Destination <- zone_dict %>% 
  select(SZ, DCounty_FIPS = county_fip)


## Attach fips to GPS Data
GPS <- GPS %>% 
  left_join(Origin, by = c('Origin_SZ17PlusCounties' = 'SZ')) %>%
  left_join(Destination, by = c('Destination_SZ17PlusCounties' = 'SZ'))


## Calculate Shares of OD by County 
CountyShare <- GPS %>% 
  group_by(origin_countyState, OCounty_FIPS, dest_countyState, DCounty_FIPS) %>% 
  tally() %>%
  ungroup() %>% 
  mutate(Target = n/sum(n))

write_csv(CountyShare, 'dev/Data_Processed/CVGPS/Calibration Targets/CountyOD.csv')



## County OD Share - Light Vehicles
CountyShare_light <- GPS %>% 
  filter(Vehicle == 'Light') %>% 
  group_by(origin_countyState, OCounty_FIPS, dest_countyState, DCounty_FIPS) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(Target = n/sum(n))

write_csv(CountyShare_light, 'dev/Data_Processed/CVGPS/Calibration Targets/CountyOD_light.csv')



## County OD Share - Medium Vehicles
CountyShare_medium <- GPS %>% 
  filter(Vehicle == 'Medium') %>% 
  group_by(origin_countyState, OCounty_FIPS, dest_countyState, DCounty_FIPS) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(Target = n/sum(n))

write_csv(CountyShare_medium, 'dev/Data_Processed/CVGPS/Calibration Targets/CountyOD_medium.csv')



## District-District OD Share ----------------------------------------------------
DistrictShare <- GPS %>% 
  group_by(ODistrict, ODistrict_num, DDistrict, DDistrict_num) %>% 
  tally() %>%
  ungroup() %>% 
  mutate(Target = n/sum(n))


write_csv(DistrictShare, 'dev/Data_Processed/CVGPS/Calibration Targets/DistrictOD.csv')



## District-District OD Share- Light Vehicles
DistrictShare_light <- GPS %>% 
  filter(Vehicle == 'Light') %>% 
  group_by(ODistrict, ODistrict_num, DDistrict, DDistrict_num) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(Target = n/sum(n))

write_csv(DistrictShare_light, 'dev/Data_Processed/CVGPS/Calibration Targets/DistrictOD_light.csv')



## District-District OD Share - Medium Vehicles
DistrictShare_medium <- GPS %>% 
  filter(Vehicle == 'Medium') %>% 
  group_by(ODistrict, ODistrict_num, DDistrict, DDistrict_num) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(Target = n/sum(n))

write_csv(DistrictShare_medium, 'dev/Data_Processed/CVGPS/Calibration Targets/DistrictOD_medium.csv')





# Tour Repeat Stops----------------------------------------------------------
## looking for stops that end in the same zone as the previous stop, how prevalent?


##Total tours in the sample: 34,543
GPS %>% 
  select(`Tour ID`) %>% 
  distinct() %>% 
  tally()


## identifies if there was a stop that was visited multiple times on a tour and how many times it was visited
gps_repeat_zones <- GPS %>% 
  select(Tour_ID = `Tour ID`, tour_leg, origin_zone, dest_zone) %>% 
  group_by(Tour_ID, dest_zone) %>% 
  tally() %>% 
  filter(n > 1) %>% 
  rename(stops = n) %>%
  group_by(Tour_ID) %>% 
  mutate(stop = row_number())





## for the above tally of repeat destinations, how many unique destinations are being visited more than once
gps_uniqueRepeats <- gps_repeat_zones %>% 
  slice(which.max(stop)) %>% 
  group_by(stop) %>% 
  tally() %>% 
  mutate(Target_amongRepeatTours= n/sum(n),
         Target_amongAllTours = (n/34453)) %>% 
  rename(RepeatZones = stop,
         Tours = n) 

write_csv(gps_uniqueRepeats, 'dev/Data_Processed/CVGPS/Calibration Targets/repeatZones_UniqueZones_CVPGS.csv')

## for the above tally, when destination zones are visited more than once what is the mean stops at a repeat for each tour, 
## then what is the distribution of that across tours
gps_repeat_stopMean <- gps_repeat_zones %>% 
  group_by(Tour_ID) %>% 
  summarise(meanRepeats = round(mean(stops), 0))


tour_meanRepeat_distr <- gps_repeat_stopMean %>% 
  group_by(meanRepeats) %>% 
  tally() %>% 
  mutate(Target = n/sum(n)) %>% 
  rename(meanRepeatsPerStop = meanRepeats,
         Tours = n)
write_csv(tour_meanRepeat_distr, 'dev/Data_Processed/CVGPS/Calibration Targets/repeatZones_nVisits_CVGPS.csv')








  

  
  




























# Intra Tour Stop Clustering Matrices (For MultiStop Tours) -----------------

## Using a subset of all trips except for return to base trips
## Create a matrix of distances between all stops on a tour, calculate mean for every tour
## Calculate distribution of mean tour matrix distances




## Read Skims
skims_tod <- readRDS('E:/Projects/Clients/CMAP/cmap_csvm/scenarios/base/outputs/skims_tod.rds')


## selecting just dist.avg, average distance across TOD
skims_tod <- skims_tod %>% 
  select(OTAZ, DTAZ, skims_dist = dist.avg)




## Get GPS Data into format and filter to multi-stop tours
### For each tour, one row per trip with origin and destination zones and trip distance (from Skims)
GPS_forMat <- GPS %>%
  filter(single_multi == 'multi') %>% 
  select(Tour_ID = `Tour ID`, OTAZ = origin_zone, DTAZ = dest_zone) %>% 
  left_join(skims_tod, by = c('OTAZ', 'DTAZ'))
  

## Identify tours with NA Origin or Destination
GroupsWithNA <- GPS_forMat %>% 
  filter(is.na(OTAZ) | is.na(DTAZ)) %>% 
  select(Tour_ID) %>% 
  distinct()


## Remove tours with NA Origin or Destination
GPS_forMat <- GPS_forMat %>% 
  filter(!Tour_ID %in% GroupsWithNA$Tour_ID)



#Intra-Tour Stop Matrix Function -------------------------------------
## Format long table of trips into table of Tour_Ids and list of Destination TAZs

GPS_forMat_Dests <- GPS_forMat %>%
  select(Tour_ID, DTAZ) %>% 
  group_by(Tour_ID) %>% 
  mutate(DTAZ = list(DTAZ)) %>% 
  distinct() %>% 
  as.data.table()

# alternate if we need EVERY combination of Destination AND origins 
# GPS_forMat_Dests <- GPS_forMat %>%
#   select(Tour_ID, OTAZ, DTAZ) %>% 
#   group_by(Tour_ID) %>% 
#   mutate( list = list(c(OTAZ, DTAZ))) %>% 
#   select(Tour_ID, list) %>% 
#   distinct() %>% 
#   as.data.table()

## Initialize a clean copy of the long format table (1 row for every trip on every tour)
table <- GPS_forMat %>% 
  as.data.table()
## Initialize an empty table where we will write the average intra-trip distance for each tour_id
empty_table <- data.table(Tour_ID = numeric(), MeanDist = numeric())
## Intitialize a Tour_ID counter
i = 1
## Specify list of columns we'll use to join tthe skims data onto our matrix inside the function
columns <- c('Tour_ID', 'OTAZ', 'DTAZ', 'skims_dist')




for (i in GPS_forMat_Dests$Tour_ID){
  
    ## Initialize a table with the trips on tour_id == i 
    temp_table <- table[table$Tour_ID == i]
    
    ## For each Trip_ID, we add our Trip Destination to a list
    list <- unlist(GPS_forMat_Dests$DTAZ[GPS_forMat_Dests$Tour_ID == i])
    
    ## If the list only has one dest for some reason, move on
    if (length(list) ==1){
      next
    }

    ## We create a list of combinations that result from the list
    mat <- combn(list, 2)
    row.names(mat) <- c('OTAZ', 'DTAZ')
    
    ## Transpose it the long way (origin taz, dest taz)
    mat <- t(mat) %>% 
      as.data.table() %>% 
      distinct()
    
    ## Join the skims distance for OD combinations in this tour_id
    mat <- skims_tod[mat, on = c('OTAZ', 'DTAZ')]
    mat[, Tour_ID := i]
    mat[, columns, with = F]
    
    ## Add these rows into our original table with the Trips
    temp_table <- bind_rows(temp_table, mat)
    
    ## Summarise the average distance between the destinations in this tour_id
    temp_table <- temp_table %>% 
      summarise(Tour_ID, MeanDist = mean(skims_dist)) %>% 
      distinct()
    
    ## Wirte the tour_id and mean distance to the empty_table we initialized
    empty_table <- bind_rows(empty_table, temp_table)
    
    ## Print trip_id to keep track of function progress
    print(i)
}



# Process Intra-Tour Stop Matrix Output -----
## Remove non-unique rows
table <- empty_table %>% 
  distinct()

## Plot distributon to check if reasonable
ggplot(table, aes(x = MeanDist)) + geom_histogram(binwidth = 1)



# Create table of distribution with 1 mile bins
## Set output as data.table
TourODMatrix_Dist_Distr <- table %>% 
  as.data.table()

## Create integer value for distance
TourODMatrix_Dist_Distr[, TourMatrixAvgDist := floor(MeanDist)]

## Calculate Distribution of Tour Destination Distance Matrix
TourODMatrix_Dist_Distr2 <- TourODMatrix_Dist_Distr %>% 
  group_by(TourMatrixAvgDist) %>% 
  tally() %>% 
  mutate(Target = n/sum(n)) %>% 
  as.data.table()

## Write Distribution Table
write_csv(TourODMatrix_Dist_Distr2, 'dev/Data_Processed/CVGPS/Calibration Targets/TourODMatrixDistAvg_CVGPS.csv')
















# Travelling Salesman Comparison -----
## Section Description:
## The proceeding code takes 






## Create a Table called LastStop.
### Used in processing the output of the TSP Algorithm
LastStop <- GPS2 %>% 
  group_by(`Tour ID`) %>% 
  slice(which.max(tour_leg)) %>% 
  select(Tour_ID = `Tour ID`, Input_TAZs = dest_zone)


## Calculate a total distance for tours with all the stops
### Will be used to compare TSP results to actual distance travelled
Actual_Dist <- GPS2 %>%
  select(`Tour ID`, trip_dist) %>%
  group_by(`Tour ID`) %>%
  summarise(`Tour ID`, Actual_Dist = sum(trip_dist)) %>%
  distinct()




# TSP: Transforming the Data for Input to the Function ---------------------------

## Remove the last trip of the tour ??? I THINK THIS COULD USE SOME DISCUSSION
### Will add it back on after the TSP has run
GPS2 <- GPS2 %>%
  filter(tour_leg != Trips) 


## Pull out all the trips on every tour (except for the final trip)
GPS_TSA <- GPS2 %>% 
  filter(tour_leg != Trips) %>% 
  filter(NStops > 1) %>% 
  select(Tour_ID = `Tour ID`, tour_leg, Trips, OTAZ = origin_zone, DTAZ = dest_zone, trip_dist, TTourTime)

## Create a temporary Table that pulls out the origin zone of the first trip
TSA_Base <- GPS_TSA %>% 
  select(Tour_ID, tour_leg, OTAZ) %>% 
  filter(tour_leg == 1) %>% 
  select(Tour_ID, BaseTAZ = OTAZ)

## Join the Tour Origin zone to the working data
GPS_TSA2 <- GPS_TSA %>% 
  select(Tour_ID, tour_leg, StopTAZ = DTAZ) %>% 
  left_join(TSA_Base, by = 'Tour_ID') %>% 
  select(Tour_ID, Tour_Sequence = tour_leg, BaseTAZ, StopTAZ)

## Collapse all the destinations for a tour into a list
## Table now contains one row per tour (Tour_ID, Tour Start Zone, List of Tour Destinations)
GPS_TSA2 <- GPS_TSA2 %>% 
  group_by(Tour_ID, BaseTAZ) %>%  
  mutate(stop.TAZs = list(StopTAZ)) %>% 
  select(Tour_ID, BaseTAZ, stop.TAZs) %>% 
  filter(!is.na(BaseTAZ)) %>% 
  distinct()


### TSP: Reading and Transforming Long Format Skims to Matrix --------------

## Read Skims Data
skims_tod <- readRDS('E:/Projects/Clients/CMAP/cmap_csvm/scenarios/base/outputs/skims_tod.rds')


## Pull out dist.avg, average distance across TOD
skims_tod <- skims_tod %>% 
  select(OTAZ, DTAZ, skims_dist = dist.avg)

## We cast the long-format skims data to a matrix
dist.mat.untouched <- as.matrix(dcast.data.table(data = skims_tod,
                                        formula = OTAZ~DTAZ,
                                        value.var = "skims_dist")[,-1])




### TSP: Function: Preparation ------------------------------------------
# Start here: Clean Matrix

## We initialize a working skims matrix
dist.mat <- dist.mat.untouched

## Label the matrix rows and columns
rownames(dist.mat) <- colnames(dist.mat)
shiftleft <- function(x, shift) c(x, x)[(1 + shift):(length(x) + shift)]

# Initial States

## A Count Variable
i = 1

## An empty table to write the TSP Solution for each tour
TSP_SOLUTIONS <- data.table(Tour_ID = numeric(), Input_leg = numeric(), TSP_leg = numeric())

## A table that keeps track of a tour destination's actual tour_leg position 
InputRefTable <- data.table(Tour_ID = numeric(), Input_TAZs = numeric(), Input_leg = numeric())
set.seed(BASE_SEED_VALUE)




# THE TSP Function ----------------------------------------------------

## For every tour_id in the input data
for (i in GPS_TSA2$Tour_ID){
  
  ## Print i to keep track of progress
  print(i)
  
  ## Define the Base and Destination Zones of interest for this Tour_ID as a character vector
  idx <- c(as.character(GPS_TSA2$BaseTAZ[GPS_TSA2$Tour_ID == i]), 
           as.character(unlist(GPS_TSA2$stop.TAZs[GPS_TSA2$Tour_ID == i]))
           )

  ## Subset the Skims Distance matrix to zonews in idx
  dist.mat.subset <- dist.mat[idx, idx,drop = FALSE]
  
  ### NOT SURE THE FUNCTIONS ARE WORKING AS INTENDED

  ## Plug in the above Matrix into the Travelling Salesman Function
  ### ATSP: Assymteric Travelling Salesman Problem - Direction Matters
  TSP_Solution <- solve_TSP(ATSP(dist.mat.subset)) 
  TSP_Solution <- as.numeric(TSP_Solution)
  
  ## Shift the solution so that the actual tour start is the origin in the TSP Solution
  TSP_Solution_2 <- shiftleft(TSP_Solution, which(TSP_Solution == 1) - 1)
  

  ## Write solution to an empty table, row by row as we iterate over each trip on the tour
  temp_table = data.table(Tour_ID = i, Input_leg = TSP_Solution)
  
  
  ## Now that we have all of our stops in order, we write the TSP order 
  temp_table[, TSP_leg := seq_len(.N), by = Tour_ID]
  
  ## We add this solution to the empty table we initialized prior to running the function
  ## Line by line as we iterate through every tour
  TSP_SOLUTIONS <- bind_rows(TSP_SOLUTIONS, temp_table)
  
  
  ## At the same time we also also write an input reference table row by row
  ## This keeps track of which Zone corresponds to eachtrip leg in the input data
  idx <- as.numeric(idx)
  ref_temp <- data.table(Tour_ID = i, Input_TAZs = idx)
  ref_temp[, Input_leg := seq_len(.N), by = Tour_ID]
  
  InputRefTable <- bind_rows(InputRefTable, ref_temp)
  
}

## Output is TSP_SOLUTIONS: (Tour_ID, 
##                           Input_leg: Order of Destinations based on their position in the input data 
##                           TSP_leg: A new Tour_leg to keep this solution in order)


#TSP: Process Output----------------------------------------------

## Join the TAZs onto the TSP solutions using the reference table produced by the function
TSP_SOLUTIONS2 <- TSP_SOLUTIONS %>% 
  left_join(InputRefTable, by = c('Tour_ID', 'Input_leg'))





## Bind the last stop onto the solution table and arrange
### Becuase input_leg and TSP_leg will be NA, they will go last which is where we want them
## Then we compute a lead to construt trips from this list of TAZs

TSP_SOLUTIONS2 <- bind_rows(TSP_SOLUTIONS2, LastStop) %>% 
  arrange(Tour_ID, TSP_leg) %>% 
  mutate(dest = lead(Input_TAZs)) 

## Recalculating TSP_Leg to index this last stop
TSP_SOLUTIONS2[, TSP_leg := seq_len(.N), by = Tour_ID]
  

## The last trip we constructed with the lead function isnt a trip so we drop it
TSP_SOLUTIONS2  <- TSP_SOLUTIONS2 %>% 
  filter(!is.na(Input_leg)) %>% 
  rename(Origin = Input_TAZs,
         Dest = dest)


## Then we join on the skims distances onto those trips using TAZs
## Sum the distance to get the distnace of the TSP Solution
TSP_SOLUTIONS2 <- TSP_SOLUTIONS2 %>% 
  left_join(skims_tod, by = c('Origin'='OTAZ', 'Dest' = 'DTAZ'))







TSP_SOLUTIONS3 <- TSP_SOLUTIONS2 %>% 
  group_by(Tour_ID) %>%
  summarise(Tour_ID, TSP_dist = sum(skims_dist)) %>% 
  distinct() 


## Join the actual distance for comparison to TSP Solution
### There are cases where the actual distance is short than the TSP distance
### I think this has to do with missing trips in the data? Jumping around that the TSP wont accoutn for in its current form
TSP_SOLUTIONS3 <- TSP_SOLUTIONS3 %>% 
  left_join(Actual_Dist, by = c('Tour_ID' = 'Tour ID')) %>% 
  filter(Actual_Dist != 0)





#TSP: Summarise output -------------------------------
## Compute the ratio of the actual to TSP distance and round to two digits

##Round the Ratio to the nearest .05
TSP_SOLUTIONS3 <- TSP_SOLUTIONS3 %>% 
  mutate(ratio = floor(Actual_Dist/TSP_dist * 100)/100) %>% 
  mutate(ratio = floor(ratio * 20)/20)
 
library(tidyverse)

## Group by Ratio and tally
TSPvActual <- TSP_SOLUTIONS3 %>% 
  group_by(ratio) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(TSPvActual, 'dev/Data_Processed/CVGPS/Calibration Targets/CVGPS_TSP_vs_Actual.csv')

















# D.T. of tour characteristics -------------------------------------------

GPSChars <- GPS %>% 
  select(Tour_ID = `Tour ID`, VehicleWeightClass_1, NStops, Trips) %>% 
  distinct()


TourSeqChars <- TSP_SOLUTIONS3 %>%
  left_join(empty_table, by = 'Tour_ID') %>% 
  left_join(GPSChars, by = "Tour_ID") %>% 
  filter(NStops > 1)

write.csv(TourSeqChars, 'dev/Data_Processed/CVGPS/Calibration Targets/CVGPS_TourCluster_chars.csv')

ggplot(TourSeqChars, aes(x = NStops, y = MeanDist)) + 
  geom_point() + 
  geom_smooth(method = 'lm')
ggsave('dev/Data_Processed/CVGPS/Calibration Targets/GGPlot_MeanDist_byNStops.tiff', width = 7, height = 7, device = 'tiff', dpi = 600)

ggplot(TourSeqChars, aes(x = MeanDist, y = ratio)) + 
  geom_point() + 
  geom_smooth(method = 'lm')
ggsave('dev/Data_Processed/CVGPS/Calibration Targets/GGPlot_TSPRatioby_MeanDist.tiff', width = 7, height = 7, device = 'tiff', dpi = 600)














# High Level Overview of Trip Charactics (for dashboard) -----------------


dash_MeanTourDist <- GPS2 %>% 
  select(`Tour ID`, TourDist) %>% 
  distinct() %>% 
  summarise(Field = 'Tours: Distance', Statistic = 'Mean (Miles)', N = n(), value = mean(TourDist))

dash_nStops <- GPS %>% 
  select(`Tour ID`, NStops) %>% 
  distinct() %>% 
  summarise(Field = 'Tours: Number of Stops (Excluding Return to Base)', 
            Statistic = 'Mean (# Stops)',
            N = n(),
            value = mean(NStops)
            )

dash_singleStop <- GPS %>% 
  select(`Tour ID`, single_multi) %>% 
  distinct() %>% 
  group_by(single_multi) %>% 
  tally() %>% 
  mutate(value = n/sum(n)) %>% 
  mutate(n = sum(n)) %>% 
  filter(single_multi == 'single') %>% 
  select(N = n, value) %>% 
  mutate(Field = 'Tours: Single Stop Tours (vs. Multi-Stop)', Statistic = 'Proportion') %>% 
  select(Field, Statistic, N, value)


dash_baseStopDist <- GPS %>% 
  summarise(Field = 'Trips: Base-to-Stop Distance', Statistic = 'Mean (Miles)', N = n(), value = mean(base_stop_dist))

dash_clusterDistance <- table %>% 
  summarise(Field = 'Tours: Cluster Distance', Statistic = 'Mean (Miles)', N = n(), value = mean(MeanDist))

dash_stopDuration <- GPS %>% 
  summarise(Field = 'Trips: Stop Duration', Statistic = 'Mean (Minutes)', N = sum(!is.na(stopTime)), value = mean(stopTime, na.rm = T))

dash_tourDuration <- GPS2 %>% 
  select(`Tour ID`, TTourTime) %>% 
  distinct() %>% 
  summarise(Field = 'Tours: Tour Duration', Statistic = 'Mean (Minutes)', N = sum(!is.na(TTourTime)), value = mean(TTourTime, na.rm = T))





dash_table <- bind_rows(dash_MeanTripDist, 
                    dash_MeanTourDist,
                    dash_nStops,
                    dash_singleStop,
                    dash_baseStopDist,
                    dash_clusterDistance,
                    dash_stopDuration,
                    dash_tourDuration) %>% 
  mutate(value = round(value, 2))

write_csv(dash_table, 'dev/Data_Processed/CVGPS/Calibration Targets/dashboard_table.csv')








