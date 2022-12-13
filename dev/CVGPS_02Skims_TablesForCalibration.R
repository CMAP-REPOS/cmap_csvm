#Takes Prepared GPS data from previous Script and creates summary calibration targets


# Setup -------------------------------------------------------------------

source("./dev/init_dev.R")
library(readxl)
library(tidyverse)
library(rbin)
SYSTEM_DEV_DATA_PATH <- file.path(SYSTEM_DEV_PATH, 'DATA_Processed')

file <- file.path(SYSTEM_DEV_PATH, 'Data_Processed', 'CVGPS', 'ToursTripsCharacteristics_SKIMS.csv')
GPS <- read_csv(file)

GPS2 <- GPS

ToursWithNA <- GPS2 %>% 
  select(Tour_ID = `Tour ID`, OTAZ = origin_zone, DTAZ = dest_zone, trip_dist, stopTime, base_stop_dist, TourType, `Reporting  Concern Indicated by "1"`) %>% 
  filter(is.na(stopTime) | is.na(base_stop_dist) | is.na(TourType) | is.na(trip_dist) | `Reporting  Concern Indicated by "1"` == 1 | is.na(OTAZ) | is.na(DTAZ)) %>% 
  distinct()

GPS <- GPS %>% 
  filter(tour_leg != Trips) %>% 
  filter(!`Tour ID` %in% c(ToursWithNA$Tour_ID))

GPS2 <- GPS2 %>% 
  filter(!`Tour ID` %in% c(ToursWithNA$Tour_ID))

#creating a column for final TAZ in tour (For Travelling salesman)
LastStop <- GPS2 %>% 
  group_by(`Tour ID`) %>% 
  slice(which.max(tour_leg)) %>% 
  select(Tour_ID = `Tour ID`, Input_TAZs = dest_zone)

Actual_Dist <- GPS2 %>% 
  select(`Tour ID`, trip_dist) %>% 
  group_by(`Tour ID`) %>% 
  summarise(`Tour ID`, Actual_Dist = sum(trip_dist)) %>% 
  distinct()

GPS2 <- GPS2 %>% 
  filter(tour_leg != Trips)



# Tour N StopsDistribution -----
NStopsDistr <- GPS2 %>% 
  select(`Tour ID`, NStops) %>%
  distinct() %>% 
  group_by(NStops) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(NStopsDistr, 'dev/Data_Processed/CVGPS/Calibration Targets/Tour_NStops_CVGPS.csv')


#Tour Single Vs Multi-Stop
Tours_SingleVsMulti <- GPS2 %>% 
  select(`Tour ID`, Trips, NStops, single_multi, TourType) %>% 
  distinct() %>% 
  group_by(single_multi) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(Tours_SingleVsMulti, 'dev/Data_Processed/CVGPS/Calibration Targets/TourSingleMulti_CVGPS.csv')

TourType_CVGPS <- GPS2 %>% 
  select(`Tour ID`,TourType, single_multi) %>% 
  distinct() %>% 
  group_by(single_multi, TourType) %>% 
  tally() %>% 
  filter(!is.na(TourType))

write_csv(TourType_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/TourType_CVGPS.csv')



# stop durations -----
#using the subset of trips that are not the final trip in the tour,
#are not 0 stop time
#and do not have a trip distance of zero

GPS <- GPS %>% 
  as.data.table()

GPS <- GPS %>% 
  mutate(Vehicle = case_when(VehicleWeightClass_1 == 'Light Duty Truck/Passenger Vehicle: Ranges from 0 to 14000 lb.' ~ 'Light',
                             VehicleWeightClass_1 == 'Medium Duty Trucks / Vans: ranges from 14001-26000 lb.' ~ 'Medium'))

GPS[, duration_15min := ceiling(stopTime / 15) * 15]
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
                         duration_15min %in% c(270 + 1:8 * 15), 10,
                         duration_15min %in% c(390 + 1:14 * 15) | duration_15min > 600, 11)]

GPS[, duration_group := factor(duration_group, labels = c("1-15", "16-30", "31-45", "46-60", "61-75", "76-90", "91-150", "151-210", "211-270", "271-390"))] #, "391+" 

##stop duration
duration_stops_CVGPS <- GPS %>% 
  group_by(duration_group) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(duration_stops_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/duration_stops_CVGPS.csv')

## stop duartion by vehicle type
duration_stops_vehicle_CVGPS <- GPS %>% 
  group_by(Vehicle, duration_group) %>% 
  tally() %>%
  group_by(Vehicle) %>% 
  mutate(Target = n/sum(n)) %>% 
  ungroup()
write_csv(duration_stops_vehicle_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/duration_stops_vehicle_CVGPS.csv')





# Base-Stop Distances -----
dist_bins <- c(0, 2, 5, 10, 20)
GPS[, dist_bin := factor(findInterval(x = base_stop_dist, vec = dist_bins),
                                     labels = c("dist_00_02", "dist_02_05", "dist_05_10", "dist_10_20", "dist_20_p"))]

BaseStopDist_CVGPS <- GPS %>% 
  group_by(dist_bin) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(BaseStopDist_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/BaseStopDist_CVGPS.csv')

BaseStopDist_vehicle_CVGPS <- GPS %>% 
  group_by(Vehicle, dist_bin) %>% 
  tally() %>% 
  group_by(Vehicle) %>% 
  mutate(Target = n/sum(n)) %>% 
  ungroup()

write_csv(BaseStopDist_vehicle_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/BaseStopDist_vehicle_CVGPS.csv')






# Trip/Tour Distances -----

#total tour distance
TourDistDistr <- GPS %>% 
  group_by(`Tour ID`) %>% 
  mutate(TourDist = sum(trip_dist)) %>% 
  select(`Tour ID`, TourDist) %>% 
  distinct() %>% 
  as.data.table()

tour_dist_bins <- c(0, 30, 60, 120, 180, 240, 360, 480)
TourDistDistr[, dist_bin := factor(findInterval(x = TourDist, vec = tour_dist_bins),
                         labels = c("dist_00_30", "dist_30_60", "dist_60_120", "dist_120_180", "dist_180_240", "dist_240_360", "dist_360_480", "dist_480_p"))]
TourDistDistr <- TourDistDistr %>% 
  group_by(dist_bin) %>% 
  tally() %>% 
  mutate(Target = n/sum(n)) %>% 
  rename(TourDist = dist_bin) %>% 
  as.data.table()


write_csv(TourDistDistr, 'dev/Data_Processed/CVGPS/Calibration Targets/TotalTourDistance.csv')


#trip distances
GPS[, TripDist1Mile := ceiling(trip_dist/ 1) * 1]

TripDistances1Mile <- GPS %>% 
  group_by(TripDist1Mile) %>% 
  tally() %>% 
  mutate(Target = n/sum(n)) %>% 
  select(Trip_dist = TripDist1Mile, n, Target)

write_csv(TripDistances1Mile, 'dev/Data_Processed/CVGPS/Calibration Targets/TripDistanceDistribution.csv')


# n tours -----
GPS %>% 
  select(Vehicle, `Tour ID`) %>% 
  distinct() %>% 
  group_by(Vehicle) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))



#total tour duration -----
GPS <- GPS %>% 
  group_by(`Tour ID`) %>% 
  mutate(TTravTime = sum(traveltime_sec),
         TStopTime = sum(stopTime),
         TTourTime = TTravTime + TStopTime)



GPS <- GPS %>% 
  as.data.table()
duration_bins <- c(0, 30, 60, 120, 180, 240, 360, 480)
GPS[, duration_bin := findInterval(TTourTime, duration_bins)]
GPS[, duration_bin := factor(duration_bin, labels = c("< 30 mins", "30-59 mins", "1-2 hours", 
                                                                "2-3 hours", "3-4 hours", "4-6 hours", 
                                                                "6-8 hours", "8+ hours"))]
tourDuration_CVGPS <- GPS %>%
  select(`Tour ID`, duration_bin) %>% 
  distinct() %>% 
  group_by(duration_bin) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(tourDuration_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/tourDuration_CVGPS.csv')


tourDuration_Vehicle_CVPGS <- GPS %>%
  select(`Tour ID`, Vehicle, duration_bin) %>% 
  distinct() %>% 
  group_by(Vehicle, duration_bin) %>% 
  tally() %>% 
  group_by(Vehicle) %>% 
  mutate(Target = n/sum(n)) %>% 
  ungroup()

write_csv(tourDuration_Vehicle_CVPGS, 'dev/Data_Processed/CVGPS/Calibration Targets/tourDuration_vehicle_CVGPS.csv')




# Tour Start Hour -----
GPS[, startTime30 := ceiling(FirstStop_MaM / 30) * 30]
GPS <- GPS %>%
  mutate(startHour = startTime30/60)


#tour start time
TourFirstArrival_CVGPS <- GPS %>%
  select(`Tour ID`, startHour) %>% 
  distinct() %>% 
  group_by(startHour) %>% 
  tally() %>% 
  mutate(Target = n/sum(n))

write_csv(TourFirstArrival_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/TourFirstArrival_CVGPS.csv')


TourFirstArrival_vehicle_CVGPS <- GPS %>%
  select(`Tour ID`, Vehicle, startHour) %>% 
  distinct() %>% 
  group_by(Vehicle, startHour) %>% 
  tally() %>% 
  group_by(Vehicle) %>% 
  mutate(Target = n/sum(n)) %>% 
  ungroup()

write_csv(TourFirstArrival_vehicle_CVGPS, 'dev/Data_Processed/CVGPS/Calibration Targets/TourFirstArrival_vehicle_CVGPS.csv')







# County-County OD counts -----

#county share all trips
zone_dict <- read_sf('dev/Data_Processed/TAZ/subzones17.shp') %>% 
  st_drop_geometry() %>% 
  select(SZ = subzone17, county_nam, county_fip, state) %>% 
  unite(county_state, c('county_nam', 'state'), sep = ', ')


Origin <- zone_dict %>% 
  select(SZ, OCounty = county_state, OCounty_FIPS = county_fip)

Destination <- zone_dict %>% 
  select(SZ, DCounty = county_state, DCounty_FIPS = county_fip)

GPS <- GPS %>% 
  left_join(Origin, by = c('Origin_SZ17PlusCounties' = 'SZ')) %>%
  left_join(Destination, by = c('Destination_SZ17PlusCounties' = 'SZ'))


CountyShare <- GPS %>% 
  group_by(OCounty, OCounty_FIPS, DCounty, DCounty_FIPS) %>% 
  tally() %>%
  filter(!is.na(OCounty)) %>% 
  ungroup() %>% 
  mutate(Target = n/sum(n))

write_csv(CountyShare, 'dev/Data_Processed/CVGPS/Calibration Targets/CountyOD.csv')



#county share light vehicle trips
CountyShare_light <- GPS %>% 
  filter(Vehicle == 'Light') %>% 
  group_by(OCounty, OCounty_FIPS, DCounty, DCounty_FIPS) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(!is.na(OCounty)) %>% 
  filter(!is.na(DCounty)) %>% 
  mutate(Target = n/sum(n))

write_csv(CountyShare_light, 'dev/Data_Processed/CVGPS/Calibration Targets/CountyOD_light.csv')

CountyShare_medium <- GPS %>% 
  filter(Vehicle == 'Medium') %>% 
  group_by(OCounty, OCounty_FIPS, DCounty, DCounty_FIPS) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(!is.na(OCounty)) %>% 
  filter(!is.na(DCounty)) %>% 
  mutate(Target = n/sum(n))

write_csv(CountyShare_medium, 'dev/Data_Processed/CVGPS/Calibration Targets/CountyOD_medium.csv')


















#next steps - avg distance between stops and comparing traveling salesman distance and time to actual time and distnace
#need actual: total time, total distance, Origin & destination TAZ
#need skims

# Intra Tour Stop Distance Matrices (For MultiStop Tours)
#first filtering to those tours with more than one stop and using the subset without the return to base/home trip (last trip on tour)



#Read Skims
skims_tod <- readRDS('E:/Projects/Clients/CMAP/cmap_csvm/scenarios/base/outputs/skims_tod.rds')

#selecting just dist.avg, average distance across TOD
skims_tod <- skims_tod %>% 
  select(OTAZ, DTAZ, skims_dist = dist.avg)




#Get Data into format
GPS_forMat <- GPS %>%
  filter(single_multi == 'multi') %>% 
  select(Tour_ID = `Tour ID`, OTAZ = origin_zone, DTAZ = dest_zone) %>% 
  left_join(skims_tod, by = c('OTAZ', 'DTAZ'))
  
GroupsWithNA <- GPS_forMat %>% 
  filter(is.na(OTAZ) | is.na(DTAZ)) %>% 
  select(Tour_ID) %>% 
  distinct()

GPS_forMat <- GPS_forMat %>% 
  filter(!Tour_ID %in% GroupsWithNA$Tour_ID)






#THE MATRIX FUNCTION -----
#format long table of trip id and detinations as a trip_id and list of destinations to make the function run more efficiently

#this will result in the orignal trips + the unique combinations among the destination
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

#initialize a table with the actual trips in the tour
table <- GPS_forMat %>% 
  as.data.table()

#initialize a table where we will write the average for each tour_id
empty_table <- data.table(Tour_ID = numeric(), MeanDist = numeric())
i = 1
columns <- c('Tour_ID', 'OTAZ', 'DTAZ', 'skims_dist')
for (i in GPS_forMat_Dests$Tour_ID){
  
    #initialize a table with the existing trips for that tour_id
    temp_table <- table[table$Tour_ID == i]
    
    #for each trip_id, we add the destinations to a list
    list <- unlist(GPS_forMat_Dests$DTAZ[GPS_forMat_Dests$Tour_ID == i])
    #if the list only has one dest for some reason, move on
    if (length(list) ==1){
      next
    }

    #we create a list of combinations that result from the list
    mat <- combn(list, 2)
    row.names(mat) <- c('OTAZ', 'DTAZ')
    
    #transpose it the long way (origin taz, dest taz)
    mat <- t(mat) %>% 
      as.data.table() %>% 
      distinct()
    
    #join the skims distance for OD combinations in this tour_id
    mat <- skims_tod[mat, on = c('OTAZ', 'DTAZ')]
    mat[, Tour_ID := i]
    mat[, columns, with = F]
    
    #adds these rows into our original table with actual trips
    temp_table <- bind_rows(temp_table, mat)
    
    #summarise the average distance for the OD combinations in this tour_id
    temp_table <- temp_table %>% 
      summarise(Tour_ID, MeanDist = mean(skims_dist)) %>% 
      distinct()
    
    #write the tour_id and average to the empty_table
    empty_table <- bind_rows(empty_table, temp_table)
    
    #print trip_id to keep track of function progress
    print(i)
}



#Process Function Output -----
#remove any repeat rows
table <- empty_table %>% 
  distinct()

#plot distributon to check if reasonable
ggplot(table, aes(x = MeanDist)) + geom_histogram(binwidth = 1)



#create table of distribution with 1 mile bins
TourODMatrix_Dist_Distr <- table %>% 
  as.data.table()

TourODMatrix_Dist_Distr[, TourMatrixAvgDist := round(Mean_Dist, 0)]

TourODMatrix_Dist_Distr2 <- TourODMatrix_Dist_Distr %>% 
  group_by(TourMatrixAvgDist) %>% 
  tally() %>% 
  mutate(Target = n/sum(n)) %>% 
  as.data.table()

#write it out
write_csv(TourODMatrix_Dist_Distr2, 'dev/Data_Processed/CVGPS/Calibration Targets/TourODMatrixDistAvg_CVGPS.csv')





#then bin and summarise





##### From cv_sim_tours.R #####

#colin says use IDX to get the distances between the dest zones
#i can just append those onto my exisisting list and get the averages




# idx <- c(as.character(firm.TAZ), as.character(stop.TAZs))
# time.mat.subset <- time.mat[idx, idx, drop = FALSE]
# 
# # Convert skims to a matrix
# time.mat <- as.matrix(dcast.data.table(data = skims, 
#                                        formula = OTAZ~DTAZ, 
#                                        value.var = "time")[, -1])
# rownames(time.mat) <- colnames(time.mat)
















#Travelling Salesman Comparison-----
#summarizing actual tour distance
skims_tod <- readRDS('E:/Projects/Clients/CMAP/cmap_csvm/scenarios/base/outputs/skims_tod.rds')

#selecting just dist.avg, average distance across TOD
skims_tod <- skims_tod %>% 
  select(OTAZ, DTAZ, skims_dist = dist.avg)
#TSP: Transforming the Data for Input to the Function -----





GPS_TSA <- GPS2 %>% 
  filter(tour_leg != Trips) %>% 
  filter(NStops > 1) %>% 
  select(Tour_ID = `Tour ID`, tour_leg, Trips, OTAZ = origin_zone, DTAZ = dest_zone, trip_dist, TTourTime)


TSA_Base <- GPS_TSA %>% 
  select(Tour_ID, tour_leg, OTAZ) %>% 
  filter(tour_leg == 1) %>% 
  select(Tour_ID, BaseTAZ = OTAZ)


GPS_TSA2 <- GPS_TSA %>% 
  select(Tour_ID, tour_leg, StopTAZ = DTAZ) %>% 
  left_join(TSA_Base, by = 'Tour_ID') %>% 
  select(Tour_ID, Tour_Sequence = tour_leg, BaseTAZ, StopTAZ)


GPS_TSA2 <- GPS_TSA2 %>% 
  group_by(Tour_ID, BaseTAZ) %>%  
  mutate(stop.TAZs = list(StopTAZ)) %>% 
  select(Tour_ID, BaseTAZ, stop.TAZs) %>% 
  filter(!is.na(BaseTAZ)) %>% 
  distinct()


### TSP: Reading and Transforming Long Format Skims to Matrix -----------
#Read Skims

#selecting just dist.avg, average distance across TOD


dist.mat.untouched <- as.matrix(dcast.data.table(data = skims_tod,
                                        formula = OTAZ~DTAZ,
                                        value.var = "skims_dist")[,-1])










  




### TSP: Function -----------
#start here: Clean Matrix
dist.mat <- dist.mat.untouched
rownames(dist.mat) <- colnames(dist.mat)
shiftleft <- function(x, shift) c(x, x)[(1 + shift):(length(x) + shift)]

#Initial States
i = 1
TSP_SOLUTIONS <- data.table(Tour_ID = numeric(), Input_leg = numeric(), TSP_leg = numeric())
InputRefTable <- data.table(Tour_ID = numeric(), Input_TAZs = numeric(), Input_leg = numeric())
set.seed(BASE_SEED_VALUE)

for (i in GPS_TSA2$Tour_ID){
  print(i)
  #define the TAZs of interest for this Tour_ID
  idx <- c(as.character(GPS_TSA2$BaseTAZ[GPS_TSA2$Tour_ID == i]), 
           as.character(unlist(GPS_TSA2$stop.TAZs[GPS_TSA2$Tour_ID == i]))
           )

  
  #subset the dist.skims.mat to this
  dist.mat.subset <- dist.mat[idx, idx,drop = FALSE]
  
  #plug into the TSP
  TSP_Solution <- solve_TSP(ATSP(dist.mat.subset))
  TSP_Solution <- as.numeric(TSP_Solution)
  
  #shift so that first stop is the base/start
  TSP_Solution <- shiftleft(TSP_Solution, which(TSP_Solution == 1) - 1)

  #write solution to empty table row by row  
  temp_table = data.table(Tour_ID = i, Input_leg = TSP_Solution)
  
  
  #now that we have all of our stops in order, we write a TSP trip leg column
  temp_table[, TSP_leg := seq_len(.N), by = Tour_ID]
  
  #and we add this tour to the empty table
  TSP_SOLUTIONS <- bind_rows(TSP_SOLUTIONS, temp_table)
  
  
  #write input reference table row by row
  idx <- as.numeric(idx)
  ref_temp <- data.table(Tour_ID = i, Input_TAZs = idx)
  ref_temp[, Input_leg := seq_len(.N), by = Tour_ID]
  
  InputRefTable <- bind_rows(InputRefTable, ref_temp)
  
}





#TSP: Process Output-----
#join the TAZs onto the TSP solutions using the reference table 
TSP_SOLUTIONS2 <- TSP_SOLUTIONS %>% 
  left_join(InputRefTable, by = c('Tour_ID', 'Input_leg'))


#bind the last stop table onto the solution table and arrange
#becuase input_leg and TSP_leg are NA, they will go last which is where we want them
TSP_SOLUTIONS2 <- bind_rows(TSP_SOLUTIONS2, LastStop) %>% 
  arrange(Tour_ID, TSP_leg) %>% 
  #then we compute a lead to construt trips from this list of TAZs
  mutate(dest = lead(Input_TAZs)) 

#adding a new tour_leg to include the last stop
TSP_SOLUTIONS2[, TSP_leg := seq_len(.N), by = Tour_ID]
  

#the last trip we constructed with the lead function isnt a possible trip so we drop it
#we find what the last one is
TSP_Last  <- TSP_SOLUTIONS2 %>% 
  group_by(Tour_ID) %>% 
  slice(which.max(TSP_leg)) %>% 
  select(Tour_ID, TSP_leg)

#then we anti join to get rid of it, now we have the right number of stops/trips
TSP_SOLUTIONS2 <- TSP_SOLUTIONS2 %>% 
  anti_join(TSP_Last, by = c('Tour_ID', 'TSP_leg')) %>% 
  select(-c(Input_leg)) %>% 
  select(Tour_ID, TSP_leg, Origin = Input_TAZs, Dest = dest)


#then we join on the skims distances onto those trips using TAZs
#summarise total distance
#join the actual distance
TSP_SOLUTIONS2 <- TSP_SOLUTIONS2 %>% 
  left_join(skims_tod, by = c('Origin'='OTAZ', 'Dest' = 'DTAZ')) %>% 
  group_by(Tour_ID) %>%
  summarise(Tour_ID, TSP_dist = sum(skims_dist)) %>% 
  distinct() 

TSP_SOLUTIONS2 <- TSP_SOLUTIONS2 %>% 
  left_join(Actual_Dist, by = c('Tour_ID' = 'Tour ID')) %>% 
  filter(Actual_Dist != 0)





#TSP: Summarise output -----
#compute the ratio of the actual to TSP distance and round to the nearest 10%
library(plyr)
TSP_SOLUTIONS3 <- TSP_SOLUTIONS2 %>% 
  mutate(ratio = round_any(Actual_Dist/TSP_dist, .05))
 
library(tidyverse)
#group by and tally
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
