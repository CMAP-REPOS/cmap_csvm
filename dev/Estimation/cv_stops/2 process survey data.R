library(data.table)
library(readxl)
library(magrittr)
library(foreign)
library(data.table)
library(pscl)

#install.packages("geosphere")

base_loc = "dev/Data_Processed/SEMCOG_Data/"

cvs_location  = file.path(base_loc, "SEMCOG_CV_20181128_Submitted_ForDistribution.xlsx")
skim_location = file.path(base_loc, "skims_avg.RDS") # not used
taz_centroids_location = file.path(base_loc, "TAZ_Centroids.csv")
zip_centroids_locations = file.path(base_loc, "CVS_Unique_Stop_ZIP_Centroids.csv")
taz_se_loc = file.path(base_loc,"TAZSocioEconomics.csv")
taz_system_location = file.path(base_loc, "TAZ_System.csv")
new_empcats_location = file.path(base_loc, "NAICS3_SEMCOGEmpCats_CMAP.csv")

# load data
cvs_establishment = read_xlsx(cvs_location, sheet = "ESTABLISHMENT")
cvs_trip          = read_xlsx(cvs_location, sheet = "TRIP")

taz_se            = fread(taz_se_loc)
taz_system = fread(taz_system_location)
# TAZ 3618 does not have any employment records so filling it with 0.
taz_se[is.na(e01_nrm)]
taz_se[,c(grep("e\\d{2}", names(taz_se), value = TRUE)):=lapply(.SD,
                                                                function(x) ifelse(is.na(x), 0, x)),
       .SDcols=c(grep("e\\d{2}", names(taz_se), value = TRUE))]

emp_cats = fread(new_empcats_location)


# exclude any long distance trips that will be modeled elsewhere
buffer_zips = unique(taz_system[TAZ_TYPE == "BUFFER", ZCTA5CE10])

# Assume Jeff has figured out the issues with the skims
skims_avg = readRDS(skim_location)
setnames(skims_avg, names(skims_avg), gsub("\\.avg", "", names(skims_avg)))

get_distance_meters = function(location_1, location_2){
  
  distance_meters = 
    geosphere::distHaversine(
      matrix(location_1, ncol = 2), 
      matrix(location_2, ncol = 2))
  
  return(distance_meters) 
}

display_prop = function(x, accuracy=0.01){
  scales::percent(prop.table(x), accuracy = accuracy)
}

# turn into data.tables
setDT(cvs_establishment)
setDT(cvs_trip)


# data needed for modeling
cvs_establishment =
  cvs_establishment[SITE_TAZID != 9999 &
                      !is.na(ESTABLISHMENT_WGHT_FCTR),
                    .(SITEID,
                      SITE_TAZID,
                      INDUSTRY, NAICS2, NAICS3,
                      TOTAL_EMPLOYEES,
                      FULL_TIME_EMPLOYEES,
                      AVG_EMPLOYEES_ON_WEEKDAY,
                      EMPLOYEES_WORK_HOME_ONCE_PER_WEEK,
                      TOTAL_VEH_OWNED_OR_LEASED,
                      OWNED_SINGLE_UNIT,
                      OWNED_COMBO_TRACTOR_TRAILER,
                      OWNED_PASSENGER_CAR_OR_SUV,
                      OWNED_PICKUP_TRUCK,
                      OWNED_VAN,
                      OWNED_OTHER_VEH,
                      TOTAL_VEH_NOT_OWNED,
                      VEH_NOT_OWNED_SU,
                      VEH_NOT_OWNED_CU,
                      VEH_NOT_OWNED_PCSUV,
                      VEH_NOT_OWNED_PICKUP,
                      VEH_NOT_OWNED_VAN,
                      VEH_NOT_OWNED_OTHER
                    )]


cvs_trip =
  cvs_trip[!STOP_ACTIVITY_OTHER %in% c("HOME", "HOME FOR EVENING"),
           .(SITEID,
             VEHNUM,
             STOP_SEQ,
             STOP_ACTIVITY,
             STOP_ACTIVITY_OTHER,
             STOP_PLACE_TYPE,
             FINAL_FACTOR,
             TRAVEL_DISTANCE,
             TRAVEL_MINUTES,
             VEH_CLASS,
             TRIP_STOP_TAZID,
             STOP_ZIP)]

# Trips to exclude
# Not sure if these trips should be excluded as we are modeling total number of stops in a day
# by an establishment in a TAZ
trips_to_exclude = cvs_trip[TRIP_STOP_TAZID == 9999 & !STOP_ZIP %in% buffer_zips, .N, .(SITEID, VEHNUM)]
cvs_trip = cvs_trip[!trips_to_exclude, on=.(SITEID, VEHNUM)]

# Get the stop info
cvs_trip[,summary(TRAVEL_DISTANCE)]
cvs_trip[,summary(TRAVEL_MINUTES)] 

# Assign Stop Purpose
stop_data = copy(cvs_trip)
# Stops by stop purpose
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
stop_data[, STOP_PURPOSE:="Return/Deadhead/Other"]
stop_data[STOP_ACTIVITY %in% c(5, 6, 11), STOP_PURPOSE:="Goods"]
stop_data[STOP_ACTIVITY %in% c(2, 3),     STOP_PURPOSE:="Intermediate"]
stop_data[STOP_ACTIVITY %in% c(8, 9, 10), STOP_PURPOSE:="Service"]
stop_data[,.N,.(STOP_PURPOSE)][order(STOP_PURPOSE), .(STOP_PURPOSE, N, Prop=display_prop(N))]

# Investigate stop_activity_other (task for later)
stop_data[,.N,.(STOP_ACTIVITY_OTHER)]

# Vehicle Class
# 1=Passenger Car
# 2=Pick-up Truck (4 wheels)
# 3=Van (Cargo or Minivan) (4 wheels)
# 4=Sport Utility Vehicle (SUV) (4 wheels)
# 5=Single Unit 2-axle (6 wheels)
# 6=Single Unit 3-axle (10 wheels)
# 7=Single Unit 4-axle (14 wheels)
# 8=Semi (all Tractor-Trailer combinations)
# 888=Other

stop_data[, .N, VEH_CLASS][order(VEH_CLASS)]

stop_data[,                   NVEH1 := 0]
stop_data[VEH_CLASS %in% 1:4, NVEH1 := 1]
stop_data[,                   NVEH2 := 0]
stop_data[VEH_CLASS %in% 5:7, NVEH2 := 1]
stop_data[,                   NVEH3 := 0]
stop_data[VEH_CLASS %in% 8,   NVEH3 := 1]

# Tags stops as residential or non-residental locations
stop_data[, STOP_RES := ifelse(STOP_PLACE_TYPE == 7,1,0)]
stop_data[, FINAL_FACTOR_RES := STOP_RES * FINAL_FACTOR]
stop_data[, FINAL_FACTOR_NON_RES := FINAL_FACTOR - FINAL_FACTOR_RES]

# Summarize the table
stop_data = stop_data[!is.na(FINAL_FACTOR),.(STOPS=.N,  # remove unweighted stops
                                             STOPS_RES = sum(STOP_RES),
                                             STOPS_NON_RES = .N - sum(STOP_RES),
                        #TOTAL_TRAVEL_DISTANCE=sum(TRAVEL_DISTANCE),
                        #AVG_TRAVEL_DISTANCE=mean(TRAVEL_DISTANCE),
                        #TOTAL_TRAVLE_TIME=sum(TRAVEL_TIME),
                        #AVG_TRAVEL_TIME=mean(TRAVEL_TIME),
                        NVEH1          = sum(NVEH1),
                        NVEH2          = sum(NVEH2),
                        NVEH3          = sum(NVEH3),
                        WEIGHTED_NVEH1 = sum(NVEH1 * FINAL_FACTOR),
                        WEIGHTED_NVEH2 = sum(NVEH2 * FINAL_FACTOR),
                        WEIGHTED_NVEH3 = sum(NVEH3 * FINAL_FACTOR),
                        WEIGHTED_STOPS = sum(FINAL_FACTOR),
                        WEIGHTED_STOPS_RES = sum(FINAL_FACTOR_RES),
                        WEIGHTED_STOPS_NON_RES = sum(FINAL_FACTOR_NON_RES)),
                     by=.(SITEID, TRIP_STOP_TAZID, STOP_ZIP, STOP_PURPOSE)]

stop_data[,.N,.(SITEID,TRIP_STOP_TAZID, STOP_ZIP)][N>4] # Total 4 purposes
stop_data[,.N,.(STOP_PURPOSE)][order(STOP_PURPOSE), .(STOP_PURPOSE, N, Prop=display_prop(N))]

# Throw away non model stop TAZ i.e. TAZ=9999
stop_data = stop_data[TRIP_STOP_TAZID!=9999]

# appending distance from establishment from centroids
taz_centroids = fread(taz_centroids_location)
taz_centroids[, .N, TAZ][N > 1]

# Expand all establishments and TAZ pair
good_stop_counts         = merge(stop_data[STOP_PURPOSE=="Goods", .(ID=1),by = .(SITEID)],
                             taz_centroids[,.(ID=1, TAZ)], by="ID", allow.cartesian = TRUE)
service_stop_counts      = merge(stop_data[STOP_PURPOSE=="Service",.(ID=1),by = .(SITEID)],
                             taz_centroids[,.(ID=1, TAZ)], by="ID", allow.cartesian = TRUE)
intermediate_stop_counts = merge(stop_data[STOP_PURPOSE=="Intermediate",.(ID=1),by = .(SITEID)],
                        taz_centroids[,.(ID=1, TAZ)], by="ID", allow.cartesian = TRUE)

# Merge the establishment info
good_stop_counts         = merge(good_stop_counts, cvs_establishment, by="SITEID")#, all.x = TRUE)
service_stop_counts      = merge(service_stop_counts, cvs_establishment, by="SITEID")#, all.x = TRUE)
intermediate_stop_counts = merge(intermediate_stop_counts, cvs_establishment, by="SITEID")#, all.x = TRUE)

# Merge the stop info
good_stop_counts         = merge(good_stop_counts, stop_data[STOP_PURPOSE=="Goods"], 
                                by.x=c("SITEID", "TAZ"),
                                by.y=c("SITEID", "TRIP_STOP_TAZID"), all.x = TRUE)
service_stop_counts      = merge(service_stop_counts, stop_data[STOP_PURPOSE=="Service"], 
                                by.x=c("SITEID", "TAZ"),
                                by.y=c("SITEID", "TRIP_STOP_TAZID"), all.x = TRUE)
intermediate_stop_counts = merge(intermediate_stop_counts, stop_data[STOP_PURPOSE=="Intermediate"], 
                                by.x=c("SITEID", "TAZ"),
                                by.y=c("SITEID", "TRIP_STOP_TAZID"), all.x = TRUE)

# Fill in zero counts
good_stop_counts[is.na(STOPS), c("STOPS", "STOPS_RES", "STOPS_NON_RES", 
                                 "NVEH1", "NVEH2", "NVEH3", "WEIGHTED_NVEH1",
                                "WEIGHTED_NVEH2", "WEIGHTED_NVEH3", "WEIGHTED_STOPS",
                                "WEIGHTED_STOPS_RES", "WEIGHTED_STOPS_NON_RES"):=0]        
service_stop_counts[is.na(STOPS), c("STOPS", "STOPS_RES", "STOPS_NON_RES", 
                                    "NVEH1", "NVEH2", "NVEH3", "WEIGHTED_NVEH1",
                                    "WEIGHTED_NVEH2", "WEIGHTED_NVEH3", "WEIGHTED_STOPS",
                                    "WEIGHTED_STOPS_RES", "WEIGHTED_STOPS_NON_RES"):=0]      
intermediate_stop_counts[is.na(STOPS), c("STOPS", "STOPS_RES", "STOPS_NON_RES", 
                                         "NVEH1", "NVEH2", "NVEH3", "WEIGHTED_NVEH1",
                                         "WEIGHTED_NVEH2", "WEIGHTED_NVEH3", "WEIGHTED_STOPS",
                                         "WEIGHTED_STOPS_RES", "WEIGHTED_STOPS_NON_RES"):=0]

# Merge SE data
good_stop_counts         = merge(good_stop_counts, taz_se, by=c("TAZ"), all.x = TRUE)
service_stop_counts      = merge(service_stop_counts, taz_se, by=c("TAZ"), all.x = TRUE)
intermediate_stop_counts = merge(intermediate_stop_counts, taz_se, by=c("TAZ"), all.x = TRUE)


# Merge Skim data
good_stop_counts         = merge(good_stop_counts, skims_avg, 
                                by.x=c("SITE_TAZID", "TAZ"),
                                by.y=c("OTAZ", "DTAZ"), all.x = TRUE)
service_stop_counts      = merge(service_stop_counts, skims_avg, 
                                by.x=c("SITE_TAZID", "TAZ"),
                                by.y=c("OTAZ", "DTAZ"), all.x = TRUE)
intermediate_stop_counts = merge(intermediate_stop_counts, skims_avg, 
                                by.x=c("SITE_TAZID", "TAZ"),
                                by.y=c("OTAZ", "DTAZ"), all.x = TRUE)

# Calculate straigt line distance
# Goods
good_stop_counts = 
  taz_centroids[, .(SITE_TAZID = TAZ, site_lon = X, site_lat = Y)][good_stop_counts, on = .(SITE_TAZID)]

good_stop_counts = taz_centroids[, .(TAZ, stop_lon = X, stop_lat = Y)][
  good_stop_counts, on = .(TAZ)]

good_stop_counts[, straight_dist := get_distance_meters(c(site_lon, site_lat), c(stop_lon, stop_lat))]

summary(good_stop_counts[, straight_dist])

good_stop_counts[, straight_dist := straight_dist / 1609.34] # converting to miles

# Service
service_stop_counts = 
  taz_centroids[, .(SITE_TAZID = TAZ, site_lon = X, site_lat = Y)][service_stop_counts, on = .(SITE_TAZID)]

service_stop_counts = taz_centroids[, .(TAZ, stop_lon = X, stop_lat = Y)][
  service_stop_counts, on = .(TAZ)]

service_stop_counts[, straight_dist := get_distance_meters(c(site_lon, site_lat), c(stop_lon, stop_lat))]

summary(service_stop_counts[, straight_dist])

service_stop_counts[, straight_dist := straight_dist / 1609.34] # converting to miles

# Intermediate
intermediate_stop_counts = 
  taz_centroids[, .(SITE_TAZID = TAZ, site_lon = X, site_lat = Y)][intermediate_stop_counts, on = .(SITE_TAZID)]

intermediate_stop_counts = taz_centroids[, .(TAZ, stop_lon = X, stop_lat = Y)][
  intermediate_stop_counts, on = .(TAZ)]

intermediate_stop_counts[, straight_dist := get_distance_meters(c(site_lon, site_lat), c(stop_lon, stop_lat))]

summary(intermediate_stop_counts[, straight_dist])

intermediate_stop_counts[, straight_dist := straight_dist / 1609.34] # converting to miles

# Gather zone employment by employment category


#FROM HERE ON OUT CHANGING EmpCatName to CMAPGroup - RICKY 9/28
#can also rename CMAPGroup for model consistency


# var_names_by_cat = emp_cats[,.N,.(EmpCatName, EmpCatGroupedName)][order(EmpCatName)] #the semcog version
var_names_by_cat = emp_cats[,.N,.(EmpCatName, CMAPGroup)][order(EmpCatName)] #new cmap version

for(model_empcat in unique(var_names_by_cat[,CMAPGroup])){
  cat(model_empcat, "\n")
  var_names = var_names_by_cat[CMAPGroup==model_empcat, EmpCatName]
  emp_name = paste0("NEmp_", gsub(" ", "_", model_empcat))
  good_stop_counts[,c(emp_name):=rowSums(.SD), .SDcols=var_names]
  service_stop_counts[,c(emp_name):=rowSums(.SD), .SDcols=var_names]
  intermediate_stop_counts[,c(emp_name):=rowSums(.SD), .SDcols=var_names]
}
good_stop_counts[,NEmp_Total:=rowSums(.SD),
                .SDcols=paste0("NEmp_", unique(var_names_by_cat[,gsub(" ", "_", CMAPGroup)]))]
service_stop_counts[,NEmp_Total:=rowSums(.SD),
                 .SDcols=paste0("NEmp_", unique(var_names_by_cat[,gsub(" ", "_", CMAPGroup)]))]
intermediate_stop_counts[,NEmp_Total:=rowSums(.SD),
                 .SDcols=paste0("NEmp_", unique(var_names_by_cat[,gsub(" ", "_", CMAPGroup)]))]

var_remove = c("stop_lon", "stop_lat", "site_lon", "site_lat", "STOP_PURPOSE", "ID")

good_stop_counts[, c(var_remove):=NULL]
service_stop_counts[, c(var_remove):=NULL]
intermediate_stop_counts[, c(var_remove):=NULL]


# Remove employment variables
good_stop_counts[, c(var_names_by_cat$EmpCatName):=NULL]
service_stop_counts[, c(var_names_by_cat$EmpCatName):=NULL]
intermediate_stop_counts[, c(var_names_by_cat$EmpCatName):=NULL]


# Assign Industry category
setkey(emp_cats, NAICSn2n3)
good_stop_counts[,IndustryCat:=emp_cats[.(NAICS2),CMAPGroup]]
good_stop_counts[is.na(IndustryCat),IndustryCat:=emp_cats[.(NAICS3),CMAPGroup]]
service_stop_counts[,IndustryCat:=emp_cats[.(NAICS2),CMAPGroup]]
service_stop_counts[is.na(IndustryCat),IndustryCat:=emp_cats[.(NAICS3),CMAPGroup]]
intermediate_stop_counts[,IndustryCat:=emp_cats[.(NAICS2),CMAPGroup]]
intermediate_stop_counts[is.na(IndustryCat),IndustryCat:=emp_cats[.(NAICS3),CMAPGroup]]

drop_var = c("FULL_TIME_EMPLOYEES", "AVG_EMPLOYEES_ON_WEEKDAY", "EMPLOYEES_WORK_HOME_ONCE_PER_WEEK", 
             "OWNED_SINGLE_UNIT", "OWNED_COMBO_TRACTOR_TRAILER", "OWNED_PASSENGER_CAR_OR_SUV", 
             "OWNED_PICKUP_TRUCK", "OWNED_VAN", "OWNED_OTHER_VEH", "TOTAL_VEH_NOT_OWNED", 
             "VEH_NOT_OWNED_SU", "VEH_NOT_OWNED_CU", "VEH_NOT_OWNED_PCSUV", "VEH_NOT_OWNED_PICKUP", 
             "VEH_NOT_OWNED_VAN", "VEH_NOT_OWNED_OTHER")

good_stop_counts[, c(drop_var):=NULL]
service_stop_counts[, c(drop_var):=NULL]
intermediate_stop_counts[, c(drop_var):=NULL]

model_loc  = 'dev/Estimation/cv_stops/'
# Count data sets
save(good_stop_counts, file = file.path(model_loc, "Stop_Counts_Goods.RData"))
save(service_stop_counts, file = file.path(model_loc, "Stop_Counts_Service.RData"))
save(intermediate_stop_counts, file = file.path(model_loc, "Stop_Counts_Intermediate.RData"))


