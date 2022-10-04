library(data.table)
library(readxl)
library(magrittr)
library(foreign)
library(data.table)
library(pscl)


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


stop_data = stop_data[!is.na(FINAL_FACTOR),.(STOPS=.N,  # remove unweighted stops
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
                        WEIGHTED_STOPS=sum(FINAL_FACTOR)),
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
good_stop_counts[is.na(STOPS), c("STOPS", "NVEH1", "NVEH2", "NVEH3", "WEIGHTED_NVEH1",
                                "WEIGHTED_NVEH2", "WEIGHTED_NVEH3", "WEIGHTED_STOPS"):=0]        
service_stop_counts[is.na(STOPS), c("STOPS", "NVEH1", "NVEH2", "NVEH3", "WEIGHTED_NVEH1",
                                   "WEIGHTED_NVEH2", "WEIGHTED_NVEH3", "WEIGHTED_STOPS"):=0]      
intermediate_stop_counts[is.na(STOPS), c("STOPS", "NVEH1", "NVEH2", "NVEH3", "WEIGHTED_NVEH1",
                                        "WEIGHTED_NVEH2", "WEIGHTED_NVEH3", "WEIGHTED_STOPS"):=0]


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


# Stop Generation Model =========================================================
# cv_stop_desc_path = file.path("dev", "Estimation", "cv_stops", "desc_summaries")
# cv_output_path = file.path("dev", "Estimation", "cv_stops", "eda_plots")
# load("dev/Estimation/cv_stops/Stop_Counts_Goods.RData")
# load("dev/Estimation/cv_stops/Stop_Counts_Service.RData")
# 
# # Descriptive Summaries
# good_stop_counts[empcats,EmpCat:=i.EmpCatName,on=.(NAICS2=NAICSn2n3)]
# good_stop_counts[empcats,EmpCat:=ifelse(is.na(EmpCat),i.EmpCatName,EmpCat),on=.(NAICS3=NAICSn2n3)]
# service_stop_counts[empcats,EmpCat:=i.EmpCatName,on=.(NAICS2=NAICSn2n3)]
# service_stop_counts[empcats,EmpCat:=ifelse(is.na(EmpCat),i.EmpCatName,EmpCat),on=.(NAICS3=NAICSn2n3)]
# 
# # Sample Size by Model Employment Category
# goods_stop_ss <- good_stop_counts[,.(NEst=length(unique(SITEID)),
#                                      StopsRecords=sum(STOPS>0), 
#                                      NoStopsRecords=sum(STOPS==0),
#                                      TotalRecords=.N,
#                                      MinStops=min(STOPS[STOPS>0]),
#                                      AvgStops=mean(STOPS[STOPS>0]),
#                                      MaxStops=max(STOPS[STOPS>0])),.(EmpCat)][order(-StopsRecords)]
# service_stop_ss <- service_stop_counts[,.(NEst=length(unique(SITEID)),
#                                           StopsRecords=sum(STOPS>0), 
#                                           NoStopsRecords=sum(STOPS==0),
#                                           TotalRecords=.N,
#                                           MinStops=min(STOPS[STOPS>0]),
#                                           AvgStops=mean(STOPS[STOPS>0]),
#                                           MaxStops=max(STOPS[STOPS>0])),.(EmpCat)][order(-StopsRecords)]
# 
# fwrite(goods_stop_ss, file = file.path(cv_stop_desc_path, "good_stop_model_empcat_samplesize.csv"))
# fwrite(service_stop_ss, file = file.path(cv_stop_desc_path, "service_stop_model_empcat_samplesize.csv"))
# 
# # Sample Size by Grouped Employment Category
# goods_stop_ss <- good_stop_counts[,.(NEst=length(unique(SITEID)),
#                                      StopsRecords=sum(STOPS>0), 
#                                      NoStopsRecords=sum(STOPS==0),
#                                      TotalRecords=.N,
#                                      MinStops=min(STOPS[STOPS>0]),
#                                      AvgStops=mean(STOPS[STOPS>0]),
#                                      MaxStops=max(STOPS[STOPS>0])),.(IndustryCat)][order(-StopsRecords)]
# service_stop_ss <- service_stop_counts[,.(NEst=length(unique(SITEID)),
#                                           StopsRecords=sum(STOPS>0), 
#                                           NoStopsRecords=sum(STOPS==0),
#                                           TotalRecords=.N,
#                                           MinStops=min(STOPS[STOPS>0]),
#                                           AvgStops=mean(STOPS[STOPS>0]),
#                                           MaxStops=max(STOPS[STOPS>0])),.(IndustryCat)][order(-StopsRecords)]
# 
# fwrite(goods_stop_ss, file = file.path(cv_stop_desc_path, "good_stop_grouped_empcat_samplesize.csv"))
# fwrite(service_stop_ss, file = file.path(cv_stop_desc_path, "service_stop_grouped_empcat_samplesize.csv"))
# 
# dist_cut = c(seq(0,50,2),Inf)
# ndist_bin = length(dist_cut)
# dist_cut_label = paste0(dist_cut[-ndist_bin], " - ", dist_cut[-1])
# dist_cut_label[(ndist_bin-1)] = paste0("> ", dist_cut[(ndist_bin-1)])
# 
# good_stop_counts[,dist_bin:=cut(dist, dist_cut, labels = dist_cut_label)]
# service_stop_counts[,dist_bin:=cut(dist, dist_cut, labels = dist_cut_label)]
# 
# goods_stop_ss <- good_stop_counts[STOPS > 0,.(NEst=length(unique(SITEID)),
#                                               StopsRecords=.N,
#                                               MinStops=min(STOPS),
#                                               AvgStops=mean(STOPS),
#                                               MaxStops=max(STOPS)),.(EmpCat, dist_bin)][order(EmpCat, dist_bin)]
# service_stop_ss <- service_stop_counts[STOPS > 0,.(NEst=length(unique(SITEID)),
#                                                    StopsRecords=.N,
#                                                    MinStops=min(STOPS),
#                                                    AvgStops=mean(STOPS),
#                                                    MaxStops=max(STOPS)),.(EmpCat, dist_bin)][order(EmpCat, dist_bin)]
# 
# fwrite(goods_stop_ss, file = file.path(cv_stop_desc_path, "good_stop_model_empcat_dist_summaries.csv"))
# fwrite(service_stop_ss, file = file.path(cv_stop_desc_path, "service_stop_model_empcat_dist_summaries.csv"))
# 
# goods_stop_ss <- good_stop_counts[STOPS > 0,.(NEst=length(unique(SITEID)),
#                                               StopsRecords=.N,
#                                               MinStops=min(STOPS),
#                                               AvgStops=mean(STOPS),
#                                               MaxStops=max(STOPS)),.(IndustryCat, dist_bin)][order(IndustryCat, dist_bin)]
# service_stop_ss <- service_stop_counts[STOPS > 0,.(NEst=length(unique(SITEID)),
#                                                    StopsRecords=.N,
#                                                    MinStops=min(STOPS),
#                                                    AvgStops=mean(STOPS),
#                                                    MaxStops=max(STOPS)),.(IndustryCat, dist_bin)][order(IndustryCat, dist_bin)]
# 
# fwrite(goods_stop_ss, file = file.path(cv_stop_desc_path, "good_stop_grouped_empcat_dist_summaries.csv"))
# fwrite(service_stop_ss, file = file.path(cv_stop_desc_path, "service_stop_grouped_empcat_dist_summaries.csv"))
# 
# 
# time_cut = c(seq(0,50,2),Inf)
# ntime_bin = length(time_cut)
# time_cut_label = paste0(time_cut[-ntime_bin], " - ", time_cut[-1])
# time_cut_label[(ntime_bin-1)] = paste0("> ", time_cut[(ntime_bin-1)])
# 
# good_stop_counts[,time_bin:=cut(time, time_cut, labels = time_cut_label)]
# service_stop_counts[,time_bin:=cut(time, time_cut, labels = time_cut_label)]
# 
# goods_stop_ss <- good_stop_counts[STOPS > 0,.(NEst=length(unique(SITEID)),
#                                               StopsRecords=.N,
#                                               MinStops=min(STOPS),
#                                               AvgStops=mean(STOPS),
#                                               MaxStops=max(STOPS)),.(EmpCat, time_bin)][order(EmpCat, time_bin)]
# service_stop_ss <- service_stop_counts[STOPS > 0,.(NEst=length(unique(SITEID)),
#                                                    StopsRecords=.N,
#                                                    MinStops=min(STOPS),
#                                                    AvgStops=mean(STOPS),
#                                                    MaxStops=max(STOPS)),.(EmpCat, time_bin)][order(EmpCat, time_bin)]
# 
# fwrite(goods_stop_ss, file = file.path(cv_stop_desc_path, "good_stop_model_empcat_time_summaries.csv"))
# fwrite(service_stop_ss, file = file.path(cv_stop_desc_path, "service_stop_model_empcat_time_summaries.csv"))
# 
# goods_stop_ss <- good_stop_counts[STOPS > 0,.(NEst=length(unique(SITEID)),
#                                               StopsRecords=.N,
#                                               MinStops=min(STOPS),
#                                               AvgStops=mean(STOPS),
#                                               MaxStops=max(STOPS)),.(IndustryCat, time_bin)][order(IndustryCat, time_bin)]
# service_stop_ss <- service_stop_counts[STOPS > 0,.(NEst=length(unique(SITEID)),
#                                                    StopsRecords=.N,
#                                                    MinStops=min(STOPS),
#                                                    AvgStops=mean(STOPS),
#                                                    MaxStops=max(STOPS)),.(IndustryCat, time_bin)][order(IndustryCat, time_bin)]
# 
# fwrite(goods_stop_ss, file = file.path(cv_stop_desc_path, "good_stop_grouped_empcat_time_summaries.csv"))
# fwrite(service_stop_ss, file = file.path(cv_stop_desc_path, "service_stop_grouped_empcat_time_summaries.csv"))
# 
# 
# # Plots
# good_stop_counts[, IndName:=IndustryCats[INDUSTRY]]
# service_stop_counts[, IndName:=IndustryCats[INDUSTRY]]
# 
# # Number of sample points by industry
# # Goods
# goods_stops_ind_ss <- ggplot(data = good_stop_counts[STOPS > 0], aes(x = reorder(IndName, STOPS, length))) +
#   geom_bar() + coord_flip() +
#   labs(title = "Sample Size of Goods Stops", subtitle = "Number of samples, Unweighted", caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("Industry Category") + scale_y_continuous(name = "Sample Size", labels = scales::comma) 
# 
# 
# ggsave(goods_stops_ind_ss, filename = file.path(cv_output_path, "goods_stops_ind_ss.png"), width = 6.5, height = 4) 
# 
# # Service
# service_stops_ind_ss <- ggplot(data = service_stop_counts[STOPS > 0], aes(x = reorder(IndName, STOPS, length))) +
#   geom_bar() + coord_flip() +
#   labs(title = "Sample Size of Goods Stops", subtitle = "Number of samples, Unweighted", caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("Industry Category") + scale_y_continuous(name = "Sample Size", labels = scales::comma) 
# ggsave(service_stops_ind_ss, filename = file.path(cv_output_path, "service_stops_ind_ss.png"), width = 6.5, height = 4) 
# 
# 
# goods_industry = c(8,9,5,7,4,3)
# service_industry = c(9,3,8,5,7,4)
# 
# # Distribution by Establishment employees
# # Goods
# good_stop_counts[STOPS > 0,SITEIDG:=as.integer(reorder(factor(SITEID),TOTAL_EMPLOYEES,unique)),.(IndName)]
# goods_stops_ind_emp = ggplot(data = good_stop_counts[STOPS > 0 & INDUSTRY %in% goods_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(SITEID=SITEIDG,TOTAL_EMPLOYEES,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = SITEID)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x, lwd=.5)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log(TOTAL_EMPLOYEES), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Goods Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Total Employees",
#                                          labels = exp,
#                                          breaks = c(0,log(2^c(1:10))))) +
#   labs(title = "Goods Stops and Business Employment Distribution", 
#        subtitle = "Number of Goods Stops, Unweighted, and Business Employment", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("Business Index")
# ggsave(goods_stops_ind_emp, filename = file.path(cv_output_path, "goods_stops_ind_emp.png"), width = 13, height = 8) 
# 
# # Service
# service_stop_counts[STOPS > 0,SITEIDS:=as.integer(reorder(factor(SITEID),TOTAL_EMPLOYEES,unique)),.(IndName)]
# service_stops_ind_emp = ggplot(data = service_stop_counts[STOPS > 0 & INDUSTRY %in% service_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(SITEID=SITEIDS,TOTAL_EMPLOYEES,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = SITEID)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log(TOTAL_EMPLOYEES), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Service Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Total Employees",
#                                          labels = exp,
#                                          breaks = c(0,log(2^c(1:10))))) +
#   labs(title = "Service Stops and Business Employment Distribution", 
#        subtitle = "Number of Service Stops, Unweighted, and Business Employment", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("Business Index")
# ggsave(service_stops_ind_emp, filename = file.path(cv_output_path, "service_stops_ind_emp.png"), width = 13, height = 8) 
# 
# # Distribution by Households at Stop Zone
# # Goods
# good_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),HH,unique)),.(IndName)]
# goods_stops_taz_hh = ggplot(data = good_stop_counts[STOPS > 0 & INDUSTRY %in% goods_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(STOPTAZ,HH,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log1p(HH), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Goods Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Households",
#                                          labels = function(x) round(expm1(x)),
#                                          breaks = c(0,log(2^c(1:14))))) +
#   labs(title = "Goods Stops and Household Distribution", 
#        subtitle = "Number of Goods Stops, Unweighted, and Households at Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(goods_stops_taz_hh, filename = file.path(cv_output_path, "goods_stops_taz_hh.png"), width = 13, height = 8) 
# 
# # Service
# service_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),HH,unique)),.(IndName)]
# service_stops_taz_hh = ggplot(data = service_stop_counts[STOPS > 0 & INDUSTRY %in% service_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(STOPTAZ,HH,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log1p(HH), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Service Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Households",
#                                          labels = function(x) round(expm1(x)),
#                                          breaks = c(0,log(2^c(1:14))))) +
#   labs(title = "Service Stops and Household Distribution", 
#        subtitle = "Number of Service Stops, Unweighted, and Households at Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(service_stops_taz_hh, 
#        filename = file.path(cv_output_path, "service_stops_taz_hh.png"), 
#        width = 13, height = 8) 
# 
# # Distribution by Industrial Employment at Stop Zone
# # Goods
# good_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),NEmp_Industrial,unique)),.(IndName)]
# goods_stops_taz_ind_emp = ggplot(data = good_stop_counts[STOPS > 0 & INDUSTRY %in% goods_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(STOPTAZ,NEmp_Industrial,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log1p(NEmp_Industrial), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Goods Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Industrial Employment",
#                                          labels = function(x) round(expm1(x)),
#                                          breaks = c(0,log(2^c(1:14))))) +
#   labs(title = "Goods Stops and Industrial Employment Distribution", 
#        subtitle = "Number of Goods Stops, Unweighted, and Industrial Employment at Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(goods_stops_taz_ind_emp, 
#        filename = file.path(cv_output_path, "goods_stops_taz_ind_emp.png"), 
#        width = 13, height = 8) 
# 
# # Service
# service_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),NEmp_Industrial,unique)),.(IndName)]
# service_stops_taz_ind_emp = ggplot(data = service_stop_counts[STOPS > 0 & INDUSTRY %in% service_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(STOPTAZ,NEmp_Industrial,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log1p(NEmp_Industrial), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Service Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Industrial Employment",
#                                          labels = function(x) round(expm1(x)),
#                                          breaks = c(0,log(2^c(1:14))))) +
#   labs(title = "Service Stops and Industrial Employment Distribution", 
#        subtitle = "Number of Service Stops, Unweighted, and Industrial Employment at Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(service_stops_taz_ind_emp, 
#        filename = file.path(cv_output_path, "service_stops_taz_ind_emp.png"), 
#        width = 13, height = 8) 
# 
# # Distribution by Retail Employment at Stop Zone
# # Goods
# good_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),NEmp_Retail,unique)),.(IndName)]
# goods_stops_taz_ret_emp = ggplot(data = good_stop_counts[STOPS > 0 & INDUSTRY %in% goods_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(STOPTAZ,NEmp_Retail,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log1p(NEmp_Retail), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Goods Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Retail Employment",
#                                          labels = function(x) round(expm1(x)),
#                                          breaks = c(0,log(2^c(1:14))))) +
#   labs(title = "Goods Stops and Retail Employment Distribution", 
#        subtitle = "Number of Goods Stops, Unweighted, and Retail Employment at Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(goods_stops_taz_ret_emp, 
#        filename = file.path(cv_output_path, "goods_stops_taz_ret_emp.png"), 
#        width = 13, height = 8) 
# 
# # Service
# service_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),NEmp_Retail,unique)),.(IndName)]
# service_stops_taz_ret_emp = ggplot(data = service_stop_counts[STOPS > 0 & INDUSTRY %in% service_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(STOPTAZ,NEmp_Retail,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log1p(NEmp_Retail), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Service Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Retail Employment",
#                                          labels = function(x) round(expm1(x)),
#                                          breaks = c(0,log(2^c(1:14))))) +
#   labs(title = "Service Stops and Retail Employment Distribution", 
#        subtitle = "Number of Service Stops, Unweighted, and Retail Employment at Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(service_stops_taz_ret_emp, 
#        filename = file.path(cv_output_path, "service_stops_taz_ret_emp.png"), 
#        width = 13, height = 8) 
# 
# # Distribution by Office Employment at Stop Zone
# # Goods
# good_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),NEmp_Office,unique)),.(IndName)]
# goods_stops_taz_off_emp = ggplot(data = good_stop_counts[STOPS > 0 & INDUSTRY %in% goods_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(STOPTAZ,NEmp_Office,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log1p(NEmp_Office), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Goods Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Office Employment",
#                                          labels = function(x) round(expm1(x)),
#                                          breaks = c(0,log(2^c(1:14))))) +
#   labs(title = "Goods Stops and Office Employment Distribution", 
#        subtitle = "Number of Goods Stops, Unweighted, and Office Employment at Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(goods_stops_taz_off_emp, 
#        filename = file.path(cv_output_path, "goods_stops_taz_off_emp.png"), 
#        width = 13, height = 8) 
# 
# # Service
# service_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),NEmp_Office,unique)),.(IndName)]
# service_stops_taz_off_emp = ggplot(data = service_stop_counts[STOPS > 0 & INDUSTRY %in% service_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(STOPTAZ,NEmp_Office,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log1p(NEmp_Office), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Service Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Office Employment",
#                                          labels = function(x) round(expm1(x)),
#                                          breaks = c(0,log(2^c(1:14))))) +
#   labs(title = "Service Stops and Office Employment Distribution", 
#        subtitle = "Number of Service Stops, Unweighted, and Office Employment at Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(service_stops_taz_off_emp, 
#        filename = file.path(cv_output_path, "service_stops_taz_off_emp.png"), 
#        width = 13, height = 8) 
# 
# # Distribution by Education Employment at Stop Zone
# # Goods
# good_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),NEmp_Education,unique)),.(IndName)]
# goods_stops_taz_edu_emp = ggplot(data = good_stop_counts[STOPS > 0 & INDUSTRY %in% goods_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(STOPTAZ,NEmp_Education,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log1p(NEmp_Education), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Goods Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Education Employment",
#                                          labels = function(x) round(expm1(x)),
#                                          breaks = c(0,log(2^c(1:14))))) +
#   labs(title = "Goods Stops and Education Employment Distribution", 
#        subtitle = "Number of Goods Stops, Unweighted, and Education Employment at Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(goods_stops_taz_edu_emp, 
#        filename = file.path(cv_output_path, "goods_stops_taz_edu_emp.png"), 
#        width = 13, height = 8) 
# 
# # Service
# service_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),NEmp_Education,unique)),.(IndName)]
# service_stops_taz_edu_emp = ggplot(data = service_stop_counts[STOPS > 0 & INDUSTRY %in% service_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(STOPTAZ,NEmp_Education,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log1p(NEmp_Education), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Service Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Education Employment",
#                                          labels = function(x) round(expm1(x)),
#                                          breaks = c(0,log(2^c(1:14))))) +
#   labs(title = "Service Stops and Education Employment Distribution", 
#        subtitle = "Number of Service Stops, Unweighted, and Education Employment at Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(service_stops_taz_edu_emp, 
#        filename = file.path(cv_output_path, "service_stops_taz_edu_emp.png"), 
#        width = 13, height = 8) 
# 
# # Distribution by Medical Services Employment at Stop Zone
# # Goods
# good_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),NEmp_Medical_Services,unique)),.(IndName)]
# goods_stops_taz_med_emp = ggplot(data = good_stop_counts[STOPS > 0 & INDUSTRY %in% goods_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(STOPTAZ,NEmp_Medical_Services,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log1p(NEmp_Medical_Services), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Goods Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Medical Services Employment",
#                                          labels = function(x) round(expm1(x)),
#                                          breaks = c(0,log(2^c(1:14))))) +
#   labs(title = "Goods Stops and Medical Services Employment Distribution", 
#        subtitle = "Number of Goods Stops, Unweighted, and Medical Services Employment at Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(goods_stops_taz_med_emp, 
#        filename = file.path(cv_output_path, "goods_stops_taz_med_emp.png"), 
#        width = 13, height = 8) 
# 
# # Service
# service_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),NEmp_Medical_Services,unique)),.(IndName)]
# service_stops_taz_med_emp = ggplot(data = service_stop_counts[STOPS > 0 & INDUSTRY %in% service_industry][
#   ,.(Stops=mean(STOPS), MinStops=min(STOPS),
#      MaxStops=max(STOPS)),.(STOPTAZ,NEmp_Medical_Services,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_line(aes(y=log1p(NEmp_Medical_Services), group=1)) +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Service Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Medical Services Employment",
#                                          labels = function(x) round(expm1(x)),
#                                          breaks = c(0,log(2^c(1:14))))) +
#   labs(title = "Service Stops and Medical Services Employment Distribution", 
#        subtitle = "Number of Service Stops, Unweighted, and Medical Services Employment at Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(service_stops_taz_med_emp, 
#        filename = file.path(cv_output_path, "service_stops_taz_med_emp.png"), 
#        width = 13, height = 8) 
# 
# # Distribution by Averate Distance to Stop Zone
# # Goods
# good_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),dist,mean)),.(IndName)]
# goods_stops_taz_dist = ggplot(data = good_stop_counts[STOPS > 0 & INDUSTRY %in% goods_industry][
#   ,.(Stops=weighted.mean(STOPS, w = 1/dist), MinStops=min(STOPS),
#      MaxStops=max(STOPS), dist=mean(dist), MinDist=min(dist), MaxDist=max(dist)),.(STOPTAZ,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_pointrange(aes(y=log(dist), ymin=log(MinDist), ymax=log(MaxDist), group=1),
#                   size=.2, shape=21, fill="black", color="grey") +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Goods Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Distance (miles)",
#                                          labels = function(x) ifelse(exp(x)<1, round(exp(x),2), round(exp(x))),
#                                          breaks = c(log(2^c(-4:14))))) +
#   labs(title = "Goods Stops and Distance Distribution", 
#        subtitle = "Number of Goods Stops, Unweighted, and Distance to Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(goods_stops_taz_dist, 
#        filename = file.path(cv_output_path, "goods_stops_taz_dist.png"), 
#        width = 13, height = 8) 
# 
# # Service
# service_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),dist,mean)),.(IndName)]
# service_stops_taz_dist = ggplot(data = service_stop_counts[STOPS > 0 & INDUSTRY %in% service_industry][
#   ,.(Stops=weighted.mean(STOPS, w = 1/dist), MinStops=min(STOPS),
#      MaxStops=max(STOPS), dist=mean(dist), MinDist=min(dist), MaxDist=max(dist)),.(STOPTAZ,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_pointrange(aes(y=log(dist), ymin=log(MinDist), ymax=log(MaxDist), group=1),
#                   size=.2, shape=21, fill="black", color="grey") +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Service Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Distance (miles)",
#                                          labels = function(x) ifelse(exp(x)<1, round(exp(x),2), round(exp(x))),
#                                          breaks = c(log(2^c(-4:14))))) +
#   labs(title = "Service Stops and Distance Distribution", 
#        subtitle = "Number of Service Stops, Unweighted, and Distance to Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(service_stops_taz_dist, 
#        filename = file.path(cv_output_path, "service_stops_taz_dist.png"), 
#        width = 13, height = 8) 
# 
# 
# # Distribution by Averate Time to Stop Zone
# # Goods
# good_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),time,mean)),.(IndName)]
# goods_stops_taz_time = ggplot(data = good_stop_counts[STOPS > 0 & INDUSTRY %in% goods_industry][
#   ,.(Stops=weighted.mean(STOPS, w = 1/time), MinStops=min(STOPS),
#      MaxStops=max(STOPS), time=mean(time), MinDist=min(time), MaxDist=max(time)),.(STOPTAZ,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_pointrange(aes(y=log(time), ymin=log(MinDist), ymax=log(MaxDist), group=1),
#                   size=.2, shape=21, fill="black", color="grey") +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Goods Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Distance (miles)",
#                                          labels = function(x) ifelse(exp(x)<1, round(exp(x),2), round(exp(x))),
#                                          breaks = c(log(2^c(-4:14))))) +
#   labs(title = "Goods Stops and Distance Distribution", 
#        subtitle = "Number of Goods Stops, Unweighted, and Distance to Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(goods_stops_taz_time, 
#        filename = file.path(cv_output_path, "goods_stops_taz_time.png"), 
#        width = 13, height = 8) 
# 
# # Service
# service_stop_counts[STOPS > 0,STOPTAZ:=as.integer(reorder(factor(TAZ),time,mean)),.(IndName)]
# service_stops_taz_time = ggplot(data = service_stop_counts[STOPS > 0 & INDUSTRY %in% service_industry][
#   ,.(Stops=weighted.mean(STOPS, w = 1/time), MinStops=min(STOPS),
#      MaxStops=max(STOPS), time=mean(time), MinDist=min(time), MaxDist=max(time)),.(STOPTAZ,IndName = reorder(IndName, -STOPS, mean))], 
#   aes(x = STOPTAZ)) +
#   geom_smooth(aes(y=log(Stops), group=1), method="lm", se=FALSE, formula = y~x)+
#   geom_pointrange(aes(y=log(Stops), ymin=log(MinStops), ymax=log(MaxStops), group=1),
#                   size=.3, shape=21, fill="red") + 
#   geom_pointrange(aes(y=log(time), ymin=log(MinDist), ymax=log(MaxDist), group=1),
#                   size=.2, shape=21, fill="black", color="grey") +
#   facet_wrap(~IndName, scales="free", ncol = 3) +
#   scale_y_continuous(name = "Service Stops", labels = exp, breaks = c(0,log(2^c(1:4))),
#                      sec.axis = sec_axis(~., name = "Distance (miles)",
#                                          labels = function(x) ifelse(exp(x)<1, round(exp(x),2), round(exp(x))),
#                                          breaks = c(log(2^c(-4:14))))) +
#   labs(title = "Service Stops and Distance Distribution", 
#        subtitle = "Number of Service Stops, Unweighted, and Distance to Stop Zones", 
#        caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
#   xlab("TAZ Index")
# ggsave(service_stops_taz_time, 
#        filename = file.path(cv_output_path, "service_stops_taz_time.png"), 
#        width = 13, height = 8) 

