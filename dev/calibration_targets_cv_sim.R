# Script to import calibration data and create targets for CVTM
# 
# Purpose:
# Import calibration data and create targets for CVTM
#
# Outputs:
# Calibrations targets in dev/Calibration
# calibration_targets_cv_sim.RDS
#
# In dev\Calibration\_Documentation
# Charts documenting cvtm calibration targets
#
# Use init_dev.R to run here instead of sourcing from _Master_Dev.R
source("./dev/init_dev.R")

### READ IN CVTM MODEL INPUTS, SCENARIO INPUTS, AND SUPPORT DATA -------------------------------------

# Use firm synthesis model step process inputs script and load objects to global environment
source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_process_inputs.R"))
firm_inputs <- new.env()
Establishments <- firm_sim_process_inputs(envir = firm_inputs)
for(n in ls(firm_inputs, all.names=TRUE)) assign(n, get(n, firm_inputs), environment())

# Use CVTM model step process inputs script and load objects to global environment
USER_PROCESS_SKIMS <- FALSE # to skip skim processing
source(file = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim_process_inputs.R"))
cv_inputs <- new.env()
system.time(cv_sim_process_inputs(envir = cv_inputs))
for(n in ls(cv_inputs, all.names=TRUE)) assign(n, get(n, cv_inputs), environment())

# Skims
skims_tod <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"))

### Import SEMCOG data, careful to distinguish SEMCOG defs from CMAP data

#EMPCAT
NAICS3_to_EmpCats <- fread('dev/Data_Processed/SEMCOG_Data/NAICS3_SEMCOGEmpCats_CMAP.csv')

#TAZSocioEconomics
TAZSocioEconomicsSEMCOG <- fread('dev/Data_Processed/SEMCOG_Data/TAZSocioEconomics.csv')

#TAZSystem
TAZ_System_SEMCOG <- fread('dev/Data_Processed/SEMCOG_Data/TAZ_System.csv')

# Skims
skims_tod_semcog <- readRDS('dev/Data_Processed/SEMCOG_Data/skims_avg.rds')

# Datasets processed for estimation
# Stop Counts (SEMCOG data)
load("./dev/Estimation/cv_stops/Stop_Counts_Goods.RData") 
good_stop_counts
load("./dev/Estimation/cv_stops/Stop_Counts_Service.RData") 
service_stop_counts

# Vehicle choice data
cv_vehicle_data <- readRDS(file.path("./dev/Estimation/cv_vehicle/cv_vehicle_processed_data.rds"))

###TODO for calibration replace with stop duration data from the GPS data and also purpose specific summaries 
# Copied from the SEMCOG original estimation
# Stop duration data
cv_stopduration_data <- readRDS(file.path("./dev/Estimation/cv_duration/cv_duration_processed_data.rds"))

# Tours data
cv_tours_data <- readRDS(file.path("./dev/Estimation/cv_tours/cv_tours_processed_data.rds"))

###TODO for calibration replace with arrival data from the GPS data
# Arrival time
cv_arrival_data <- readRDS(file.path("./dev/Estimation/cv_arrival/cv_arrival_processed_data.rds"))

###TODO for calibration summaries from the SEMCOG original estimation (not updated here?)
# Intermediate stops
cv_intermediate_data <- readRDS(file.path("./dev/Estimation/cv_intermediate/cv_intermediate_processed_data.rds"))
cv_intermediate_attraction_data <- readRDS(file.path("./dev/Estimation/cv_intermediate/cv_intermediate_attraction_processed_data.rds"))

### 3. Simulate Commercial Vehicle Movements -------------------------------------------------
model_step_targets_cv_sim <- list()

### cv_sim_activities

# Calibration of the cv_sim_activities is a confirmatory check 
# that the draws from the input model distribution are matched by the outputs
cv_activities_model <- readRDS(file.path(SYSTEM_DATA_PATH, "cv_activities_model.RDS"))

targetdt <- melt.data.table(cv_activities_model,
                 id.vars = "EmpCatGroupedName", 
                 variable.name = "Activity", 
                 value.name = "Target")
setnames(targetdt, "EmpCatGroupedName", "Category")
targetdt[, c("CatType", "Units") := .("Employment Category", "Proportion of Firms")]

model_step_targets_cv_sim[["cv_sim_activities"]] <- targetdt

### cv_sim_scheduledstops -----------------------------------------------------------

# Calibration of the cv_sim_scheduledstops model:

### what is the approach?
# This model is estimated using the SEMCOG CVS data. 
# The expansion factors from the survey data can be used to create an expanded set
# of stops by purpose and industry operating the truck.

# Calibration outcome variables:
# include mean firm-to-stop distance, by industry group, 
# and number of stops generated per establishment employee. 

# Consideration of land use variables to ensure 
# the right amount of commercial vehicle traffic in residential zones. 
# To do this, append TAZ-level land use information—
# households and employment—to observations in the survey data. 
# The strategy would be to compare average zonal employment by key industries 
# as well as number of households in the zones of observed stops to those predicted by the model, 
# and use these to adjust some of the attractor variables in the model.

# In the dashboard
# Firm to stop distance

# Import the model coefficients
# What segments/calibration tests do their specifications suggest?

cv_goods_model <- readRDS(file.path(SYSTEM_DATA_PATH, "cv_goods_model.RDS"))
cv_service_model <- readRDS(file.path(SYSTEM_DATA_PATH, "cv_service_model.RDS"))
cv_goods_model
cv_service_model

stop_counts <- rbind(good_stop_counts[, Activity := "Goods"],
                     service_stop_counts[, Activity := "Service"])
rm(good_stop_counts, service_stop_counts)

stop_counts[, NAICS2 := as.integer(NAICS2)]
stop_counts[, NAICS3 := as.integer(NAICS3)]

# Summaries by stop type
# Mean firm-to-stop distance, by industry group, 
mean_stop_distance <- stop_counts[,.(Stops = sum(STOPS), StopsRes = sum(STOPS_RES), StopsNonRes = sum(STOPS_NON_RES), 
               MeanDistance = sum(dist * STOPS)/sum(STOPS),
               MeanDistanceRes = sum(dist * STOPS_RES)/sum(STOPS_RES),
               MeanDistanceNonRes = sum(dist * STOPS_NON_RES)/sum(STOPS_NON_RES),
               WeightedStops = sum(WEIGHTED_STOPS),
               WeightedStopsRes = sum(WEIGHTED_STOPS_RES),
               WeightedStopsNonRes = sum(WEIGHTED_STOPS_NON_RES),
               WeightedMeanDistance = sum(dist * WEIGHTED_STOPS)/sum(WEIGHTED_STOPS),
               WeightedMeanDistanceRes = sum(dist * WEIGHTED_STOPS_RES)/sum(WEIGHTED_STOPS_RES),
               WeightedMeanDistanceNonRes = sum(dist * WEIGHTED_STOPS_NON_RES)/sum(WEIGHTED_STOPS_NON_RES)), keyby = Activity]

mean_stop_distance_industry <- stop_counts[,.(Stops = sum(STOPS), StopsRes = sum(STOPS_RES), StopsNonRes = sum(STOPS_NON_RES), 
                                              MeanDistance = sum(dist * STOPS)/sum(STOPS),
                                              MeanDistanceRes = sum(dist * STOPS_RES)/sum(STOPS_RES),
                                              MeanDistanceNonRes = sum(dist * STOPS_NON_RES)/sum(STOPS_NON_RES),
                                              WeightedStops = sum(WEIGHTED_STOPS),
                                              WeightedStopsRes = sum(WEIGHTED_STOPS_RES),
                                              WeightedStopsNonRes = sum(WEIGHTED_STOPS_NON_RES),
                                              WeightedMeanDistance = sum(dist * WEIGHTED_STOPS)/sum(WEIGHTED_STOPS),
                                              WeightedMeanDistanceRes = sum(dist * WEIGHTED_STOPS_RES)/sum(WEIGHTED_STOPS_RES),
                                              WeightedMeanDistanceNonRes = sum(dist * WEIGHTED_STOPS_NON_RES)/sum(WEIGHTED_STOPS_NON_RES)), keyby = .(Activity, IndustryCat)]

distance_bins <- c(seq(0,10, by = 2), seq(15,60, by = 5), 70, 80)
stop_counts[, distance_bin := distance_bins[findInterval(dist, distance_bins)]]

stop_distance_dist <- stop_counts[,.(Stops = sum(STOPS),
               WeightedStops = sum(WEIGHTED_STOPS)), keyby = .(Activity, distance_bin)][, c("PctStops", "Target") := .(Stops/sum(Stops), WeightedStops/sum(WeightedStops)), by = Activity]

# Number of stops generated per establishment employee.  
# Use weighted stops and total employment in the SEMCOG model region in that industry

TAZEmpSEMCOG <- copy(TAZSocioEconomicsSEMCOG)[,c("HH", "POP") := NULL]
emp_ind <- melt.data.table(TAZEmpSEMCOG,
                id.vars = c("TAZ"),
                variable.name = "EmpCatName",
                value.name = "Employment")

emp_ind[NAICS3_to_EmpCats, EmpCatGroupedName := CMAPGroup, on = "EmpCatName"]

# sum by EmpCatGroupedName for just the internal SEMCOG TAZs
emp_ind_total <- emp_ind[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                         .(Employment = sum(Employment)), 
                         by = EmpCatGroupedName]

emp_stops  <- merge(emp_ind_total,
                    mean_stop_distance_industry[,.(Activity, EmpCatGroupedName = IndustryCat, Stops, StopsRes, StopsNonRes, WeightedStops, WeightedStopsRes, WeightedStopsNonRes)],
                    by = "EmpCatGroupedName")

emp_stops[, StopsEmp := WeightedStops/Employment]
emp_stops[, StopsResEmp := WeightedStopsRes/Employment]
emp_stops[, StopsNonResEmp := WeightedStopsNonRes/Employment]

### Stops generation/distance distribution as a function of 
### employment and household density around the establishment
### assumption is that distance to stops are shorter in higher density areas
### expected to vary by activity and industry

# summarize establishments

est <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                   .(dist = min(dist)), by = .(SITE_TAZID, SITEID, IndustryCat, TAZ)]
# add TAZ area
#assuming sqmi is the same as AREA - RICKY 
est[TAZ_System_SEMCOG[,.(TAZ, AREA)], AREA := i.AREA, on = "TAZ"]
# add SE data
est[TAZSocioEconomicsSEMCOG, c("HH", "POP") := .(i.HH, i.POP), on = "TAZ"]
est[emp_ind[,.(Employment = sum(Employment)), by = TAZ],
    NEmp_Total := i.Employment, on = "TAZ"]

# calculate average HH and emp density for TAZ within 1,2,5,10 miles for each establishment
density1 <- est[dist < 1,
                        .(AREA = sum(AREA), 
                          HH = sum(HH), 
                          POP = sum(POP), 
                          NEmp_Total = sum(NEmp_Total),
                          DistThresh = "1 mile"),
                        keyby = .(SITE_TAZID, SITEID)][, c("DensHH", "DensPOP", "DensEmp") :=
                                                         .(HH/AREA, POP/AREA, NEmp_Total/AREA)]

density2 <- est[dist < 2,
                        .(AREA = sum(AREA), 
                          HH = sum(HH), 
                          POP = sum(POP), 
                          NEmp_Total = sum(NEmp_Total),
                          DistThresh = "2 miles"),
                        keyby = .(SITE_TAZID, SITEID)][, c("DensHH", "DensPOP", "DensEmp") :=
                                                         .(HH/AREA, POP/AREA, NEmp_Total/AREA)]
density5 <- est[dist < 5,
                        .(AREA = sum(AREA), 
                          HH = sum(HH), 
                          POP = sum(POP), 
                          NEmp_Total = sum(NEmp_Total),
                          DistThresh = "5 miles"),
                        keyby = .(SITE_TAZID, SITEID)][, c("DensHH", "DensPOP", "DensEmp") :=
                                                         .(HH/AREA, POP/AREA, NEmp_Total/AREA)]

density10 <- est[dist < 10,
                        .(AREA = sum(AREA), 
                          HH = sum(HH), 
                          POP = sum(POP), 
                          NEmp_Total = sum(NEmp_Total),
                          DistThresh = "10 miles"),
                        keyby = .(SITE_TAZID, SITEID)][, c("DensHH", "DensPOP", "DensEmp") :=
                                                         .(HH/AREA, POP/AREA, NEmp_Total/AREA)]

est_density <- rbind(density1, density2, density5, density10)

est_density[, DensHHbin := seq(0, 2000, by = 500)[findInterval(DensHH, seq(0, 2000, by = 500))]]
est_density[, DensPOPbin := seq(0, 5000, by = 1000)[findInterval(DensPOP, seq(0, 5000, by = 1000))]]
est_density[, DensEmpbin := seq(0, 3000, by = 500)[findInterval(DensEmp, seq(0, 3000, by = 500))]]
est_density[,.N, keyby = DensHHbin]
est_density[,.N, keyby = DensPOPbin]
est_density[,.N, keyby = DensEmpbin]

# Simplify to compare est-stop distance distributions by industry by density bin
# 1 mile
stop_counts[est_density[DistThresh == "1 mile"],
            c("DensHHbin", "DensEmpbin") := .(i.DensHHbin, i.DensEmpbin),
            on = "SITEID"]

# 1 mile, HH
mean_stop_distance_dens1hh <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                                          .(Stops = sum(STOPS), 
                                     MeanDistance = sum(dist * STOPS)/sum(STOPS),
                                     WeightedStops = sum(WEIGHTED_STOPS),
                                     WeightedMeanDistance = sum(dist * WEIGHTED_STOPS)/sum(WEIGHTED_STOPS)), 
                                     keyby = .(Activity, DensHHbin)]

# 1 mile, EMP
mean_stop_distance_dens1emp <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                                           .(Stops = sum(STOPS), 
                                             MeanDistance = sum(dist * STOPS)/sum(STOPS),
                                             WeightedStops = sum(WEIGHTED_STOPS),
                                             WeightedMeanDistance = sum(dist * WEIGHTED_STOPS)/sum(WEIGHTED_STOPS)), 
                                           keyby = .(Activity, DensEmpbin)]

# 2 mile
stop_counts[est_density[DistThresh == "2 miles"],
            c("DensHHbin", "DensEmpbin") := .(i.DensHHbin, i.DensEmpbin),
            on = "SITEID"]

# 2 mile, HH
mean_stop_distance_dens2hh <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                                          .(Stops = sum(STOPS), 
                                             MeanDistance = sum(dist * STOPS)/sum(STOPS),
                                             WeightedStops = sum(WEIGHTED_STOPS),
                                             WeightedMeanDistance = sum(dist * WEIGHTED_STOPS)/sum(WEIGHTED_STOPS)), 
                                          keyby = .(Activity, DensHHbin)]

# 2 mile, EMP
mean_stop_distance_dens2emp <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                                           .(Stops = sum(STOPS), 
                                              MeanDistance = sum(dist * STOPS)/sum(STOPS),
                                              WeightedStops = sum(WEIGHTED_STOPS),
                                              WeightedMeanDistance = sum(dist * WEIGHTED_STOPS)/sum(WEIGHTED_STOPS)), 
                                           keyby = .(Activity, DensEmpbin)]

# 5 mile
stop_counts[est_density[DistThresh == "5 miles"],
            c("DensHHbin", "DensEmpbin") := .(i.DensHHbin, i.DensEmpbin),
            on = "SITEID"]

# 5 mile, HH
mean_stop_distance_dens5hh <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                                          .(Stops = sum(STOPS), 
                                             MeanDistance = sum(dist * STOPS)/sum(STOPS),
                                             WeightedStops = sum(WEIGHTED_STOPS),
                                             WeightedMeanDistance = sum(dist * WEIGHTED_STOPS)/sum(WEIGHTED_STOPS)), 
                                          keyby = .(Activity, DensHHbin)]

# 5 mile, EMP
mean_stop_distance_dens5emp <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                                           .(Stops = sum(STOPS), 
                                              MeanDistance = sum(dist * STOPS)/sum(STOPS),
                                              WeightedStops = sum(WEIGHTED_STOPS),
                                              WeightedMeanDistance = sum(dist * WEIGHTED_STOPS)/sum(WEIGHTED_STOPS)), 
                                           keyby = .(Activity, DensEmpbin)]

# 10 mile
stop_counts[est_density[DistThresh == "10 miles"],
            c("DensHHbin", "DensEmpbin") := .(i.DensHHbin, i.DensEmpbin),
            on = "SITEID"]

# 10 mile, HH
mean_stop_distance_dens10hh <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                                           .(Stops = sum(STOPS), 
                                             MeanDistance = sum(dist * STOPS)/sum(STOPS),
                                             WeightedStops = sum(WEIGHTED_STOPS),
                                             WeightedMeanDistance = sum(dist * WEIGHTED_STOPS)/sum(WEIGHTED_STOPS)), 
                                           keyby = .(Activity, DensHHbin)]

# 10 mile, EMP
mean_stop_distance_dens10emp <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                                            .(Stops = sum(STOPS), 
                                              MeanDistance = sum(dist * STOPS)/sum(STOPS),
                                              WeightedStops = sum(WEIGHTED_STOPS),
                                              WeightedMeanDistance = sum(dist * WEIGHTED_STOPS)/sum(WEIGHTED_STOPS)), 
                                            keyby = .(Activity, DensEmpbin)]

# split by industry
# 10 mile, HH
mean_stop_distance_dens10hh_ind <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                                               .(Stops = sum(STOPS), 
                                              MeanDistance = sum(dist * STOPS)/sum(STOPS),
                                              WeightedStops = sum(WEIGHTED_STOPS),
                                              WeightedMeanDistance = sum(dist * WEIGHTED_STOPS)/sum(WEIGHTED_STOPS)), 
                                              keyby = .(Activity, DensHHbin, IndustryCat)]

dcast.data.table(mean_stop_distance_dens10hh_ind,
                 Activity+DensHHbin~IndustryCat,
                 fun.aggregate = sum,
                 value.var = "WeightedMeanDistance")

dcast.data.table(mean_stop_distance_dens10hh_ind,
                 Activity+DensHHbin~IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Stops")

# 10 mile, EMP
mean_stop_distance_dens10emp_ind <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                                                .(Stops = sum(STOPS), 
                                               MeanDistance = sum(dist * STOPS)/sum(STOPS),
                                               WeightedStops = sum(WEIGHTED_STOPS),
                                               WeightedMeanDistance = sum(dist * WEIGHTED_STOPS)/sum(WEIGHTED_STOPS)), 
                                               keyby = .(Activity, DensEmpbin, IndustryCat)]

dcast.data.table(mean_stop_distance_dens10emp_ind,
                 Activity+DensEmpbin~IndustryCat,
                 fun.aggregate = sum,
                 value.var = "WeightedMeanDistance")

dcast.data.table(mean_stop_distance_dens10emp_ind,
                 Activity+DensEmpbin~IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Stops")

# Look at stops per TAZ based on TAZ employment and HH
stops_taz <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                         .(Stops = sum(STOPS), WeightedStops = sum(WEIGHTED_STOPS)), 
                         keyby = .(TAZ, Activity)]

stops_taz[TAZSocioEconomicsSEMCOG, c("HH", "POP") := .(i.HH, i.POP), on = "TAZ"]
stops_taz[emp_ind[,.(Employment = sum(Employment)), by = TAZ],
              NEmp_Total := i.Employment, on = "TAZ"]
stops_taz[, StopsHH := ifelse(HH > 0, WeightedStops/HH, 0)]
stops_taz[, StopsEmp := ifelse(NEmp_Total > 0, WeightedStops/NEmp_Total, 0)]
stops_taz[, HH_Emp := HH + NEmp_Total]

bins <- seq(0, 5000, by = 500)
stops_taz[, HH_bin := bins[findInterval(HH, bins)]]
stops_taz[, Emp_bin := bins[findInterval(NEmp_Total, bins)]]
stops_taz[, HH_Emp_bin := bins[findInterval(HH_Emp, bins)]]

stops_taz[,.(Stops = sum(Stops), 
             WeightedStops = sum(WeightedStops),
             HH = sum(HH)),
          keyby = .(Activity, HH_bin)][, Rate := WeightedStops/HH][]

stops_taz[,.(Stops = sum(Stops), 
             WeightedStops = sum(WeightedStops),
             Emp = sum(NEmp_Total)),
          keyby = .(Activity, Emp_bin)][, Rate := WeightedStops/Emp][]

stop_taz_hh_emp <- stops_taz[,.(Stops = sum(Stops), 
             WeightedStops = sum(WeightedStops),
             HH_Emp = sum(HH_Emp)),
          keyby = .(Activity, HH_Emp_bin)][, Rate := WeightedStops/HH_Emp][]

stops_taz_ind <- stop_counts[TAZ %in% TAZ_System_SEMCOG[TAZ_TYPE == "SEMCOG"]$TAZ,
                             .(Stops = sum(STOPS), WeightedStops = sum(WEIGHTED_STOPS)), 
                             keyby = .(TAZ, Activity, IndustryCat)]
stops_taz_ind[TAZSocioEconomicsSEMCOG, c("HH", "POP") := .(i.HH, i.POP), on = "TAZ"]
stops_taz_ind[emp_ind[,.(Employment = sum(Employment)), by = TAZ],
          NEmp_Total := i.Employment, on = "TAZ"]
stops_taz_ind[, StopsHH := ifelse(HH > 0, WeightedStops/HH, 0)]
stops_taz_ind[, StopsEmp := ifelse(NEmp_Total > 0, WeightedStops/NEmp_Total, 0)]
stops_taz_ind[, HH_Emp := HH + NEmp_Total]

bins <- seq(0, 5000, by = 500)
stops_taz_ind[, HH_bin := bins[findInterval(HH, bins)]]
stops_taz_ind[, Emp_bin := bins[findInterval(NEmp_Total, bins)]]
stops_taz_ind[, HH_Emp_bin := bins[findInterval(HH_Emp, bins)]]

stops_taz_ind_hh_emp <- stops_taz_ind[,.(Stops = sum(Stops), 
             WeightedStops = sum(WeightedStops),
             HH_Emp = sum(HH_Emp)),
          keyby = .(Activity, IndustryCat, HH_Emp_bin)][, Rate := WeightedStops/HH_Emp][]

stops_taz_ind_hh_emp

dcast.data.table(stops_taz_ind_hh_emp,
                 Activity+HH_Emp_bin~IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Rate")

dcast.data.table(stops_taz_ind_hh_emp,
                 Activity+HH_Emp_bin~IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Stops")

### CV GPS

# Read in the summaries from the CMAP CV GPS data
gps_dist <- fread('dev/Data_Processed/CVGPS/Calibration Targets/BaseStopDist_CVGPS.csv')
gps_dist_veh <- fread('dev/Data_Processed/CVGPS/Calibration Targets/BaseStopDist_vehicle_CVGPS.csv')
gps_dist_mean <- fread('dev/Data_Processed/CVGPS/Calibration Targets/BaseStopMean_CVGPS.csv')
gps_tour_repzones_visits <- fread('dev/Data_Processed/CVGPS/Calibration Targets/repeatZones_nVisits_CVGPS.csv')
gps_tour_repzones_unique <- fread('dev/Data_Processed/CVGPS/Calibration Targets/repeatZones_UniqueZones_CVPGS.csv')

# Add the targets to the list
model_step_targets_cv_sim[["cv_sim_scheduledstops"]] <- list(mean_stop_distance = mean_stop_distance,
                                                           mean_stop_distance_industry = mean_stop_distance_industry,
                                                           stop_distance_dist = stop_distance_dist,
                                                           emp_stops = emp_stops,
                                                           mean_stop_distance_dens10hh = mean_stop_distance_dens10hh,
                                                           mean_stop_distance_dens10emp = mean_stop_distance_dens10emp,
                                                           mean_stop_distance_dens10hh_ind = mean_stop_distance_dens10hh_ind,
                                                           mean_stop_distance_dens10emp_ind = mean_stop_distance_dens10emp_ind,
                                                           stop_taz_hh_emp = stop_taz_hh_emp,
                                                           stops_taz_ind_hh_emp = stops_taz_ind_hh_emp,
                                                           gps_dist = gps_dist,
                                                           gps_dist_veh = gps_dist_veh,
                                                           gps_dist_mean = gps_dist_mean,
                                                           gps_tour_repzones_visits = gps_tour_repzones_visits,
                                                           gps_tour_repzones_unique = gps_tour_repzones_unique)

### cv_sim_vehicle -----------------------------------------------------------

# Calibration of the cv_sim_vehicle model:

# The vehicle choice model is applied to each scheduled stop generated by a synthetic firm, 
# taking into account industry, scheduled stop purpose, and distance from the establishment. 
# The forecasted shares of each vehicle type can be calibrated by industry type 
# by adjusting the alternative-specific constants for light and medium trucks 
# relative to the reference alternative, heavy trucks.

# Outcome Variable	
# Vehicle type by stop type
# Light	Medium	Heavy
# Goods	% of Stops	% of Stops	% of Stops
# Service	% of Stops	% of Stops	% of Stops

# Segmentation
# Industry Groups (based on SEMCOG 18 employment categories and groupings)

# Validation Approach:  
# The expanded SEMCOG CVS data provides the best information for 
# initially calibrating this model, with traffic classification counts
# used to further adjust vehicle shares, after assigning vehicles to the regional highway network. 
# This implies a 2-stage process in which we first vehicle choice to the CVS shares 
# and then later calibrate vehicle type choice again based on discrepancies 
# between the network assignment results and the traffic counts. 

# A limitation of the second stage of adjustment is that calibration 
# can only be done for all stops types and all industries as one single group. 
# We recommend maintaining the industry-specific and stop activity specific 
# constants in the model specification calibrated to the CVS data 
# under the assumption that they represent a reasonable estimate of 
# variation between industry groups and stop types.

# In the dashboard
# Vehicle shares


#can add similar tables from CMAP CVGPS tables
#shares by vehicle type & distance



# Import the model coefficients
# What segments/calibration tests does the specifications suggest?
cv_vehicle_model <- readRDS(file.path(SYSTEM_DATA_PATH, "cv_vehicle_model.RDS"))
apollo::apollo_modelOutput(cv_vehicle_model)

# Summaries
cv_vehicle_data[, veh_class_name := factor(veh_choice, labels = c("Light" ,"Medium" ,"Heavy"))]
cv_vehicle_data[, Activity := ifelse(activity_deliver + activity_pickup > 0, "Goods",
                                     ifelse(activity_service > 0, "Service", 
                                            ifelse(activity_return > 0, "Return", "Intermediate")))]

# Vehicle shares: weighted percent of stops by activity
vehicle_stops_activity <- cv_vehicle_data[,.(Stops = .N, FINAL_FACTOR = sum(FINAL_FACTOR)), 
                keyby = .(Vehicle = veh_class_name, Activity)][, c("PctStops", "Target") := .(Stops/sum(Stops), FINAL_FACTOR/sum(FINAL_FACTOR)), keyby = Activity]

# Check whether industry specific results different enought to suggest
# industry specific calibration
vehicle_stops_activity_ind <- cv_vehicle_data[,.(Stops = .N, FINAL_FACTOR = sum(FINAL_FACTOR)), 
                                          keyby = .(Vehicle = veh_class_name, Activity, 
                                                    IndustryCat = model_emp_cat)][, c("PctStops", "Target") := 
                                                                                    .(Stops/sum(Stops), FINAL_FACTOR/sum(FINAL_FACTOR)), 
                                                                                  keyby = .(Activity, IndustryCat)]

# Cross tab by industry
dcast.data.table(vehicle_stops_activity_ind,
                 Activity + Vehicle ~ IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of stops) by industry
dcast.data.table(vehicle_stops_activity_ind,
                 Activity + Vehicle ~ IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Stops")

# Vehicle choice by distance
dist_bins <- c(0, 2, 5, 10, 20)
cv_vehicle_data[, dist_bin := factor(findInterval(x = dist, vec = dist_bins),
                                     labels = c("dist_00_02", "dist_02_05", "dist_05_10", "dist_10_20", "dist_20_p"))]

vehicle_stops_activity_dist <- cv_vehicle_data[,.(Stops = .N, FINAL_FACTOR = sum(FINAL_FACTOR)), 
                                              keyby = .(Vehicle = veh_class_name, Activity, 
                                                        dist_bin)][, c("PctStops", "Target") := .(Stops/sum(Stops), FINAL_FACTOR/sum(FINAL_FACTOR)), 
                                                                                                       keyby = .(Activity, dist_bin)]

# Cross tab by distance
dcast.data.table(vehicle_stops_activity_dist,
                 Activity + Vehicle ~ dist_bin,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of stops) by distance
dcast.data.table(vehicle_stops_activity_dist,
                 Activity + Vehicle ~ dist_bin,
                 fun.aggregate = sum,
                 value.var = "Stops")

# Check on relationship between length and industry
### The outputs from stop generation should look like this
stops_activity_dist_ind <- cv_vehicle_data[,.(Stops = .N, FINAL_FACTOR = sum(FINAL_FACTOR)), 
                                               keyby = .(Activity, dist_bin, 
                                                         IndustryCat = model_emp_cat)][, c("PctStops", "PctWght") := 
                                                                         .(Stops/sum(Stops), FINAL_FACTOR/sum(FINAL_FACTOR)), 
                                                                       keyby = .(Activity, dist_bin)] 

dcast.data.table(stops_activity_dist_ind,
                 Activity + dist_bin ~ IndustryCat,
                 fun.aggregate = sum,
                 value.var = "PctWght")

dcast.data.table(stops_activity_dist_ind,
                 Activity + dist_bin ~ IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Stops")

# Add the targets to the list
model_step_targets_cv_sim[["cv_sim_vehicle"]] <- list(vehicle_stops_activity = vehicle_stops_activity,
                                                      vehicle_stops_activity_ind = vehicle_stops_activity_ind,
                                                      vehicle_stops_activity_dist = vehicle_stops_activity_dist,
                                                      stops_activity_dist_ind = stops_activity_dist_ind)

### cv_sim_stopduration  -----------------------------------------------------------

# Calibration of the cv_sim_stopduration model:

# Outcome Variable 	
# Duration in minutes at a stop

# Segmentation
# Activity type, industry, and vehicle types

# Validation Approach  

# The expanded SEMCOG CVS data provides a sufficient sample from which to 
# derive target distributions for stop duration, segmented by vehicle types. 

# Vehicle type has been found to be a good proxy for the amount of time spent
# in deliveries and pickups because the size of the truck usually is a good 
# indicator for the amount of goods being moved, 
# and in addition, services stops have been observed to vary considerably in length
# and often take much longer than delivery stop.

# The activity at the stop is important beyond just differentiating between goods and service stops, 
# since some stops will be of intermediate types—breaks/meals, vehicle servicing. 
# Intermediate stops are included in the observations in the SEMCOG CVS data.

# The CMAP passive GPS data, can be used to adjust 
# the aggregate distribution after the segmented distributions derived from 
# the survey data are applied.

# Import the model coefficients
# What segments/calibration tests does the specifications suggest?
cv_stopduration_model <- readRDS(file.path(SYSTEM_DATA_PATH, "cv_stopduration_model.RDS"))
cv_stopduration_model$estimate

# Summaries
cv_stopduration_data[, duration_15min := ceiling(duration / 15) * 15]

cv_stopduration_data[, 
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

cv_stopduration_data[, duration_group := factor(duration_group, labels = c("1-15", "16-30", "31-45", "46-60", "61-75", "76-90", "91-150", "151-210", "211-270", "271-390", "391+"))]

cv_stopduration_data[, veh_class_name := factor(veh_choice, labels = c("Light" ,"Medium" ,"Heavy"))]
cv_stopduration_data[, Activity := ifelse(activity_deliver + activity_pickup > 0, "Goods",
                                     ifelse(activity_service > 0, "Service", 
                                            ifelse(activity_return > 0, "Return", "Intermediate")))]
#need to get new employment groups
# Industry
setnames(cv_stopduration_data, "model_emp_cat", "EmpCatName")
cv_stopduration_data[NAICS3_to_EmpCats, IndustryCat := i.CMAPGroup, on = "EmpCatName"]

# Duration shares: weighted percent of durations
duration_stops <- cv_stopduration_data[,.(Stops = .N, FINAL_FACTOR = sum(FINAL_FACTOR)), 
                                       keyby = .(duration_group)][, c("PctStops", "Target") := .(Stops/sum(Stops), FINAL_FACTOR/sum(FINAL_FACTOR))]

# Durations by activity
duration_stops_activity <- cv_stopduration_data[,.(Stops = .N, FINAL_FACTOR = sum(FINAL_FACTOR)), 
                                          keyby = .(duration_group, Activity)][, c("PctStops", "Target") := .(Stops/sum(Stops), FINAL_FACTOR/sum(FINAL_FACTOR)), keyby = Activity]

# Cross tab by activity
dcast.data.table(duration_stops_activity,
                 duration_group ~ Activity,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of stops) by activity
dcast.data.table(duration_stops_activity,
                 duration_group ~ Activity,
                 fun.aggregate = sum,
                 value.var = "Stops")

# Durations by activity and vehicle type
duration_stops_activity_vehicle <- cv_stopduration_data[,.(Stops = .N, FINAL_FACTOR = sum(FINAL_FACTOR)), 
                                                        keyby = .(duration_group, Activity, Vehicle = veh_class_name)][, c("PctStops", "Target") := .(Stops/sum(Stops), FINAL_FACTOR/sum(FINAL_FACTOR)), keyby = .(Activity, Vehicle)]

# Cross tab by activity
dcast.data.table(duration_stops_activity_vehicle,
                 duration_group ~ Activity + Vehicle,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of stops) by activity
dcast.data.table(duration_stops_activity_vehicle,
                 duration_group ~ Activity + Vehicle,
                 fun.aggregate = sum,
                 value.var = "Stops")

### CV GPS

# Read in the summaries from the CMAP CV GPS data
gps_duration_stops <- fread('dev/Data_Processed/CVGPS/Calibration Targets/duration_stops_CVGPS.csv')
gps_duration_stops_mean <- fread('dev/Data_Processed/CVGPS/Calibration Targets/duration_stops_Mean_CVGPS.csv')


# Add the targets to the list
model_step_targets_cv_sim[["cv_sim_stopduration"]] <- list(duration_stops = duration_stops,
                                                           duration_stops_activity = duration_stops_activity,
                                                           duration_stops_activity_vehicle = duration_stops_activity_vehicle,
                                                           gps_duration_stops = gps_duration_stops,
                                                           gps_duration_stops_mean = gps_duration_stops_mean)

### cv_sim_tours  -----------------------------------------------------------

# Calibration of the cv_sim_tours model:
# STOP CLUSTERING MODEL

# To calibrate the stop clustering model, the leaf weight threshold 
# is adjusted using the distribution of stops observed by vehicle type
# in the survey data. The threshold is adjusted such that the 
# average number of simulated stops per tour is approximately equal 
# to the average number of observed stops per tour. 

# Note that, at this point, the order in which each stop on the tour
# is visited has not been determined and travel times are not known. 
# Accordingly, the threshold for the sum of stop durations is set to 
# a sufficiently low value that it will allow for travel time between 
# the establishment and each stop and the possibility of inserting intermediate 
# (non-scheduled stops), as described below in the Intermediate Stops model.

# Outcome Variable 	
# Set of stops with spatial locations identified to be assigned to a multi-stop tour by a single vehicle

# Segmentation 	
# Vehicle types

# Validation Approach
# The expanded SEMCOG CVS data should provide a sufficient sample from which to 
# derive target distributions for total tour duration, segmented by vehicle types. 

# The passive GPS data can also be used for vehicles classes present in those data. 

# The algorithm does not distinguish between stop activities, since there may be 
# multiple stop activity types on the same tour, so the lack of activity information 
# in the GPS data is not a barrier to its use for this model. 

# The observed mean tour duration by vehicle type will be used to adjust the 
# algorithm’s “branch weight limit” (maximum tour duration allowed) up or down. 
# Since the algorithm is a deterministic optimization approach, 
# it does not provide good controls for adjusting the shape of the 
# forecast distribution; therefore, reliance on matching mean tour duration 
# values is the best way to control predicted vehicle miles/hours traveled per tour. 
# For this reason, we do not recommend using the maximum observed tour 
# duration directly as the tour duration limit, since statistical outliers 
# or even 95th percentile values may not provide good controls on distributions. 

# As a second measure of model validity, we recommend considering the number 
# of total stops per tour, either single stop or more than one stop, 
# which indicates a multi-stop tour. 

# The hierarchical clustering algorithm will produce some single-stop tours in 
# addition to the single stops that are associated with a business and 
# vehicle and are not necessary to run through the stop clustering model. 
# The number of single stop tours produced by the component will 
# therefore be combined with that group of already created single 
# stop tours for comparison with the target data.

# Total tour duration will also be influenced by the insertion of 
# intermediate stops into the tour, which take place in a downstream step 
# and are conditioned by the spatial relationships among the scheduled stops 
# already on the tour. Therefore, calibration of the stop clustering model 
# requires that the entire model stream be run, so that tallies of tour duration
# and stops per tour include intermediate stops.

# TOUR TYPE MODEL
# The following types of tour will be created by this model component. 
# •	Single stop clusters will be identified as either:
#   o	base-base single-stop tours, 
# o	base to not-base single-stop tours, 
# o	not-base to base single-stop tours, or
# o	not-base to not-base single-stop tours.
# •	Multi-stop clusters will be identified as either:
#   o	base-base multi-stop tours, 
# o	base to not-base multi-stop tours, 
# o	not-base to base multi-stop tours, or
# o	not-base to not-base multi-stop tours

# The form of the model is a multinomial logit model that selects from the four tour types. 
# This is estimated using the SEMCOG CVS, and is segmented by vehicle type and whether 
# the tour is a single stop or multi-stop tour. Variables that affect the preferences 
# for the tour types include the industry operating the commercial vehicle, 
# the purpose of the stops in the tour (e.g., delivery or service), 
# the number of stops in the tours, and the proximity of the stops to the business establishment.

# Outcome Variable 	
# Set of tours formed from one or more stops with a tour type associated with them

# Segmentation 	
# Vehicle types, industry

# Validation Approach
# This model component’s choice of tour types can be calibrated 
# using the weighted tour type shares observed in the SEMCOG CVS.

# TOUR SEQUENCING MODEL

# Given a set of clustered stops comprising a tour, 
# the route sequencing model will apply the TSP algorithm and create a routing sequence 

# Outcome Variable 	
# Set of stops with spatial locations identified to be assigned to a peddling tour 
# by a single vehicle, sorted in the order to be visited

# Segmentation 	
# Vehicle types, and groups of number of stops in a tour 
# (e.g., two to three stops, four to six stops, more than six stops)

# Validation Approach  
# The only control in the stop sequencing model is to decide how many 
# iterations to let the TSP algorithm run to find an optimal routed sequence.  
# For this model, the large number of observations in the passive GPS data present 
# an opportunity for informed calibration of the heavy vehicles segment. 
# In order to estimate “how far from optimal” we should allow, we recommend 
# making comparisons to the observed stop sequences in the observed data. 
# The large passive dataset should include an abundance of observations from which to 
# infer the error tolerance in the sequencing model. The approach would be to assign 
# travel times to the trips between stops on tours in the observed data, using modeled 
# zone-to-zone skims to calculate the differences between the observed travel times and 
# what could have been obtained if the stops were sequenced to minimize travel time, 
# using the TSP algorithm. 
# Since tour complexity influences this process, the average tour travel time “error” 
# (difference between actual and optimal) would then be used to set the maximum number of 
# iterations for the TSP algorithm for tours of different lengths, in terms of number of stops. 
# The exact number of tour-length calibration groups will be subject to testing.

# In the dashboard
# Scheduled stops per tour
# Single stop tours

# Import the model coefficients
# What segments/calibration tests does the specifications suggest?
cv_tours_model <- readRDS(file.path(SYSTEM_DATA_PATH, "cv_tours_model.RDS"))
apollo::apollo_modelOutput(cv_tours_model)

# Summaries
cv_tours_data[, veh_class_name := factor(veh_choice, labels = c("Light" ,"Medium" ,"Heavy"))]

# Industry
cv_tours_data[,.N, keyby = .(IndustryCat = model_emp_cat)]

# Tour type labels
tour_type_labels <- c("bbm","bbs","bnm","bns","nbm","nb0","nbs","nnm")
cv_tours_data[, tour_type_choice := factor(tour_type_choice, labels = tour_type_labels)]

# Tour shares: weighted percent of tours types
# weighting weights up the incidence of multistop tours as it weighted at the trip level
# weight by total factor/num stops?
tour_types <- cv_tours_data[!is.na(tour_type_choice) & tour_type_choice != "nb0",
                            .(Tours = .N, NumStops = sum(num_stops), FINAL_FACTOR = sum(FINAL_FACTOR/num_stops)), 
                                       keyby = .(tour_type_choice)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR))]

# tours by vehicle
tour_types_vehicle <- cv_tours_data[!is.na(tour_type_choice) & tour_type_choice != "nb0",
                                    .(Tours = .N, NumStops = sum(num_stops), FINAL_FACTOR = sum(FINAL_FACTOR/num_stops)), 
                                    keyby = .(tour_type_choice, Vehicle = veh_class_name)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR)), keyby = Vehicle]

# Cross tab by vehicle
dcast.data.table(tour_types_vehicle,
                 tour_type_choice ~ Vehicle,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of tours) by vehicle
dcast.data.table(tour_types_vehicle,
                 tour_type_choice ~ Vehicle,
                 fun.aggregate = sum,
                 value.var = "Tours")

# tours by vehicle and industry
tour_types_vehicle_industry <- cv_tours_data[!is.na(tour_type_choice) & tour_type_choice != "nb0",
                                             .(Tours = .N, NumStops = sum(num_stops), FINAL_FACTOR = sum(FINAL_FACTOR/num_stops)), 
                                             keyby = .(tour_type_choice, Vehicle = veh_class_name, IndustryCat = model_emp_cat)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR)), keyby = .(Vehicle, IndustryCat)]

# Cross tab by vehicle and industry
dcast.data.table(tour_types_vehicle_industry,
                 tour_type_choice ~ IndustryCat + Vehicle,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of tours) by vehicle and industry
dcast.data.table(tour_types_vehicle_industry,
                 tour_type_choice ~ IndustryCat + Vehicle,
                 fun.aggregate = sum,
                 value.var = "Tours")

# Tour stops and durations 
cv_tours_data[num_stops < 5, num_stops_group := num_stops]
cv_tours_data[num_stops %in% 5:6, num_stops_group := 5]
cv_tours_data[num_stops %in% 7:10, num_stops_group := 6]
cv_tours_data[num_stops > 10, num_stops_group := 7]
cv_tours_data[, num_stops_group := factor(num_stops_group, labels = c("1", "2", "3", "4", "5-6", "7-10", "11+"))]

tour_stops <- cv_tours_data[!is.na(tour_type_choice) & tour_type_choice != "nb0",
                            .(Tours = .N, NumStops = sum(num_stops), FINAL_FACTOR = sum(FINAL_FACTOR/num_stops)), 
                            keyby = .(num_stops_group)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR))]

tour_stops_vehicle <- cv_tours_data[!is.na(tour_type_choice) & tour_type_choice != "nb0",
                            .(Tours = .N, NumStops = sum(num_stops), FINAL_FACTOR = sum(FINAL_FACTOR/num_stops)), 
                            keyby = .(num_stops_group, Vehicle = veh_class_name)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR)), by = Vehicle]

# Cross tab by vehicle
dcast.data.table(tour_stops_vehicle,
                 num_stops_group ~ Vehicle,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of tours) by vehicle
dcast.data.table(tour_stops_vehicle,
                 num_stops_group ~ Vehicle,
                 fun.aggregate = sum,
                 value.var = "Tours")

tour_stops_industry <- cv_tours_data[!is.na(tour_type_choice) & tour_type_choice != "nb0",
                                    .(Tours = .N, NumStops = sum(num_stops), FINAL_FACTOR = sum(FINAL_FACTOR/num_stops)), 
                                    keyby = .(num_stops_group, IndustryCat = model_emp_cat)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR)), by = IndustryCat]

# Cross tab by industry
dcast.data.table(tour_stops_industry,
                 num_stops_group ~ IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of tours) by industry
dcast.data.table(tour_stops_industry,
                 num_stops_group ~ IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Tours")

# Split the total duration into bins
duration_bins <- c(0, 30, 60, 120, 180, 240, 360, 480)
cv_tours_data[, duration_bin := findInterval(total_stop_duration, duration_bins)]
cv_tours_data[, duration_bin := factor(duration_bin, labels = c("< 30 mins", "30-59 mins", "1-2 hours", 
                                                                "2-3 hours", "3-4 hours", "4-6 hours", 
                                                                "6-8 hours", "8+ hours"))]

tour_duration <- cv_tours_data[!is.na(tour_type_choice) & tour_type_choice != "nb0",
                            .(Tours = .N, NumStops = sum(num_stops), FINAL_FACTOR = sum(FINAL_FACTOR/num_stops)), 
                            keyby = .(duration_bin)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR))]


tour_duration_vehicle <- cv_tours_data[!is.na(tour_type_choice) & tour_type_choice != "nb0",
                                    .(Tours = .N, NumStops = sum(num_stops), FINAL_FACTOR = sum(FINAL_FACTOR/num_stops)), 
                                    keyby = .(duration_bin, Vehicle = veh_class_name)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR)), by = Vehicle]

# Cross tab by vehicle
dcast.data.table(tour_duration_vehicle,
                 duration_bin ~ Vehicle,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of tours) by vehicle
dcast.data.table(tour_duration_vehicle,
                 duration_bin ~ Vehicle,
                 fun.aggregate = sum,
                 value.var = "Tours")

tour_duration_industry <- cv_tours_data[!is.na(tour_type_choice) & tour_type_choice != "nb0",
                                     .(Tours = .N, NumStops = sum(num_stops), FINAL_FACTOR = sum(FINAL_FACTOR/num_stops)), 
                                     keyby = .(duration_bin, IndustryCat = model_emp_cat)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR)), by = IndustryCat]

# Cross tab by industry
dcast.data.table(tour_duration_industry,
                 duration_bin ~ IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of tours) by industry
dcast.data.table(tour_duration_industry,
                 duration_bin ~ IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Tours")

### CV GPS

# Read in the summaries from the CMAP CV GPS data
gps_duration_tour <- fread('dev/Data_Processed/CVGPS/Calibration Targets/tourDuration_CVGPS.csv')
gps_distance_tour <- fread('dev/Data_Processed/CVGPS/Calibration Targets/TotalTourDistance.csv')
gps_numstops_tour <- fread('dev/Data_Processed/CVGPS/Calibration Targets/Tour_NStops_CVGPS.csv')
gps_singlemulti_tour <- fread('dev/Data_Processed/CVGPS/Calibration Targets/TourSingleMulti_CVGPS.csv')
gps_tourtype_tour <- fread('dev/Data_Processed/CVGPS/Calibration Targets/TourType_CVGPS.csv')
gps_cluster_tour <- fread('dev/Data_Processed/CVGPS/Calibration Targets/TourODMatrixDistAvg_CVGPS.csv')


# Add the targets to the list
model_step_targets_cv_sim[["cv_sim_tours"]] <- list(tour_types = tour_types,
                                                    tour_types_vehicle = tour_types_vehicle,
                                                    tour_types_vehicle_industry = tour_types_vehicle_industry,
                                                    tour_stops = tour_stops,
                                                    tour_stops_vehicle = tour_stops_vehicle,
                                                    tour_stops_industry = tour_stops_industry,
                                                    tour_duration = tour_duration,
                                                    tour_duration_vehicle = tour_duration_vehicle,
                                                    tour_duration_industry = tour_duration_industry,
                                                    gps_duration_tour = gps_duration_tour,
                                                    gps_distance_tour = gps_distance_tour,
                                                    gps_numstops_tour = gps_numstops_tour,
                                                    gps_singlemulti_tour = gps_singlemulti_tour,
                                                    gps_tourtype_tour = gps_tourtype_tour,
                                                    gps_cluster_tour = gps_cluster_tour)

### cv_sim_scheduledtrips  -----------------------------------------------------------

# Calibration of the cv_sim_scheduledtrips model:

# The first stop arrival model is calibrated by adjusting the alternative-specific constants, 
# representing 30-minute intervals

# Outcome Variable
# Choice of a 30-minute time interval during which to start the tour 

# Segmentation: 	
# Vehicle types, industry groups

# Validation Approach

# The SEMCOG CVS data should provide a sufficient sample from which develop target distributions 
# for tour-starting time of day, segmented by vehicle types and industry, and for heavy vehicles 
# the passive GPS data can be used as a source for aggregate adjustments. 
# Assigned volumes by time of day can be compared with traffic counts to provide a source for 
# further adjustments, with the caveat that the time of day for trips is also dependent on tour 
# length and the number of intermediate stops added into the tour. 
# The model is heavily constant-driven; the estimated constants can be initially adjusted to match the 
# CVS data followed by additional adjustments to match secondary targets derived from the passive GPS data 
# or traffic counts.

# In the dashboard
# First stop arrival time

# Import the model coefficients
# What segments/calibration tests does the specifications suggest?
cv_arrival_model <- readRDS(file.path(SYSTEM_DATA_PATH, "cv_arrival_model.RDS"))
apollo::apollo_modelOutput(cv_arrival_model)
arrival_labels <- sapply(strsplit(names(cv_arrival_model$estimate)[1:28],"_"),"[[",2)

# Summaries
cv_arrival_data[, arrival_choice := factor(arrival_choice, labels = arrival_labels)]

cv_arrival_data[, veh_class_name := factor(veh_choice, labels = c("Light" ,"Medium" ,"Heavy"))]

# Industry
cv_arrival_data[,.N, keyby = .(Industry = model_emp_cat)]

tour_arrival <- cv_arrival_data[, .(Tours = .N, FINAL_FACTOR = sum(FINAL_FACTOR)), 
                               keyby = .(arrival_choice)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR))]

tour_arrival_vehicle <- cv_arrival_data[, .(Tours = .N, FINAL_FACTOR = sum(FINAL_FACTOR)), 
                                             keyby = .(arrival_choice, Vehicle = veh_class_name)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR)), by = Vehicle]

# Cross tab by vehicle
dcast.data.table(tour_arrival_vehicle,
                 arrival_choice ~ Vehicle,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of tours) by vehicle
dcast.data.table(tour_arrival_vehicle,
                 arrival_choice ~ Vehicle,
                 fun.aggregate = sum,
                 value.var = "Tours")

tour_arrival_industry <- cv_arrival_data[, .(Tours = .N, FINAL_FACTOR = sum(FINAL_FACTOR)), 
                                              keyby = .(arrival_choice, IndustryCat = model_emp_cat)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR)), by = IndustryCat]

# Cross tab by industry
dcast.data.table(tour_arrival_industry,
                 arrival_choice ~ IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of tours) by industry
dcast.data.table(tour_arrival_industry,
                 arrival_choice ~ IndustryCat,
                 fun.aggregate = sum,
                 value.var = "Tours")

tour_arrival_type <- cv_arrival_data[, .(Tours = .N, FINAL_FACTOR = sum(FINAL_FACTOR)), 
                                              keyby = .(arrival_choice, TOUR_STOP_TYPE)][, c("PctTours", "Target") := .(Tours/sum(Tours), FINAL_FACTOR/sum(FINAL_FACTOR)), by = TOUR_STOP_TYPE]

# Cross tab by TOUR_STOP_TYPE
dcast.data.table(tour_arrival_type,
                 arrival_choice ~ TOUR_STOP_TYPE,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of tours) by TOUR_STOP_TYPE
dcast.data.table(tour_arrival_type,
                 arrival_choice ~ TOUR_STOP_TYPE,
                 fun.aggregate = sum,
                 value.var = "Tours")

### CV GPS

# Read in the summaries from the CMAP CV GPS data
gps_arrival_tour <- fread('dev/Data_Processed/CVGPS/Calibration Targets/TourFirstArrival_CVGPS.csv')

# Add the targets to the list
model_step_targets_cv_sim[["cv_sim_scheduledtrips"]] <- list(tour_arrival = tour_arrival,
                                                             tour_arrival_vehicle = tour_arrival_vehicle,
                                                             tour_arrival_industry = tour_arrival_industry,
                                                             tour_arrival_type = tour_arrival_type,
                                                             gps_arrival_tour = gps_arrival_tour)

### cv_sim_intermediatestops -----------------------------------------------------------

# Calibration of the cv_sim_intermediatestops model:

# INTERMEDIATE STOP CHOICE
# The intermediate stop choice model is applied for each tour, until no 
# additional intermediate stops are added in between the scheduled stops. 

# Calibration of the model involves adjusting the alternative-specific constants 
# for the three stop types. The metrics typically used in the calibration process are 
# derived from survey data and include:
# •	Intermediate stops per scheduled stop;
# •	Rate of meal or break stops per 8 hours of time spent on tours;
# •	Rate of refueling stops per 100 miles traveled; and
# •	Mean number of sequential stops per tour.

# Outcome Variable 	
# No stop or 1 stop for either vehicle servicing, break/meal, or “other” type

# Segmentation 	
# Vehicle types

# Validation Approach:  
# The expanded SEMCOG CVS data can be used to derive performance measures calibrate the model:
# •	Intermediate stops per scheduled stop
# •	Meal/break stops per 8 hours
# •	Refueling stops per 100 miles
# •	Mean sequential intermediate stops
# The alternative specific constants in the model for each stop type can be adjustment to match the performance measures.

# INTERMEDIATE STOP DESTINATION
# Parameters for the intermediate stop destination model will be estimaed from the SEMCOG CVS data. 

# Outcome Variable 	
# Location (TAZ ID) for an intermediate stop

# Segmentation
# Intermediate stop type  	

# Validation Approach
# Calibration of this model is relatively simple, which is to minimize deviation travel distance 
# from what could have been obtained without the stop inserted. The expanded SEMCOG CVS data 
# identifies intermediate stops and should have enough observations to derive benchmark value 
# for mean deviation distances by type of intermediate stop.

# In the dashboard
# Intermediate stops per scheduled stop
# Meals or break stops per 8 hours
# Refueling stop frequency

# Import the model coefficients
# What segments/calibration tests does the specifications suggest?
cv_intermediate_model <- readRDS(file.path(SYSTEM_DATA_PATH, "cv_intermediate_model.RDS"))
cv_intermediate_attraction_model <- readRDS(file.path(SYSTEM_DATA_PATH, "cv_intermediate_model_attraction.RDS"))
cv_intermediate_deviations <- readRDS(file.path(SYSTEM_DATA_PATH, "cv_intermediate_deviations.RDS"))
apollo::apollo_modelOutput(cv_intermediate_model)
apollo::apollo_modelOutput(cv_intermediate_attraction_model)

# Summaries
cv_intermediate_data[, intermediate_stop_type := factor(intermediate_stop_type, labels = c("No Int. Stop", "Driver Needs", "Vehicle Service", "Other Int. Stop"))]
cv_intermediate_data[, intermediate_stop := ifelse(intermediate_stop_type == "No Int. Stop", 0, 1)]

cv_intermediate_data[, veh_class_name := factor(veh_choice, labels = c("Light" ,"Medium" ,"Heavy"))]

# Industry
setnames(cv_intermediate_data, "model_emp_cat", "EmpCatName")
cv_intermediate_data[NAICS3_to_EmpCats, IndustryCat := i.EmpCatGroupedName, on = "EmpCatName"]
  
# Add the total trip distance
cv_intermediate_data[cv_intermediate_data[STOP_SEQ > 1, 
                                          .(tour_distance = sum(dist)), 
                                          by = .(SITEID, VEHNUM, TOUR_NUM)],
                    tour_distance := i.tour_distance,
                    on = .(SITEID, VEHNUM, TOUR_NUM)]

intermediate_stops <- cv_intermediate_data[, .(AllStops = .N, IntermediateStops = sum(intermediate_stop), FINAL_FACTOR_ALL = sum(FINAL_FACTOR), FINAL_FACTOR_INT = sum(FINAL_FACTOR * intermediate_stop)), 
                                     keyby = .(Vehicle = veh_class_name)][, c("PctStops", "Target") := .(IntermediateStops/(AllStops-IntermediateStops), FINAL_FACTOR_INT/(FINAL_FACTOR_ALL - FINAL_FACTOR_INT))]

intermediate_stops_type_veh <- cv_intermediate_data[,.(Stops = .N, FINAL_FACTOR = sum(FINAL_FACTOR)), 
                                                      keyby = .(intermediate_stop_type, Vehicle = veh_class_name)][, c("PctStops", "Target") := .(Stops/sum(Stops), FINAL_FACTOR/sum(FINAL_FACTOR)), keyby = Vehicle]

# Cross tab by vehicle
dcast.data.table(intermediate_stops_type_veh,
                 intermediate_stop_type ~ Vehicle,
                 fun.aggregate = sum,
                 value.var = "Target")

# Sample sizes (number of tours) by vehicle
dcast.data.table(intermediate_stops_type_veh,
                 intermediate_stop_type ~ Vehicle,
                 fun.aggregate = sum,
                 value.var = "Stops")

# Rate of refueling stops per 100 miles
intermediate_refueling <- cv_intermediate_data[, .(RefuelingStops = sum((intermediate_stop_type == "Vehicle Service")*1),
                                                   Distance = sum(dist),
                                                   FINAL_FACTOR_INT = sum(FINAL_FACTOR * (intermediate_stop_type == "Vehicle Service")),
                                                   FINAL_FACTOR_DIST = sum(FINAL_FACTOR * dist)),
                                           keyby = .(Vehicle = veh_class_name)][, c("StopsPer100", "Target") := .(RefuelingStops/(Distance/100), FINAL_FACTOR_INT/(FINAL_FACTOR_DIST/100))]

# Driver needs breaks per 8 hours
### Add some time per tour information

### Summaries of deviation: add some deviation information

# Add the targets to the list
model_step_targets_cv_sim[["cv_sim_intermediatestops"]] <- list(intermediate_stops = intermediate_stops,
                                                                intermediate_stops_type_veh = intermediate_stops_type_veh,
                                                                intermediate_refueling = intermediate_refueling)

### SAVE THE LIST OF TARGETS --------------------------------------------------------------------------------

# Save the model step targets, a list with tables of target results for each submodel
saveRDS(model_step_targets_cv_sim, 
        file = file.path(SYSTEM_DEV_CALIBRATION_PATH, 
                         "calibration_targets_cv_sim.RDS"))

# check 
model_step_targets_cv_sim <- readRDS(file = file.path(SYSTEM_DEV_CALIBRATION_PATH, "calibration_targets_cv_sim.RDS"))



