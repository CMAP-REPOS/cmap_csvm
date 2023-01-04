# Script to import calibration data and create targets for CVTM

# Use init_dev.R to run here instead of sourcing from _Master_Dev.R
source("./dev/init_dev.R")

# support data
TAZ_System <- fread(file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"))

skims_tod <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"))

NAICS3_to_EmpCats <- fread(file.path(SYSTEM_DEV_DATA_PATH, "SEMCOG_Data", "NAICS3_SEMCOGEmpCats_CMAP.csv"))

# Tables derived from GPS data
countyod_all <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                            "CVGPS",
                            "Calibration Targets",
                            "CountyOD.csv"))

countyod_light <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                  "CVGPS",
                                  "Calibration Targets",
                                  "CountyOD_light.csv"))

countyod_medium <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                  "CVGPS",
                                  "Calibration Targets",
                                  "CountyOD_medium.csv"))

districtod_all <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                "CVGPS",
                                "Calibration Targets",
                                "DistrictOD.csv"))

districtod_light <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                  "CVGPS",
                                  "Calibration Targets",
                                  "DistrictOD_light.csv"))

districtod_medium <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                   "CVGPS",
                                   "Calibration Targets",
                                   "DistrictOD_medium.csv"))

# Trip length distributions
tripdistancedist <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                    "CVGPS",
                                    "Calibration Targets",
                                    "TripDistanceDistribution.csv"))

tripdistancedist_district <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                    "CVGPS",
                                    "Calibration Targets",
                                    "TripDistanceDistribution_tripOriginDistrict.csv"))

tripdistancedist_basedistrict <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                             "CVGPS",
                                             "Calibration Targets",
                                             "TripDistanceDistribution_baseDistrict.csv"))

# Base to stop distribution overall from GPS data
basestopdist_district <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                         "CVGPS",
                                         "Calibration Targets",
                                         "BaseStopDist_district_CVGPS.csv"))


# Tour Distance overall from GPS
tourdistance <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                         "CVGPS",
                                         "Calibration Targets",
                                         "TotalTourDistance.csv"))

# Tour Number of Stops
tournumstopsdistance <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                "CVGPS",
                                "Calibration Targets",
                                "Tour_NStops_CVGPS.csv"))

# Tour First Stop Arrival
tourfirststoparrival <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                        "CVGPS",
                                        "Calibration Targets",
                                        "TourFirstArrival_CVGPS.csv"))

# Trip Time of Day
tripstarttimedist <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                        "CVGPS",
                                        "Calibration Targets",
                                        "TripDepartHour_CVGPS.csv"))

tripstarttimedist_basedistrict <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                     "CVGPS",
                                     "Calibration Targets",
                                     "TripDepartHour_baseDistrict_CVGPS.csv"))

tripstarttimedist_district <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                                  "CVGPS",
                                                  "Calibration Targets",
                                                  "TripDepartHour_tripOriginDistrict_CVGPS.csv"))

# Stop Duration

duration_stop_dist <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                      "CVGPS",
                                      "Calibration Targets",
                                      "duration_stops_CVGPS.csv"))

duration_stop_mean <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                      "CVGPS",
                                      "Calibration Targets",
                                      "duration_stops_Mean_CVGPS.csv"))

# Clustering stop spread

tour_cluster <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                "CVGPS",
                                "Calibration Targets",
                                "TourODMatrixDistAvg_CVGPS.csv"))

# Summary table
gps_summary_table <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                     "CVGPS",
                                     "Calibration Targets",
                                     "dashboard_table.csv"))


gps_tour_repzones_visits <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                            "CVGPS",
                                            "Calibration Targets",
                                            "repeatZones_nVisits_CVGPS.csv"))

gps_tour_repzones_unique <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                            "CVGPS",
                                            "Calibration Targets",
                                            "repeatZones_UniqueZones_CVPGS.csv"))

# VMT Targets

semcog_industry_veh_od <- fread(file.path(SYSTEM_DEV_DATA_PATH,
                                          "SEMCOG_Data",
                                          "data_proc_cvs_trips_vmt_ind_veh_ie.csv"))


### DEVELOP LIST OF TARGET TABLES  ------------------------------------------------------------------------

model_step_targets_tt_sim <- list()

### 4. Produce Regional Trip Tables -------------------------------------------------------------------------

# Validation Tables suggested:

# Total trips by vehicle type
# Region area to region area (e.g., Chicago, Cook (Not Chicago), Rest of CMAP, Rest of Region) by vehicle type
# County to county trips by vehicle type
# Similar measures for VMT and VHT

# Trip length frequencies for time and distance by
# Overall
# vehicle type
# Region (e.g., Chicago, Cook (Not Chicago), Rest of CMAP, Rest of Region)


# VMT Targets
# Need targets by vehicle type based on the FHWA data 
# discounted for transport industry sector and without IX/XI/XX

semcog_industry_veh_od[NAICS3_to_EmpCats,
                       EmpCatGroupedName := i.CMAPGroup,
                       on = "EmpCatName"]

semcog_industry_veh_od <- semcog_industry_veh_od[,.(NumberTrips = sum(NumberTrips),
                                                    ExpandedTrips = sum(ExpandedTrips),
                                                    VMT = round(sum(VMT))), 
                                                 keyby = .(EmpCatGroupedName, VehClassLMH, ODGroup)]

# Calculate total by vehicle for transport_Industry vs other industries and internal vs external
semcog_industry_veh_od[, Industry_Included := ifelse(EmpCatGroupedName == "Transport_Industry", "Exclude", "Include")]

semcog_indinc_intext <- semcog_industry_veh_od[,.(NumberTrips = sum(NumberTrips),
                                                  ExpandedTrips = sum(ExpandedTrips),
                                                  VMT = round(sum(VMT))), 
                                               keyby = .(VehClassLMH, ODGroup,Industry_Included)]

semcog_indinc_intext[, PctVMT := VMT/sum(VMT), by = VehClassLMH]
semcog_indinc_intext[Industry_Included == "Include" & ODGroup == "Internal Trip"]
### TODO check the definition of the external trips...are too high a proportion of trips account for as external?

# Estimates of VMT by class based on FHWA data
vmt_est_fhwa <- data.table(Vehicle = c("Light", "Medium", "Heavy"),
                           TotalVMT = c(20912265,	 9195910, 12597821))

vmt_est_fhwa[semcog_indinc_intext[Industry_Included == "Include" & ODGroup == "Internal Trip", .(Vehicle = VehClassLMH, PctVMT)],
             VMTFactor := i.PctVMT, on = "Vehicle"] 

vmt_est_fhwa[, CSV_Int_VMT := TotalVMT * VMTFactor]

# Add the target to the list 
model_step_targets_tt_sim[["tt_build"]] <- list(countyod_all = countyod_all,
                                                countyod_light = countyod_light,
                                                countyod_medium = countyod_medium,
                                                districtod_all = districtod_all,
                                                districtod_light = districtod_light,
                                                districtod_medium = districtod_medium,
                                                tripdistancedist = tripdistancedist,
                                                tripdistancedist_district = tripdistancedist_district,
                                                tripdistancedist_basedistrict = tripdistancedist_basedistrict,
                                                basestopdist_district = basestopdist_district,
                                                tourdistance = tourdistance,
                                                tournumstopsdistance = tournumstopsdistance,
                                                tourfirststoparrival = tourfirststoparrival,
                                                tripstarttimedist = tripstarttimedist,
                                                tripstarttimedist_district = tripstarttimedist_district,
                                                tripstarttimedist_basedistrict = tripstarttimedist_basedistrict,
                                                duration_stop_dist = duration_stop_dist,
                                                duration_stop_mean = duration_stop_mean,
                                                tour_cluster = tour_cluster,
                                                gps_summary_table = gps_summary_table,
                                                gps_tour_repzones_visits = gps_tour_repzones_visits,
                                                gps_tour_repzones_unique = gps_tour_repzones_unique,
                                                vmt_est_fhwa = vmt_est_fhwa)

### SAVE THE LIST OF TARGETS --------------------------------------------------------------------------------

str(model_step_targets_tt_sim)

# Save the model step targets, a list with tables of target results for each submodel
saveRDS(model_step_targets_tt_sim, 
        file = file.path(SYSTEM_DEV_CALIBRATION_PATH, 
                         "calibration_targets_tt_sim.RDS"))

# copy file into the outputs folder for direct use in the dashboard summaries of validation results
saveRDS(model_step_targets_tt_sim, 
        file = file.path(SYSTEM_DEV_CALIBRATION_PATH, "outputs",
                         "calibration_targets_tt_sim.RDS"))
