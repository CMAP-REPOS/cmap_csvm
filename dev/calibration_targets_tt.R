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

# Estimates of VMT by class based on FHWA data
# Read in from the FHWA spreadsheets and process here to create targets

# From FHWA HS 2017 (use 2017 numbers for SEMCOG) and 2019 (use 2019 numbers for CMAP)
# https://www.fhwa.dot.gov/policyinformation/statistics/2017/hm71.cfm
hm71_2017 <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "VMT", "FHWA_VMT_Data.xlsx"),
                  sheet = "UA VMT 2017", startRow = 15))
vm2_2017 <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "VMT", "FHWA_VMT_Data.xlsx"),
                            sheet = "Annual VMT 2017", startRow = 15))
vm4urban_2017 <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "VMT", "FHWA_VMT_Data.xlsx"),
                                 sheet = "Pct Veh Urban 2017", startRow = 15))

hm71_2019 <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "VMT", "FHWA_VMT_Data.xlsx"),
                                  sheet = "UA VMT 2019", startRow = 15))
vm2_2019 <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "VMT", "FHWA_VMT_Data.xlsx"),
                                 sheet = "Annual VMT 2019", startRow = 15))
vm4urban_2019 <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "VMT", "FHWA_VMT_Data.xlsx"),
                                      sheet = "Pct Veh Urban 2019", startRow = 15))

# UA definition 
ua <- fread(file.path(SYSTEM_DEV_DATA_PATH, "VMT", "ua_county_rel_10.txt"))
ua[, CountyFIPS := STATE * 1000 + COUNTY]

# semcog model report to estimate share of commercial vehicles as proportion of all passenger
# (model calibrated to combination of Urban area VMT and Expanded SEMCOG CVS data)

semcog_vmt_model <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "VMT", "FHWA_VMT_Data.xlsx"),
                                         sheet = "SEMCOG", rows = 58:70))

# Process for the CMAP/Chicago region
# Records representing Chicago IL-IN urban area
chicago <- ua[UANAME == "Chicago, IL--IN Urbanized Area"]

# Rest of the model region
cmap_fips <- unique(TAZ_System$CountyFIPS)
cmap <- ua[CountyFIPS %in% cmap_fips]
cmap[, CHICAGO := ifelse(UANAME == "Chicago, IL--IN Urbanized Area", "In_UA", "Outside_UA")]
cmap_summary <- cmap[,.(POPPT = sum(POPPT), HUPT = sum(HUPT)), keyby = .(CHICAGO) ][, POP_Pct := POPPT/sum(POPPT)][]

chicago_vmt <- hm71_2019[UrbanizedArea == "Chicago, IL--IN"]$Dvmt_Total * 1000
cmap_summary[, DVMT := ifelse(CHICAGO =="In_UA", 
                              chicago_vmt, 
                              chicago_vmt/cmap_summary[CHICAGO == "In_UA"]$POP_Pct * POP_Pct)]

fwrite(cmap_summary, file.path(SYSTEM_DEV_DATA_PATH, "VMT", "cmap_summary.csv"))

# Bench mark against SEMCOG 
detroit <- ua[UANAME == "Detroit, MI Urbanized Area"]
semcog_fips <- unique(detroit[CNAME != "Lapeer County"]$CountyFIPS)
semcog <- ua[CountyFIPS %in% semcog_fips]
semcog[, DETROIT := ifelse(UANAME == "Detroit, MI Urbanized Area", "In_UA", "Outside_UA")]
semcog_summary <- semcog[,.(POPPT = sum(POPPT), HUPT = sum(HUPT)), keyby = .(DETROIT) ][, POP_Pct := POPPT/sum(POPPT)][]

# A tiny part of the UA is in Lapeer County, outside the SEMCOG model area
# but it is negligible (71 people) so ignore
detroit[CNAME == "Lapeer County"]

detroit_vmt <- hm71_2017[UrbanizedArea == "Detroit, MI"]$Dvmt_Total * 1000
semcog_summary[, DVMT := ifelse(DETROIT =="In_UA", 
                                detroit_vmt, 
                                detroit_vmt/semcog_summary[DETROIT == "In_UA"]$POP_Pct * POP_Pct)]

fwrite(semcog_summary, file.path(SYSTEM_DEV_DATA_PATH, "VMT", "semcog_summary.csv"))

# Allocation factors for VMT to vehicle types
# Calculate Annual VMT by Interstate, Other Arterials, and Other roads
vm2 <- rbind(vm2_2017[State %in% c("Michigan"),.(State, Urban_Interstate, Urban_OtherFreeways, Urban_OtherPrincipalArterials,
                                                  Urban_MinorArterial, Urban_MajorCollector, Urban_MinorCollector,         
                                                  Urban_Local, Urban_Total)],
             vm2_2019[State %in% c("Illinois"),.(State, Urban_Interstate, Urban_OtherFreeways, Urban_OtherPrincipalArterials,
                                                             Urban_MinorArterial, Urban_MajorCollector, Urban_MinorCollector,         
                                                             Urban_Local, Urban_Total)])

vm2[, Urban_OtherArterials := Urban_OtherFreeways + Urban_OtherPrincipalArterials + Urban_MinorArterial]
vm2[, Urban_Other := Urban_MajorCollector + Urban_MinorCollector + Urban_Local]
vm2 <- melt.data.table(vm2,
                       id.vars = "State",
                       variable.name = "Area_Road",
                       value.name = "Avmt")
vm2[, c("Area", "Road") := tstrsplit(Area_Road, split = "_", fixed = TRUE)]

# Allocate VMT by vehicle class
vm4urban <- rbind(vm4urban_2017[State %in% c("Michigan")],
                  vm4urban_2019[State %in% c("Illinois")])

vm4urban <- melt.data.table(vm4urban,
                            id.vars = "State",
                            variable.name = "Road_Vehicle",
                            value.name = "PctVmt")
vm4urban[, PctVmt := PctVmt/100]
vm4urban[, c("Road", "Vehicle") := tstrsplit(Road_Vehicle, split = "_", fixed = TRUE)]
vm4urban[vm2, Avmt := i.Avmt, on = c("State", "Road")]
vm4urban[, Avmt_Road_Vehicle := Avmt * PctVmt]
vm4urban[Vehicle != "Total", 
         Pct_Avmt_Road_Vehicle := Avmt_Road_Vehicle/sum(Avmt_Road_Vehicle), 
         by = .(Road, State)]

# Apply rates to the Urban Area VMT to estimate VMT by vehicle class by Roads class in UAs
hm71 <- rbind(hm71_2017[UrbanizedArea %in% c("Detroit, MI")],
              hm71_2019[UrbanizedArea %in% c("Chicago, IL--IN")])

hm71 <- hm71[UrbanizedArea %in% c("Chicago, IL--IN", "Detroit, MI"),
             .(UrbanizedArea, 
               Dvmt_Interstate, 
               Dvmt_OtherFreeways, Dvmt_OtherPrincipalArterials, Dvmt_MinorArterial, 
               Dvmt_MajorCollector, Dvmt_MinorCollector, Dvmt_Local, Dvmt_Total)]

hm71[, Dvmt_OtherArterials := Dvmt_OtherFreeways + Dvmt_OtherPrincipalArterials + Dvmt_MinorArterial]
hm71[, Dvmt_Other := Dvmt_MajorCollector + Dvmt_MinorCollector + Dvmt_Local]
hm71 <- melt.data.table(hm71,
                       id.vars = "UrbanizedArea",
                       variable.name = "Measure_Road",
                       value.name = "Dvmt")
hm71[, c("Measure", "Road") := tstrsplit(Measure_Road, split = "_", fixed = TRUE)]
hm71[, State := ifelse(UrbanizedArea == "Chicago, IL--IN", "Illinois", "Michigan")]

vm4urban[hm71, c("Dvmt", "UrbanizedArea") := .(i.Dvmt, i.UrbanizedArea), on = c("State", "Road")]
vm4urban[, Dvmt_Road_Vehicle := Dvmt * Pct_Avmt_Road_Vehicle]
fwrite(vm4urban, file.path(SYSTEM_DEV_DATA_PATH, "VMT", "vm4urban.csv"))

# Sum by urbanized area by vehicle type
vmt_vehicle <- vm4urban[Vehicle != "Total",.(Dvmt_Vehicle = sum(Dvmt_Road_Vehicle)), by = .(State, UrbanizedArea, Vehicle)]
vmt_vehicle[, Pct_Dvmt_Vehicle := Dvmt_Vehicle/sum(Dvmt_Vehicle), by = State]

# Scale the Dvmt to the large model region 
vmt_vehicle[, ModelRegion := ifelse(UrbanizedArea == "Chicago, IL--IN", "CMAP", "SEMCOG")]
cmap_summary <- add_totals(cmap_summary, rowtotal = FALSE)
cmap_dvmt_ua_ratio <- cmap_summary[CHICAGO == "Total"]$DVMT/cmap_summary[CHICAGO == "In_UA"]$DVMT
semcog_summary <- add_totals(semcog_summary, rowtotal = FALSE)
semcog_dvmt_ua_ratio <- semcog_summary[DETROIT == "Total"]$DVMT/semcog_summary[DETROIT == "In_UA"]$DVMT

vmt_vehicle[, UA_Ratio := ifelse(ModelRegion == "CMAP", cmap_dvmt_ua_ratio, semcog_dvmt_ua_ratio)]
vmt_vehicle[, Dvmt_Vehicle_ModelRegion := Dvmt_Vehicle * UA_Ratio]

# Calculate factors for splitting the light vehicle into passenger and light commercial
# And for converting an average day to an average weekday

semcog_vmt_model[, Passenger := SOV + HOV2 + HOV3]
semcog_vmt_model[, LightVehicle := Passenger + LightTruck]
pct_light_com = semcog_vmt_model[FunctionalClass == "Total"]$LightTruck/semcog_vmt_model[FunctionalClass == "Total"]$LightVehicle
pct_all_com = semcog_vmt_model[FunctionalClass == "Total"]$LightTruck/semcog_vmt_model[FunctionalClass == "Total"]$Total

daily_weekday_factor = semcog_vmt_model[FunctionalClass == "Total"]$Total/semcog_summary[DETROIT == "Total"]$DVMT

vmt_vehicle[ , VehicleLMH := ifelse(Vehicle == "CombinationTrucks", "Heavy",
                                    ifelse(Vehicle == "SingleUnitTrucks", "Medium", "Light"))]

# Factor DVMT to weekday
vmt_vehicle[, Dvmt_Vehicle_ModelRegion_Weekday := Dvmt_Vehicle_ModelRegion * daily_weekday_factor]
fwrite(vmt_vehicle, file.path(SYSTEM_DEV_DATA_PATH, "VMT", "vmt_vehicle.csv"))

# Split light vehicle vmt by passenger and commercial 
vmt_vehiclelmh <- vmt_vehicle[, .(Dvmt_Weekday = sum(Dvmt_Vehicle_ModelRegion_Weekday)),
                              by = .(VehicleLMH, State, UrbanizedArea, ModelRegion)]
vmt_vehiclelmh[, Passenger_LC := ifelse(VehicleLMH == "Light", Dvmt_Weekday * (1-pct_light_com), 0)]
vmt_vehiclelmh[, Commercial_LC := ifelse(VehicleLMH == "Light", Dvmt_Weekday * pct_light_com, Dvmt_Weekday)]

vmt_vehiclelmh[, Dvmt_Total := sum(Dvmt_Weekday), by = ModelRegion]
vmt_vehiclelmh[, Commercial_AC := ifelse(VehicleLMH == "Light", Dvmt_Total * pct_all_com, Dvmt_Weekday)]
vmt_vehiclelmh[, Dvmt_Truck := sum(Commercial_AC), by = ModelRegion]
vmt_vehiclelmh[, Passenger_AC := ifelse(VehicleLMH == "Light", Dvmt_Total - Dvmt_Truck, 0)]

# Average across the two methods and convert to miles
vmt_vehiclelmh[, Passenger := (Passenger_LC + Passenger_AC)/2 * 1000]
vmt_vehiclelmh[, Commercial := (Commercial_LC + Commercial_AC)/2 * 1000]
fwrite(vmt_vehiclelmh, file.path(SYSTEM_DEV_DATA_PATH, "VMT", "vmt_vehiclelmh.csv"))

# SUmmarize for truck for CMAP
vmt_est_fhwa <- vmt_vehiclelmh[ModelRegion == "CMAP",
                               .(Vehicle = VehicleLMH,
                                 TotalVMT = Commercial)]

vmt_est_fhwa[semcog_indinc_intext[Industry_Included == "Include" & ODGroup == "Internal Trip", .(Vehicle = VehClassLMH, PctVMT)],
             VMTFactor := i.PctVMT, on = "Vehicle"] 

vmt_est_fhwa[, CSV_Int_VMT := TotalVMT * VMTFactor]
fwrite(vmt_est_fhwa, file.path(SYSTEM_DEV_DATA_PATH, "VMT", "vmt_est_fhwa.csv"))

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
