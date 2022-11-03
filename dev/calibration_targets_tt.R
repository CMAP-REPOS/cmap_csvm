# Script to import calibration data and create targets for CVTM

# Use init_dev.R to run here instead of sourcing from _Master_Dev.R
# source("./dev/init_dev.R")

# support data
TAZ_System <- fread(file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"))

skims_tod <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"))

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

### DEVELOP LIST OF TARGET TABLES  ------------------------------------------------------------------------

model_step_targets_tt_sim <- list()

### 4. Produce Regional Trip Tables -------------------------------------------------------------------------

# Validation Tables suggested:

# Total trips by vehicle type
# Trips by OD Segment 
# Trips by OD Segment by vehicle type
# County/external (total) to county/external (total) trips by vehicle type
# Trips by external station group (state/province)

# Similar measures for VMT and VHT

# Trip length frequencies for time and distance by
# Overall
# vehicle type
# OD Segment
# OD segment by vehicle type

# Add the target to the list 
model_step_targets_tt_sim[["tt_build"]] <- list(countyod_all = countyod_all,
                                                countyod_light = countyod_light,
                                                countyod_medium = countyod_medium)

### SAVE THE LIST OF TARGETS --------------------------------------------------------------------------------

str(model_step_targets_tt_sim)

# Save the model step targets, a list with tables of target results for each submodel
saveRDS(model_step_targets_tt_sim, 
        file = file.path(SYSTEM_DEV_CALIBRATION_PATH, 
                         "calibration_targets_tt_sim.RDS"))

