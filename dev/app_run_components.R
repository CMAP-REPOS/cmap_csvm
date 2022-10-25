### Interactive script for debugging CMAP CSVM

### Set up R and create command arguments as if the model was called from batch ----------------

# Clear the environment
rm(list = ls())

# Define which scenario/year to run
# Base scenario, called in the style of batch file
SYSTEM_COMMAND_ARGS <- c("base", 2019)
# Alternative base using the freeflow skims (need to set skim condition variable in BASE_VARIABLES)
# SYSTEM_COMMAND_ARGS <- c("base_freeflow", 2019)

### Initialize Application -------------------------------------------------------------------

# Start the rFreight application
# source(file.path("lib", "scripts", "init_start_rFreight_model.R"))
# Content of init_start_rFreight_model.R:

# initialization script for rFreight applications

# Load global variables
source(file.path("lib", "scripts", "_SYSTEM_VARIABLES.R"))
source(file.path(SYSTEM_SCRIPTS_PATH, "_BASE_VARIABLES.R"))
source(file.path(SYSTEM_SCRIPTS_PATH, "_SCENARIO_VARIABLES.R"))
source(file.path(SYSTEM_SCRIPTS_PATH, "_USER_VARIABLES.R"))

# Install rFreight and any packages not available on CRAN
source(file.path(SYSTEM_SCRIPTS_PATH, "init_install_special_packages.R"))

# Load current rFreight installation
suppressWarnings(suppressMessages(library(rFreight,
                                          lib.loc = SYSTEM_PKGS_PATH)))

# Check for new rFreight version, load rFreight and other packages, create output folder
initializeApp(rFreight.path = SYSTEM_RFREIGHT_PATH,
              output.path = SCENARIO_OUTPUT_PATH,
              lib = SYSTEM_PKGS_PATH,
              packages = c(SYSTEM_PKGS),
              reload.rFreight = FALSE)

cat("Running the", SCENARIO_NAME, "scenario for", SCENARIO_YEAR, "\n")
SCENARIO_RUN_START   <- Sys.time()


### FIRM SYNTHESIS =============================================================

# if (SCENARIO_RUN_FIRMSYN) {
#   
#   cat("Starting Firm Synthesis Step", "\n")
#   
#   # Load executive functions (process inputs and simulation)
#   source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_process_inputs.R"))
#   source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim.R"))
#   
#   # Process inputs
#   cat("Processing Firm Synthesis Inputs", "\n")
#   firm_inputs <- new.env()
#   cbp <- firm_sim_process_inputs(envir = firm_inputs)
#   
#   # Run simulation
#   cat("Running Firm Synthesis Simulation", "\n")
#   firm_sim_results <- suppressMessages(
#     run_sim(
#       FUN = firm_sim,
#       data = cbp,
#       packages = SYSTEM_PKGS,
#       lib = SYSTEM_PKGS_PATH,
#       inputEnv = firm_inputs
#     )
#   )
#   
#   # Save inputs and results
#   cat("Saving Firm Synthesis Database", "\n")
#   save(firm_sim_results, 
#        firm_inputs, 
#        file = file.path(SCENARIO_OUTPUT_PATH,
#                         SYSTEM_FIRMSYN_OUTPUTNAME))
#   
#   rm(firm_sim_results, 
#      firm_inputs,
#      cbp)
#   
#   gc(verbose = FALSE)
#   
# }


# Load executive functions (process inputs and simulation)
source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_process_inputs.R"))
source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim.R"))

# Process inputs
firm_inputs <- new.env()
Establishments <- firm_sim_process_inputs(envir = firm_inputs)

# For scratch only, bring model component input environment variables into the
# global environment
for(n in ls(firm_inputs, all.names=TRUE)) assign(n, get(n, firm_inputs), environment())
cbp <- Establishments

# Begin progress tracking
progressStart(action = "Simulating...", task = "Firms", dir = SCENARIO_LOG_PATH, subtasks = FALSE)

# Different approach in base and future scenarios: base year start from start,
# future year build on base year scaled firm list
if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
  
  cat("Creating Base Year Establishment List", "\n")
  
  # Process and enumerate the CBP data
  progressUpdate(prop = 1/4, dir = SCENARIO_LOG_PATH)
  ScenarioFirms <- firm_synthesis_enumerate(cbp = cbp,
                                            EmpBounds = EmpBounds,
                                            emp_control_taz = emp_control_taz,
                                            cbp_ag = cbp_ag)
  
  # Allocate from counties to mesozones
  progressUpdate(prop = 2/4, dir = SCENARIO_LOG_PATH)
  ScenarioFirms <- firm_synthesis_mesozones(Firms = ScenarioFirms,
                                            mzemp = mzemp)
 
  # Scale the employment to TAZ controls
  progressUpdate(prop = 3/4, dir = SCENARIO_LOG_PATH)
  ScenarioFirms <- firm_synthesis_scaling(Firms = ScenarioFirms,
                                          emp_control_taz = emp_control_taz,
                                          TAZ_System = TAZ_System,
                                          EmpBounds = EmpBounds)
  
} else {
  
  # Future year/alternative scenario
  if(file.exists(SCENARIO_BASEFIRMS)){
    
    cat("Updating Base Year Establishment List with Future Control Data", "\n")
    
    # Load the output from the base year firm synthesis model
    progressUpdate(prop = 1/4, dir = SCENARIO_LOG_PATH)
    
    load(SCENARIO_BASEFIRMS)
    ScenarioFirms <- firm_sim_results$ScenarioFirms
    rm(firm_sim_results)
    
    # Scale the emplyoment
    progressUpdate(prop = 3/4, dir = SCENARIO_LOG_PATH)
    ScenarioFirms <- firm_synthesis_scaling(Firms = ScenarioFirms,
                                            emp_control_taz = emp_control_taz,
                                            TAZ_System = TAZ_System,
                                            EmpBounds = EmpBounds)
    
  } else {
    
    stop("No Base Scenario outputs available. Please run the Base Scenario first.")
    
  }
}

# Add employment classifications
progressUpdate(prop = 4/4, dir = SCENARIO_LOG_PATH)
cat("Adding Employment Group Variables", "\n")

ScenarioFirms[UEmpCats[, .(n2 = EmpCatName, EmpCatGroupedName)], 
            EmpCatGroupedName := i.EmpCatGroupedName,
            on = "n2"]

# End progress tracking
progressEnd(dir = SCENARIO_LOG_PATH)

# Return results
firm_sim_results <- list(ScenarioFirms = ScenarioFirms, 
            TAZLandUseCVTM = TAZLandUseCVTM)

# End progress tracking
progressEnd(dir = SCENARIO_LOG_PATH)

# Save inputs and results
save(firm_sim_results, firm_inputs, file = file.path(SCENARIO_OUTPUT_PATH,
                                                     SYSTEM_FIRMSYN_OUTPUTNAME))
rm(list = names(firm_inputs)) # For scratch only, remove variables that were added from model component environment
rm(firm_sim_results, firm_inputs, Establishments)
gc(verbose = FALSE)

### Commercial Vehicle Touring Model ===========================================
# Load executive functions (process inputs and simulation)
source(file = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim.R"))
source(file = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim_process_inputs.R"))

# Set the skim processing to false to use existing skims (will still run if they are not present)
USER_PROCESS_SKIMS <- FALSE 

# Process inputs
cv_inputs <- new.env()
ScenarioFirms <- cv_sim_process_inputs(envir = cv_inputs)
firms <- ScenarioFirms

# For scratch only, bring model component input environment variables into the
# global environment
for(n in ls(cv_inputs, all.names=TRUE)) assign(n, get(n, cv_inputs), environment())

# Begin progress tracking
progressStart(action = "Simulating...", task = "Commercial Vehicle Movements", dir = SCENARIO_LOG_PATH)

# Read skims from .rds files
skims_tod <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"))

# Run simuation
# Simulate firm activities
firmActivities <- cv_sim_activities(firms = firms, 
                                    cv_activities_model = cv_activities_model)
gc()

# Simulate scheduled stops
firmStops <- cv_sim_scheduledstops(firmActivities = firmActivities,
                                   skims = skims_tod[, .(OTAZ, DTAZ, time = time.avg, dist = dist.avg, toll = toll.avg)],
                                   firms = firms,
                                   numZones = numZones,
                                   d_bars = d_bars,
                                   hurdle_support = hurdle_support,
                                   TAZLandUseCVTM = TAZLandUseCVTM,
                                   cv_goods_model = cv_goods_model,
                                   cv_service_model = cv_service_model)
gc()

# Simulate vehicle choice
firmStopsVeh <- cv_sim_vehicle(database = firmStops, 
                               firms = firms,
                               skims = skims_tod[, .(OTAZ, DTAZ, dist = dist.avg)],
                               model = cv_vehicle_model)
gc()
###
#will this model allow for heavy vehicles? Given there were none in the GPS data?
#-RICKY 9/19/22
#modify code for no heavy vehicles via the mode specific constants - Colin 9/20/2022

###

# Simulate stop duration
firmStopsVehDur <- cv_sim_stopduration(database = firmStopsVeh, 
                                       model = cv_stopduration_model,
                                       firms = firms)
gc()

# Simulate tours and routing
firmTourSequence <- cv_sim_tours(firmStopsVehDur = firmStopsVehDur,
                                 firms = firms,
                                 branch.limit = branch.limit,
                                 skims = skims_tod[, .(OTAZ, DTAZ, time = time.avg, dist = dist.avg, toll = toll.avg)],
                                 model = cv_tours_model)
gc()

# Simulate scheduled trips
scheduledTrips <- cv_sim_scheduledtrips(firmTourSequence = firmTourSequence,
                                        firms = firms,
                                        skims_tod = skims_tod,
                                        model = cv_arrival_model)
gc()

# Simulate intermediate stops
allTrips <- cv_sim_intermediatestops(database = scheduledTrips,
                                     firms = firms,
                                     skims_tod = skims_tod,
                                     model = cv_intermediate_model,
                                     cv_intermediate_attraction_model = cv_intermediate_attraction_model,
                                     cv_stopduration_model = cv_stopduration_model,
                                     deviance.threshold = deviance.threshold,
                                     intstop.deviations = intstop.deviations,
                                     TAZLandUseCVTM = TAZLandUseCVTM)
gc()

cv_sim_results <- list(cv_trips = allTrips)
cv_sim_results$cv_trips[, TourID := as.integer(factor(paste(BusID, Vehicle, TourID)))]

# End progress tracking
progressEnd(dir = SCENARIO_LOG_PATH)

# Save inputs and results
save(cv_sim_results, cv_inputs, file = file.path(SCENARIO_OUTPUT_PATH, 
                                                 SYSTEM_CVTM_OUTPUTNAME))


rm(list = names(cv_inputs)) # For scratch only, remove variables that were added from model component environment
rm(cv_sim_results, cv_inputs)
gc(verbose = FALSE)


### TRIP TABLES ================================================================

cat("Producing Commercial Vehicle Trip Tables", "\n")

# Load executive functions
source(file.path(SYSTEM_SCRIPTS_PATH, "tt_build.R"))
source(file.path(SYSTEM_SCRIPTS_PATH, "tt_build_process_inputs.R"))

# Process inputs
cat("Processing Commercial Vehicle Trip Tables Inputs", "\n")
tt_inputs <- new.env()
cv_trips <- tt_build_process_inputs(envir = tt_inputs)

# For scratch only, bring model component input environment variables into the
# global environment
for(n in ls(tt_inputs, all.names=TRUE)) assign(n, get(n, tt_inputs), environment())

# Create trip tables
cat("Writing Commercial Vehicle Trip Tables to OMX Files", "\n")
tt_list <- suppressMessages(
  run_sim(
    FUN = tt_build,
    data = cv_trips,
    packages = SYSTEM_PKGS,
    lib = SYSTEM_PKGS_PATH,
    inputEnv = tt_inputs
  )
)

# Save inputs and results
cat("Saving Commercial Vehicle Trip Tables Database", "\n")
save(tt_list,
     file = file.path(SCENARIO_OUTPUT_PATH,
                      SYSTEM_TT_OUTPUTNAME))

# Clean up workspace
rm(list = names(tt_inputs)) # For scratch only, remove variables that were added from model component environment
rm(cv_trips, 
   tt_list,
   tt_inputs)
gc(verbose = FALSE)


# load(file = file.path(SCENARIO_OUTPUT_PATH,
#                       SYSTEM_TT_OUTPUTNAME))
# 
# names(tt_list)
# sum(tt_list$TripTable$trips)
# listOMX("./scenarios/base/outputs/CV_Trip_Tables.omx")

# ### DASHBOARD ==================================================================

### Run dashboard
cat("Producing Commercial Vehicle Dashboard", "\n")

# Load executive functions
source(file = file.path(SYSTEM_SCRIPTS_PATH, "db_build.R"))
source(file = file.path(SYSTEM_SCRIPTS_PATH, "db_build_process_inputs.R"))

# Process inputs
cat("Processing Commercial Vehicle Dashboard Inputs", "\n")
db_inputs <- new.env()
db_build_process_inputs(envir = db_inputs)

# For scratch only, bring model component input environment variables into the
# global environment
for(n in ls(db_inputs, all.names=TRUE)) assign(n, get(n, db_inputs), environment())

# Generate dashboard and spreadsheet
cat("Rendering Commercial Vehicle Dashboard and Spreadsheet", "\n")
# dashboardFileLoc <- suppressWarnings(suppressMessages(
#   run_sim(FUN = db_build, data = NULL,
#           packages = SYSTEM_PKGS, lib = SYSTEM_PKGS_PATH,
#           inputEnv = db_inputs
#   )
# ))


# Pandoc location inside the model
rmarkdown::find_pandoc(dir = SYSTEM_PANDOC_PATH)

# Render the dashboard into HTML
#db_build_render()

dashboardName <- "ReportDashboard.html"

# Generate dashboard
rmarkdown::render(file.path(SYSTEM_SCRIPTS_PATH, "db_markdown", 
                            "ReportDashboard.Rmd"),
                  output_dir = SCENARIO_OUTPUT_PATH,
                  output_file = dashboardName,
                  quiet = TRUE)

# The following lines change the rendering engine from SVG to Canvas, which
# speeds up map rendering considerably.
dashboardHTML <- readLines(file.path(SCENARIO_OUTPUT_PATH, dashboardName))
idx <- which(dashboardHTML == "window.FlexDashboardComponents = [];")[1]
dashboardHTML <- append(dashboardHTML, "L_PREFER_CANVAS = true;", after = idx)
writeLines(dashboardHTML, file.path(SCENARIO_OUTPUT_PATH, dashboardName))

# Create summary spreadsheet using model outputs including assignment flow tables
if(SCENARIO_DB_SPREADSHEET & USER_SPREADSHEET_SUMMARIES){
  db_build_spreadsheet()
}

dashboardFileLoc <- file.path(SCENARIO_OUTPUT_PATH, "ReportDashboard.html")

# Save results to Rdata file
cat("Saving Dashboard Tabulations Database", "\n")
save(db_inputs, 
     file = file.path(SCENARIO_OUTPUT_PATH, 
                      SYSTEM_DB_OUTPUTNAME)) 
rm(list = names(db_inputs)) # For scratch only, remove variables that were added from model component environment

