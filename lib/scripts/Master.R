
### Initialize Application -------------------------------------------------------------------

# Start the rFreight application
source(file.path("lib", "scripts", "init_start_rFreight_model.R"))

cat("Running the", SCENARIO_NAME, "scenario for", SCENARIO_YEAR, "\n")
SCENARIO_RUN_START   <- Sys.time()

### 1. Firm Synthesis ------------------------------------------------------------------------
 
if (SCENARIO_RUN_FIRMSYN) {
  
  cat("Starting Firm Synthesis Step", "\n")
  
  # Load executive functions (process inputs and simulation)
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_process_inputs.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim.R"))

  # Process inputs
  cat("Processing Firm Synthesis Inputs", "\n")
  firm_inputs <- new.env()
  cbp <- firm_sim_process_inputs(envir = firm_inputs)

  # Run simulation
  cat("Running Firm Synthesis Simulation", "\n")
  firm_sim_results <- suppressMessages(
    run_sim(
      FUN = firm_sim,
      data = cbp,
      packages = SYSTEM_PKGS,
      lib = SYSTEM_PKGS_PATH,
      inputEnv = firm_inputs
    )
  )
  
  # Save inputs and results
  cat("Saving Firm Synthesis Database", "\n")
  save(firm_sim_results, 
       firm_inputs, 
       file = file.path(SCENARIO_OUTPUT_PATH,
                        SYSTEM_FIRMSYN_OUTPUTNAME))
  
  rm(firm_sim_results, 
     firm_inputs,
     cbp)
  
  gc(verbose = FALSE)
  
}


### 2. Simulate Commercial Vehicle Movements  ------------------------------------------------------------------------

if (SCENARIO_RUN_CVTM) {

  cat("Starting Commercial Vehicle Touring Step", "\n")
  
  # Load executive functions (process inputs and simulation)
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim_process_inputs.R"))

  # Process inputs
  cat("Processing Commercial Vehicle Touring Inputs", "\n")
  cv_inputs <- new.env()
  ScenarioFirms <- cv_sim_process_inputs(envir = cv_inputs)
  
  # Run simuation
  cat("Running Commercial Vehicle Touring Simulation", "\n")
  cv_sim_results <- list()
  cv_sim_results <- suppressMessages(run_sim(FUN = cv_sim, data = ScenarioFirms,
                                                      k = USER_PROCESSOR_CORES, 
                                                      packages = SYSTEM_PKGS, 
                                                      lib = SYSTEM_PKGS_PATH,
                                                      inputEnv = cv_inputs))
  
  cv_sim_results$cv_trips[, TourID := as.integer(factor(paste(BusID, Vehicle, TourID)))]
  
  # Save inputs and results
  cat("Saving Commercial Vehicle Touring Database", "\n")
  save(cv_sim_results, 
       cv_inputs, 
       file = file.path(SCENARIO_OUTPUT_PATH, 
                        SYSTEM_CVTM_OUTPUTNAME))

  # Clean up workspace
  rm(cv_sim_results, 
     cv_inputs, 
     ScenarioFirms)
  
  gc(verbose = FALSE)
  

}

### 3. Produce Trip Tables -------------------------------------------------------------------------

if (SCENARIO_RUN_TT) {

  cat("Producing Commercial Vehicle Trip Tables", "\n")
  
  # Load executive functions
  source(file.path(SYSTEM_SCRIPTS_PATH, "tt_build.R"))
  source(file.path(SYSTEM_SCRIPTS_PATH, "tt_process_inputs.R"))

  # Process inputs
  cat("Processing Commercial Vehicle Trip Tables Inputs", "\n")
  tt_inputs <- new.env()
  cv_trips <- tt_process_inputs(envir = tt_inputs)

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
  rm(tt_list,
     tt_inputs)

  gc(verbose = FALSE)

}

# ### 4. Produce Dashboard -------------------------------------------------------------------------
# 
# SCENARIO_RUN_DURATION <- Sys.time() - SCENARIO_RUN_START
# 
# if (SCENARIO_RUN_DB) {
# 
#   # Load executive functions
#   source(file = file.path(SYSTEM_SCRIPTS_PATH, "db_build.R"))
#   
#   # source(file = file.path(SYSTEM_SCRIPTS_PATH, "db_process_inputs.R"))
#   # 
#   # # Process inputs
#   # db_inputs <- new.env()
#   # db_process_inputs(envir = db_inputs)
#   # 
#   
#   # Generate dashboard
#   DashboardRender(data.display = "both", scenarios = SCENARIO_NAME)
#   
#   # dashboardFileLoc <- suppressWarnings(suppressMessages(
#   #   run_sim(
#   #     FUN = db_build,
#   #     data = NULL,
#   #     packages = SYSTEM_REPORT_PKGS,
#   #     lib = SYSTEM_PKGS_PATH,
#   #     inputEnv = db_inputs
#   #   )
#   # ))
#   
# }
