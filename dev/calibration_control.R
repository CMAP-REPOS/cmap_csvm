# Calibraton Control Script

# support and component specific functions
source("./dev/calibration_functions.R")

# Flow:
# 1. Define overall settings for calibration (any global variables)
# 2. Define flow of models
# 3. Define calls for each model
# 4. Define targets for each model
# 5. Define parameters to adjust for each model
# 6. Define approach to adjusting parameter to converge on targets
# 7. Define criteria for judging calibration and stopping adjustment
# 8. Definte tabular and graphical visualization of calibration (iterations and final)
# 9. Save calibrated settings
# 10. Rerun to confirm calibration

### 1. Settings
CALIBRATION_MAX_ITER <- 5 # Maximum number of iterations
CALIBRATION_RESET_TO_ESTIMATED <- TRUE # whether to copy over original estimated models to reset the model
CALIBRATION_RUN <- "Calibration_Run_1"
if(dir.exists(file.path("./dev/Calibration", CALIBRATION_RUN))){
  stop("Calibration Folder Exists. Update the CALIBRATION_RUN name")
} else {
  CALIBRATION_RUN_FOLDER <- file.path("./dev/Calibration", CALIBRATION_RUN)
  dir.create(CALIBRATION_RUN_FOLDER)
}

### 2. Define flow of models
# what are we going to calibrate?
# create a list of model components to calibrate
# use the main function names as components titles
models <- list(firm_sim = list(firm_sim_taz_land_use = list(require_calibration = TRUE,
                                                            submodel_results_name = "TAZLandUseCVTM",
                                                            last_output_step = NULL,
                                                            estimated_models = NULL),
                               firm_sim_scale_employees = list(require_calibration = TRUE,
                                                               submodel_results_name = "ScenarioFirms",
                                                               last_output_step = NULL,
                                                               estimated_models = NULL)),
               cv_sim = list(cv_sim_activities = list(require_calibration = TRUE,
                                                      submodel_results_name = "firmActivities",
                                                      last_output_step = NULL,
                                                      estimated_models = NULL),
                             cv_sim_scheduledstops = list(require_calibration = FALSE,
                                                          submodel_results_name = "firmStops",
                                                          last_output_step = "cv_sim_activities",
                                                          estimated_models = NULL),
                             cv_sim_vehicle = list(require_calibration = FALSE,
                                                   submodel_results_name = "firmStopsVeh",
                                                   last_output_step = "cv_sim_scheduledstops",
                                                   estimated_models = NULL),
                             cv_sim_stopduration = list(require_calibration = TRUE,
                                                        submodel_results_name = "firmStopsVehDur",
                                                        last_output_step = "cv_sim_vehicle",
                                                        estimated_models = list(cv_stopduration_model = "cv_stopduration_model")),
                             cv_sim_tours = list(require_calibration = FALSE,
                                                 submodel_results_name = "firmTourSequence",
                                                 last_output_step = "cv_sim_stopduration",
                                                 estimated_models = NULL),
                             cv_sim_scheduledtrips = list(require_calibration = FALSE,
                                                          submodel_results_name = "scheduledTrips",
                                                          last_output_step = "cv_sim_tours",
                                                          estimated_models = NULL),
                             cv_sim_intermediatestops = list(require_calibration = FALSE,
                                                             submodel_results_name = "allTrips",
                                                             last_output_step = "cv_sim_scheduledtrips",
                                                             estimated_models = NULL))) 

### 3. Run the model individual step by step cycling through the main model components and their submodels
# steps required:
# 1. initialize model system
# 2. start the main model step
# 3. if past the first submodel, load the saved results from the previous submodel
# 4. run the submodel
### This is where the testing is done compare model results with targets, make adjustments, rerun
### until convergence is achieved, and the update coefficients etc are save
# 5. save the results after the submodel
# 6. loop back to 3 until the main model step done
# 7. rerun the complete main model step start to finish in normal way
# 8. loop back to 2 until all the main model steps done

# Initialize model system
source("./dev/init_dev.R")

# Rest the model objects
if(CALIBRATION_RESET_TO_ESTIMATED){
  
  paths_to_models <- file.path("./dev/Estimation", c("cv_activities/cv_activities_model.RDS",
                                                   "cv_stops/new_models/goods/cv_goods_model.RDS",
                                                   "cv_stops/new_models/services/cv_service_model.RDS",
                                                   "cv_vehicle/final_model/cv_vehicle_model.rds",
                                                   "cv_duration/cv_stopduration_model.rds",
                                                   "cv_tours/cv_tours_model.rds",
                                                   # "cv_arrival/cv_arrival_model.rds",
                                                   # "cv_intermediate/cv_intermediate_model.rds",
                                                   # "cv_intermediate/cv_intermediate_deviations.rds",
                                                   "cv_intermediate/cv_intermediate_model_attraction.rds"))

  file.copy(from = paths_to_models,
            to = "./lib/data",
            overwrite = TRUE)

}
  
# Set the run model to calibration so built in calibration features are activated
USER_RUN_MODE <- "Calibration"

# Set the skim processing to false to use existing skims (will still run if they are not present)
USER_PROCESS_SKIMS <- FALSE 

# Start the main model step
# loop over main model steps
for(model_step_num in 1:length(models)){

  #model_step_num <- 1
  model_step_name <- names(models)[model_step_num]
  model_step_submodels <- names(models[[model_step_name]])
  
  print(paste("Starting calibration of main model step:", model_step_name))
  
  # create environment, source and run the process inputs script, and add the model step function
  model_step_inputs <- process_inputs(model_step = model_step_name)
  
  # check to see if any submodels in mainmodel are getting calibrated and grab targets if true
  if (sum(unlist(lapply(models[[model_step_name]], "[[", "require_calibration")) == TRUE) > 0) {
    # Load the model step target, a list with tables of target results for each submodel
    model_step_target <- readRDS(paste0("./dev/Calibration/calibration_targets_", model_step_name, ".RDS"))
  }
  
  # Run the main model step's submodels sequentially
  # Use the repeated process: load outputs from previous step, run step, save results
  # Loop over the submodels
  
  for(submodel_num in 1:length(model_step_submodels)){
    
    #submodel_num <- 1
    # Name to use for submodel files
    submodel_name <- model_step_submodels[submodel_num]  
    
    print(paste("  Starting calibration of submodel:", submodel_name))
    
    # Load the outputs (submodel_calibrated_list) from the last submodel
    last_output <- list()
    if(!is.null(models[[model_step_num]][[submodel_name]]$last_output_step)){
      last_output_step <- models[[model_step_num]][[submodel_name]]$last_output_step
      for(i in 1:length(models[[model_step_num]][[submodel_name]]$last_output_step)){
        last_output[[last_output_step[i]]] <- readRDS(file.path(CALIBRATION_RUN_FOLDER, 
                                                                paste0(last_output_step[i], 
                                                                       ".RDS")))
      }
    } else {
      last_output[[1]] <- list()
      last_output[[1]]$submodel_results <- NULL
    }
    
    # Start the calibration routine for this submodel
    submodel_calibrated <- FALSE
    submodel_iter <- 1
    submodel_results_name <- models[[model_step_num]][[submodel_name]]$submodel_results_name
    submodel_calibrated_list = list()
    
    while(!submodel_calibrated & submodel_iter <= CALIBRATION_MAX_ITER){
    
      print(paste("    Iteration:", submodel_iter))
      
      # Run the submodel  
      submodel_results <- 
        run_submodel(
          model_step_name,
          model_step_submodels,
          submodel_name, 
          submodel_results_name,
          model_step_inputs,
          last_output)
      
      if (models[[model_step_name]][[submodel_name]][["require_calibration"]]) {
    
        # Test the results against targets and adjust parameters if needed
        submodel_calibrated_list <- do.call(paste0("calibrate_", submodel_name),
                                            args = list(submodel_calibrated = submodel_calibrated, 
                                                        submodel_results = submodel_results, 
                                                        model_step_target = model_step_target[[submodel_name]]))
        
        submodel_calibrated <- submodel_calibrated_list$submodel_calibrated
        
        if(!submodel_calibrated) {
          # move to the next iteration
          submodel_iter <- submodel_iter + 1
          
          # update the parameters returned from the function in the input environment
          if(!is.null(models[[model_step_num]][[submodel_name]]$estimated_models)) {
            for(est_mod_num in 1:length(models[[model_step_num]][[submodel_name]]$estimated_models)){
              model_step_inputs$model_step_env[[models[[model_step_num]][[submodel_name]]$estimated_models[[est_mod_num]]]] = 
                submodel_calibrated_list$submodel_parameters[[est_mod_num]]
            }
          }
        }  
      } else { 
        # not a component that required iterative adjustment
        submodel_calibrated <- TRUE
      }
    }
    
    # save the calibrated submodel results and calibration comparisons
    submodel_calibrated_list$submodel_results <- submodel_results
    submodel_calibrated_list$submodel_results_name <- submodel_results_name
    saveRDS(submodel_calibrated_list, file = file.path(CALIBRATION_RUN_FOLDER,paste0(submodel_name, ".RDS")))
    
    # save the updated submodel parameters
    if(!is.null(models[[model_step_num]][[submodel_name]]$estimated_models)) {
      
      # save the calibration model objects to their respective files
      ### TODO update if objects need to be something other than an RDS file
      lapply(1:length(models[[model_step_num]][[submodel_name]]$estimated_models),
             function(x) saveRDS(object = submodel_calibrated_list$submodel_parameters[[x]],
                                 file = file.path(SYSTEM_DATA_PATH,
                                                  paste0(models[[model_step_num]][[submodel_name]]$estimated_models[[x]],".RDS"))))
      
      print(paste("  Calibration complete and parameters saved for", submodel_name))
      
    } else {
      
      print(paste("  Calibration complete for", submodel_name))
      
    }
  
  } # end loop on submodels
  
  print(paste("Completed calibration of main model step:", model_step_name))

  # Re-run the main model step from scratch in application with the updated parameters
  model_step_rfreightcall <- run_model_step(model_step_name)
  print(paste("Application test for main model step complete using call:", model_step_rfreightcall))

} # end loop on main model step

# Run the remaining model steps: trip table and dashboard
model_step_rfreightcall <- run_model_step("tt_build")
print(paste("Application test complete using call:", model_step_rfreightcall))

model_step_rfreightcall <- run_model_step("db_build")
print(paste("Application test complete using call:", model_step_rfreightcall))
