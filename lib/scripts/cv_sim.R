
# Master function for executing the commercial vehicle model.
cv_sim <- function(firms) {
  
  # Begin progress tracking
  progressStart(action = "Simulating...", task = "Commercial Vehicle Movements", dir = SCENARIO_LOG_PATH)
  
  # Define run_steps if it is not already in the environment (default to running all steps)
  if(!exists("run_step")) run_step <- rep(TRUE, 8)
  
  # Read skims from .rds file
  skims_tod <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"))
  
  if(run_step[1]){
    
    # Simulate firm activities
    cat("Simulating Firm's Commercial Vehicle Activities", "\n")
    firmActivities <- cv_sim_activities(firms = firms, 
                                        cv_activities_model = cv_activities_model)
    gc()
  }
  
  if(run_step[2]){

    # Simulate scheduled stops
    cat("Simulating Commercial Vehicle Scheduled Stops", "\n")
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
  }

  if(run_step[3]){

    # Simulate vehicle choice
    cat("Simulating Commercial Vehicle Choice", "\n")
    firmStopsVeh <- cv_sim_vehicle(database = firmStops,
                                   model = cv_vehicle_model,
                                   firms = firms,
                                   skims = skims_tod[, .(OTAZ, DTAZ, dist = dist.avg)])
    gc()
  }

  if(run_step[4]){

    # Simulate stop duration
    cat("Simulating Commercial Vehicle Stop Duration", "\n")
    firmStopsVehDur <- cv_sim_stopduration(database = firmStopsVeh,
                                           model = cv_stopduration_model,
                                           firms = firms)
    gc()
  }

  # if(run_step[5]){
  #   
  #   # Simulate tours and routing
  #   cat("Simulating Commercial Vehicle Tour Type and Routing", "\n")
  #   firmTourSequence <- cv_sim_tours(firmStopsVehDur = firmStopsVehDur,
  #                                    firms = firms,
  #                                    branch.limit = branch.limit,
  #                                    skims = skims_tod[, .(OTAZ, DTAZ, time = time.avg, dist = dist.avg, toll = toll.avg)],
  #                                    model = cv_tours_model)
  #   gc()
  # }
  # 
  # if(run_step[6]){
  #   
  #   # Simulate scheduled trips
  #   cat("Simulating Commercial Vehicle Trip Scheduling", "\n")
  #   scheduledTrips <- cv_sim_scheduledtrips(firmTourSequence = firmTourSequence,
  #                                           firms = firms,
  #                                           skims_tod = skims_tod,
  #                                           model = cv_arrival_model)
  #   gc()
  # }
  # 
  # if(run_step[7]){
  #   
  #   # Simulate intermediate stops
  #   cat("Simulating Commercial Vehicle Non-Scheduled Stops", "\n")
  #   allTrips <- cv_sim_intermediatestops(database = scheduledTrips,
  #                                        firms = firms,
  #                                        skims_tod = skims_tod,
  #                                        model = cv_intermediate_model,
  #                                        cv_intermediate_attraction_model = cv_intermediate_attraction_model,
  #                                        cv_stopduration_model = cv_stopduration_model,
  #                                        deviance.threshold = deviance.threshold,
  #                                        intstop.deviations = intstop.deviations,
  #                                        TAZLandUseCVTM = TAZLandUseCVTM)
  #   gc()
  # }
  # 
  # if(run_step[8]){
  #   
  #   # Add external station information
  #   cat("Adding Commercial Vehicle Trip External Stations", "\n")
  #   
  #   # This step requires the buffer-external-internal skims.
  #   # for memory management, remove the regular skims first.
  #   rm(skims_tod)
  #   skims_buffer <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "skims_buffer.rds"))
  #   
  #   allTrips <- cv_sim_externalstations(cv_trips = allTrips,
  #                                       skims_buffer = skims_buffer)
  #   gc()
  # }
  
  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)

  if(USER_RUN_MODE == "Calibration"){
    return(get(submodel_results_name))
  } else {
    
    ###TEMP just return results through active steps with the table from each step returned
    
    return(list(#cv_trips = allTrips
      firmActivities = firmActivities,
      firmStops = firmStops,
      firmStopsVeh = firmStopsVeh,
      firmStopsVehDur = firmStopsVehDur))
  }
}
