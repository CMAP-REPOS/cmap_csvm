
# This function loads all necessary inputs into envir, after any needed transformations
cv_sim_process_inputs <- function(envir) {
  
  ### Load project input files
  project.files <- list(cv_sim_activities        = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim_activities.R"),
                        cv_sim_scheduledstops    = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim_scheduledstops.R"),
                        cv_sim_vehicle           = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim_vehicle.R"),
                        cv_sim_stopduration      = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim_stopduration.R"),
                        cv_sim_tours             = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim_tours.R"),
                        cv_sim_scheduledtrips    = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim_scheduledtrips.R"),
                        cv_sim_intermediatestops = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim_intermediatestops.R"),
                        cv_activities_model       = file.path(SYSTEM_DATA_PATH, "cv_activities_model.RDS"),
                        cv_goods_res_model        = file.path(SYSTEM_DATA_PATH, "cv_goods_res_model.RDS"),
                        cv_goods_non_res_model    = file.path(SYSTEM_DATA_PATH, "cv_goods_non_res_model.RDS"),
                        cv_service_res_model      = file.path(SYSTEM_DATA_PATH, "cv_service_res_model.RDS"),
                        cv_service_non_res_model  = file.path(SYSTEM_DATA_PATH, "cv_service_non_res_model.RDS"),
                        cv_vehicle_model          = file.path(SYSTEM_DATA_PATH, "cv_vehicle_model.RDS"),
                        cv_stopduration_model     = file.path(SYSTEM_DATA_PATH, "cv_stopduration_model.RDS"),
                        cv_tours_model            = file.path(SYSTEM_DATA_PATH, "cv_tours_model.RDS"),
                        cv_arrival_model          = file.path(SYSTEM_DATA_PATH, "cv_arrival_model.RDS"),
                        cv_intermediate_model     = file.path(SYSTEM_DATA_PATH, "cv_intermediate_model.RDS"),
                        cv_intermediate_attraction_model = file.path(SYSTEM_DATA_PATH, "cv_intermediate_attraction_model.RDS"),
                        intstop.deviations        = file.path(SYSTEM_DATA_PATH, "cv_intermediate_deviations.rds"),
                        settings                  = file.path(SYSTEM_DATA_PATH, "cv_settings.RData"),
                        skims_names               = file.path(SYSTEM_DATA_PATH, 'data_skim_names.csv'))
  
  loadInputs(files = project.files, envir = envir)
  
  ### Load inputs/outputs from earlier steps
  load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FIRMSYN_OUTPUTNAME))
  envir[["TAZLandUseCVTM"]] <- firm_sim_results[["TAZLandUseCVTM"]]
  ScenarioFirms <- firm_sim_results[["ScenarioFirms"]][!is.na(TAZ)]
  
  ### Load scenario input files
  
  scenario.files <- c(scenario_configuration = file.path(SCENARIO_INPUT_PATH, "scenario_adjustments.R"))
  loadInputs(files = scenario.files, envir = envir)

  ### Load skims
  
  # Run skim process if user process skims is TRUE or the skims file does not exist
  if(USER_PROCESS_SKIMS | !file.exists(file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"))){
  
    # Import skim matrices for time, distance, and tolls.
    skims_names <- envir[["skims_names"]][condition == BASE_SKIM_CONDITION]
    
    # loop over the three vehicle types
    for(vehicle in unique(skims_names$vehicle_type)){
    
    # loop over the time periods and create a combined time, distance, and toll skims for each zone pair
      if(USER_PROCESSOR_CORES > 1){
        require(parallel)
    
        clust <- makeCluster(USER_PROCESSOR_CORES)
    
        clusterCall(clust,
                    fun = function(packages, lib) lapply(X = as.list(packages), FUN = library, character.only = TRUE, lib.loc = lib),
                    packages = c("rFreight", "rhdf5"), lib = SYSTEM_PKGS_PATH)
    
        clusterExport(clust, varlist = getGlobalVars(), envir = .GlobalEnv)
    
        clusterExport(clust,
                      c("skims_names"),
                      envir = environment())
    
        skims_int <- parLapply(clust, 
                                   1:length(BASE_TOD_RANGES),
                                   function(x){
                                     skims <- read_skims_from_omx(omxinputpaths = file.path(SCENARIO_INPUT_PATH, skims_names[time_period == x & vehicle_type == vehicle]$file_name), #lists path of files with x time period
                                                                  subsetvar = paste0('P',skims_names[time_period == x & vehicle_type == vehicle]$time_period),
                                                                  matnames = skims_names[time_period == x & vehicle_type == vehicle]$matrix_name,
                                                                  variablenames = skims_names[time_period == x & vehicle_type == vehicle]$skims_type,
                                                                  row_lookup_name = "zone_number",
                                                                  col_lookup_name = "zone_number") 
                                     return(skims[[1]])
                                   })
        stopCluster(clust)
    
      } else {
    
        skims_int <- lapply(1:length(BASE_TOD_RANGES),
                                   function(x){
                                     skims <- read_skims_from_omx(omxinputpaths = file.path(SCENARIO_INPUT_PATH, skims_names[time_period == x & vehicle_type == vehicle]$file_name), #lists path of files with x time period
                                                                  subsetvar = paste0('P',skims_names[time_period == x & vehicle_type == vehicle]$time_period),
                                                                  matnames = skims_names[time_period == x & vehicle_type == vehicle]$matrix_name,
                                                                  variablenames = skims_names[time_period == x & vehicle_type == vehicle]$skims_type,
                                                                  row_lookup_name = "zone_number",
                                                                  col_lookup_name = "zone_number") 
                                     return(skims[[1]])
                                   })
    
      }
    
      names(skims_int) <- names(BASE_TOD_RANGES)
    
      # Join all skim tables together, giving each time of day a weighting for calculating average skim values
      assign(paste0("skims_tod_", vehicle), 
             joinSkimTables(skims_int,
             by = c("OTAZ", "DTAZ"),
             tod.ranges = BASE_TOD_RANGES,
             var.names = c("time", "dist", "toll"),
             weights = rep(1,length(BASE_TOD_RANGES))))
    
    }
  
    ### TODO update model code to be able to use vehicle type specific skims them remove this and 
    ###      save combined skims as skims_tod
    saveRDS(skims_tod_light,
            file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"),
            compress = FALSE)
    
    skims_tod <- rbindlist(list(light = skims_tod_light, 
                                medium = skims_tod_medium,
                                heavy = skims_tod_heavy),
                           idcol = "vehicle")
    
    saveRDS(skims_tod,
            file.path(SCENARIO_OUTPUT_PATH, "skims_tod_vehicle.rds"),
            compress = FALSE)
    
  } # end if proces skims
  
  gc()
  
  ### Return
  return(ScenarioFirms)
  
}
