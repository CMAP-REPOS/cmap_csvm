
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
                        cv_sim_externalstations  = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim_externalstations.R"),
                        cv_activities_model       = file.path(SYSTEM_DATA_PATH, "cv_activities_model.RDS"),
                        cv_goods_model            = file.path(SYSTEM_DATA_PATH, "cv_goods_model.RDS"),
                        cv_service_model          = file.path(SYSTEM_DATA_PATH, "cv_service_model.RDS"),
                        cv_vehicle_model          = file.path(SYSTEM_DATA_PATH, "cv_vehicle_model.RDS"),
                        cv_stopduration_model     = file.path(SYSTEM_DATA_PATH, "cv_stopduration_model.RDS"),
                        cv_tours_model            = file.path(SYSTEM_DATA_PATH, "cv_tours_model.RDS"),
                        cv_arrival_model          = file.path(SYSTEM_DATA_PATH, "cv_arrival_model.RDS"),
                        cv_intermediate_model     = file.path(SYSTEM_DATA_PATH, "cv_intermediate_model.RDS"),
                        cv_intermediate_attraction_model = file.path(SYSTEM_DATA_PATH, "cv_intermediate_model_attraction.RDS"),
                        intstop.deviations        = file.path(SYSTEM_DATA_PATH, "cv_intermediate_deviations.rds"),
                        settings                  = file.path(SYSTEM_DATA_PATH, "cv_settings.RData"),
                        skims_names               = file.path(SYSTEM_DATA_PATH, 'data_skim_names.csv'))
  
  loadInputs(files = project.files, envir = envir)
  
  ### Load inputs/outputs from earlier steps
  load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FIRMSYN_OUTPUTNAME))
  envir[["TAZLandUseCVTM"]] <- firm_sim_results[["TAZLandUseCVTM"]]
  ScenarioFirms <- firm_sim_results[["RegionFirms"]][!is.na(TAZ)]
  setnames(ScenarioFirms, "n2", "EmpCatName")

  ## Add calibrated parameters to environment individually
  envir[["cv_calibrated_parameters"]]
  for(i in 1:nrow(envir[["cv_calibrated_parameters"]])){
         assign(envir[["cv_calibrated_parameters"]]$Variable[i],
         envir[["cv_calibrated_parameters"]]$Value[i],
         envir = envir)
  }

  ### Load scenario input files

  ### TODO need to import correct skims for full zone17 zone system by time period here
  #scenario.files <- c(skims  = file.path(SCENARIO_INPUT_PATH, "cmap_data_zone_skims.csv"))

  #loadInputs(files = scenario.files, envir = envir)

  # ### Load skims

  # ### TEMP skim for testing (just averaged fields required for stops and vehicle choice)
  # skims_tod <- merge(data.table(OTAZ = BASE_TAZ_INTERNAL, k = 1),
  #                    data.table(DTAZ = BASE_TAZ_INTERNAL, k = 1),
  #                    by = "k",
  #                    allow.cartesian = TRUE)[, k := NULL]
  #
  # setkey(skims_tod, OTAZ, DTAZ)
  # skims_tod[, time.avg := runif(.N, min = 1, max = 80)]
  # skims_tod[, dist.avg := runif(.N, min = 1, max = 50)]
  # skims_tod[, toll.avg := 0]


  # Import skim matrices for time, distance, and tolls.
  # are tolls skims available?
  skims_names <- envir[["skims_names"]][condition == BASE_SKIM_CONDITION]
   
  if(BASE_TOLL_SKIM_AVAILABLE) {
    num_skims <- 3
    # skim_names <- c(SCENARIO_CV_SKIM_TIME, SCENARIO_CV_SKIM_DIST, SCENARIO_CV_SKIM_TOLL)
    variable_names <- c("time","dist","toll")
  } else {
    num_skims <- 2
    # skim_names <- c(SCENARIO_CV_SKIM_TIME, SCENARIO_CV_SKIM_DIST)
    variable_names <- c("time","dist")
  }

  # loop over the time periods and create a combined time, distance, and toll skims for each zone pair
  if(USER_PROCESSOR_CORES > 1){
    require(parallel)

    clust <- makeCluster(USER_PROCESSOR_CORES)

    clusterCall(clust,
                fun = function(packages, lib) lapply(X = as.list(packages), FUN = library, character.only = TRUE, lib.loc = lib),
                packages = c("rFreight", "rhdf5"), lib = SYSTEM_PKGS_PATH)

    clusterExport(clust, varlist = getGlobalVars(), envir = .GlobalEnv)

    clusterExport(clust,
                  c("num_skims", "skim_names", "variable_names"),
                  envir = environment())

    skims_int <- parLapply(clust, #here
                               1:length(BASE_TOD_RANGES),
                               function(x){
                                 skims <- read_skims_from_omx(omxinputpaths = skims_names[time_period == x]$file_path, #lists path of files with x time period
                                                              subsetvar = paste0('p',skims_names[time_period == x]$time_period),
                                                              matnames = skims_names[time_period ==x]$matrix_name,
                                                              variablenames = skims_names[time_period ==x]$skims_type,
                                                              row_lookup_name = "zone_number",
                                                              col_lookup_name = "zone_number") 
                                 # Create zero value toll tables
                                 if(!BASE_TOLL_SKIM_AVAILABLE) skims[[1]][, toll := 0]
                                 return(skims)
                               })
    stopCluster(clust)

  } else {

    skims_int <- lapply(1:length(BASE_TOD_RANGES),
                               function(x){
                                 skims <- read_skims_from_omx(omxinputpaths = skims_names[time_period == x]$file_path, #lists path of files with x time period
                                                              subsetvar = paste0('p',skims_names[time_period == x]$time_period),
                                                              matnames = skims_names[time_period ==x]$matrix_name,
                                                              variablenames = skims_names[time_period ==x]$skims_type,
                                                              row_lookup_name = "zone_number",
                                                              col_lookup_name = "zone_number") 
                                 # Create zero value toll tables
                                 if(!BASE_TOLL_SKIM_AVAILABLE) skims[[1]][, toll := 0]
                                 return(skims)
                               })

  }

  names(skims_int) <- names(BASE_TOD_RANGES)

  # Join all skim tables together, giving each time of day a weighting for calculating average skim values

  #ERROR HERE -RICKY 9/20/22
  skims_tod <- joinSkimTables(skims_int,
                              by = c("OTAZ", "DTAZ"),
                              tod.ranges = BASE_TOD_RANGES,
                              var.names = c("time", "dist", "toll"),
                              weights = rep(1,length(BASE_TOD_RANGES)))

  saveRDS(skims_tod,
          file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"),
          compress = FALSE)

  # Keep the portions of the skims_int_buf table that cross the external station boundary
  # for use in building SEMCOG specific trip tables

  skims_buffer <- rbindlist(lapply(1:length(skims_int_buf),
                                   function(x) skims_int_buf[[x]][SKIMTYPE %in% c("Buffer to SEMCOG", "SEMCOG to Buffer", "Buffer traversing SEMCOG")][, TOD := names(skims_int_buf)[x]]))

  saveRDS(skims_buffer,
          file.path(SCENARIO_OUTPUT_PATH, "skims_buffer.rds"),
          compress = FALSE)

  saveRDS(skims_tod, 
          file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"),
          compress = FALSE)
  
  gc()
  
  ### Return
  return(ScenarioFirms)
  
}
