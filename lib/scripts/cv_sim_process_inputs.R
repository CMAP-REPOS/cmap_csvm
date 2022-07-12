
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
                        settings                  = file.path(SYSTEM_DATA_PATH, "cv_settings.RData"))
  
  loadInputs(files = project.files, envir = envir)
  
  ### Load inputs/outputs from earlier steps
  load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FIRMSYN_OUTPUTNAME))
  envir[["TAZLandUseCVTM"]] <- firm_sim_results[["TAZLandUseCVTM"]]
  ScenarioFirms <- firm_sim_results[["RegionFirms"]][!is.na(TAZ)]

  ## Add calibrated parameters to environment individually
  envir[["cv_calibrated_parameters"]]
  for(i in 1:nrow(envir[["cv_calibrated_parameters"]])){
         assign(envir[["cv_calibrated_parameters"]]$Variable[i],
         envir[["cv_calibrated_parameters"]]$Value[i],
         envir = envir)
  }
  
  ### Load scenario input files
  
  # ### Load skims
  # # Import skim matrices for time, distance, and tolls.
  # # are tolls skims available?
  # if(BASE_TOLL_SKIM_AVAILABLE) {
  #   num_skims <- 3
  #   skim_names <- c(SCENARIO_CV_SKIM_TIME, SCENARIO_CV_SKIM_DIST, SCENARIO_CV_SKIM_TOLL)
  #   variable_names <- c("time","dist","toll")
  # } else {
  #   num_skims <- 2
  #   skim_names <- c(SCENARIO_CV_SKIM_TIME, SCENARIO_CV_SKIM_DIST)
  #   variable_names <- c("time","dist")
  # }
  #   
  # # read the buffer skim from OMX: daily skims
  # skims_buf <- read_skims_from_omx(omxinputpaths = rep(file.path(SYSTEM_DATA_PATH, "TravelTimeSkimBuffer.omx"), num_skims),
  #                                  subsetvar = rep("Daily", num_skims),
  #                                  matnames = variable_names,
  #                                  variablenames = variable_names,
  #                                  row_lookup_name = "zone_number",
  #                                  col_lookup_name = "zone_number")
  # 
  # if(!BASE_TOLL_SKIM_AVAILABLE) lapply(skims_buf, set, j = "toll", value = 0)
  # 
  # # Apply TAZ penalities if any specified
  # # data.frame with TAZ and Penalty
  # if(exists("BASE_TAZ_PENALTY")){
  #   for(i in 1:nrow(BASE_TAZ_PENALTY)){
  #     skims_buf$Daily[(OTAZ == BASE_TAZ_PENALTY$TAZ[i] | DTAZ == BASE_TAZ_PENALTY$TAZ[i]) & time > 0, 
  #                     time := time + BASE_TAZ_PENALTY$Penalty[i]]    
  #   }
  # }
  # 
  # 
  # # Define function to create complete skim:
  # # 1. SEMCOG internal to internal from skims
  # # 2. Buffer internal to internal from skims_buf for zone pairs that are allowed
  # # 3. Buffer to SEMCOG shortest path from buffer to external and external to semcog TAZ
  # # 4. SEMCOG to buffer shortest path from semcog to external and external to buffer TAZ
  # # 5. buffer to SEMCOG to buffer for zone pairs that require traverse: buffer to external, external to external, external to buffer
  # combine_internal_buffer_skim <- function(int, buf, cv_externals){
  #   
  #   # require data.table in case the function is called using parLapply
  #   require(data.table, lib.loc = SYSTEM_PKGS_PATH)
  #   
  #   # tag the skims with the TAZ type for both OTAZ and DTAZ
  #   c_taz_type <- data.table(TAZ = c(BASE_TAZ_MODEL_REGION, BASE_TAZ_EXTERNAL, BASE_TAZ_BUFFER),
  #                            TAZ_TYPE = c(rep("SEMCOG", length(BASE_TAZ_MODEL_REGION)),
  #                                         rep("EXTERNAL", length(BASE_TAZ_EXTERNAL)),
  #                                         rep("BUFFER", length(BASE_TAZ_BUFFER))))
  #   
  #   int[c_taz_type[,.(OTAZ = TAZ, TAZ_TYPE)], OTYPE := i.TAZ_TYPE, on = "OTAZ"]
  #   int[c_taz_type[,.(DTAZ = TAZ, TAZ_TYPE)], DTYPE := i.TAZ_TYPE, on = "DTAZ"]
  #   buf[c_taz_type[,.(OTAZ = TAZ, TAZ_TYPE)], OTYPE := i.TAZ_TYPE, on = "OTAZ"]
  #   buf[c_taz_type[,.(DTAZ = TAZ, TAZ_TYPE)], DTYPE := i.TAZ_TYPE, on = "DTAZ"]
  #   
  #   # remove any rows that are not to or from a buffer TAZ and any rows that are to or from SEMCOG TAZs
  #   buf <- buf[(OTYPE == "BUFFER" | DTYPE == "BUFFER") & OTYPE != "SEMCOG" & DTYPE != "SEMCOG"]
  #   
  #   # remove any rows that use an external that is not active in this scenario
  #   inactive_external <- cv_externals[ScenarioActive == FALSE]$TAZ
  #   int <- int[!OTAZ %in% inactive_external & !DTAZ %in% inactive_external]
  #   buf <- buf[!OTAZ %in% inactive_external & !DTAZ %in% inactive_external]
  #   
  #   # buffer zone pairs that don't require a traverse of SEMCOG (this is #2)
  #   buf_no_tr <- buf[!is.na(time) & OTYPE == "BUFFER" & DTYPE == "BUFFER"]
  #   # buffer zone pairs that require a traverse of SEMCOG and need shortest paths developing
  #   buf_tr <- buf[is.na(time) & OTYPE == "BUFFER" & DTYPE == "BUFFER"]
  #   # buffer to external/external to buffer pairs that are allowable (no traverse required to reach the external)
  #   buf_ext <- buf[!is.na(time) & OTYPE == "BUFFER" & DTYPE == "EXTERNAL"]
  #   ext_buf <- buf[!is.na(time) & OTYPE == "EXTERNAL" & DTYPE == "BUFFER"]
  #   # semcog to external/external to semcog pairs
  #   int_ext <- int[OTYPE == "SEMCOG" & DTYPE == "EXTERNAL"]
  #   ext_int <- int[OTYPE == "EXTERNAL" & DTYPE == "SEMCOG"]
  #   # semcog external to external (remove any identical pairs)
  #   ext_ext <- int[OTYPE == "EXTERNAL" & DTYPE == "EXTERNAL" & OTAZ != DTAZ]
  #   
  #   # Develop #3 Buffer to SEMCOG shortest path from buffer to external and external to semcog TAZ
  #   # shortest paths based on travel time, and then get the distances and tolls for those paths
  #   # create all allowed combinations of buffer to external and external to SEMCOG
  #   buf_ext_int <- merge(buf_ext[,.(OTAZ, EXT = DTAZ, OTYPE, dist.buf = dist, time.buf = time, toll.buf = toll)],
  #                        ext_int[,.(EXT = OTAZ, DTAZ, DTYPE, dist.int = dist, time.int = time, toll.int = toll)],
  #                        by = "EXT",
  #                        allow.cartesian = TRUE)
  #   buf_ext_int[, time := time.buf + time.int]
  #   buf_ext_int <- buf_ext_int[buf_ext_int[, .I[which.min(time)], by = .(OTAZ, DTAZ)]$V1]
  #   buf_ext_int[, dist := dist.buf + dist.int]
  #   buf_ext_int[, toll := toll.buf + toll.int]
  #   
  #   # Develop #4 SEMCOG to buffer shortest path from semcog to external and external to buffer TAZ
  #   # shortest paths based on travel time, and then get the distances and tolls for those paths
  #   # create all allowed combinations of semcog to external and external to buffer TAZ
  #   int_ext_buf <- merge(int_ext[,.(OTAZ, EXT = DTAZ, OTYPE, dist.int = dist, time.int = time, toll.int = toll)],
  #                        ext_buf[,.(EXT = OTAZ, DTAZ, DTYPE, dist.buf = dist, time.buf = time, toll.buf = toll)],
  #                        by = "EXT",
  #                        allow.cartesian = TRUE)
  #   int_ext_buf[, time := time.buf + time.int]
  #   int_ext_buf <- int_ext_buf[int_ext_buf[, .I[which.min(time)], by = .(OTAZ, DTAZ)]$V1]
  #   int_ext_buf[, dist := dist.buf + dist.int]
  #   int_ext_buf[, toll := toll.buf + toll.int]
  #   
  #   # Develop #5 buffer to SEMCOG to buffer for zone pairs that require traverse: 
  #   # buffer to external, external to external, external to buffer
  #   # select the buf to ext records for each OTAZ in the require traverse
  #   buf_ext_tr <- buf_ext[OTAZ %in% unique(buf_tr$OTAZ)]
  #   ext_buf_tr <- ext_buf[DTAZ %in% unique(buf_tr$DTAZ)]
  #   
  #   # create the buffer TAZ to external entry to SEMCOG to external exit from SEMCOG legs and find shortest paths for each 
  #   # origin TAZ to external exit route
  #   buf_ext_ext <- merge(buf_ext_tr[,.(OTAZ, EXT1 = DTAZ, OTYPE, dist.buf1 = dist, time.buf1 = time, toll.buf1 = toll)],
  #                        ext_ext[,.(EXT1 = OTAZ, EXT2 = DTAZ, dist.int = dist, time.int = time, toll.int = toll)],
  #                        by = "EXT1",
  #                        allow.cartesian = TRUE)
  #   buf_ext_ext[, time := time.buf1 + time.int]
  #   buf_ext_ext <- buf_ext_ext[buf_ext_ext[, .I[which.min(time)], by = .(OTAZ, EXT2)]$V1]
  #   
  #   # add on the leg from the external exit from SEMCOG to the destination buffer TAZ
  #   buf_ext_ext_buf <- merge(buf_ext_ext,
  #                            ext_buf_tr[,.(EXT2 = OTAZ, DTAZ, DTYPE, dist.buf2 = dist, time.buf2 = time, toll.buf2 = toll)],
  #                            by = "EXT2",
  #                            allow.cartesian = TRUE)
  #   buf_ext_ext_buf[, time := time + time.buf2]
  #   buf_ext_ext_buf <- buf_ext_ext_buf[buf_ext_ext_buf[, .I[which.min(time)], by = .(OTAZ, DTAZ)]$V1]
  #   
  #   # filter any zone pairs that are not required (i.e., they do not require a traverse of SEMCOG) 
  #   buf_ext_ext_buf <- buf_ext_ext_buf[buf_tr[,.(OTAZ, DTAZ)], on = c("OTAZ", "DTAZ")]
  #   
  #   # calculate toll and distance
  #   buf_ext_ext_buf[, dist := dist.buf1 + dist.int + dist.buf2]
  #   buf_ext_ext_buf[, toll := toll.buf1 + toll.int + toll.buf2]
  #   
  #   # assemble the complete list of TAZ pairs with the dist, times, and tolls
  #   int_buf <- rbind(int[, SKIMTYPE := "SEMCOG"],
  #                    buf_no_tr[, SKIMTYPE := "Buffer no traverse"],
  #                    buf_ext_int[, SKIMTYPE := "Buffer to SEMCOG"],
  #                    int_ext_buf[, SKIMTYPE := "SEMCOG to Buffer"],
  #                    buf_ext_ext_buf[, SKIMTYPE := "Buffer traversing SEMCOG"],
  #                    use.names = TRUE,
  #                    fill = TRUE)
  #   
  #   return(int_buf)
  #   
  # }
  # 
  # # loop over the time periods and create a combined time, distance, and toll skims for each zone pair
  # if(USER_PROCESSOR_CORES > 1){
  #   require(parallel)
  #   
  #   clust <- makeCluster(USER_PROCESSOR_CORES)
  #   
  #   clusterCall(clust, 
  #               fun = function(packages, lib) lapply(X = as.list(packages), FUN = library, character.only = TRUE, lib.loc = lib),
  #               packages = c("rFreight", "rhdf5"), lib = SYSTEM_PKGS_PATH)
  #   
  #   clusterExport(clust, varlist = getGlobalVars(), envir = .GlobalEnv)
  #   
  #   clusterExport(clust, 
  #                 c("combine_internal_buffer_skim", 
  #                   "num_skims", "skim_names", "variable_names",
  #                   "skims_buf", "cv_externals"), 
  #                 envir = environment())
  #  
  #   skims_int_buf <- parLapply(clust, 
  #                              1:length(BASE_TOD_RANGES), 
  #                              function(x){
  #                                skims <- read_skims_from_omx(omxinputpaths = rep(SCENARIO_CV_SKIM_PATHS[x], num_skims),
  #                                                             subsetvar = rep(names(BASE_TOD_RANGES)[x], num_skims),
  #                                                             matnames = skim_names,
  #                                                             variablenames = variable_names,
  #                                                             row_lookup_name = "ZoneID",
  #                                                             col_lookup_name = "ZoneID")
  #                                # Create zero value toll tables
  #                                if(!BASE_TOLL_SKIM_AVAILABLE) skims[[1]][, toll := 0]
  #                                combine_internal_buffer_skim(skims[[1]], skims_buf[["Daily"]], cv_externals)
  #                              })
  #   stopCluster(clust)
  #   
  # } else {
  #   
  #   skims_int_buf <- lapply(1:length(BASE_TOD_RANGES), 
  #                              function(x){
  #                                skims <- read_skims_from_omx(omxinputpaths = rep(SCENARIO_CV_SKIM_PATHS[x], num_skims),
  #                                                             subsetvar = rep(names(BASE_TOD_RANGES)[x], num_skims),
  #                                                             matnames = skim_names,
  #                                                             variablenames = variable_names,
  #                                                             row_lookup_name = "ZoneID",
  #                                                             col_lookup_name = "ZoneID")
  #                                # Create zero value toll tables
  #                                if(!BASE_TOLL_SKIM_AVAILABLE) skims[[1]][, toll := 0]
  #                                combine_internal_buffer_skim(skims[[1]], skims_buf[["Daily"]], cv_externals)
  #                              })
  #   
  # }
  # 
  # names(skims_int_buf) <- names(BASE_TOD_RANGES)
  # 
  # # Join all skim tables together, giving each time of day a weighting for calculating average skim values
  # 
  # skims_tod <- joinSkimTables(skims_int_buf,
  #                             by = c("OTAZ", "DTAZ"),
  #                             tod.ranges = BASE_TOD_RANGES,
  #                             var.names = c("time", "dist", "toll"),
  #                             weights = rep(1,length(BASE_TOD_RANGES)))
  # 
  # saveRDS(skims_tod, 
  #         file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"),
  #         compress = FALSE)
  # 
  # # Keep the portions of the skims_int_buf table that cross the external station boundary 
  # # for use in building SEMCOG specific trip tables
  # 
  # skims_buffer <- rbindlist(lapply(1:length(skims_int_buf), 
  #                                  function(x) skims_int_buf[[x]][SKIMTYPE %in% c("Buffer to SEMCOG", "SEMCOG to Buffer", "Buffer traversing SEMCOG")][, TOD := names(skims_int_buf)[x]]))
  # 
  # saveRDS(skims_buffer, 
  #         file.path(SCENARIO_OUTPUT_PATH, "skims_buffer.rds"),
  #         compress = FALSE)
  # 
  gc()
  
  ### Return
  return(ScenarioFirms)
  
}
