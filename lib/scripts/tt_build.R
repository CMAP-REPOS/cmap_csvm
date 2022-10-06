# Converts the trip list to a trip table
tt_build <- function(cv_trips) {
  
  # Begin progress tracking
  progressStart(action = "Writing...", task = "Trip Tables", dir = SCENARIO_LOG_PATH, subtasks = FALSE)
  
  ### Aggregate cv_trips up to a trip table and write results as a set of OMX matrices
  cat("Aggregating Commercial Vehicle Tours to OD Trips", "\n")
  
  TripTable <- cv_trips[, .(trips = .N),
                       by = .(OTAZ, DTAZ, Vehicle, TOD)]
  
  setkeyv(TripTable, c("OTAZ", "DTAZ", "Vehicle", "TOD"))
  
  # Create a table of matrix names and types consistent with the CMAP model
  # Currently there are commercial vehicle trip tables for each combination of
  # light, medium, heavy trucks and the eight time periods plus daily totals, for 27 tables
  vehicletypes <- c("Light", "Medium", "Heavy")
  vehicletypes_lc <- c("l", "m", "h")
  todlabels <- names(BASE_TOD_RANGES)
  todlabels_lc <- tolower(todlabels)
  
  matnames <- paste0(rep(vehicletypes_lc, each = length(todlabels_lc)+1),"truck_",
              rep(c(todlabels_lc, "daily"), length(vehicletypes_lc)))
                   
  outputMatrices <- data.table(matname = matnames,
                               vehicletype = rep(vehicletypes, each = length(todlabels)+1),
                               tod = rep(c(todlabels, "all"), length(vehicletypes)))
  
  outputMatrices[, matrix_description := paste(SCENARIO_NAME, tod, vehicletype, "CV matrix")]
  
  # Write the CV trip tables to an OMX file in the scenario output folder
  cat("Write Commercial Vehicle Trips Tables to OMX File", "\n")
    
  tt_list <- write_triptables_to_omx(omxoutputpath = rep(file.path(SCENARIO_OUTPUT_PATH, "CV_Trip_Tables.omx"),
                                                nrow(outputMatrices)),
                            tazids = zone_number,
                            triptable = TripTable,
                            matnames = outputMatrices$matname,
                            matdescriptions = outputMatrices$matrix_description,
                            dim1name = "TOD",
                            dim2name = "Vehicle",
                            dim1 = outputMatrices$tod,
                            dim2 = outputMatrices$vehicletype)
  
  # Add the TripTable to the matrix list for saving
  tt_list[["TripTable"]] <- TripTable
  
  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)
  
  return(tt_list)
  
  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)
  
}




