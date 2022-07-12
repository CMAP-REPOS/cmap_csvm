
# Adding external station IDs to the trip list and times, distances, and tolls for model region and buffer portions of trips
cv_sim_externalstations <- function(cv_trips, skims_buffer){
  
  progressUpdate(subtaskprogress = 0, subtask = "External Stations", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  ### Associate a time period with each trip
  
  # Compute a trip time mid-point to determine time of day period
  # For trips that do not cross midnight
  cv_trips[MAMDepart <= MAMArrive, MAMMid := (MAMArrive - MAMDepart)/2 + MAMDepart]
  # For trips that cross midnight
  cv_trips[MAMDepart > MAMArrive, MAMMid := (as.numeric(MAMArrive) + 1440 - as.numeric(MAMDepart))/2 + MAMDepart]
  
  # Time of day period
  # where to cut trip
  cutfield <- c("MAMDepart", "MAMMid", "MAMArrive")[match(BASE_TIME_PERIOD_TRIP_POINT, c("START", "MIDDLE", "END"))]
  
  # use rFreight::getSkiTOD function to allocated to TOD, and convert character to factor
  cv_trips[, TOD := factor(getSkimTOD(get(cutfield), 
                               BASE_TOD_RANGES,
                               FALSE),
                           levels = names(BASE_TOD_RANGES))]
  
  ### Add the external stations and elements of the trip times and distances used to the trip list
  
  # Merge wtih skims buffer
  cv_trips[skims_buffer,
           c("OTYPE", "DTYPE", "SKIMTYPE", "EXT", "EXT1", "EXT2", "dist.int", "time.int", "dist.buf", "time.buf", "dist.buf1", "time.buf1", "dist.buf2", "time.buf2") := 
             .(i.OTYPE, i.DTYPE, i.SKIMTYPE, i.EXT, i.EXT1, i.EXT2, i.dist.int, i.time.int, i.dist.buf, i.time.buf, i.dist.buf1, i.time.buf1, i.dist.buf2, i.time.buf2),
           on = c("OTAZ", "DTAZ", "TOD")]
  
  # Update the fields for any within model region (non-buffer) trips
  cv_trips[is.na(OTYPE) & OTAZ %in% BASE_TAZ_MODEL_REGION, OTYPE := "SEMCOG"]
  cv_trips[is.na(DTYPE) & DTAZ %in% BASE_TAZ_MODEL_REGION, DTYPE := "SEMCOG"]
  cv_trips[is.na(SKIMTYPE) & OTYPE == "SEMCOG" & DTYPE == "SEMCOG", SKIMTYPE := "Within SEMCOG"]
  cv_trips[is.na(dist.int) & SKIMTYPE == "Within SEMCOG", dist.int := Distance]
  cv_trips[is.na(time.int) & SKIMTYPE == "Within SEMCOG", time.int := TravelTime]
  
  # Update the fields for any within buffer (no traverse) trips
  cv_trips[is.na(OTYPE) & OTAZ %in% BASE_TAZ_BUFFER, OTYPE := "BUFFER"]
  cv_trips[is.na(DTYPE) & DTAZ %in% BASE_TAZ_BUFFER, DTYPE := "BUFFER"]
  cv_trips[is.na(SKIMTYPE) & OTYPE == "BUFFER" & DTYPE == "BUFFER", SKIMTYPE := "Buffer no traverse"]
  cv_trips[is.na(dist.buf) & SKIMTYPE == "Buffer no traverse", dist.buf := Distance]
  cv_trips[is.na(time.buf) & SKIMTYPE == "Buffer no traverse", time.buf := TravelTime]
  
  progressUpdate(subtaskprogress = 1, subtask = "External Stations", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  return(cv_trips)
  
}
