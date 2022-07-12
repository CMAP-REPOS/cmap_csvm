
# Commercial Vehicle Scheduled Stops Simulation
cv_sim_scheduledstops <- function(firmActivities, skims, firms, TAZLandUseCVTM, numZones, d_bars, hurdle_support,
                                  cv_goods_model, cv_service_model) {
  
  progressUpdate(subtaskprogress = 0, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Drop zones with no employment and no households
  TAZLandUseCVTM <- TAZLandUseCVTM[!(NEmp_Total == 0 & HH == 0)]
  
  # Add firm details
  firmActivities <- merge(firmActivities, firms[, .(BusID, TAZ, EmpCatGroupedName, TOTAL_EMPLOYEES = Emp)], by = "BusID")
  
  # Add dummy variables by EmpCatGroupedName
  firmActivities <- dcast.data.table(firmActivities, 
                                     BusID + Activity + TAZ + EmpCatGroupedName + TOTAL_EMPLOYEES ~ EmpCatGroupedName,
                                     fun.aggregate = length, 
                                     fill = 0)
  
  progressUpdate(subtaskprogress = 0.2, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Create potential zone samples for each firm based on zone attractiveness measure
  firmsInd <- unique(firmActivities[, .(BusID, EmpCatGroupedName, TAZ)])
  setkey(firmsInd, TAZ, EmpCatGroupedName)
  tazInd <- unique(firmsInd[, .(EmpCatGroupedName, TAZ)])
  tazZones <- merge(tazInd, skims[DTAZ %in% BASE_TAZ_INTERNAL, .(TAZ = OTAZ, DTAZ, dist)],
                     by = "TAZ", allow.cartesian = TRUE)
  
  # Add total household and employment numbers for potential stop zone
  tazZones <- merge(tazZones, TAZLandUseCVTM[, .(DTAZ = TAZ, HH, NEmp_Total)], by = "DTAZ") 
  
  # Add flag for crossing international border
  tazZones[, boundary := ifelse((TAZ %in% BASE_TAZ_US & DTAZ %in% BASE_TAZ_CANADA)|
                                  (TAZ %in% BASE_TAZ_CANADA & DTAZ %in% BASE_TAZ_US),
                                1,0)]
  
  # Calculate zone attractiveness
  Beta <- 2
  tazZones[, Attraction := (Beta * NEmp_Total + HH) * exp(-(dist + BASE_INTERNATIONAL_PENALTY * boundary)/d_bars[as.character(EmpCatGroupedName)])]
  tazZones[, c("dist", "HH", "NEmp_Total") := NULL]
  setkey(tazZones, TAZ, EmpCatGroupedName, DTAZ)
  
  # Create indexing table for sampling by industry/firm
  firmSample <- firmsInd[,.(firms = .N, firmsMin = .I[which.min(.I)], firmsMax = .I[which.max(.I)]), keyby = .(TAZ, EmpCatGroupedName)]
  firmSample[tazZones[, .(Zones = .N, ZonesMin = .I[which.min(.I)], ZonesMax = .I[which.max(.I)]), by = .(TAZ, EmpCatGroupedName)],
                       c("Zones", "ZonesMin", "ZonesMax") := .(Zones, ZonesMin, ZonesMax),
                       on = c("TAZ", "EmpCatGroupedName")]
  
  # Adding differential sampling size for internal and buffer zones
  # Buffer zones tend to be larger (more aggregate) so reduce the number of zones sampled in the buffer
  firmSample[, numZones := ifelse(TAZ %in% BASE_TAZ_MODEL_REGION, numZones, numZones/2)]
  
  # List of zone and firm index sequences for each market
  ZonesIndex <- mapply(seq, from = firmSample$ZonesMin, to = firmSample$ZonesMax, SIMPLIFY = FALSE)
  firmsIndex <- mapply(seq, from = firmSample$firmsMin, to = firmSample$firmsMax)
  
  # Extract probabilities (attraction values)
  ProbList <- lapply(1:length(ZonesIndex), function(x) tazZones$Attraction[ZonesIndex[[x]]])
    
  # List of zones samples
  set.seed(BASE_SEED_VALUE)
  SampledZones <- mapply(sample.int, 
                         firmSample$Zones, 
                         size = firmSample$firms*firmSample$numZones,
                         prob = ProbList,
                         MoreArgs = list(replace = TRUE))
  
  # Update the ZonesIndex with the samples
  ZonesIndex <- unlist(lapply(1:length(SampledZones), function(x) ZonesIndex[[x]][SampledZones[[x]]]))
    
  # Matching firms for each set of zones
  firmsIndex <- unlist(mapply(rep, x = firmsIndex, each = firmSample$numZones))
  
  # Enumerate the lists of firms and zones
  firmZoneSet <- cbind(firmsInd[firmsIndex, .(BusID)], tazZones[ZonesIndex, .(TAZ = DTAZ)])
  rm(firmsInd, firmSample, tazInd, tazZones, ZonesIndex, firmsIndex, ProbList, SampledZones)
  gc()
  
  ### Service stops
  
  progressUpdate(subtaskprogress = 0.3, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Firm-zone combinations and skims
  firmStops.Service <- merge(firmActivities[Activity == "Service"],
                             firmZoneSet[, .(BusID, DTAZ = TAZ)],
                             by = "BusID", allow.cartesian = TRUE)
  firmStops.Service <- merge(firmStops.Service, skims[, .(TAZ = OTAZ, DTAZ, time, dist, toll)],
                             by = c("TAZ", "DTAZ"))
  
  # Attach zone attributes
  firmStops.Service <- merge(firmStops.Service, TAZLandUseCVTM, by.x = "DTAZ", by.y = "TAZ")
  
  # Calibration adjustment of hurdle constant
  cv_service_model$coefficients$zero["(Intercept)"] <- cv_service_model$coefficients$zero["(Intercept)"] + CAL_CVTM_SCHED_SERVICE

  # Simulate number of scheduled stops
  firmStops.Service[, NStops := as.integer(montecarlo.predict(object = cv_service_model, newdata = .SD, at = hurdle_support))]
  
  # Check if stops are bunching at the end of the hurdle support
  propHurdleMax.Service <- firmStops.Service[NStops > 0L, prop.table(table(NStops))[max(hurdle_support)]]
  if (is.na(propHurdleMax.Service)) propHurdleMax.Service <- 0
  if (propHurdleMax.Service > 0.05) warning(paste0(round(propHurdleMax.Service, 3)*100, "% of non-zero Service stop generations are hitting the upper bound of the current hurdle support domain (", max(hurdle_support), ").\nThis could pose a problem in calibration. If so, try increasing the domain of 'hurdle_support'."), immediate. = TRUE)
  
  # Keep just those with 1 or more stops
  firmStops.Service <- firmStops.Service[NStops > 0]
  gc()
  
  ### Goods stops
  
  progressUpdate(subtaskprogress = 0.5, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Firm-zone combinations and skims
  firmStops.Goods <- merge(firmActivities[Activity == "Goods"],
                           firmZoneSet[, .(BusID, DTAZ = TAZ)],
                           by = "BusID", allow.cartesian = TRUE)
  firmStops.Goods <- merge(firmStops.Goods, skims[, .(TAZ = OTAZ, DTAZ, time, dist, toll)],
                           by = c("TAZ", "DTAZ"))
  
  # Attach zone attributes
  firmStops.Goods <- merge(firmStops.Goods, TAZLandUseCVTM, by.x = "DTAZ", by.y = "TAZ")
  
  # Calibration adjustment of hurdle constant
  cv_goods_model$coefficients$zero["(Intercept)"] <- cv_goods_model$coefficients$zero["(Intercept)"] + CAL_CVTM_SCHED_GOODS
  
  # Simulate number of scheduled stops
  firmStops.Goods[, NStops := as.integer(montecarlo.predict(object = cv_goods_model, newdata = .SD, at = hurdle_support))]
  
  # Check if stops are bunching at the end of the hurdle support
  propHurdleMax.Goods <- firmStops.Goods[NStops > 0L, prop.table(table(NStops))[max(hurdle_support)]]
  if (is.na(propHurdleMax.Goods)) propHurdleMax.Goods <- 0
  if (propHurdleMax.Goods > 0.05) warning(paste0(round(propHurdleMax.Goods, 3)*100, "% of non-zero Goods stop generations are hitting the upper bound of the current hurdle support domain (", max(hurdle_support), ").\nThis could pose a problem in calibration. If so, try increasing the domain of 'hurdle_support'."), immediate. = TRUE)
  
  # Keep just those with 1 or more stops
  firmStops.Goods <- firmStops.Goods[NStops > 0]
  gc()
  
  ### Build table of firm stops
  
  progressUpdate(subtaskprogress = 0.8, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  firmStops <- rbind(firmStops.Service, firmStops.Goods)
  
  # Expand to one row per stop
  firmStops <- firmStops[rep(1:.N, times = NStops)]
  
  # Add StopID
  firmStops[, StopID := 1:.N, by = BusID]
  
  # Drop extra variables and key
  firmStops <- firmStops[, .(BusID, StopID, DTAZ, Activity)]
  setkey(firmStops, BusID, StopID)
  
  progressUpdate(subtaskprogress = 1, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  return(firmStops)

}
