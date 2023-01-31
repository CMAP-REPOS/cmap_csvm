
# Commercial Vehicle Scheduled Stops Simulation
cv_sim_scheduledstops <- function(firmActivities, skims, firms, TAZLandUseCVTM, numZones, d_bars, hurdle_support,
                                  cv_goods_res_model, cv_goods_non_res_model, 
                                  cv_service_res_model, cv_service_non_res_model) {
  
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
  
  # Manually add any missing employment variable levels where there are no firms
  uEmpCatGroupedName <- gsub(pattern = "NEmp_", "", 
                             names(TAZLandUseCVTM)[grep(pattern = "NEmp_", names(TAZLandUseCVTM))])
  uEmpCatGroupedName <- uEmpCatGroupedName[!uEmpCatGroupedName %in% c("Total")]
  uEmpCatGroupedName.Missing <- uEmpCatGroupedName[!uEmpCatGroupedName %in% names(firmActivities)]
  if(length(uEmpCatGroupedName.Missing) > 0) firmActivities[, (uEmpCatGroupedName.Missing) := 0]
  
  progressUpdate(subtaskprogress = 0.2, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Create potential zone samples for each firm based on zone attractiveness measure
  firmsInd <- unique(firmActivities[, .(BusID, EmpCatGroupedName, TAZ)])
  setkey(firmsInd, TAZ, EmpCatGroupedName)
  tazInd <- unique(firmsInd[, .(EmpCatGroupedName, TAZ)])
  tazZones <- merge(tazInd, skims[DTAZ %in% BASE_TAZ_INTERNAL, .(TAZ = OTAZ, DTAZ, dist)],
                     by = "TAZ", allow.cartesian = TRUE)
  
  # Add total household and employment numbers for potential stop zone
  tazZones <- merge(tazZones, TAZLandUseCVTM[, .(DTAZ = TAZ, HH, NEmp_Total)], by = "DTAZ") 
  
  # Add adjustments to distance sensitivity for service and goods
  # Weight an average factor based on the number of firms activity combinations in each purpose type
  firmActivitiesXt <- dcast.data.table(firmActivities[, .(Emp = sum(TOTAL_EMPLOYEES)), by = .(EmpCatGroupedName, Activity)],
                   EmpCatGroupedName ~ Activity, fun.aggregate = sum, value.var = "Emp")
  firmActivitiesXt[ , c("PctGoods") := Goods/(Goods + Service)]
  firmActivitiesXt[ , AttractionFactor := PctGoods * base_dist_goods_factor + (1-PctGoods) * base_dist_service_factor]
  
  tazZones[firmActivitiesXt, AttractionFactor := i.AttractionFactor, on = "EmpCatGroupedName"]
  
  # Calculate zone attractiveness
  Beta <- 2
  tazZones[, Attraction := (Beta * NEmp_Total + HH) * exp(-dist/(AttractionFactor * d_bars[as.character(EmpCatGroupedName)]))]
  tazZones[, AttractionRes := (HH) * exp(-dist/(AttractionFactor * d_bars[as.character(EmpCatGroupedName)]))]
  tazZones[, AttractionNonRes := (Beta * NEmp_Total) * exp(-dist/(AttractionFactor * d_bars[as.character(EmpCatGroupedName)]))]
  
  tazZones[, c("dist", "HH", "NEmp_Total") := NULL]
  setkey(tazZones, TAZ, EmpCatGroupedName, DTAZ)
  
  # Create indexing table for sampling by industry/firm
  firmSample <- firmsInd[,.(firms = .N, firmsMin = .I[which.min(.I)], firmsMax = .I[which.max(.I)]), keyby = .(TAZ, EmpCatGroupedName)]
  firmSample[tazZones[, .(Zones = .N, ZonesMin = .I[which.min(.I)], ZonesMax = .I[which.max(.I)]), by = .(TAZ, EmpCatGroupedName)],
                       c("Zones", "ZonesMin", "ZonesMax") := .(Zones, ZonesMin, ZonesMax),
                       on = c("TAZ", "EmpCatGroupedName")]
  
  # Adding sampling size for internal zones
  firmSample[, numZones := numZones]
  
  # List of zone and firm index sequences for each market
  ZonesIndex <- mapply(seq, from = firmSample$ZonesMin, to = firmSample$ZonesMax, SIMPLIFY = FALSE)
  firmsIndex <- mapply(seq, from = firmSample$firmsMin, to = firmSample$firmsMax)
  
  # Extract probabilities (attraction values)
  ProbListRes <- lapply(1:length(ZonesIndex), function(x) tazZones$AttractionRes[ZonesIndex[[x]]])
  ProbListNonRes <- lapply(1:length(ZonesIndex), function(x) tazZones$AttractionNonRes[ZonesIndex[[x]]])
  
  # List of zones samples
  set.seed(BASE_SEED_VALUE)
  SampledZonesRes <- mapply(sample.int, 
                           firmSample$Zones, 
                           size = firmSample$firms*firmSample$numZones,
                           prob = ProbListRes,
                           MoreArgs = list(replace = TRUE))
    
  set.seed(BASE_SEED_VALUE)
  SampledZonesNonRes <- mapply(sample.int, 
                           firmSample$Zones, 
                           size = firmSample$firms*firmSample$numZones,
                           prob = ProbListNonRes,
                           MoreArgs = list(replace = TRUE))
  
  # Update the ZonesIndex with the samples
  ZonesIndexRes <- unlist(lapply(1:length(SampledZonesRes), function(x) ZonesIndex[[x]][SampledZonesRes[[x]]]))
  ZonesIndexNonRes <- unlist(lapply(1:length(SampledZonesNonRes), function(x) ZonesIndex[[x]][SampledZonesNonRes[[x]]]))
  
  # Matching firms for each set of zones
  firmsIndex <- unlist(mapply(rep, x = firmsIndex, each = firmSample$numZones))
  
  # Enumerate the lists of firms and zones
  firmZoneSetRes <- cbind(firmsInd[firmsIndex, .(BusID)], tazZones[ZonesIndexRes, .(TAZ = DTAZ)])
  firmZoneSetNonRes <- cbind(firmsInd[firmsIndex, .(BusID)], tazZones[ZonesIndexNonRes, .(TAZ = DTAZ)])
  
  rm(firmsInd, firmSample, tazInd, tazZones, ZonesIndexRes, ZonesIndexNonRes, 
     firmsIndex, ProbListRes, ProbListNonRes, SampledZonesRes, SampledZonesNonRes)
  gc()
  
  ### Service stops Res
    
  progressUpdate(subtaskprogress = 0.3, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
    
  # Firm-zone combinations and skims
  firmStops.ServiceRes <- merge(firmActivities[Activity == "Service"],
                             firmZoneSetRes[, .(BusID, DTAZ = TAZ)],
                             by = "BusID", allow.cartesian = TRUE)
  firmStops.ServiceRes <- merge(firmStops.ServiceRes, skims[, .(TAZ = OTAZ, DTAZ, time, dist, toll)],
                             by = c("TAZ", "DTAZ"))
  
  # Attach zone attributes
  
  cols <- c("TAZ", "HH", names(TAZLandUseCVTM)[grepl("NEmp", names(TAZLandUseCVTM))])
  names(TAZLandUseCVTM)[grepl("NEmp", names(TAZLandUseCVTM))]
  firmStops.ServiceRes <- merge(firmStops.ServiceRes, TAZLandUseCVTM[,cols, with = FALSE], by.x = "DTAZ", by.y = "TAZ")
  
  # Scenario Specific adjustment to service model:
  # Hurdle constant
  cv_service_res_model$coefficients$zero["(Intercept)"] <- cv_service_res_model$coefficients$zero["(Intercept)"] + asc_service_adj
  # Time sensitivity in the service model
  cv_service_res_model$coefficients$zero["log(time)"] <- cv_service_res_model$coefficients$zero["log(time)"] * impedance_service_factor
  
  # Simulate number of scheduled stops
  firmStops.ServiceRes[, NStops := as.integer(montecarlo.predict(object = cv_service_res_model, newdata = .SD, at = hurdle_support))]
  
  # Check if stops are bunching at the end of the hurdle support
  propHurdleMax.Service <- firmStops.ServiceRes[NStops > 0L, prop.table(table(NStops))[max(hurdle_support)]]
  if (is.na(propHurdleMax.Service)) propHurdleMax.Service <- 0
  if (propHurdleMax.Service > 0.05) warning(paste0(round(propHurdleMax.Service, 3)*100, "% of non-zero Service stop generations are hitting the upper bound of the current hurdle support domain (", max(hurdle_support), ").\nThis could pose a problem in calibration. If so, try increasing the domain of 'hurdle_support'."), immediate. = TRUE)
  
  # Keep just those with 1 or more stops
  firmStops.ServiceRes <- firmStops.ServiceRes[NStops > 0]
  gc()
    
  ### Service stops Non Res
    
  progressUpdate(subtaskprogress = 0.3, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
    
  # Firm-zone combinations and skims
  firmStops.ServiceNonRes <- merge(firmActivities[Activity == "Service"],
                             firmZoneSetNonRes[, .(BusID, DTAZ = TAZ)],
                             by = "BusID", allow.cartesian = TRUE)
  firmStops.ServiceNonRes <- merge(firmStops.ServiceNonRes, skims[, .(TAZ = OTAZ, DTAZ, time, dist, toll)],
                             by = c("TAZ", "DTAZ"))
  
  # Attach zone attributes
  
  cols <- c("TAZ", "HH", names(TAZLandUseCVTM)[grepl("NEmp", names(TAZLandUseCVTM))])
  names(TAZLandUseCVTM)[grepl("NEmp", names(TAZLandUseCVTM))]
  firmStops.ServiceNonRes <- merge(firmStops.ServiceNonRes, TAZLandUseCVTM[,cols, with = FALSE], by.x = "DTAZ", by.y = "TAZ")
  
  # Scenario Specific adjustment to service model:
  # Hurdle constant
  cv_service_non_res_model$coefficients$zero["(Intercept)"] <- cv_service_non_res_model$coefficients$zero["(Intercept)"] + asc_service_adj
  # Time sensitivity in the service model
  cv_service_non_res_model$coefficients$zero["log(time)"] <- cv_service_non_res_model$coefficients$zero["log(time)"] * impedance_service_factor
  
  # Simulate number of scheduled stops
  firmStops.ServiceNonRes[, NStops := as.integer(montecarlo.predict(object = cv_service_non_res_model, newdata = .SD, at = hurdle_support))]
  
  # Check if stops are bunching at the end of the hurdle support
  propHurdleMax.Service <- firmStops.ServiceNonRes[NStops > 0L, prop.table(table(NStops))[max(hurdle_support)]]
  if (is.na(propHurdleMax.Service)) propHurdleMax.Service <- 0
  if (propHurdleMax.Service > 0.05) warning(paste0(round(propHurdleMax.Service, 3)*100, "% of non-zero Service stop generations are hitting the upper bound of the current hurdle support domain (", max(hurdle_support), ").\nThis could pose a problem in calibration. If so, try increasing the domain of 'hurdle_support'."), immediate. = TRUE)
  
  # Keep just those with 1 or more stops
  firmStops.ServiceNonRes <- firmStops.ServiceNonRes[NStops > 0]
  gc()
    
  ### Goods stops Res
    
  progressUpdate(subtaskprogress = 0.5, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
    
  # Firm-zone combinations and skims
  firmStops.GoodsRes <- merge(firmActivities[Activity == "Goods"],
                           firmZoneSetRes[, .(BusID, DTAZ = TAZ)],
                           by = "BusID", allow.cartesian = TRUE)
  firmStops.GoodsRes <- merge(firmStops.GoodsRes, skims[, .(TAZ = OTAZ, DTAZ, time, dist, toll)],
                           by = c("TAZ", "DTAZ"))
  
  # Attach zone attributes
  
  cols <- c("TAZ", "HH", names(TAZLandUseCVTM)[grepl("NEmp", names(TAZLandUseCVTM))])
  names(TAZLandUseCVTM)[grepl("NEmp", names(TAZLandUseCVTM))]
  firmStops.GoodsRes <- merge(firmStops.GoodsRes, TAZLandUseCVTM[,cols, with = FALSE], by.x = "DTAZ", by.y = "TAZ")
  
  # Scenario Specific adjustment to goods model:
  # Hurdle constant
  cv_goods_res_model$coefficients$zero["(Intercept)"] <- cv_goods_res_model$coefficients$zero["(Intercept)"] + asc_goods_adj
  # Distance sensitivity in the goods model
  cv_goods_res_model$coefficients$zero["log(dist)"] <- cv_goods_res_model$coefficients$zero["log(dist)"] * impedance_goods_factor
  
  # Simulate number of scheduled stops
  firmStops.GoodsRes[, NStops := as.integer(montecarlo.predict(object = cv_goods_res_model, newdata = .SD, at = hurdle_support))]
  
  # Check if stops are bunching at the end of the hurdle support
  propHurdleMax.Goods <- firmStops.GoodsRes[NStops > 0L, prop.table(table(NStops))[max(hurdle_support)]]
  if (is.na(propHurdleMax.Goods)) propHurdleMax.Goods <- 0
  if (propHurdleMax.Goods > 0.05) warning(paste0(round(propHurdleMax.Goods, 3)*100, "% of non-zero Goods stop generations are hitting the upper bound of the current hurdle support domain (", max(hurdle_support), ").\nThis could pose a problem in calibration. If so, try increasing the domain of 'hurdle_support'."), immediate. = TRUE)
  
  # Keep just those with 1 or more stops
  firmStops.GoodsRes <- firmStops.GoodsRes[NStops > 0]
  gc()
  
  ### Goods stops Non Res
  
  progressUpdate(subtaskprogress = 0.5, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Firm-zone combinations and skims
  firmStops.GoodsNonRes <- merge(firmActivities[Activity == "Goods"],
                           firmZoneSetNonRes[, .(BusID, DTAZ = TAZ)],
                           by = "BusID", allow.cartesian = TRUE)
  firmStops.GoodsNonRes <- merge(firmStops.GoodsNonRes, skims[, .(TAZ = OTAZ, DTAZ, time, dist, toll)],
                           by = c("TAZ", "DTAZ"))
  
  # Attach zone attributes
  
  cols <- c("TAZ", "HH", names(TAZLandUseCVTM)[grepl("NEmp", names(TAZLandUseCVTM))])
  names(TAZLandUseCVTM)[grepl("NEmp", names(TAZLandUseCVTM))]
  firmStops.GoodsNonRes <- merge(firmStops.GoodsNonRes, TAZLandUseCVTM[,cols, with = FALSE], by.x = "DTAZ", by.y = "TAZ")
  
  # Scenario Specific adjustment to goods model:
  # Hurdle constant
  cv_goods_non_res_model$coefficients$zero["(Intercept)"] <- cv_goods_non_res_model$coefficients$zero["(Intercept)"] + asc_goods_adj
  # Distance sensitivity in the goods model
  cv_goods_non_res_model$coefficients$zero["log(dist)"] <- cv_goods_non_res_model$coefficients$zero["log(dist)"] * impedance_goods_factor
  
  # Simulate number of scheduled stops
  firmStops.GoodsNonRes[, NStops := as.integer(montecarlo.predict(object = cv_goods_non_res_model, newdata = .SD, at = hurdle_support))]
  
  # Check if stops are bunching at the end of the hurdle support
  propHurdleMax.Goods <- firmStops.GoodsNonRes[NStops > 0L, prop.table(table(NStops))[max(hurdle_support)]]
  if (is.na(propHurdleMax.Goods)) propHurdleMax.Goods <- 0
  if (propHurdleMax.Goods > 0.05) warning(paste0(round(propHurdleMax.Goods, 3)*100, "% of non-zero Goods stop generations are hitting the upper bound of the current hurdle support domain (", max(hurdle_support), ").\nThis could pose a problem in calibration. If so, try increasing the domain of 'hurdle_support'."), immediate. = TRUE)
  
  # Keep just those with 1 or more stops
  firmStops.GoodsNonRes <- firmStops.GoodsNonRes[NStops > 0]
  gc()
    
  ### Build table of firm stops
  
  progressUpdate(subtaskprogress = 0.8, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  firmStops <- rbind(firmStops.ServiceRes[, StopLocType := "Res"],
                       firmStops.ServiceNonRes[, StopLocType := "NonRes"],
                       firmStops.GoodsRes[, StopLocType := "Res"],
                       firmStops.GoodsNonRes[, StopLocType := "NonRes"])
  
  # Expand to one row per stop
  firmStops <- firmStops[rep(1:.N, times = NStops)]
  
  # Add StopID
  firmStops[, StopID := 1:.N, by = BusID]
  
  # Drop extra variables and key
  firmStops <- firmStops[, .(BusID, StopID, DTAZ, Activity, StopLocType)]
  setkey(firmStops, BusID, StopID)
  
  progressUpdate(subtaskprogress = 1, subtask = "Stop Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  return(firmStops)

}
