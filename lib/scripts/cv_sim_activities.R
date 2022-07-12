
# Commercial Vehicle Scheduled Activities Simulation
cv_sim_activities <- function(firms, cv_activities_model) {

  progressUpdate(subtaskprogress = 0, subtask = "Firm Activities", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Add cumulative shares for goods and services by model employment category
  firmActivities <- merge(firms[,.(BusID, EmpCatGroupedName)],
                          cv_activities_model[, .(EmpCatGroupedName, Goods, Service = Goods + Service, GoodsAndService = Goods + Service + GoodsAndService)],
                          by = "EmpCatGroupedName")
  
  # Simulate a tour type (Goods, Service, or Both)
  set.seed(BASE_SEED_VALUE)
  firmActivities[, TMP_RAND := runif(.N)]
  firmActivities[, Activity := ifelse(TMP_RAND < Goods, "Goods", 
                                      ifelse(TMP_RAND < Service, "Service", 
                                             ifelse(TMP_RAND < GoodsAndService, "GoodsAndService", "Other")))]
  
  # Duplicate records for establishments making both goods and service tours
  # Remove other, those are businesses that do not make any commercial vehicle trips covered by the model
  firmActivities <- rbind(firmActivities[Activity %in% c("Goods", "Service")],
                          firmActivities[Activity == "GoodsAndService"][, Activity := "Goods"],
                          firmActivities[Activity == "GoodsAndService"][, Activity := "Service"])
  
  # Reduce table to just BusID and activity, convert activity to factor, and key
  firmActivities <- firmActivities[, .(BusID, Activity = factor(Activity))]
  setkey(firmActivities, BusID, Activity)
  
  progressUpdate(subtaskprogress = 1, subtask = "Firm Activities", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  return(firmActivities)

}
