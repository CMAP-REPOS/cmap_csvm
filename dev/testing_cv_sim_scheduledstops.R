# Sensitivty testing on sceduled stops model
# Run the app_run_components.R script as far as after the cv_sim_activities model

firmActivitiesCopy <- copy(firmActivities)
firmsCopy <- copy(firms)
TAZLandUseCVTMCopy <- copy(TAZLandUseCVTM)
cv_goods_modelCopy <- copy(cv_goods_model)
cv_service_modelCopy <- copy(cv_service_model)


# Loop over the scheduled stops model with a range of TAZ data
sens_levels <- seq(0.9, 1.1, by = 0.1)
sens_list <- list()

for(sl in sens_levels){

  # update the model inputs
  firmActivities <- copy(firmActivitiesCopy)
  firms <- copy(firmsCopy)
  TAZLandUseCVTM <- copy(TAZLandUseCVTMCopy)
  cv_goods_model <- copy(cv_goods_modelCopy)
  cv_service_model <- copy(cv_service_modelCopy)
  
  # factor the TAZLandUseCVTM
  # Household
  TAZLandUseCVTM[, HH := round(HH * sl)]
  
  # Simulate scheduled stops
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
  
  sens_list[[as.character(sl)]] <- firmStops

}
