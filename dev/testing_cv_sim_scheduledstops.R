# Sensitivty testing on sceduled stops model
# Run the app_run_components.R script as far as after the cv_sim_activities model

firmActivitiesCopy <- copy(firmActivities)
firmsCopy <- copy(firms)
TAZLandUseCVTMCopy <- copy(TAZLandUseCVTM)
cv_goods_modelCopy <- copy(cv_goods_model)
cv_service_modelCopy <- copy(cv_service_model)
cv_goods_res_modelCopy <- copy(cv_goods_res_model)
cv_service_res_modelCopy <- copy(cv_service_res_model)
cv_goods_non_res_modelCopy <- copy(cv_goods_non_res_model)
cv_service_non_res_modelCopy <- copy(cv_service_non_res_model)

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
  cv_goods_res_model <- copy(cv_goods_res_modelCopy)
  cv_service_res_model <- copy(cv_service_res_modelCopy)
  cv_goods_non_res_model <- copy(cv_goods_non_res_modelCopy)
  cv_service_non_res_model <- copy(cv_service_non_res_modelCopy)
  
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
                                     cv_goods_res_model = cv_goods_res_model,
                                     cv_goods_non_res_model = cv_goods_non_res_model,
                                     cv_service_model = cv_service_model,
                                     cv_service_res_model = cv_service_res_model,
                                     cv_service_non_res_model = cv_service_non_res_model,
                                     segment_res_non_res = TRUE)
  gc()
  
  sens_list[[as.character(sl)]] <- firmStops

}


sens_results <- rbindlist(sens_list, idcol = "Level")

sens_results[,.(Stops = .N), keyby = .(Level, Activity)]
sens_results[,.(Stops = .N), keyby = .(Level, Activity, StopLocType)]

# cv_goods_model$coefficients$zero
# cv_goods_model$coefficients$count
# cv_service_model$coefficients$zero
# cv_service_model$coefficients$count


TAZLandUseCVTMCopy
sens_results <- sens_results[,.(Stops = .N), keyby = .(Level, DTAZ, Activity, StopLocType)]
sens_results <- dcast.data.table(sens_results,
                                 Level + DTAZ ~ Activity + StopLocType,
                                 fun.aggregate = sum, value.var = "Stops")
sens_results[, Stops_Res := Goods_Res + Service_Res]
sens_results[, Stops_NonRes := Goods_NonRes + Service_NonRes]
sens_results[, Stops := Stops_Res + Stops_NonRes]
sens_results[, Goods := Goods_Res + Goods_NonRes]
sens_results[, Service := Service_Res + Service_NonRes]
sens_results[TAZLandUseCVTMCopy[,.(DTAZ = TAZ, NEmp_Total, HH)],
             c("NEmp_Total", "HH") := .(i.NEmp_Total, i.HH),
             on = "DTAZ"]
sens_results[Level == 1, .(cor(Stops, NEmp_Total), cor(Goods, NEmp_Total), cor(Service, NEmp_Total))]
sens_results[Level == 1, .(cor(Stops, HH), cor(Goods, HH), cor(Service, HH))]

saveRDS(sens_results, "./dev/Testing/Sched_Stops_Sens_Test_Res_NonRes.rds")