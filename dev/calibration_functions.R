# run model step function temporarily updated here as bug in rFreight version

run_model_step <- function (model_step_name, command_args_format = c("firm_sim", "cv_sim", "tt_build", "db_build"), 
          command_args_dafaults = rep(FALSE, 4), system_wait = TRUE) 
{
  components_to_run <- command_args_dafaults
  components_to_run[match(model_step_name, command_args_format)] <- TRUE
  rfreightcall <- paste("Rscript.exe run_cmap_csvm.R base 2019", 
                        paste(components_to_run, collapse = " "))
  system(rfreightcall, wait = system_wait)
  return(rfreightcall)
}


# Submodel specific calibration functions (should all be called "calibrate_" + submodel name)

# Firm Synthesis

# Process TAZ land use data
calibrate_firm_sim_taz_land_use <- function(submodel_calibrated, submodel_results, model_step_target){
 
  # Create Comparison between the target data and the model results
  
  submodel_results_summary <- melt.data.table(submodel_results,
                                      id.vars = c("TAZ", "Mesozone", "CountyFIPS"),
                                      variable.name = "Category",
                                      value.name = "Model")
  
  submodel_comparison <- merge(model_step_target, 
                               submodel_results_summary, 
                               by = c("TAZ", "Mesozone", "CountyFIPS", "Category"), 
                               all = TRUE)
  
  submodel_comparison[is.na(Target), Target := 0]
  submodel_comparison[is.na(Model), Model := 0]
  
  # Comparison: 
  # The difference between the model and target employment at a TAZ/Category level
  # should be zero. Both datasets are integer values and should be equal
  
  submodel_comparison[, Difference := abs(Model - Target)]
  
  # Set a threshold for the model to reach
  # Number of TAZ/Category combinations where the difference is greater than zero
  submodel_difference_threshold <- 0
  submodel_criteria <- 0
  submodel_test <- submodel_comparison[Difference > submodel_difference_threshold,.N]
  
  # Evaluate whether the model is calibrated and either set submodel_calibrated to true or adjust parameters
  submodel_parameters <- list()
  
  if(submodel_test <= submodel_criteria) {
    submodel_calibrated <- TRUE
  } else {
    # No parameter adjustments for this model, difference means that there is an error somewhere in the code or inputs
    # For example mismatches in the SE data coding and the input data. Manual inspection is required
    submodel_parameters <- submodel_comparison[Difference > submodel_difference_threshold]
    # Set submodel_calibrated to true to stop iterations
    submodel_calibrated <- TRUE
  }
  
  # return a list of items to support calibration and debugging
  return(list(submodel_calibrated = submodel_calibrated,
              submodel_comparison = submodel_comparison,
              submodel_difference_threshold = submodel_difference_threshold,
              submodel_criteria = submodel_criteria,
              submodel_test = submodel_test,
              submodel_parameters = submodel_parameters))
  
}

# Scale firms to TAZ employment forecasts
calibrate_firm_sim_scale_employees <- function(submodel_calibrated, submodel_results, model_step_target){

  # Create Comparison between the target data and the model results

  submodel_results_summary <- submodel_results[TAZ %in% BASE_TAZ_INTERNAL,
                                      .(Model = as.integer(sum(Emp))), 
                                      keyby = .(TAZ, Category = as.character(EmpCatName))]
 
  submodel_comparison <- merge(model_step_target$taz_emp_target[, Category := as.character(Category)], 
                               submodel_results_summary, 
                               by = c("TAZ", "Category"), 
                               all = TRUE)
  
  submodel_comparison[is.na(Target), Target := 0]
  submodel_comparison[is.na(Model), Model := 0]
  
  # Comparison: 
  # The difference between the model and target employment at a TAZ/Category level
  # should be zero. Both datasets are integer values and should be equal
  
  submodel_comparison[, Difference := abs(Model - Target)]
  
  # Set a threshold for the model to reach
  # Number of TAZ/Category combinations where the difference is greater than zero
  submodel_difference_threshold <- 0
  submodel_criteria <- 0
  submodel_test <- submodel_comparison[Difference > submodel_difference_threshold,.N]
  
  # Evaluate whether the model is calibrated and either set submodel_calibrated to true or adjust parameters
  submodel_parameters <- list()
  
  if(submodel_test <= submodel_criteria) {
    submodel_calibrated <- TRUE
  } else {
    # No parameter adjustments for this model, difference means that there is an error somewhere in the code or inputs
    # For example mismatches in the SE data coding and the input data. Manual inspection is required
    submodel_parameters <- submodel_comparison[Difference > submodel_difference_threshold]
    # Set submodel_calibrated to true to stop iterations
    submodel_calibrated <- TRUE
  }

  # return a list of items to support calibration and debugging
  return(list(submodel_calibrated = submodel_calibrated,
              submodel_comparison = submodel_comparison,
              submodel_difference_threshold = submodel_difference_threshold,
              submodel_criteria = submodel_criteria,
              submodel_test = submodel_test,
              submodel_parameters = submodel_parameters))

}

# Commercial Vehicle Touring Model

# Simulate firm activities
calibrate_cv_sim_activities <- function(submodel_calibrated, submodel_results, model_step_target){
  
  # Identify any firms as goods, service, or both goods and service
  submodel_results_summary <- dcast.data.table(submodel_results, 
                                               BusID ~ Activity, 
                                               fun.aggregate = length)
  
  submodel_results_summary[, GoodsAndService := ifelse(Goods == 1 & Service == 1, 1L, 0L)]
  submodel_results_summary[GoodsAndService == 1, c("Goods", "Service") := 0L]
  
  submodel_results_summary <- melt.data.table(submodel_results_summary,
                                   id.vars = "BusID",
                                   variable.name = "Activity",
                                   value.name = "Firms")
                                   
  # Summaries by employment category and calculate percentage of firms of each activity type
  submodel_results_summary[model_step_inputs$model_step_data[,.(BusID, EmpCatGroupedName)],
                           EmpCatGroupedName := i.EmpCatGroupedName,
                           on = "BusID"]
                                   
  submodel_results_summary <- submodel_results_summary[,.(Firms = as.integer(sum(Firms))), 
                                               keyby = .(Activity, Category = EmpCatGroupedName)]
  submodel_results_summary <- rbind(submodel_results_summary,
                                    model_step_inputs$model_step_data[!BusID %in% submodel_results$BusID,.(Firms = .N, Activity = "Other"),
                                    keyby = ,(Category = EmpCatGroupedName)])
  
  submodel_results_summary[, Model := Firms/sum(Firms), by = Category]
  
  # Create Comparison between the target data and the model results
  submodel_comparison <- merge(model_step_target, 
                               submodel_results_summary, 
                               by = c("Activity", "Category"), 
                               all = TRUE)
  
  submodel_comparison[is.na(Target), Target := 0]
  submodel_comparison[is.na(Model), Model := 0]
  
  # Comparison: 
  # The difference between the model and target percentage of firms at a Activity/Category level
  # should be small but is not expected to be zero due to simulation differences. 
  
  submodel_comparison[, Difference := abs(Model - Target)]
  
  # Set a threshold for the model to reach
  # Number of Activity/Category combinations where the difference is greater than 5% for cases where there
  # are less that 1000 of those firms in the region, and 2% where there are more
  submodel_difference_threshold <- model_step_inputs$model_step_data[,.(Firms = .N), 
                                                                     by = .(Category = EmpCatGroupedName)][, Threshold := ifelse(Firms < 1000, 0.05, 0.02)]
  submodel_comparison[submodel_difference_threshold, Threshold := i.Threshold, on = "Category"]
  
  submodel_criteria <- 0
  submodel_test <- submodel_comparison[Difference > Threshold,.N]
  
  # Evaluate whether the model is calibrated and either set submodel_calibrated to true or adjust parameters
  submodel_parameters <- list()
  
  if(submodel_test <= submodel_criteria) {
    submodel_calibrated <- TRUE
  } else {
    # No parameter adjustments for this model, 
    # difference means that there is an error somewhere in the code or inputs,
    # or that the calibration criteria are unreasonably tight for this approach
    # Manual inspection is required
    submodel_parameters <- submodel_comparison[Difference > Threshold]
    # Set submodel_calibrated to true to stop iterations
    submodel_calibrated <- TRUE
  }
  
  # return a list of items to support calibration and debugging
  return(list(submodel_calibrated = submodel_calibrated,
              submodel_comparison = submodel_comparison,
              submodel_difference_threshold = submodel_difference_threshold,
              submodel_criteria = submodel_criteria,
              submodel_test = submodel_test,
              submodel_parameters = submodel_parameters))
  
}


# Simulate scheduled stops
calibrate_cv_sim_scheduledstops <- function(submodel_calibrated, submodel_results, model_step_target){
  
  # Create a summary table from the submodel results
  # Firm industry and employment and TAZ
  # busines to stop distance
  submodel_results[model_step_inputs$model_step_data[,.(BusID, TAZ, EmpCatGroupedName, Emp)],
                           c("OTAZ", "EmpCatGroupedName", "Emp") := .(i.TAZ, i.EmpCatGroupedName, i.Emp),
                           on = "BusID"]
  
  submodel_results[model_step_inputs$model_step_env$skims_tod,
                    Distance := i.dist.avg,
                   on = c("OTAZ", "DTAZ")]
  
  # model_step_target$emp_stops:
  # Number of stops generated per establishment employee  
  # Uses weighted stops and total employment in the model region in that industry
  # Rate StopsEmp is a per employee per day rate by activity and employment category of the business operating the truck
  
  submodel_results_emp_stops = rbind(submodel_results[,.(ModelStops = .N), keyby = .(Activity, EmpCatGroupedName)],
                                     submodel_results[,.(ModelStops = .N, EmpCatGroupedName = "Total"), keyby = .(Activity)])[order(Activity, EmpCatGroupedName)]
  submodel_results_emp_stops_res <- rbind(submodel_results[StopLocType == "Res",.(ModelStopsRes = .N), keyby = .(Activity, EmpCatGroupedName)],
                                          submodel_results[StopLocType == "Res",.(ModelStopsRes = .N, EmpCatGroupedName = "Total"), keyby = .(Activity)])[order(Activity, EmpCatGroupedName)]
  submodel_results_emp_stops_non_res <- rbind(submodel_results[StopLocType == "NonRes",.(ModelStopsNonRes = .N), keyby = .(Activity, EmpCatGroupedName)],
                                          submodel_results[StopLocType == "NonRes",.(ModelStopsNonRes = .N, EmpCatGroupedName = "Total"), keyby = .(Activity)])[order(Activity, EmpCatGroupedName)]
  submodel_results_emp_stops[submodel_results_emp_stops_res,
                             ModelStopsRes := i.ModelStopsRes,
                             on = c("Activity", "EmpCatGroupedName")]
  submodel_results_emp_stops[submodel_results_emp_stops_non_res,
                             ModelStopsNonRes := i.ModelStopsNonRes,
                             on = c("Activity", "EmpCatGroupedName")]
  
  # remove any industries for which there are no stops from the emp total
  cols <- grep("NEmp_",names(model_step_inputs$model_step_env$TAZLandUseCVTM))
  industry_emp_totals <- colSums(model_step_inputs$model_step_env$TAZLandUseCVTM[,..cols])
  industry_emp_totals <- data.table(EmpCatGroupedName = sub("NEmp_" , "", names(industry_emp_totals)),
                                    Emp = industry_emp_totals)
  industry_emp_totals = industry_emp_totals[EmpCatGroupedName %in% unique(submodel_results_emp_stops$EmpCatGroupedName)]
  industry_emp_totals[EmpCatGroupedName == "Total", Emp := sum(industry_emp_totals[EmpCatGroupedName != "Total"]$Emp)]
  
  submodel_results_emp_stops[industry_emp_totals, Emp := i.Emp, on = "EmpCatGroupedName"]
  
  submodel_results_emp_stops[, Model := ModelStops/Emp]
  submodel_results_emp_stops[, ModelRes := ModelStopsRes/Emp]
  submodel_results_emp_stops[, ModelNonRes := ModelStopsNonRes/Emp]
  
  # Estimate totals and add to the targets and then merge with model
  # Exclude Transport_Industry as not covered by the CSVM 
  model_step_target$emp_stops_total <- model_step_target$emp_stops[!EmpCatGroupedName %in% c("Transport_Industry", "Total") ,
                                                                    .(Employment = sum(Employment), 
                                                                      Stops = sum(Stops),
                                                                      StopsRes = sum(StopsRes),
                                                                      StopsNonRes = sum(StopsNonRes),
                                                                      WeightedStops = sum(WeightedStops),
                                                                      WeightedStopsRes = sum(WeightedStopsRes),
                                                                      WeightedStopsNonRes = sum(WeightedStopsNonRes),
                                                                      EmpCatGroupedName = "Total"), 
                                                                   by = .(Activity)][, c("StopsEmp","StopsResEmp", "StopsNonResEmp") := 
                                                                                       .(WeightedStops/Employment, WeightedStopsRes/Employment, WeightedStopsNonRes/Employment)]
  
  model_step_target$emp_stops <- rbind(model_step_target$emp_stops[!EmpCatGroupedName %in% c("Transport_Industry", "Total") ],
                                       model_step_target$emp_stops_total)
  
  submodel_comparison_emp_stops <- merge(model_step_target$emp_stops[, .(Activity, EmpCatGroupedName, Target = StopsEmp, TargetRes = StopsResEmp, TargetNonRes = StopsNonResEmp)], 
                                         submodel_results_emp_stops, 
                                         by = c("Activity", "EmpCatGroupedName"), 
                                         all = TRUE)
  
  submodel_comparison_emp_stops[is.na(submodel_comparison_emp_stops)] <- 0
  
  submodel_comparison_emp_stops[, c("TargetStops", "TargetStopsRes", "TargetStopsNonRes") := 
                                  .(Target * Emp, TargetRes * Emp, TargetNonRes * Emp)]
  
  
  # names(model_step_target)
  # "mean_stop_distance"               
  # "mean_stop_distance_industry"      
  # "stop_distance_dist"              
  # "emp_stops"                        
  # "mean_stop_distance_dens10hh"      
  # "mean_stop_distance_dens10emp"    
  # "mean_stop_distance_dens10hh_ind"  
  # "mean_stop_distance_dens10emp_ind" 
  # "stop_taz_hh_emp"                 
  # "stops_taz_ind_hh_emp"
  # "gps_dist"
  # "gps_dist_veh"
  # "gps_dist_mean"
  # "gps_tour_repzones_visits"
  # "gps_tour_repzones_unique"
  
  # model_step_target$stop_taz_hh_emp:
  # Stops per sum of employment and HH in a TAZ by activity
  
  # model_step_target$stops_taz_ind_hh_emp:
  # Stops per sum of employment and HH in a TAZ by activity and the industry of the business operating the truck
  
  # model_step_target$mean_stop_distance
  # Average distance by activity from business to stop location
  submodel_results_stop_dist = submodel_results[,.(Model = mean(Distance)), keyby = .(Activity)]
  submodel_comparison_stop_dist <- merge(model_step_target$mean_stop_distance[, .(Activity, Target = WeightedMeanDistance)], 
                                         submodel_results_stop_dist, 
                                         by = c("Activity"), 
                                         all = TRUE)
  
  submodel_comparison_stop_dist[is.na(submodel_comparison_stop_dist)] <- 0
  
  # model_step_target$mean_stop_distance_industry
  # Average distance by activity and industry of the business operating the truck from business to stop location
  submodel_results_stop_dist_ind = submodel_results[,.(Model = mean(Distance)), keyby = .(Activity, EmpCatGroupedName)]
  submodel_comparison_stop_dist_ind <- merge(model_step_target$mean_stop_distance_industry[, .(Activity, EmpCatGroupedName = IndustryCat, Target = WeightedMeanDistance)], 
                                         submodel_results_stop_dist_ind, 
                                         by = c("Activity", "EmpCatGroupedName"), 
                                         all = TRUE)
  
  submodel_comparison_stop_dist_ind[is.na(submodel_comparison_stop_dist_ind)] <- 0
  
  # model_step_target$gps_dist
  # Binned distance buisness to stop, grouped in a similar way to the parameters used in the vehicle model
  stop_dist_bins <- c(0,2,5,10,20)
  submodel_results[, dist_bin := model_step_target$gps_dist$dist_bin[findInterval(Distance, stop_dist_bins)]]
  submodel_results_stop_dist_gps = submodel_results[,.(ModelStops = .N), keyby = .(dist_bin)]
  submodel_results_stop_dist_gps[, Model := ModelStops/sum(ModelStops)]
  
  submodel_comparison_stop_dist_gps <- merge(model_step_target$gps_dist[, .(dist_bin, Target)], 
                                             submodel_results_stop_dist_gps, 
                                         by = c("dist_bin"), 
                                         all = TRUE)
  
  submodel_comparison_stop_dist_gps[is.na(submodel_comparison_stop_dist_gps)] <- 0
  
  # model_step_target$gps_dist_mean
  submodel_comparison_stop_dist_gps_mean <- cbind(model_step_target$gps_dist_mean[, .(Target = mean)], 
                                             submodel_results[,.(Model = mean(Distance))])
  
  # Factor the the Target value to account for intermediate stops reducing the mean distance when added later
  # the observed data already includes the intermediate stops so is too short
  # distance is running at about 15% too low
  DISTANCE_CORRECTION = 1.15
  submodel_comparison_stop_dist_gps_mean[, Target := Target * DISTANCE_CORRECTION]
  
  # model_step_target$gps_tour_repzones_unique
  # Calculate the prpoortion of Businesses that have more than one stop generated in a TAZ
  # Not quite the same as tours given that clustering and vehicle assignment is still to take place
  # but most stops in the same TAZ get put in the same cluster and most stops are light vehicle
  # the overestimate here is balanced out by the addition of intermediate stops 
  # which often end in the same TAZ as an existing stop
  submodel_results_bus <- submodel_results[, .(Stops = .N), keyby = .(BusID)]
  submodel_results_viz <- submodel_results[, .(Visits = .N), keyby = .(BusID, DTAZ)][Visits > 1]
  submodel_results_viz <- submodel_results_viz[,.(Repeats = .N), keyby = BusID]
  submodel_results_bus[submodel_results_viz, Repeats := i.Repeats, on = "BusID"]
  submodel_results_bus[is.na(Repeats), Repeats := 0]
  submodel_results_rep <- submodel_results_bus[,.(Tours = .N), keyby = Repeats]
  submodel_results_rep[, PctTours := Tours/sum(Tours)]
  
  # express the adjustment based on the target of tours with a repeated stop 
  # the model is a count model so a more positive value for the intercept gives higher likelihood of repeated stops
  submodel_comparison_rep <- cbind(model_step_target$gps_tour_repzones_unique[, .(Target = sum(Target_amongAllTours))], 
                                   submodel_results_rep[Repeats == 0, .(Model = 1- PctTours)])
  
  # Comparison: 
  # The difference between the model and target stops
  # should be small but is not expected to be zero due to simulation differences. 
  
  submodel_comparison_emp_stops[, Difference := abs(ModelStops - TargetStops)]
  submodel_comparison_emp_stops[, DifferenceRes := abs(ModelStopsRes - TargetStopsRes)]
  submodel_comparison_emp_stops[, DifferenceNonRes := abs(ModelStopsNonRes- TargetStopsNonRes)]
  
  # Set a threshold for the model to reach
  submodel_difference_threshold = 1000
  submodel_comparison_emp_stops[, Threshold := submodel_difference_threshold]
  
  submodel_criteria <- 0
  submodel_test <- submodel_comparison_emp_stops[Difference > Threshold,.N]
  
  # Evaluate whether the model is calibrated and either set submodel_calibrated to true or adjust parameters
  submodel_parameters <- list()
  
  if(submodel_test <= submodel_criteria) {
    submodel_calibrated <- TRUE
  } else {
    # Parameter adjustments 
    # Adjust the constants in the model
    coefficients =
      rbind(data.table(
        Activity = "Goods",
        StopLocType = "Res",
        modelstep = "count",
        coefficient = names(model_step_inputs$model_step_env$cv_goods_res_model$coefficients$count),
        estimate = model_step_inputs$model_step_env$cv_goods_res_model$coefficients$count),
        data.table(
          Activity = "Goods",
          StopLocType = "Res",
          modelstep = "zero",
          coefficient = names(model_step_inputs$model_step_env$cv_goods_res_model$coefficients$zero),
          estimate = model_step_inputs$model_step_env$cv_goods_res_model$coefficients$zero),
        data.table(
          Activity = "Service",
          StopLocType = "Res",
          modelstep = "count",
          coefficient = names(model_step_inputs$model_step_env$cv_service_res_model$coefficients$count),
          estimate = model_step_inputs$model_step_env$cv_service_res_model$coefficients$count),
        data.table(
          Activity = "Service",
          StopLocType = "Res",
          modelstep = "zero",
          coefficient = names(model_step_inputs$model_step_env$cv_service_res_model$coefficients$zero),
          estimate = model_step_inputs$model_step_env$cv_service_res_model$coefficients$zero),
        data.table(
          Activity = "Goods",
          StopLocType = "NonRes",
          modelstep = "count",
          coefficient = names(model_step_inputs$model_step_env$cv_goods_non_res_model$coefficients$count),
          estimate = model_step_inputs$model_step_env$cv_goods_non_res_model$coefficients$count),
        data.table(
          Activity = "Goods",
          StopLocType = "NonRes",
          modelstep = "zero",
          coefficient = names(model_step_inputs$model_step_env$cv_goods_non_res_model$coefficients$zero),
          estimate = model_step_inputs$model_step_env$cv_goods_non_res_model$coefficients$zero),
        data.table(
          Activity = "Service",
          StopLocType = "NonRes",
          modelstep = "count",
          coefficient = names(model_step_inputs$model_step_env$cv_service_non_res_model$coefficients$count),
          estimate = model_step_inputs$model_step_env$cv_service_non_res_model$coefficients$count),
        data.table(
          Activity = "Service",
          StopLocType = "NonRes",
          modelstep = "zero",
          coefficient = names(model_step_inputs$model_step_env$cv_service_non_res_model$coefficients$zero),
          estimate = model_step_inputs$model_step_env$cv_service_non_res_model$coefficients$zero))
    
    
    # Stops by industry
    submodel_comparison_emp_stops[, Ratio := Target/Model]
    submodel_comparison_emp_stops[, RatioRes := TargetRes/ModelRes]
    submodel_comparison_emp_stops[, RatioNonRes := TargetNonRes/ModelNonRes]
    
    submodel_comparison_emp_stops[, Adjustment := ifelse(is.infinite(Ratio)|Ratio == 0, 0, log(Ratio))]
    submodel_comparison_emp_stops[, AdjustmentRes := ifelse(is.infinite(RatioRes)|RatioRes == 0, 0, log(RatioRes))]
    submodel_comparison_emp_stops[, AdjustmentNonRes := ifelse(is.infinite(RatioNonRes)|RatioNonRes == 0, 0, log(RatioNonRes))]
    
    # Distance by industry/activity/mean overall
    submodel_comparison_stop_dist[, Ratio := Target/Model]
    submodel_comparison_stop_dist[, Adjustment := ifelse(is.infinite(Ratio)|Ratio == 0, 0, log(Ratio))]
    submodel_comparison_stop_dist_ind[, Ratio := Target/Model]
    submodel_comparison_stop_dist_ind[, Adjustment := ifelse(is.infinite(Ratio)|Ratio == 0, 0, log(Ratio))]
    submodel_comparison_stop_dist_gps_mean[, Ratio := Target/Model]
    submodel_comparison_stop_dist_gps_mean[, Adjustment := ifelse(is.infinite(Ratio)|Ratio == 0, 0, log(Ratio))]
    
    # Repeated Stops
    submodel_comparison_rep[, Ratio := (Target)/(Model)]
    submodel_comparison_rep[, Adjustment := ifelse(is.infinite(Ratio)|Ratio == 0, 0, log(Ratio))]
    
    # calibration approach: 
    # adjust intercept and then employment specific constants
    # then activity specifc adjustment to distance or time coeffs, then overall adjustment to distance or time coeffs
    # then adjust count intercept to correct repeated stops in same TAZ
    # 1. intercept in zero model, 
    # 2. employment specific constants
    # 3. distance or time by activity
    # 4. overall distance or time
    # 5. count intercepts
    # maximum iter is 31 so 6 complete cycles and final intercept adjustment
    if(submodel_iter %%5 == 0){ # 5,10,15 etc iterations
      
      # Apply the total adjustment to the intercept in the counts model
      coefficients[modelstep == "count" & coefficient == "(Intercept)",
                   adjustment := submodel_comparison_rep$Adjustment]
      coefficients[!is.na(adjustment), estimate := estimate + adjustment]
      
    } else if (submodel_iter %%5 == 1) { # 1,6,11,16 etc iterations
      
      # Apply the total adjustment to the intercept (Res and NonRes)
      coefficients[submodel_comparison_emp_stops[EmpCatGroupedName == "Total",
                                                 .(Activity, StopLocType = "Res", 
                                                   coefficient = "(Intercept)", 
                                                   modelstep = "zero", Adjustment, AdjustmentRes, AdjustmentNonRes)], 
                   adjustment := i.AdjustmentRes, on = c("Activity", "StopLocType", "modelstep", "coefficient")]
      
      coefficients[submodel_comparison_emp_stops[EmpCatGroupedName == "Total",
                                                 .(Activity, StopLocType = "NonRes", 
                                                   coefficient = "(Intercept)", 
                                                   modelstep = "zero", Adjustment, AdjustmentRes, AdjustmentNonRes)], 
                   adjustment := i.AdjustmentNonRes, on = c("Activity", "StopLocType", "modelstep", "coefficient")]
      
      coefficients[!is.na(adjustment), estimate := estimate + adjustment]
      
    } else if (submodel_iter %%5 == 2) { # 2,7,12,17 etc iterations
      
      # Normalize the adjustments to zero out retail adjustment (it is the zero level)
      submodel_comparison_emp_stops[submodel_comparison_emp_stops[EmpCatGroupedName == "Retail"], 
                                    c("Adjustment", "AdjustmentRes", "AdjustmentNonRes") := 
                                        .(Adjustment - i.Adjustment, 
                                          AdjustmentRes - i.AdjustmentRes, 
                                          AdjustmentNonRes - i.AdjustmentNonRes),
                                    on = "Activity"]
      
      # Apply the emp group adjustments to their parameters
      coefficients[submodel_comparison_emp_stops[,.(Activity, StopLocType = "Res",
                                                    coefficient = EmpCatGroupedName, modelstep = "zero", 
                                                    Adjustment, AdjustmentRes, AdjustmentNonRes)], 
                   adjustment := i.AdjustmentRes, on = c("Activity","StopLocType", "modelstep", "coefficient")]
      
      coefficients[submodel_comparison_emp_stops[,.(Activity, StopLocType = "NonRes",
                                                    coefficient = EmpCatGroupedName, modelstep = "zero", 
                                                    Adjustment, AdjustmentRes, AdjustmentNonRes)], 
                   adjustment := i.AdjustmentNonRes, on = c("Activity","StopLocType", "modelstep", "coefficient")]
      
      coefficients[!is.na(adjustment), estimate := estimate + adjustment]
    
    } else if (submodel_iter %%5 == 3) { # 3,8,13,18 etc iterations
      
      # Apply the mean distance adjustments by activity
      # to the relevant distance or time coefficient in the zero model
      submodel_comparison_stop_dist[, coefficient := ifelse(Activity == "Goods", "log(dist)", "log(time)")]
      coefficients[submodel_comparison_stop_dist[,.(Activity, coefficient, modelstep = "zero", Adjustment)],
                   adjustment := i.Adjustment, on = c("Activity", "modelstep", "coefficient")]
      coefficients[!is.na(adjustment), estimate := estimate + adjustment]
      
    } else if (submodel_iter %%5 == 4) { # 4,9,14,19 etc iterations
      
      # Apply the overall mean distance adjustments 
      # to the relevant distance or time coefficient in the zero model
      submodel_comparison_stop_dist_gps_mean <- rbind(submodel_comparison_stop_dist_gps_mean[, c("Activity", "coefficient") := .("Goods", "log(dist)")],
                                                      submodel_comparison_stop_dist_gps_mean[, c("Activity", "coefficient") := .("Service", "log(time)")])
      
      coefficients[submodel_comparison_stop_dist_gps_mean[,.(Activity, coefficient, modelstep = "zero", Adjustment)],
                   adjustment := i.Adjustment, on = c("Activity", "modelstep", "coefficient")]
      coefficients[!is.na(adjustment), estimate := estimate + adjustment]
      
    }
    
    new_coefficients_zero_goods_res = coefficients[Activity == "Goods" & StopLocType == "Res" & modelstep == "zero", estimate]
    names(new_coefficients_zero_goods_res) = coefficients[Activity == "Goods" & StopLocType == "Res" & modelstep == "zero", coefficient]
    model_step_inputs$model_step_env$cv_goods_res_model$coefficients$zero = new_coefficients_zero_goods_res

    new_coefficients_zero_service_res = coefficients[Activity == "Service" & StopLocType == "Res" & modelstep == "zero", estimate]
    names(new_coefficients_zero_service_res) = coefficients[Activity == "Service" & StopLocType == "Res" & modelstep == "zero", coefficient]
    model_step_inputs$model_step_env$cv_service_res_model$coefficients$zero = new_coefficients_zero_service_res

    new_coefficients_count_goods_res = coefficients[Activity == "Goods" & StopLocType == "Res" & modelstep == "count", estimate]
    names(new_coefficients_count_goods_res) = coefficients[Activity == "Goods" & StopLocType == "Res" & modelstep == "count", coefficient]
    model_step_inputs$model_step_env$cv_goods_res_model$coefficients$count = new_coefficients_count_goods_res

    new_coefficients_count_service_res = coefficients[Activity == "Service" & StopLocType == "Res" & modelstep == "count", estimate]
    names(new_coefficients_count_service_res) = coefficients[Activity == "Service" & StopLocType == "Res" & modelstep == "count", coefficient]
    model_step_inputs$model_step_env$cv_service_res_model$coefficients$count = new_coefficients_count_service_res

    new_coefficients_zero_goods_non_res = coefficients[Activity == "Goods" & StopLocType == "NonRes" & modelstep == "zero", estimate]
    names(new_coefficients_zero_goods_non_res) = coefficients[Activity == "Goods" & StopLocType == "NonRes" & modelstep == "zero", coefficient]
    model_step_inputs$model_step_env$cv_goods_non_res_model$coefficients$zero = new_coefficients_zero_goods_non_res
    
    new_coefficients_zero_service_non_res = coefficients[Activity == "Service" & StopLocType == "NonRes" & modelstep == "zero", estimate]
    names(new_coefficients_zero_service_non_res) = coefficients[Activity == "Service" & StopLocType == "NonRes" & modelstep == "zero", coefficient]
    model_step_inputs$model_step_env$cv_service_non_res_model$coefficients$zero = new_coefficients_zero_service_non_res
    
    new_coefficients_count_goods_non_res = coefficients[Activity == "Goods" & StopLocType == "NonRes" & modelstep == "count", estimate]
    names(new_coefficients_count_goods_non_res) = coefficients[Activity == "Goods" & StopLocType == "NonRes" & modelstep == "count", coefficient]
    model_step_inputs$model_step_env$cv_goods_non_res_model$coefficients$count = new_coefficients_count_goods_non_res
    
    new_coefficients_count_service_non_res = coefficients[Activity == "Service" & StopLocType == "NonRes" & modelstep == "count", estimate]
    names(new_coefficients_count_service_non_res) = coefficients[Activity == "Service" & StopLocType == "NonRes" & modelstep == "count", coefficient]
    model_step_inputs$model_step_env$cv_service_non_res_model$coefficients$count = new_coefficients_count_service_non_res
    
  }
  
  submodel_comparison = submodel_comparison_emp_stops
  submodel_extra = list(submodel_comparison_stop_dist = submodel_comparison_stop_dist,
                        submodel_comparison_stop_dist_ind = submodel_comparison_stop_dist_ind,
                        submodel_comparison_stop_dist_gps_mean = submodel_comparison_stop_dist_gps_mean,
                        submodel_comparison_rep = submodel_comparison_rep)
                         
  submodel_parameters[["cv_goods_res_model"]] = model_step_inputs$model_step_env$cv_goods_res_model
  submodel_parameters[["cv_goods_non_res_model"]] = model_step_inputs$model_step_env$cv_goods_non_res_model
  submodel_parameters[["cv_service_res_model"]] = model_step_inputs$model_step_env$cv_service_res_model
  submodel_parameters[["cv_service_non_res_model"]] = model_step_inputs$model_step_env$cv_service_non_res_model
  
  # return a list of items to support calibration and debugging
  return(list(submodel_calibrated = submodel_calibrated,
              submodel_comparison = submodel_comparison,
              submodel_difference_threshold = submodel_difference_threshold,
              submodel_criteria = submodel_criteria,
              submodel_test = submodel_test,
              submodel_parameters = submodel_parameters,
              submodel_extra = submodel_extra))
  
}

calibrate_cv_sim_vehicle = 
  function(
    submodel_calibrated, 
    submodel_results, 
    model_step_target){
  
    # Create a summary table from the submodel results
    # Firm industry and employment and TAZ
    # busines to stop distance
    submodel_results[model_step_inputs$model_step_data[,.(BusID, TAZ, EmpCatGroupedName, Emp)],
                     c("OTAZ", "EmpCatGroupedName", "Emp") := .(i.TAZ, i.EmpCatGroupedName, i.Emp),
                     on = "BusID"]
    
    submodel_results[model_step_inputs$model_step_env$skims_tod,
                     Distance := i.dist.avg,
                     on = c("OTAZ", "DTAZ")]
    
    # Summarise the model results: overall, by categories for activity, by employment group
    submodel_results_activity_emp_summary <- submodel_results[,.(ModelStops = .N), keyby = .(Vehicle, Activity, EmpCatGroupedName)]
    submodel_results_activity_emp_summary[, Model := ModelStops/sum(ModelStops), by = .(Activity, EmpCatGroupedName)]
    
    # Create Comparison between the target data and the model results
    # Remove any employment types not covered by the model
    # Add any missing activity/vehicle categories with small target shares and recalculate 
    
    vehicle_activity_ind <- data.table(expand.grid(Vehicle = unique(submodel_results_activity_emp_summary$Vehicle),
                                                   Activity = unique(submodel_results_activity_emp_summary$Activity),
                                                   EmpCatGroupedName = unique(submodel_results_activity_emp_summary$EmpCatGroupedName)))
    
    vehicle_activity_ind[model_step_target$vehicle_stops_activity_ind[, .(Vehicle, Activity, EmpCatGroupedName = IndustryCat, FINAL_FACTOR)], 
                         TargetStops := i.FINAL_FACTOR, on = c("Vehicle", "Activity", "EmpCatGroupedName")]
    vehicle_activity_ind[is.na(TargetStops), TargetStops := 10]
    
    vehicle_activity_ind[, Target := TargetStops/sum(TargetStops), by = .(Activity, EmpCatGroupedName)]
    
    submodel_comparison <- merge(vehicle_activity_ind, 
                                 submodel_results_activity_emp_summary, 
                                 by = c("Vehicle", "Activity", "EmpCatGroupedName"), 
                                 all = TRUE)
    
    submodel_comparison[is.na(Target), Target := 0]
    submodel_comparison[is.na(Model), Model := 0]
    
    # Comparison: 
    # The difference between the model and target shares by vehicle alternative and activity/emp group combination
    submodel_comparison[, Difference := abs(Model - Target)]
    
    # Set a threshold for the model to reach
    submodel_difference_threshold <- 0.001
    submodel_criteria <- 0
    submodel_test <- submodel_comparison[, sqrt(mean((Difference)^2))]
    
    # Evaluate whether the model is calibrated and either set submodel_calibrated to true or adjust parameters
    submodel_parameters <- list()
    
    if(submodel_test - submodel_difference_threshold <= submodel_criteria) {
      submodel_calibrated <- TRUE
    } else {
      
      # Adjust the constants in the model
      
      if(submodel_iter %% 3 == 1){ # iter 1,4, etc
        
        # Alternative specific constants
        # Normalize adjustment to keep heavy at zero
        submodel_comparison_adj <- submodel_comparison[,.(ModelStops = sum(ModelStops), 
                                                          TargetStops = sum(TargetStops)), 
                                                       keyby = .(Vehicle)]
        submodel_comparison_adj[, c("Model", "Target") := .(ModelStops/sum(ModelStops), 
                                                            TargetStops/sum(TargetStops)) ]
        submodel_comparison_adj[, Adjustment := log(Target/Model)]
        submodel_comparison_adj[, Adjustment := Adjustment - submodel_comparison_adj[Vehicle == "Heavy"]$Adjustment]
        submodel_comparison_adj[, coefficient := 
                                  c("asc_light","asc_medium","asc_heavy")[match(Vehicle, c("Light", "Medium","Heavy"))]]
        
      } else if(submodel_iter %% 3 == 2){ # iter 2, 5, etc
        
        # Activity variables
        # Normalize adjustment to keep heavy at zero
        # Apply goods ajustments to alternative specific constants
        # Make a further relative adjustment to the service variables to account for the asc adjustment
        submodel_comparison_adj <- submodel_comparison[,.(ModelStops = sum(ModelStops), 
                                                          TargetStops = sum(TargetStops)), 
                                                       keyby = .(Vehicle, Activity)]
        submodel_comparison_adj[, c("Model", "Target") := .(ModelStops/sum(ModelStops), 
                                                            TargetStops/sum(TargetStops)),
                                by = Activity]
        submodel_comparison_adj[, Adjustment := log(Target/Model)]
        submodel_comparison_adj[submodel_comparison_adj[Vehicle == "Heavy"],
                                Adjustment := Adjustment - i.Adjustment,
                                on = "Activity"]
        
        submodel_comparison_adj[submodel_comparison_adj[Activity == "Goods",.(Adjustment, Vehicle, Activity = "Service")],
                                Adjustment := Adjustment - i.Adjustment,
                                on = c("Activity", "Vehicle")]
        
        submodel_comparison_adj[Activity == "Goods", coefficient := 
                                  c("asc_light","asc_medium","asc_heavy")[match(Vehicle, c("Light", "Medium","Heavy"))]]
        submodel_comparison_adj[Activity == "Service", coefficient := 
                                  c("beta_v1_activity_service","beta_v2_activity_service","asc_heavy")[match(Vehicle, c("Light", "Medium","Heavy"))]]
        
      } else {  # iter 3,6, etc
        
        # Employment variables
        # Normalize adjustment to keep heavy at zero
        # Apply retail ajustments to alternative specific constants
        # Make a further relative adjustment to the non-retail employment variables to account for the asc adjustment
        submodel_comparison_adj <- submodel_comparison[,.(ModelStops = sum(ModelStops), 
                                                          TargetStops = sum(TargetStops)), 
                                                       keyby = .(Vehicle, EmpCatGroupedName)]
        submodel_comparison_adj[, c("Model", "Target") := .(ModelStops/sum(ModelStops), 
                                                            TargetStops/sum(TargetStops)),
                                by = EmpCatGroupedName]
        submodel_comparison_adj[, Adjustment := log(Target/Model)]
        submodel_comparison_adj[submodel_comparison_adj[Vehicle == "Heavy"],
                                Adjustment := Adjustment - i.Adjustment,
                                on = "EmpCatGroupedName"]
        
        submodel_comparison_adj[submodel_comparison_adj[EmpCatGroupedName == "Retail",.(Adjustment, Vehicle)],
                                Adjustment := ifelse(EmpCatGroupedName != "Retail", Adjustment - i.Adjustment, Adjustment),
                                on = "Vehicle"]
        
        submodel_comparison_adj[EmpCatGroupedName == "Retail", coefficient := 
                                  c("asc_light","asc_medium","asc_heavy")[match(Vehicle, c("Light", "Medium","Heavy"))]]
        submodel_comparison_adj[EmpCatGroupedName != "Retail", coefficient := 
                                  c("beta_v1_industry_","beta_v2_industry_","asc_heavy")[match(Vehicle, c("Light", "Medium","Heavy"))]]
        submodel_comparison_adj[EmpCatGroupedName != "Retail" & Vehicle %in% c("Light", "Medium"),
                                coefficient := paste0(coefficient, tolower(EmpCatGroupedName))]
        
      }
      
      coefficients = 
        data.table(
          coefficient = names(model_step_inputs$model_step_env$cv_vehicle_model$estimate), 
          estimate = model_step_inputs$model_step_env$cv_vehicle_model$estimate)
      
      coefficients[submodel_comparison_adj, adjustment := i.Adjustment, on = "coefficient"]
      coefficients[!is.na(adjustment), estimate := estimate + adjustment] 
      new_coefficients = coefficients[, estimate]
      names(new_coefficients) = coefficients[, coefficient]
      model_step_inputs$model_step_env$cv_vehicle_model$estimate = new_coefficients
      
    }
    
    submodel_parameters[["cv_vehicle_model"]] = model_step_inputs$model_step_env$cv_vehicle_model
    
  
  # return a list of items to support calibration and debugging
  return(list(submodel_calibrated = submodel_calibrated,
              submodel_comparison = submodel_comparison,
              submodel_difference_threshold = submodel_difference_threshold,
              submodel_criteria = submodel_criteria,
              submodel_test = submodel_test,
              submodel_parameters = submodel_parameters))
  
}

calibrate_cv_sim_stopduration = 
  function(
    submodel_calibrated, 
    submodel_results, 
    model_step_target){
    
    # create a label/choice correspondence
    duration_choice <- data.table(duration_group = levels(model_step_target$duration_stops$duration_group),
                                  choice = 1:length(levels(model_step_target$duration_stops$duration_group)))
    
    # Summarise the model results: activity and vehicle type 
    submodel_results_activity_veh_summary <- submodel_results[,.(ModelStops = .N), keyby = .(Vehicle, Activity, choice)]
    submodel_results_activity_veh_summary[, Model := ModelStops/sum(ModelStops), by = .(Vehicle, Activity)]
    
    # Create Comparison between the target data and the model results
    # Add any missing activity/vehicle categories with small target shares and recalculate 
    vehicle_activity <- data.table(expand.grid(choice = duration_choice$choice,
                                               Vehicle = unique(submodel_results_activity_veh_summary$Vehicle),
                                               Activity = unique(submodel_results_activity_veh_summary$Activity)))
    model_step_target$duration_stops_activity_vehicle[duration_choice, choice := i.choice, on = "duration_group"]
    
    vehicle_activity[model_step_target$duration_stops_activity_vehicle, 
                         TargetStops := i.FINAL_FACTOR, on = c("Vehicle", "Activity", "choice")]
    vehicle_activity[is.na(TargetStops), TargetStops := 10]
    
    vehicle_activity[, Target := TargetStops/sum(TargetStops), by = .(Vehicle, Activity)]
    
    submodel_comparison <- merge(vehicle_activity, 
                                 submodel_results_activity_veh_summary, 
                                 by = c("choice", "Vehicle", "Activity"), 
                                 all = TRUE)
    
    submodel_comparison[is.na(Target), Target := 0]
    submodel_comparison[is.na(Model), Model := 0]
    
    # Comparison: 
    # The difference between the model and target shares by duration alternative
    
    submodel_comparison[, Difference := abs(Model - Target)]
    
    # Set a threshold for the model to reach
    submodel_difference_threshold <- 0.001
    submodel_criteria <- 0
    submodel_test <- submodel_comparison[, sqrt(mean((Difference)^2))]
    
    # Evaluate whether the model is calibrated and either set submodel_calibrated to true or adjust parameters
    submodel_parameters <- list()
    
    if(submodel_test - submodel_difference_threshold <= submodel_criteria) {
      submodel_calibrated <- TRUE
    } else {
      
      # Adjust the constants in the model
      
      if(submodel_iter %% 3 == 1){ # iter 1,4, etc
        
        # Alternative specific constants
        # Normalize adjustment to keep asc_15 at zero
        submodel_comparison_adj <- submodel_comparison[,.(ModelStops = sum(ModelStops), 
                                                          TargetStops = sum(TargetStops)), 
                                                       keyby = .(choice)]
        submodel_comparison_adj[, c("Model", "Target") := .(ModelStops/sum(ModelStops), 
                                                            TargetStops/sum(TargetStops)) ]
        submodel_comparison_adj[, Adjustment := log(Target/Model)]
        submodel_comparison_adj[, Adjustment := Adjustment - submodel_comparison_adj[choice == 1]$Adjustment]
        
        submodel_comparison_adj[, coefficient := 
                                  names(model_step_inputs$model_step_env$cv_stopduration_model$estimate)[1:11][match(choice, 1:11)]]
        
      } else if(submodel_iter %% 3 == 2){ # iter 2, 5, etc
        
        # Activity variables
        # Normalize adjustments to base level at zero
        
        duration_choice[, coefficient := c(rep("b_activity_service_0_45",3),
                                           rep("b_activity_service_60_75",2),
                                           rep("b_activity_service_90_plus",6))]
        
        submodel_comparison[duration_choice, coefficient := i.coefficient, on = "choice"]
        
        submodel_comparison_adj <- submodel_comparison[Activity == "Service",.(ModelStops = sum(ModelStops), 
                                                          TargetStops = sum(TargetStops)), 
                                                       keyby = coefficient]
        submodel_comparison_adj[, c("Model", "Target") := .(ModelStops/sum(ModelStops), 
                                                            TargetStops/sum(TargetStops))]
        submodel_comparison_adj[, Adjustment := log(Target/Model)]
        
        submodel_comparison_adj[, Adjustment := Adjustment - submodel_comparison_adj[coefficient == "b_activity_service_0_45"]$Adjustment]
        
      } else {  # iter 3,6, etc
        
        # Vehicle variables
        # Normalize adjustments to base level at zero
        
        duration_choice[, coefficient_med := c(rep("asc_15",1),
                                               rep("b_is_med_veh_30_75",4),
                                           rep("b_is_med_veh_90_plus",6))]
        duration_choice[, coefficient_hvy := c(rep("asc_15",1),
                                               rep("b_is_hvy_veh_30_90",5),
                                               rep("b_is_hvy_veh_150_plus",5))]
        
        duration_choice_veh <- rbind(duration_choice[, .(choice, coefficient = coefficient_med, Vehicle = "Medium")],
                                 duration_choice[, .(choice, coefficient = coefficient_hvy, Vehicle = "Heavy")])
        
        submodel_comparison[duration_choice_veh, coefficient := i.coefficient, on = c("choice", "Vehicle")]
        
        submodel_comparison_adj <- submodel_comparison[Vehicle %in% c("Medium", "Heavy"),.(ModelStops = sum(ModelStops), 
                                                          TargetStops = sum(TargetStops)), 
                                                       keyby = .(coefficient, Vehicle)]
        submodel_comparison_adj[, c("Model", "Target") := .(ModelStops/sum(ModelStops), 
                                                            TargetStops/sum(TargetStops)),
                                    by = "Vehicle"]
        submodel_comparison_adj[, Adjustment := log(Target/Model)]
        
        submodel_comparison_adj[submodel_comparison_adj[coefficient == "asc_15"],
                                    Adjustment := Adjustment - i.Adjustment, on = "Vehicle"]
        
      }
      
      coefficients = 
        data.table(
          coefficient = names(model_step_inputs$model_step_env$cv_stopduration_model$estimate), 
          estimate = model_step_inputs$model_step_env$cv_stopduration_model$estimate)
      
      coefficients[submodel_comparison_adj, adjustment := i.Adjustment, on = "coefficient"]
      coefficients[!is.na(adjustment), estimate := estimate + adjustment] 
      new_coefficients = coefficients[, estimate]
      names(new_coefficients) = coefficients[, coefficient]
      model_step_inputs$model_step_env$cv_stopduration_model$estimate = new_coefficients
  
    }
    
    submodel_parameters[["cv_stopduration_model"]] = model_step_inputs$model_step_env$cv_stopduration_model
    
    # Add labeling
    submodel_comparison <- merge(duration_choice[,.(stop_duration = duration_group, choice)],
                                 submodel_comparison[,.(choice, Vehicle, Activity, TargetStops, Target, ModelStops, Model, Difference)],
                                 by = "choice",
                                 all = TRUE)
    submodel_comparison[, stop_duration := factor(stop_duration, levels = duration_choice$duration_group, ordered = TRUE)]
    
    # return a list of items to support calibration and debugging
    return(list(submodel_calibrated = submodel_calibrated,
                submodel_comparison = submodel_comparison,
                submodel_difference_threshold = submodel_difference_threshold,
                submodel_criteria = submodel_criteria,
                submodel_test = submodel_test,
                submodel_parameters = submodel_parameters))
    
  }


calibrate_cv_sim_tours = 
  function(
    submodel_calibrated, 
    submodel_results, 
    model_step_target){
    
    # Summarise the model results: vehicle type, and by single/multistop within tour type as need to adjust seperately 
    submodel_results_veh_summary <- submodel_results[SequenceID == 1,.(ModelTours = .N), keyby = .(Vehicle, TourType)]
    submodel_results_veh_summary[, SingleMultiple := ifelse(TourType %in% c("bbs", "bns", "nbs", "nns"), "Single", "Multiple")]
    submodel_results_veh_summary[, Model := ModelTours/sum(ModelTours), by = .(Vehicle, SingleMultiple)]
    
    # Create Comparison between the target data and the model results
    vehicle_tourtype <- data.table(expand.grid(Vehicle = unique(submodel_results_veh_summary$Vehicle),
                                               TourType = unique(submodel_results_veh_summary$TourType)))
    vehicle_tourtype[, SingleMultiple := ifelse(TourType %in% c("bbs", "bns", "nbs", "nns"), "Single", "Multiple")]
    
    
    vehicle_tourtype[model_step_target$tour_types_vehicle[,.(TourType = tour_type_choice, Vehicle, FINAL_FACTOR)], 
                     TargetTours := i.FINAL_FACTOR, on = c("Vehicle", "TourType")]
    vehicle_tourtype[is.na(TargetTours), TargetTours := 10]
    
    vehicle_tourtype[, Target := TargetTours/sum(TargetTours), by = .(Vehicle, SingleMultiple)]
    
    submodel_comparison <- merge(vehicle_tourtype, 
                                 submodel_results_veh_summary, 
                                 by = c("TourType", "SingleMultiple", "Vehicle"), 
                                 all = TRUE)
    
    submodel_comparison[is.na(Target), Target := 0]
    submodel_comparison[is.na(Model), Model := 0]
    
    # Summarize single vs multi stop
    submodel_comparison_single_multi <- submodel_comparison[,.(TargetTours = sum(TargetTours),
                                                               ModelTours = sum(ModelTours)),
                                                            by = SingleMultiple]
    submodel_comparison_single_multi[, c("Target", "Model") := .(TargetTours/sum(TargetTours), ModelTours/sum(ModelTours))]
    
    # Comparison: 
    # The difference between the model and target shares by duration alternative
    
    submodel_comparison[, Difference := abs(Model - Target)]
    
    # Set a threshold for the model to reach
    submodel_difference_threshold <- 0.001
    submodel_criteria <- 0
    submodel_test <- submodel_comparison[, sqrt(mean((Difference)^2))]
    
    # Evaluate whether the model is calibrated and either set submodel_calibrated to true or adjust parameters
    submodel_parameters <- list()
    
    if(submodel_test - submodel_difference_threshold <= submodel_criteria) {
      submodel_calibrated <- TRUE
    } else {
      # Adjust the constants in the model
      
      # if(submodel_iter %% 3 == 1){ # iter 1,4, etc
      # 
      #   # Adjust the clustering parameters (branch limits) 
      #   # to create the right proportion of single vs multi stop clusters
      #   
      #   
      #   
      #   
      # } else 
        
        if(submodel_iter %% 2 == 1){ # iter 2,5, etc
          
        # Alternative specific constants
        # Normalize adjustment to keep asc_bbm at zero for multiple stop tours
        # Normalize adjustment to keep asc_bbs at initial value for single stop tours
        submodel_comparison_adj <- submodel_comparison[,.(ModelTours = sum(ModelTours), 
                                                          TargetTours = sum(TargetTours)), 
                                                       keyby = .(TourType, SingleMultiple)]
        
        submodel_comparison_adj[, c("Model", "Target") := .(ModelTours/sum(ModelTours), 
                                                            TargetTours/sum(TargetTours)),
                                by = SingleMultiple]
        
        submodel_comparison_adj[, coefficient := paste0("asc_", TourType)]
        
        submodel_comparison_adj[, Adjustment := log(Target/Model)]
        
        submodel_comparison_adj[SingleMultiple == "Multiple", 
                                Adjustment := Adjustment - submodel_comparison_adj[coefficient == "asc_bbm"]$Adjustment]
        
        submodel_comparison_adj[SingleMultiple == "Single", 
                                Adjustment := Adjustment - submodel_comparison_adj[coefficient == "asc_bbs"]$Adjustment]
        
      } else {  # iter 3,6, etc
        
        # Vehicle variables
        # Normalize adjustments to base level at zero
        
        submodel_comparison[, coefficient := paste0("asc_", TourType)]
        submodel_comparison[Vehicle == "Medium", coefficient := paste0(coefficient, "_is_med_veh")]
        submodel_comparison[Vehicle == "Heavy", coefficient := paste0(coefficient, "_is_hvy_veh")]
        
        submodel_comparison_adj <- submodel_comparison[,.(ModelTours = sum(ModelTours), 
                                                       TargetTours = sum(TargetTours)), 
                                                       keyby = .(coefficient, Vehicle, TourType, SingleMultiple)]
        
        submodel_comparison_adj[, c("Model", "Target") := .(ModelTours/sum(ModelTours), 
                                                            TargetTours/sum(TargetTours)),
                                by = .(Vehicle, SingleMultiple)]
        
        submodel_comparison_adj[, Adjustment := log(Target/Model), by = SingleMultiple]
        
        submodel_comparison_adj[submodel_comparison_adj[TourType %in% c("bbm", "bbs")],
                                Adjustment := Adjustment - i.Adjustment, on = c("Vehicle", "SingleMultiple")]
        
      }
      
      coefficients = 
        data.table(
          coefficient = names(model_step_inputs$model_step_env$cv_tours_model$estimate), 
          estimate = model_step_inputs$model_step_env$cv_tours_model$estimate)
      
      coefficients[submodel_comparison_adj, adjustment := i.Adjustment, on = "coefficient"]
      coefficients[!is.na(adjustment), estimate := estimate + adjustment] 
      new_coefficients = coefficients[, estimate]
      names(new_coefficients) = coefficients[, coefficient]
      model_step_inputs$model_step_env$cv_tours_model$estimate = new_coefficients
      
    }
    
    submodel_parameters[["cv_tours_model"]] = model_step_inputs$model_step_env$cv_tours_model
    submodel_extra = list(submodel_comparison_single_multi = submodel_comparison_single_multi)
    
    # return a list of items to support calibration and debugging
    return(list(submodel_calibrated = submodel_calibrated,
                submodel_comparison = submodel_comparison,
                submodel_difference_threshold = submodel_difference_threshold,
                submodel_criteria = submodel_criteria,
                submodel_test = submodel_test,
                submodel_parameters = submodel_parameters,
                submodel_extra = submodel_extra))
    
  }


calibrate_cv_sim_scheduledtrips = 
  function(
    submodel_calibrated, 
    submodel_results, 
    model_step_target){
    
    # Summarise the model results: by model choice (remove NAs, only choice on first tour stop), and vehicle type 
    submodel_results_veh_summary <- submodel_results[!is.na(model_choice),.(ModelTours = .N), keyby = .(Vehicle, model_choice)]
    submodel_results_veh_summary[, Model := ModelTours/sum(ModelTours), by = Vehicle]
    
    # Create Comparison between the target data and the model results
    vehicle_arrival <- data.table(expand.grid(Vehicle = unique(submodel_results_veh_summary$Vehicle),
                                               model_choice = unique(submodel_results_veh_summary$model_choice)))
    
    vehicle_arrival[model_step_target$tour_arrival_vehicle[,.(model_choice = arrival_choice, Vehicle, FINAL_FACTOR)], 
                     TargetTours := i.FINAL_FACTOR, on = c("Vehicle", "model_choice")]
    vehicle_arrival[is.na(TargetTours), TargetTours := 10]
    
    vehicle_arrival[, Target := TargetTours/sum(TargetTours), by = Vehicle]
    
    submodel_comparison <- merge(vehicle_arrival, 
                                 submodel_results_veh_summary, 
                                 by = c("Vehicle", "model_choice"), 
                                 all = TRUE)
    
    submodel_comparison[is.na(TargetTours), TargetTours := 0]
    submodel_comparison[is.na(ModelTours), ModelTours := 0]
    submodel_comparison[is.na(Target), Target := 0]
    submodel_comparison[is.na(Model), Model := 0]
    
    # Comparison: 
    # The difference between the model and target shares by duration alternative
    
    submodel_comparison[, Difference := abs(Model - Target)]
    
    # Set a threshold for the model to reach
    submodel_difference_threshold <- 0.001
    submodel_criteria <- 0
    submodel_test <- submodel_comparison[, sqrt(mean((Difference)^2))]
    
    # Evaluate whether the model is calibrated and either set submodel_calibrated to true or adjust parameters
    submodel_parameters <- list()
    
    if(submodel_test - submodel_difference_threshold <= submodel_criteria) {
      submodel_calibrated <- TRUE
    } else {
      # Adjust the constants in the model
      # More (2/3) iterations adjusting the overall constants as there are a lot of them
      # End with final iterations (15th) adjusting overall constants
      if(submodel_iter %% 3 %in% c(0,1)){ # iter 1,3,4,6 etc
        
        # Alternative specific constants
        # Normalize adjustment to keep asc_overnight at zero
        submodel_comparison_adj <- submodel_comparison[,.(ModelTours = sum(ModelTours), 
                                                          TargetTours = sum(TargetTours)), 
                                                       keyby = .(model_choice)]
        
        submodel_comparison_adj[, c("Model", "Target") := .(ModelTours/sum(ModelTours), 
                                                            TargetTours/sum(TargetTours))]
        
        submodel_comparison_adj[, coefficient := paste0("asc_", model_choice)]
        
        submodel_comparison_adj[, Adjustment := log(Target/Model)]
        
        submodel_comparison_adj[, Adjustment := Adjustment - submodel_comparison_adj[coefficient == "asc_overnight"]$Adjustment]
        
      } else {  # iter 2,5, etc
        
        # Vehicle variables
        # Normalize adjustments to base level at zero
        
        submodel_comparison[, coefficient := paste0("asc_", model_choice)]
        submodel_comparison[Vehicle %in% c("Medium", "Heavy") & model_choice != "overnight",
                            coefficient := sub("asc_", "asc_is_med_hvy_veh_", coefficient)]
        
        submodel_comparison[Vehicle %in% c("Medium", "Heavy") & model_choice %in% c("1900", "2000", "2100"),
                            coefficient := "asc_is_med_hvy_veh_1900_2100"]
        
        submodel_comparison_adj <- submodel_comparison[Vehicle %in% c("Medium", "Heavy"),
                                                       .(ModelTours = sum(ModelTours), 
                                                          TargetTours = sum(TargetTours)), 
                                                       keyby = coefficient]
        
        submodel_comparison_adj[, c("Model", "Target") := .(ModelTours/sum(ModelTours), 
                                                            TargetTours/sum(TargetTours))]
        
        submodel_comparison_adj[, Adjustment := log(Target/Model)]
        
        submodel_comparison_adj[, Adjustment := Adjustment - submodel_comparison_adj[coefficient == "asc_overnight"]$Adjustment]
        
      }
      
      coefficients = 
        data.table(
          coefficient = names(model_step_inputs$model_step_env$cv_arrival_model$estimate), 
          estimate = model_step_inputs$model_step_env$cv_arrival_model$estimate)
      
      coefficients[submodel_comparison_adj, adjustment := i.Adjustment, on = "coefficient"]
      coefficients[!is.na(adjustment), estimate := estimate + adjustment] 
      new_coefficients = coefficients[, estimate]
      names(new_coefficients) = coefficients[, coefficient]
      model_step_inputs$model_step_env$cv_arrival_model$estimate = new_coefficients
      
    }
    
    submodel_parameters[["cv_arrival_model"]] = model_step_inputs$model_step_env$cv_arrival_model
    
    # return a list of items to support calibration and debugging
    return(list(submodel_calibrated = submodel_calibrated,
                submodel_comparison = submodel_comparison,
                submodel_difference_threshold = submodel_difference_threshold,
                submodel_criteria = submodel_criteria,
                submodel_test = submodel_test,
                submodel_parameters = submodel_parameters))
    
  }

calibrate_cv_sim_intermediatestops = 
  function(
    submodel_calibrated, 
    submodel_results, 
    model_step_target){
    
    stop_type  <- data.table(Activity = levels(submodel_results$Activity),
                             intermediate_stop_type = c(rep("No Int. Stop", 3), "Driver Needs", "Vehicle Service", "Other Int. Stop"), 
                             stop_abb = c(rep("ns",3),"dn", "vs", "ot"))
    
    # Summarise the model results: by stop type, and vehicle type 
    submodel_results_veh_summary <- submodel_results[,.(ModelStops = .N), keyby = .(Vehicle, Activity)]
    submodel_results_veh_summary[stop_type, stop_abb := i.stop_abb, on = "Activity"]
    submodel_results_veh_summary <- submodel_results_veh_summary[,.(ModelStops = sum(ModelStops)), keyby = .(Vehicle, stop_abb)]
    submodel_results_veh_summary[, Model := ModelStops/sum(ModelStops), by = Vehicle]
    
    # Create Comparison between the target data and the model results
    vehicle_stop_type <- data.table(expand.grid(Vehicle = unique(submodel_results_veh_summary$Vehicle),
                                                stop_abb = unique(submodel_results_veh_summary$stop_abb)))
    
    vehicle_stop_type[stop_type, intermediate_stop_type := i.intermediate_stop_type, on = "stop_abb"]
    
    vehicle_stop_type[model_step_target$intermediate_stops_type_veh[,.(intermediate_stop_type, Vehicle, FINAL_FACTOR)], 
                    TargetStops := i.FINAL_FACTOR, on = c("Vehicle", "intermediate_stop_type")]
    vehicle_stop_type[is.na(TargetStops), TargetStops := 10]
    
    vehicle_stop_type[, Target := TargetStops/sum(TargetStops), by = Vehicle]
    
    submodel_comparison <- merge(vehicle_stop_type, 
                                 submodel_results_veh_summary, 
                                 by = c("Vehicle", "stop_abb"), 
                                 all = TRUE)
    
    submodel_comparison[is.na(TargetStops), TargetStops := 0]
    submodel_comparison[is.na(ModelStops), ModelStops := 0]
    submodel_comparison[is.na(Target), Target := 0]
    submodel_comparison[is.na(Model), Model := 0]
    
    # Comparison: 
    # The difference between the model and target shares by duration alternative
    
    submodel_comparison[, Difference := abs(Model - Target)]
    
    # Set a threshold for the model to reach
    submodel_difference_threshold <- 0.001
    submodel_criteria <- 0
    submodel_test <- submodel_comparison[, sqrt(mean((Difference)^2))]
    
    # Evaluate whether the model is calibrated and either set submodel_calibrated to true or adjust parameters
    submodel_parameters <- list()
    
    if(submodel_test - submodel_difference_threshold <= submodel_criteria) {
      submodel_calibrated <- TRUE
    } else {
      # Adjust the constants in the model
      
      if(submodel_iter %% 2 == 1){ # iter 1,3, etc
        
        # Alternative specific constants
        # Normalize adjustment to keep asc_ns at zero
        submodel_comparison_adj <- submodel_comparison[,.(ModelStops = sum(ModelStops), 
                                                          TargetStops = sum(TargetStops)), 
                                                       keyby = .(stop_abb)]
        
        submodel_comparison_adj[, c("Model", "Target") := .(ModelStops/sum(ModelStops), 
                                                            TargetStops/sum(TargetStops))]
        
        submodel_comparison_adj[, coefficient := paste0("asc_", stop_abb)]
        
        submodel_comparison_adj[, Adjustment := log(Target/Model)]
        
        submodel_comparison_adj[, Adjustment := Adjustment - submodel_comparison_adj[coefficient == "asc_ns"]$Adjustment]
        
      } else {  # iter 2,4, etc
        
        # Vehicle variables on vs and ot stops
        # Normalize adjustments to base level (combination  of ns/dn) at zero
        
        submodel_comparison[, coefficient := "base"]
        submodel_comparison[Vehicle == "Medium" & stop_abb %in% c("vs", "ot"),
                            coefficient := paste0("b_med_veh_", stop_abb)]
        submodel_comparison[Vehicle == "Heavy" & stop_abb %in% c("vs", "ot"),
                            coefficient := paste0("b_hvy_veh_", stop_abb)]
        
        submodel_comparison_adj <- submodel_comparison[Vehicle %in% c("Medium", "Heavy"),
                                                       .(ModelStops = sum(ModelStops), 
                                                         TargetStops = sum(TargetStops)), 
                                                       keyby = .(Vehicle, coefficient)]
        
        submodel_comparison_adj[, c("Model", "Target") := .(ModelStops/sum(ModelStops), 
                                                            TargetStops/sum(TargetStops)),
                                by = Vehicle]
        
        submodel_comparison_adj[, Adjustment := log(Target/Model)]
        
        submodel_comparison_adj[submodel_comparison_adj[coefficient == "base"], Adjustment := Adjustment - i.Adjustment, on = "Vehicle"]
        
      }
      
      coefficients = 
        data.table(
          coefficient = names(model_step_inputs$model_step_env$cv_intermediate_model$estimate), 
          estimate = model_step_inputs$model_step_env$cv_intermediate_model$estimate)
      
      coefficients[submodel_comparison_adj, adjustment := i.Adjustment, on = "coefficient"]
      coefficients[!is.na(adjustment), estimate := estimate + adjustment] 
      new_coefficients = coefficients[, estimate]
      names(new_coefficients) = coefficients[, coefficient]
      model_step_inputs$model_step_env$cv_intermediate_model$estimate = new_coefficients
      
    }
    
    submodel_parameters[["cv_intermediate_model"]] = model_step_inputs$model_step_env$cv_intermediate_model
    submodel_parameters[["cv_intermediate_attraction_model"]] = model_step_inputs$model_step_env$cv_intermediate_attraction_model
    
    # return a list of items to support calibration and debugging
    return(list(submodel_calibrated = submodel_calibrated,
                submodel_comparison = submodel_comparison,
                submodel_difference_threshold = submodel_difference_threshold,
                submodel_criteria = submodel_criteria,
                submodel_test = submodel_test,
                submodel_parameters = submodel_parameters))
    
  }



# generic function for calibrating logit-based choice model
calibrate_logit_based_model = 
  function(
    model_to_calibrate,
    simulation_function,
    alternatives_to_calibrate,
    targets,
    coeff_to_adjust_string = 'asc',
    scale_adjustment = 1,
    max_adjustment_iterations = 100, 
    fit_tolerance = 0.001,
    ...) {
    
    iteration = 0
    
    model_calibrated = copy(model_to_calibrate)
    
    while (iteration < max_adjustment_iterations) {
      
      coefficients = 
        data.table(
          coefficient = names(model_calibrated$estimate), 
          estimate = model_calibrated$estimate)
      
      # apollo prediction
      
      results = 
        simulation_function(
          model = model_calibrated,
          ...)
      
      gc()
      
      # results
      results = results[, .N, .(alt = get(alternatives_to_calibrate))]
      
      results[, prop := N / sum(N)]
      
      results[, N := NULL]
      
      results = results[!is.na(alt)]
      
      # adjust constants
      
      results = targets[results, on = .(alt)]
      
      fit = results[, sqrt(mean((target - prop)^2))]
      
      #cat("Current fit (RMSE): ", fit, "\n")
      
      results[, adjustment := log(target / prop)]
      
      results[, coefficient := paste0(coeff_to_adjust_string, "_", tolower(alt))]
      
      results[, c("alt", "target", "prop") := NULL]
      
      results = results[coefficients, on = .(coefficient)]
      
      results[!is.na(adjustment), estimate := estimate + adjustment] 
      
      new_coefficients = results[, estimate]
      names(new_coefficients) = results[, coefficient]
      
      model_calibrated$estimate = new_coefficients
      
      # report/print fit
      
      iteration = iteration + 1
      
      cat("Iteration", iteration, "complete - Fit is", fit, "\n")
      
      if (fit < fit_tolerance) {
        cat("Exit due to fit tolerance being achieved.\n")
        break
      }
      
      if (iteration == max_adjustment_iterations) {
        cat("Exit due to max iterations.\n")
        cat("Final fit (RMSE): ", fit, "\n")
      }
    }
    
    # do this last
    model_calibrated$estimate = model_calibrated$estimate * scale_adjustment
    
    return(
      list(
        submodel_calibrated = model_calibrated,
        submodel_comparison = results))
    
  }

