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
                                      keyby = .(TAZ, Category = n2)]
 
  submodel_comparison <- merge(model_step_target$taz_emp_target, 
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
  
  # Identify any firms as goods, service, or both goods and service
  submodel_results_summary <- dcast.data.table(submodel_results, 
                                               BusID ~ Activity, 
                                               fun.aggregate = length)
  
  submodel_results_summary[, GoodsAndService := ifelse(Goods == 1 & Service == 1, 1L, 0L)]
  submodel_results_summary[GoodsAndService == 1, c("Goods", "Service") := 0L]
  
  submodel_results_summary <- melt(submodel_results_summary,
                                   id.vars = "BusID",
                                   variable.name = "Activity",
                                   value.name = "Firms")
  
  # Summaries by employment category and calculate percentage of firms of each activity type
  submodel_results_summary[model_step_inputs$model_step_data[,.(BusID, EmpCatName)],
                           EmpCatName := i.EmpCatName,
                           on = "BusID"]
  
  submodel_results_summary <- submodel_results_summary[,.(Firms = as.integer(sum(Firms))), 
                                                       keyby = .(Activity, Category = EmpCatName)]
  
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
                                                                     by = .(Category = EmpCatName)][, Threshold := ifelse(Firms < 1000, 0.05, 0.02)]
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
    submodel_parameters <- submodel_comparison[Difference > submodel_difference_threshold]
  }
  
  # return a list of items to support calibration and debugging
  return(list(submodel_calibrated = submodel_calibrated,
              submodel_comparison = submodel_comparison,
              submodel_difference_threshold = submodel_difference_threshold,
              submodel_criteria = submodel_criteria,
              submodel_test = submodel_test,
              submodel_parameters = submodel_parameters))
  
}

calibrate_cv_sim_vehicle_choice = 
  function(
    submodel_calibrated, 
    submodel_results, 
    model_step_target){
  
  
  # Identify any firms as goods, service, or both goods and service
  submodel_results_summary <- dcast.data.table(submodel_results, 
                                               BusID ~ Activity, 
                                               fun.aggregate = length)
  
  submodel_results_summary[, GoodsAndService := ifelse(Goods == 1 & Service == 1, 1L, 0L)]
  submodel_results_summary[GoodsAndService == 1, c("Goods", "Service") := 0L]
  
  submodel_results_summary <- melt(submodel_results_summary,
                                   id.vars = "BusID",
                                   variable.name = "Activity",
                                   value.name = "Firms")
  
  # Summaries by employment category and calculate percentage of firms of each activity type
  submodel_results_summary[model_step_inputs$model_step_data[,.(BusID, EmpCatName)],
                           EmpCatName := i.EmpCatName,
                           on = "BusID"]
  
  submodel_results_summary <- submodel_results_summary[,.(Firms = as.integer(sum(Firms))), 
                                                       keyby = .(Activity, Category = EmpCatName)]
  
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
                                                                     by = .(Category = EmpCatName)][, Threshold := ifelse(Firms < 1000, 0.05, 0.02)]
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
    submodel_parameters <- submodel_comparison[Difference > submodel_difference_threshold]
  }
  
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
    
    # Summarise the model results
    submodel_results_summary <- submodel_results[,.(ModelStops = .N), keyby = choice]
    submodel_results_summary[, Model := ModelStops/sum(ModelStops)]
    
    # Create Comparison between the target data and the model results
    model_step_target$duration_stops[, choice := .I]
    
    submodel_comparison <- merge(model_step_target$duration_stops, 
                                 submodel_results_summary, 
                                 by = c("choice"), 
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
    submodel_choices_constants <- data.table(choice = 1:11,
                                             coefficient = names(model_step_inputs$model_step_env$cv_stopduration_model$estimate)[1:11])
    
    if(submodel_test - submodel_difference_threshold <= submodel_criteria) {
      submodel_calibrated <- TRUE
    } else {
      # Adjust the constants in the model
      coefficients = 
        data.table(
          coefficient = names(model_step_inputs$model_step_env$cv_stopduration_model$estimate), 
          estimate = model_step_inputs$model_step_env$cv_stopduration_model$estimate)
      
      coefficients[submodel_choices_constants, choice := i.choice, on = "coefficient"]
      submodel_comparison[, Adjustment := log(Target / Model)]
      coefficients[submodel_comparison, adjustment := i.Adjustment, on = "choice"]
      coefficients[!is.na(adjustment), estimate := estimate + adjustment] 
      new_coefficients = coefficients[, estimate]
      names(new_coefficients) = coefficients[, coefficient]
      model_step_inputs$model_step_env$cv_stopduration_model$estimate = new_coefficients
  
    }
    
    submodel_parameters[["cv_stopduration_model"]] = model_step_inputs$model_step_env$cv_stopduration_model
    
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

