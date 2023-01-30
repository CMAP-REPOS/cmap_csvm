
# Commercial Vehicle Stop Duration Simulation
cv_sim_stopduration <- function(database, model, firms) {

  progressUpdate(subtaskprogress = 0, subtask = "Stop Duration", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Recode vehicle type categories
  database[, is_med_veh := 1 * (Vehicle == "Medium")]
  database[, is_hvy_veh := 1 * (Vehicle == "Heavy")]
  
  # Recode activity categories
  database[, activity_service := 1 * (Activity == "Service")]
  database[, activity_driver_needs := 1 * (Activity == "Break/Meal")]
  database[, activity_other := 1 * (Activity %in% c("Vehicle Service", "Other"))]
  
  # Code availability
  database[, av_15 := 1]
  database[, av_30 := 1]
  database[, av_45 := 1]
  database[, av_60 := 1]
  database[, av_75 := 1]
  database[, av_90 := 1]
  database[, av_150 := 1]
  database[, av_210 := 1]
  database[, av_270 := 1]
  database[, av_390 := 1]
  database[, av_600 := 1]
  
  # Add ID
  database[, ID := BusID]
  
  database[, apollo_sequence := StopID]
  
  model[['apollo_control']][['debug']] = FALSE
  model[['apollo_control']][['cpp']] = FALSE
  model[['apollo_control']][['analyticGrad']] = TRUE
  model[['apollo_control']][['matrixMult']] = FALSE
  model[['apollo_control']][['subMaxV']] = TRUE
  
  apollo_inputs = 
    list(
      apollo_beta_names = names(model$apollo_beta),
      apollo_fixed = model$apollo_fixed,
      database = database,
      apollo_control = model$apollo_control,
      apollo_randCoeff = NA,
      apollo_HB = NA,
      apollo_lcPars = NA,
      draws = NA,
      class_specific = 0,
      silent = TRUE
    )

  # Define probabilities function
  apollo_probabilities =
    function(
      apollo_beta, 
      apollo_inputs, 
      functionality = "estimate"){
      
      ### Attach inputs and detach after function exit
      apollo_attach(apollo_beta, apollo_inputs)
      on.exit(apollo_detach(apollo_beta, apollo_inputs))
      
      ### Create list of probabilities P
      P = list()
      
      ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
      V = list()
      
      V[['a_15']] = 
        asc_15
      
      V[['a_30']] = 
        asc_30 + 
        b_is_med_veh_30_75 * is_med_veh + 
        b_is_hvy_veh_30_90 * is_hvy_veh + 
        b_activity_other_30 * activity_other + 
        b_activity_driver_needs_30 * activity_driver_needs 
      
      V[['a_45']] = 
        asc_45 + 
        b_is_med_veh_30_75 * is_med_veh + 
        b_is_hvy_veh_30_90 * is_hvy_veh + 
        b_activity_other_45 * activity_other + 
        b_activity_driver_needs_45 * activity_driver_needs           
      
      V[['a_60']] = 
        asc_60 + 
        b_is_med_veh_30_75 * is_med_veh + 
        b_is_hvy_veh_30_90 * is_hvy_veh + 
        b_activity_service_60_75 * activity_service + 
        b_activity_other_60_90 * activity_other + 
        b_activity_driver_needs_60_90 * activity_driver_needs   
      
      V[['a_75']] = 
        asc_75 + 
        b_is_med_veh_30_75 * is_med_veh + 
        b_is_hvy_veh_30_90 * is_hvy_veh + 
        b_activity_service_60_75 * activity_service + 
        b_activity_other_60_90 * activity_other + 
        b_activity_driver_needs_60_90 * activity_driver_needs 
      
      V[['a_90']] = 
        asc_90 + 
        b_is_med_veh_90_plus * is_med_veh + 
        b_is_hvy_veh_30_90 * is_hvy_veh + 
        b_activity_service_90_plus * activity_service + 
        b_activity_other_60_90 * activity_other + 
        b_activity_driver_needs_60_90 * activity_driver_needs
      
      V[['a_150']] = 
        asc_150 + 
        b_is_med_veh_90_plus * is_med_veh + 
        b_is_hvy_veh_150_plus * is_hvy_veh + 
        b_activity_service_90_plus * activity_service + 
        b_activity_other_150_plus * activity_other + 
        b_activity_driver_needs_150_plus * activity_driver_needs  
      
      V[['a_210']] = 
        asc_210 + 
        b_is_med_veh_90_plus * is_med_veh + 
        b_is_hvy_veh_150_plus * is_hvy_veh + 
        b_activity_service_90_plus * activity_service + 
        b_activity_other_150_plus * activity_other + 
        b_activity_driver_needs_150_plus * activity_driver_needs 
      
      V[['a_270']] = 
        asc_270 + 
        b_is_med_veh_90_plus * is_med_veh + 
        b_is_hvy_veh_150_plus * is_hvy_veh + 
        b_activity_service_90_plus * activity_service + 
        b_activity_other_150_plus * activity_other + 
        b_activity_driver_needs_150_plus * activity_driver_needs
      
      V[['a_390']] = 
        asc_390 + 
        b_is_med_veh_90_plus * is_med_veh + 
        b_is_hvy_veh_150_plus * is_hvy_veh + 
        b_activity_service_90_plus * activity_service + 
        b_activity_other_150_plus * activity_other + 
        b_activity_driver_needs_150_plus * activity_driver_needs
      
      V[['a_600']] = 
        asc_600 + 
        b_is_med_veh_90_plus * is_med_veh + 
        b_is_hvy_veh_150_plus * is_hvy_veh + 
        b_activity_service_90_plus * activity_service + 
        b_activity_other_150_plus * activity_other + 
        b_activity_driver_needs_150_plus * activity_driver_needs
      
      ### Define settings for MNL model component
      mnl_settings = list(
        alternatives = c(
          a_15 = 1,
          a_30 = 2,
          a_45 = 3,
          a_60 = 4,
          a_75 = 5,
          a_90 = 6,
          a_150 = 7,
          a_210 = 8,
          a_270 = 9,
          a_390 = 10,
          a_600 = 11),
        avail = 
          list(
            a_15 = av_15,
            a_30 = av_30,
            a_45 = av_45,
            a_60 = av_60,
            a_75 = av_75,
            a_90 = av_90,
            a_150 = av_150,
            a_210 = av_210,
            a_270 = av_270,
            a_390 = av_390,
            a_600 = av_600),
        choiceVar    = 1,
        V            = V
      )
      
      ### Compute probabilities using MNL model
      P[["model"]] = apollo_mnl(mnl_settings, functionality)
      
      ### Take product across observation for same individual
      P = apollo_panelProd(P, apollo_inputs, functionality)
      
      ### Prepare and return outputs of function
      P = apollo_prepareProb(P, apollo_inputs, functionality)
      
      return(P)
    }
  
  # Apply the model
  prediction =
    data.table(
      apollo_prediction(
        model,
        apollo_probabilities,
        apollo_inputs))
  
  cols = names(prediction)[!names(prediction) %in% c("ID", "Observation", "chosen")]
  
  # Simulate an alternative choice for each observation
  ### TODO finish function with alt labeling and move to rFreight
  prediction_simulate <- function(prediction, 
                                  seed_value = NULL, 
                                  alternative_labels = NULL){
  
    if(!is.null(seed_value)) set.seed(seed_value)
    prediction[, TMP_RAND := runif(.N)]  
    
    prediction = 
      melt.data.table(
        prediction[,-"chosen"],
        id.vars = c("ID", "Observation", "TMP_RAND"),
        variable.name = "Alternative",
        value.name = "Probability")
  
    prediction[, CumProb := cumsum(Probability), 
               keyby = .(ID, Observation)]
    
    prediction[, CumProbLower := shift(CumProb, fill = 0), 
               by = .(ID, Observation)]
  
    return(prediction[TMP_RAND > CumProbLower & TMP_RAND < CumProb, 
               .(Alternative, ID, Observation)])
  
  }
  
  prediction <- prediction_simulate(prediction, seed_value = BASE_SEED_VALUE)
  
  database[prediction[,.(ID, apollo_sequence = Observation, Alternative)], 
           choice := as.integer(i.Alternative),
           on = c("ID", "apollo_sequence")]
  
  # Simulate a specific minute within each duration range
  duration_bounds = 
    data.table(
      choice = 1:length(cols), 
      upper = as.integer(sapply(strsplit(cols, split = "a_"),"[[",2)))
  
  duration_bounds[, lower := shift(upper, fill = 1)]
  
  database[
    duration_bounds, 
    c("lower", "upper") := .(i.lower, i.upper), 
    on = "choice"]
  
  set.seed(BASE_SEED_VALUE)
  
  database[, StopDuration := as.integer(runif(.N, min = lower, max = upper)), .(lower, upper)]
  
  progressUpdate(subtaskprogress = 1, subtask = "Stop Duration", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  return(database[, .(BusID, StopID, DTAZ, Activity, StopLocType, Vehicle, StopDuration, choice, lower, upper)])

}
