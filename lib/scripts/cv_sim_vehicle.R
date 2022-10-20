
# Commercial Vehicle Choice Simulation
cv_sim_vehicle <- function(database, model, firms, skims) {

  progressUpdate(subtaskprogress = 0, subtask = "Vehicle Choice", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Add firm details
  database[firms, c("EmpCatGroupedName", "TAZ") := .(i.EmpCatGroupedName, i.TAZ), on = "BusID"]
  
  # Look up firm to stop distance
  database[skims[, .(TAZ = OTAZ, DTAZ, dist)], dist := i.dist, on = c("TAZ", "DTAZ")]

  # Recode distance categories
  database[, dist_00_02 := (dist <= 2) * 1]
  database[, dist_02_05 := (dist > 2 ) * (dist <= 5 ) * 1]
  database[, dist_02_10 := (dist > 2 ) * (dist <= 10) * 1]
  database[, dist_05_10 := (dist > 5 ) * (dist <= 10) * 1]
  database[, dist_10_20 := (dist > 10) * (dist <= 20) * 1]
  database[, dist_20_p  := (dist > 20) * 1]
  
  # Recode employment categories
  database[, industry_retail := 1 * (EmpCatGroupedName %in% c('Retail'))]
  database[, industry_wholesale := 1 * (EmpCatGroupedName %in% c('Wholesale'))]
  database[, industry_construction := 1 * (EmpCatGroupedName %in% c("Construction"))]
  database[, industry_transport_industry := 1 * (EmpCatGroupedName %in% c("Transport_Industry"))]
  database[, industry_admin_support_waste := 1 * (EmpCatGroupedName %in% c("Admin_Support_Waste"))]
  database[, industry_ed_health_social_public := 1 * (EmpCatGroupedName %in% c("Ed_Health_Social_Public"))]
  database[, industry_service_other := 1 * (EmpCatGroupedName %in% c("Service_Other"))]
  database[, industry_office_professional := 1 * (EmpCatGroupedName %in% c("Office_Professional"))]
  database[, industry_service_foodDrink := 1 * (EmpCatGroupedName %in% c("Service_FoodDrink"))]
  
  # Recode activity categories
  database[, activity_deliver_pickup := 1 * (Activity == "Goods")]
  database[, activity_service := 1 * (Activity == "Service")]
  
  # Code availability
  database[, av_light := 1]
  database[, av_medium := 1]
  database[, av_heavy  := 1]
  
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
      V[['light']]  = 
        asc_light
        beta_v1_industry_retail *	industry_retail +
        beta_v1_industry_wholesale * industry_wholesale +
        beta_v1_industry_construction *	industry_construction +
        beta_v1_industry_transport_industry *	industry_transport_industry +
        beta_v1_industry_admin_support_waste * industry_admin_support_waste +
        beta_v1_industry_ed_health_social_public *	industry_ed_health_social_public +
        beta_v1_industry_service_other * industry_service_other + 
        beta_v1_industry_office_professional *	industry_office_professional +
        beta_v1_industry_service_foodDrink * industry_service_foodDrink + 
        beta_v1_activity_deliver_pickup * activity_deliver_pickup +
        beta_v1_activity_service * activity_service +
        beta_v1_dist_00_02 * dist_00_02 + 
        beta_v1_dist_02_05 * dist_02_05 + 
        beta_v1_dist_05_10 * dist_05_10 + 
        beta_v1_dist_10_20 * dist_10_20 + 
        beta_v1_dist_20_p  * dist_20_p 
      
      V[['medium']] = 
        asc_medium + 
        beta_v2_industry_retail *	industry_retail +
        beta_v2_industry_wholesale * industry_wholesale +
        beta_v2_industry_construction *	industry_construction +
        beta_v2_industry_transport_industry *	industry_transport_industry +
        beta_v2_industry_admin_support_waste * industry_admin_support_waste +
        beta_v2_industry_ed_health_social_public *	industry_ed_health_social_public +
        beta_v2_industry_service_other * industry_service_other + 
        beta_v2_industry_office_professional *	industry_office_professional +
        beta_v2_industry_service_foodDrink * industry_service_foodDrink +
        beta_v2_activity_deliver_pickup * activity_deliver_pickup +
        beta_v2_activity_service * activity_service +
        beta_v2_dist_00_02 * dist_00_02 + 
        beta_v2_dist_02_05 * dist_02_05 + 
        beta_v2_dist_05_10 * dist_05_10 + 
        beta_v2_dist_10_20 * dist_10_20 + 
        beta_v2_dist_20_p  * dist_20_p 
      
      V[['heavy']]  = asc_heavy
      
      ### Define settings for MNL model component
      mnl_settings = list(
        alternatives = c(light = 1, medium = 2, heavy = 3),
        avail        = list(light = av_light, medium = av_medium, heavy = av_heavy),
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
  
  progressUpdate(subtaskprogress = 0.5, subtask = "Vehicle Choice", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Calculate cumulative probabilities and simulate
  prediction[, medium := light + medium]
  prediction[, heavy  := 1]
  set.seed(BASE_SEED_VALUE)
  prediction[, TMP_RAND := runif(.N)]
  prediction[, Vehicle := factor(ifelse(TMP_RAND < light, 1, 
                                        ifelse(TMP_RAND < medium, 2, 3)),
                                levels = 1:3, 
                                labels = c("Light", "Medium", "Heavy"), 
                                ordered = TRUE)]
  
  # Add choice to database
  database[prediction, Vehicle := i.Vehicle, on = "ID"]
  
  progressUpdate(subtaskprogress = 1, subtask = "Vehicle Choice", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  return(database[, .(BusID, StopID, DTAZ, Activity, Vehicle)])

}
