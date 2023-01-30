
# Commercial Vehicle Scheduled Trip Simulation
cv_sim_scheduledtrips <- function(firmTourSequence, firms, skims_tod, model) {

  progressUpdate(subtaskprogress = 0, subtask = "Arrival Time", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Convert tour sequences into trips
  scheduledTrips <- merge(firmTourSequence[, .(BusID, Vehicle, TourID, SequenceID, OTAZ = StopTAZ)],
                          firmTourSequence[, .(BusID, Vehicle, TourID, SequenceID = SequenceID - 1L, DTAZ = StopTAZ, Activity, StopLocType, StopDuration, TourType, TourStartEndLoc)],
                          by = c("BusID", "Vehicle", "TourID", "SequenceID"))
  
  # Drop Start Tour activity level
  scheduledTrips[, Activity := factor(Activity)]
  
  # Set TripID
  setnames(scheduledTrips, "SequenceID", "TripID")
  
  # Get average distance and travel time since the arrival window is still unknown
  scheduledTrips[skims_tod, 
                 c("Distance", "TravelTime") := .(i.dist.avg, i.time.avg), 
                 on = c("OTAZ", "DTAZ")]
  
  progressUpdate(subtaskprogress = 0.2, subtask = "Arrival Time", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Apply arrival time model to each tour
  database <- scheduledTrips[, .(TotalStopDuration = sum(StopDuration)), by = .(BusID, Vehicle, TourID, TourType)]
  
  # Add activity at the first stop on the tour
  database[scheduledTrips[TripID == 1, .(Activity, BusID, Vehicle, TourID)],
           Activity := i.Activity,
           on = c("BusID", "Vehicle", "TourID")]
  
  # Recode tour type characteristics
  database[, begin_not_base := 1 * (substr(TourType,1,1) == "n")]
  database[, end_not_base := 1 * (substr(TourType,2,2) == "n")]
  database[, is_single_stop := 1 * (substr(TourType,3,3) == "s")]
  database[, is_no_stop := 1 * (substr(TourType,3,3) == "0")]
  
  # Recode vehicle type categories
  database[, is_med_veh := 1 * (Vehicle == "Medium")]
  database[, is_hvy_veh := 1 * (Vehicle == "Heavy")]
  database[, is_med_hvy_veh := 1 * (Vehicle %in% c("Medium", "Heavy"))]
 
  # Code availability
  database[, av_overnight := 1]
  database[, av_0600 := 1]
  database[, av_0700 := 1]
  database[, av_0730 := 1]
  database[, av_0800 := 1]
  database[, av_0830 := 1]
  database[, av_0900 := 1]
  database[, av_0930 := 1]
  database[, av_1000 := 1]
  database[, av_1030 := 1]
  database[, av_1100 := 1]
  database[, av_1130 := 1]
  database[, av_1200 := 1]
  database[, av_1230 := 1]
  database[, av_1300 := 1]
  database[, av_1330 := 1]
  database[, av_1400 := 1]
  database[, av_1430 := 1]
  database[, av_1500 := 1]
  database[, av_1530 := 1]
  database[, av_1600 := 1]
  database[, av_1630 := 1]
  database[, av_1700 := 1]
  database[, av_1730 := 1]
  database[, av_1800 := 1]
  database[, av_1900 := 1]
  database[, av_2000 := 1]
  database[, av_2100 := 1]
  
  # Add ID
  database[, ID := BusID]
  
  database[, apollo_sequence := TourID]
  
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
      
      V[['overnight']] = asc_overnight
      V[['a_0600']]    = asc_0600 + asc_is_med_hvy_veh_0600 * is_med_hvy_veh + asc_is_no_stop_0600_0730 * is_no_stop 	   
      V[['a_0700']]    = asc_0700 + asc_is_med_hvy_veh_0700 * is_med_hvy_veh + asc_is_no_stop_0600_0730 * is_no_stop 	   
      V[['a_0730']]    = asc_0730 + asc_is_med_hvy_veh_0730 * is_med_hvy_veh + asc_is_no_stop_0600_0730 * is_no_stop 	   
      V[['a_0800']]    = asc_0800 + asc_is_med_hvy_veh_0800 * is_med_hvy_veh  						   
      V[['a_0830']]    = asc_0830 + asc_is_med_hvy_veh_0830 * is_med_hvy_veh  					 	   
      V[['a_0900']]    = asc_0900 + asc_is_med_hvy_veh_0900 * is_med_hvy_veh  					 							+ asc_begin_not_base_0900_1630 * begin_not_base	   
      V[['a_0930']]    = asc_0930 + asc_is_med_hvy_veh_0930 * is_med_hvy_veh  												+ asc_begin_not_base_0900_1630 * begin_not_base 	   
      V[['a_1000']]    = asc_1000 + asc_is_med_hvy_veh_1000 * is_med_hvy_veh  					 							+ asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1030']]    = asc_1030 + asc_is_med_hvy_veh_1030 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1100']]    = asc_1100 + asc_is_med_hvy_veh_1100 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1130']]    = asc_1130 + asc_is_med_hvy_veh_1130 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1200']]    = asc_1200 + asc_is_med_hvy_veh_1200 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1230']]    = asc_1230 + asc_is_med_hvy_veh_1230 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1300']]    = asc_1300 + asc_is_med_hvy_veh_1300 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1330']]    = asc_1330 + asc_is_med_hvy_veh_1330 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1400']]    = asc_1400 + asc_is_med_hvy_veh_1400 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1430']]    = asc_1430 + asc_is_med_hvy_veh_1430 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1500']]    = asc_1500 + asc_is_med_hvy_veh_1500 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1530']]    = asc_1530 + asc_is_med_hvy_veh_1530 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1600']]    = asc_1600 + asc_is_med_hvy_veh_1600 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1630']]    = asc_1630 + asc_is_med_hvy_veh_1630 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop + asc_begin_not_base_0900_1630 * begin_not_base  	   
      V[['a_1700']]    = asc_1700 + asc_is_med_hvy_veh_1700 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop 						 	 + asc_end_not_base_1700_2100 * end_not_base 	   
      V[['a_1730']]    = asc_1730 + asc_is_med_hvy_veh_1730 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop 							 + asc_end_not_base_1700_2100 * end_not_base 	   
      V[['a_1800']]    = asc_1800 + asc_is_med_hvy_veh_1800 * is_med_hvy_veh + asc_single_stop_1030_1800 * is_single_stop 							 + asc_end_not_base_1700_2100 * end_not_base 	   
      V[['a_1900']]    = asc_1900 + asc_is_med_hvy_veh_1900_2100 * is_med_hvy_veh												 + asc_end_not_base_1700_2100 * end_not_base 	   
      V[['a_2000']]    = asc_2000 + asc_is_med_hvy_veh_1900_2100 * is_med_hvy_veh												 + asc_end_not_base_1700_2100 * end_not_base 	   
      V[['a_2100']]    = asc_2100 + asc_is_med_hvy_veh_1900_2100 * is_med_hvy_veh												 + asc_end_not_base_1700_2100 * end_not_base 	   
      
      ### Define settings for MNL model component
      mnl_settings = list(
        alternatives = c(
          overnight = 1,
          a_0600 = 2,
          a_0700 = 3,
          a_0730 = 4,
          a_0800 = 5,
          a_0830 = 6,
          a_0900 = 7,
          a_0930 = 8,
          a_1000 = 9,
          a_1030 = 10,
          a_1100 = 11,
          a_1130 = 12,
          a_1200 = 13,
          a_1230 = 14,
          a_1300 = 15,
          a_1330 = 16,
          a_1400 = 17,
          a_1430 = 18,
          a_1500 = 19,
          a_1530 = 20,
          a_1600 = 21,
          a_1630 = 22,
          a_1700 = 23,
          a_1730 = 24,
          a_1800 = 25,
          a_1900 = 26,
          a_2000 = 27,
          a_2100 = 28),
        avail        = 
          list(
            overnight = av_overnight,
            a_0600 = av_0600,
            a_0700 = av_0700,
            a_0730 = av_0730,
            a_0800 = av_0800,
            a_0830 = av_0830,
            a_0900 = av_0900,
            a_0930 = av_0930,
            a_1000 = av_1000,
            a_1030 = av_1030,
            a_1100 = av_1100,
            a_1130 = av_1130,
            a_1200 = av_1200,
            a_1230 = av_1230,
            a_1300 = av_1300,
            a_1330 = av_1330,
            a_1400 = av_1400,
            a_1430 = av_1430,
            a_1500 = av_1500,
            a_1530 = av_1530,
            a_1600 = av_1600,
            a_1630 = av_1630,
            a_1700 = av_1700,
            a_1730 = av_1730,
            a_1800 = av_1800,
            a_1900 = av_1900,
            a_2000 = av_2000,
            a_2100 = av_2100),
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
  
  progressUpdate(subtaskprogress = 0.4, subtask = "Arrival Time", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Apply the model
  prediction =
    data.table(
      apollo_prediction(
        model,
        apollo_probabilities,
        apollo_inputs))
  
  progressUpdate(subtaskprogress = 0.6, subtask = "Arrival Time", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Simulate an alternative choice for each stop
  cols = names(prediction)[!names(prediction) %in% c("ID", "Observation", "chosen")]
  
  prediction = 
    melt.data.table(
      prediction[,-"chosen"],
      id.vars = c("ID", "Observation"),
      variable.name = "Alternative",
      value.name = "Probability")
  
  prediction[, CumProb := cumsum(Probability), keyby = .(ID, Observation)]
  prediction[, CumProbLower := shift(CumProb, fill = 0), by = .(ID, Observation)]
  
  set.seed(BASE_SEED_VALUE)
  
  database[, TMP_RAND := runif(.N)]
  
  prediction[
    database[,.(ID, Observation = apollo_sequence, TMP_RAND)],
    TMP_RAND := i.TMP_RAND,
    on = c("ID", "Observation")]
  
  database[prediction[TMP_RAND > CumProbLower & TMP_RAND < CumProb, 
                      .(Alternative, ID, apollo_sequence = Observation)], 
           choice := as.integer(i.Alternative),
           on = c("ID", "apollo_sequence")]
  
  database[, 
    model_choice := 
      fcase(
        choice == 1, 'overnight',
        choice == 2, '0600',
        choice == 3, '0700',
        choice == 4, '0730',
        choice == 5, '0800',
        choice == 6, '0830',
        choice == 7, '0900',
        choice == 8, '0930',
        choice == 9, '1000',
        choice == 10, '1030',
        choice == 11, '1100',
        choice == 12, '1130',
        choice == 13, '1200',
        choice == 14, '1230',
        choice == 15, '1300',
        choice == 16, '1330',
        choice == 17, '1400',
        choice == 18, '1430',
        choice == 19, '1500',
        choice == 20, '1530',
        choice == 21, '1600',
        choice == 22, '1630',
        choice == 23, '1700',
        choice == 24, '1730',
        choice == 25, '1800',
        choice == 26, '1900',
        choice == 27, '2000',
        choice == 28, '2100')]
  
  # Simulate a specific minute within the arrival time range
  arrival_bounds <- data.table(choice = 1:length(cols), 
                                lower = c("2200", sapply(strsplit(cols[2:length(cols)], split = "a_"),"[[",2)))
  arrival_bounds[, lower_mam := as.integer(substr(lower,1,2))*60 + as.integer(substr(lower,3,4))]
  arrival_bounds[, upper_mam := shift(lower_mam, -1)]
  arrival_bounds[is.na(upper_mam), upper_mam := lower_mam + 60]
  arrival_bounds[upper_mam < lower_mam, upper_mam := upper_mam + 1440]
  
  database[arrival_bounds, 
           c("lower_mam", "upper_mam") := .(i.lower_mam, i.upper_mam), 
           on = "choice"]
  
  set.seed(BASE_SEED_VALUE)
  database[, MAMArrive := as.mam(round(runif(.N, min = lower_mam, max = upper_mam))), .(lower_mam, upper_mam)]
  
  progressUpdate(subtaskprogress = 0.7, subtask = "Arrival Time", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Put skims in extra-long format -- select just the records required based on the trip ods we are simulating
  tod.ranges <- attr(skims_tod, "tod.ranges")
  tod.right <- attr(skims_tod, "tod.right")
  
  od <- unique(scheduledTrips[,.(OTAZ, DTAZ)])
  setkey(od, OTAZ, DTAZ)
  setkey(skims_tod, OTAZ, DTAZ)
  skims_tod_od <- skims_tod[od]
  skims.long <- meltSkimTableByTOD(skims_tod_od, tod_names = names(tod.ranges))
  
  # Add back to trip table and calculate departure time for the first trip
  scheduledTrips <- merge(scheduledTrips, database[, .(BusID, Vehicle, TourID, TripID = 1L, MAMArrive, choice, lower_mam, upper_mam, model_choice)],
                          by = c("BusID", "Vehicle", "TourID", "TripID"), all.x = TRUE)
  
  scheduledTrips[TripID == 1L, TOD := getSkimTOD(MAMArrive, tod.ranges = tod.ranges, tod.right = tod.right)]
  
  scheduledTrips[skims.long,
                 c("MAMDepart", "TravelTime", "Distance") := .(MAMArrive - i.time, i.time, i.dist),
                 on = c("OTAZ", "DTAZ", "TOD")]
  
  progressUpdate(subtaskprogress = 0.8, subtask = "Arrival Time", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Calculate arrival and depart times for subsequent stops
  for (i in 2:scheduledTrips[, max(TripID)]) {
    
    scheduledTrips[scheduledTrips[TripID == i - 1L, .(BusID, Vehicle, TourID, TripID = i, OTAZ, DTAZ,
                                                      LastStopMAMArrive = MAMArrive,
                                                      LastStopDuration = StopDuration)],
                   MAMDepart := i.LastStopMAMArrive + i.LastStopDuration,
                   on = c("BusID", "Vehicle", "TourID", "TripID")]
    
    scheduledTrips[TripID == i, TOD := getSkimTOD(time = MAMDepart, tod.ranges = tod.ranges, tod.right = tod.right)]
    
    scheduledTrips[skims.long[, .(TripID = i, OTAZ, DTAZ, TOD, dist, time)],
                   c("TravelTime", "Distance", "MAMArrive") := .(i.time, i.dist, MAMDepart + i.time),
                   on = c("TripID", "OTAZ", "DTAZ", "TOD")]
  
  }
  
  scheduledTrips[, TOD := NULL]
  
  # Set variable order
  setcolorder(scheduledTrips, neworder = c("BusID", "Vehicle", "TourID", "TripID", "OTAZ", "DTAZ",
                                           "MAMDepart", "TravelTime", "Distance", "MAMArrive", 
                                           "StopDuration", "Activity", "StopLocType", "TourType", "TourStartEndLoc"))
  
  progressUpdate(subtaskprogress = 1, subtask = "Arrival Time", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  return(scheduledTrips)

}
