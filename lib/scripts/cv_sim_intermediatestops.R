
# Commercial Vehicle Intermediate Stop Simulation
cv_sim_intermediatestops <- function(database, firms, skims_tod, 
                                     model, cv_intermediate_attraction_model, 
                                     cv_stopduration_model, deviance.threshold, 
                                     intstop.deviations, TAZLandUseCVTM) {

  progressUpdate(subtaskprogress = 0, subtask = "Intermediate Stops", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  ### Predict any intermediate stops
  
  # Add firm details
  database[firms, c("EmpCatName", "TAZ") := .(i.EmpCatName, i.TAZ), on = "BusID"]
  
  # Look up firm to stop distance
  database[skims_tod[, .(TAZ = OTAZ, DTAZ, dist.avg)], distToEst := i.dist.avg, on = c("TAZ", "DTAZ")]
  
  # Calculate remaining scheduled stop duration
  database[, RemainingDuration := sum(StopDuration) - cumsum(StopDuration) + StopDuration, by = .(BusID, Vehicle, TourID)]
  
  # Transform the variables
  database[, log_duration := log(1 + RemainingDuration)]
  database[, log_distance := log(1 + distToEst)]
  database[, distance_1_2 := 1 * (distToEst > 1 & distToEst <= 2)]
  database[, distance_2_5 := 1 * (distToEst > 2 & distToEst <= 5)]
  database[, distance_5_10 := 1 * (distToEst > 5 & distToEst <= 10)]
  database[, distance_10plus := 1 * (distToEst > 10)]
  database[, distance_2_10 := 1 * (distToEst > 2 & distToEst <= 10)]
  database[, distance_1_10 := 1 * (distToEst > 1 & distToEst <= 10)]
  database[, distance_2plus := 1 * (distToEst > 2)]
  database[, duration_0 := 1 * (RemainingDuration == 0)]
  
  # Trip departing around lunch time? (11:30am to 12:30pm)
  database[, is_next_lunch_time := (MAMDepart >= 690 & MAMDepart <= 750)*1L]
  
  # Recode vehicle type categories
  database[, is_med_veh := 1 * (Vehicle == "Medium")]
  database[, is_hvy_veh := 1 * (Vehicle == "Heavy")]
  
  # Code availability
  database[, av_ns := 1]
  database[, av_dn := 1]
  database[, av_vs := 1]
  database[, av_ot := 1]
  
  # Add ID
  database[, ID := BusID]
  
  database[, apollo_sequence := TripID]
  
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
      
      V[['ns']] = asc_ns
      
      V[['dn']] = asc_dn + 
        b_log_duration_dn * log_duration + 
        b_is_lunch_dn * is_next_lunch_time + 
        b_distance_1_2_dn * distance_1_2 + 
        b_distance_2_10_dn * distance_2_10 + 
        b_distance_10plus_dn * distance_10plus
      
      V[['vs']] = asc_vs + 
        b_log_duration_vs * log_duration + 
        b_med_veh_vs * is_med_veh + 
        b_hvy_veh_vs * is_hvy_veh + 
        b_distance_1_10_vs * distance_1_10 + 
        b_distance_10plus_vs * distance_10plus
      
      V[['ot']] = asc_ot + 
        b_log_duration_ot * log_duration + 
        b_med_veh_ot * is_med_veh + 
        b_hvy_veh_ot * is_hvy_veh + 
        b_distance_1_2_ot * distance_1_2 + 
        b_distance_2plus_ot * distance_2plus
      
      ### Define settings for MNL model component
      mnl_settings = list(
        alternatives = c(
          ns = 1,
          dn = 2,
          vs = 3,
          ot = 4),
        avail = 
          list(
            ns = av_ns,
            dn = av_dn,
            vs = av_vs,
            ot = av_ot),
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
  
  progressUpdate(subtaskprogress = 0.1, subtask = "Intermediate Stops", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Apply the model
  prediction <- apollo_prediction(model,
                                  apollo_probabilities,
                                  apollo_inputs)
  
  # Calculate the cumulative probability for the intermediate stops
  database[, c("P_None", "P_Break", "P_Refuel", "P_Other") := 
             data.table(prediction[,c("ns", "dn", "vs", "ot")])]
  
  database[, CDF_None   := P_None]
  database[, CDF_Break  := P_None + P_Break]
  database[, CDF_Refuel := P_None + P_Break + P_Refuel]
  database[, CDF_Other  := P_None + P_Break + P_Refuel + P_Other]
  
  progressUpdate(subtaskprogress = 0.2, subtask = "Intermediate Stops", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Simulate intermediate stops until it is chosen to proceed to the next scheduled stop
  database[, proceedToNextStop := 0L]
  database[, IntStop_1 := factor(NA)]
  database[, IntStop_2 := factor(NA)]
  database[, IntStop_3 := factor(NA)]
  
  cc <- 1
  
  while(TRUE) {
    
    # Random draw
    set.seed(BASE_SEED_VALUE)
    database[proceedToNextStop == 0L, Draw := runif(.N)]
    database[Draw < CDF_None, proceedToNextStop := 1L]
    
    # If all trips have been determined to go to next stop, exit loop
    if (all(database[, proceedToNextStop] == 1L) | cc == 4) break
    
    # Otherwise, assign an intermediate stop
    database[Draw >= CDF_None   & Draw < CDF_Break,  paste0("IntStop_", cc) := "Break/Meal"]
    database[Draw >= CDF_Break  & Draw < CDF_Refuel, paste0("IntStop_", cc) := "Vehicle Service"]
    database[Draw >= CDF_Refuel & Draw < CDF_Other,  paste0("IntStop_", cc) := "Other"]
    
    # Remove this stop from subsequent choices
    database[eval(parse(text = paste0("IntStop_", cc))) == "Break/Meal", P_Break := 0]
    database[eval(parse(text = paste0("IntStop_", cc))) == "Vehicle Service", P_Refuel := 0]
    database[eval(parse(text = paste0("IntStop_", cc))) == "Other", P_Other := 0]
  
    # Renormalize probabilities to account for reduced alternative sets
    database[proceedToNextStop == 0L, P_Sum := (P_None + P_Break + P_Refuel + P_Other)]
    database[proceedToNextStop == 0L, P_None   := P_None   / P_Sum]
    database[proceedToNextStop == 0L, P_Break  := P_Break  / P_Sum]
    database[proceedToNextStop == 0L, P_Refuel := P_Refuel / P_Sum]
    database[proceedToNextStop == 0L, P_Other  := P_Other  / P_Sum]
    database[proceedToNextStop == 0L, CDF_None   := P_None]
    database[proceedToNextStop == 0L, CDF_Break  := P_None + P_Break]
    database[proceedToNextStop == 0L, CDF_Refuel := P_None + P_Break + P_Refuel]
    database[proceedToNextStop == 0L, CDF_Other  := P_None + P_Break + P_Refuel + P_Other]
    
    cc <- cc + 1
    
  }
  
  progressUpdate(subtaskprogress = 0.4, subtask = "Intermediate Stops", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Count number of intermediate stops
  database[, NintStops := (!is.na(IntStop_1))*1L + (!is.na(IntStop_2))*1L + (!is.na(IntStop_3))*1L]
  
  # Build tables of scheduled trips and an enumerated table of intermediate trips
  scheduledTrips <- database[,.(BusID, EmpCatName, TAZ, Vehicle, TourID, TripID, 
                                NintStops, OTAZ, DTAZ, Activity, StopDuration, 
                                TravelTime, Distance, MAMArrive, MAMDepart,
                                TourType, TourStartEndLoc)]
  
  intTrips <- database[rep(1:.N, times = NintStops), .(BusID, TAZ, EmpCatName, Vehicle, TourID, TripID, 
                                                       OTAZ, StopTAZ = DTAZ, DTAZ = NA, Activity, 
                                                       NintStops, IntStop_1, IntStop_2, IntStop_3, 
                                                       TourType, TourStartEndLoc)]
  
  # Update activities of intermediate stops
  intTrips[, StopID := .I]
  
  intTrips[, MinID := min(StopID), by = .(BusID, Vehicle, TourID, TripID)]
  intTrips[, MaxID := max(StopID), by = .(BusID, Vehicle, TourID, TripID)]
  
  intTrips[NintStops == 1L & StopID == MinID, Activity := IntStop_1]
  intTrips[NintStops == 2L & StopID == MinID, Activity := IntStop_1]
  intTrips[NintStops == 2L & StopID == MinID + 1L, Activity := IntStop_2]
  intTrips[NintStops == 3L & StopID == MinID, Activity := IntStop_1]
  intTrips[NintStops == 3L & StopID == MinID + 1L, Activity := IntStop_2]
  intTrips[NintStops == 3L & StopID == MinID + 2L, Activity := IntStop_3]
  
  # Add intermediate trip ID
  intTrips[, IntID := 1:.N, by = .(BusID, Vehicle, TourID, TripID)]
  
  # Some variables are unknown for these intermediate stops - set to NA
  intTrips[, OTAZ := ifelse(NintStops > 1L & IntID > 1L, NA, OTAZ)]
  
  # Predict intermediate stop durations
  intTrips[, StopDuration := cv_sim_stopduration(database = intTrips[,.(BusID, StopID, DTAZ, Activity, Vehicle)], 
                                                 firms = firms,
                                                 model = cv_stopduration_model)$StopDuration]
  
  progressUpdate(subtaskprogress = 0.5, subtask = "Intermediate Stops", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Create a table of all trips
  allTrips <- rbind(scheduledTrips[, Scheduled := 1L],
                    intTrips[,.(BusID, EmpCatName, TAZ, Vehicle, TourID, TripID, IntID,
                               NintStops, OTAZ, DTAZ, StopTAZ,
                               Activity, StopDuration, TourType, 
                               TourStartEndLoc, IntStop_1, IntStop_2, IntStop_3)][, Scheduled := 0L], 
                    fill = TRUE, 
                    use.names = TRUE)[order(BusID, Vehicle, TourID, TripID, IntID)]
  
  # The OTAZ is now unknown for scheduled stops preceeded by an intermediate stop
  allTrips[, OTAZ := ifelse(NintStops > 0L & Scheduled == 1L, NA, OTAZ)]
  
  ### Loop through sequential intermediate stops and:
  #
  # 1 - predict zone that the first intermediate stop takes place in
  # 2 - this destination becomes the origin for the next intermediate stop
  # 3 - repeat, until all intermediate stops have origins and destination zones
  
  # Build a table of just intermediate stops
  intStopZone <- allTrips[!is.na(IntID), .(BusID, Vehicle, TourID, 
                                           TripID, IntID, NintStops, 
                                           OTAZ, DTAZ, StopTAZ, Activity)]
  
  setkey(intStopZone, BusID, Vehicle, TourID, TripID, IntID)
  
  intStop.attr <- cv_intermediate_attraction_model$estimate[cv_intermediate_attraction_model$estimate != 0]
  
  # Increase the deviance threshold if it is too tight and leaves TAZs disconnected
  skims_tod[, min.distance := max(min(dist.avg), deviance.threshold), by = OTAZ]
  
  progressUpdate(subtaskprogress = 0.6, subtask = "Intermediate Stops", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  cc <- 1L
  while (any(intStopZone[, is.na(DTAZ)])) {
    
    # Get distance to next scheduled stop
    tempDT <- merge(intStopZone[!is.na(OTAZ) & is.na(DTAZ), !"DTAZ", with = FALSE], 
                    skims_tod[, .(OTAZ, StopTAZ = DTAZ, D_ij = dist.avg)],
                    by = c("OTAZ", "StopTAZ"))
    
    # Get skims to possible intermediate zones
    tempDT <- merge(tempDT, 
                    skims_tod[DTAZ %in% BASE_TAZ_INTERNAL & dist.avg <= min.distance, 
                              .(OTAZ, DTAZ, T_ik = time.avg, D_ik = dist.avg)],
                    by = "OTAZ", allow.cartesian = TRUE)
    
    # Get skims from possible intermediate zones to next scheduled stop zone
    tempDT[skims_tod[, .(DTAZ = OTAZ, StopTAZ = DTAZ, time.avg, dist.avg)],
           c("T_kj", "D_kj") := .(i.time.avg, i.dist.avg),
           on = c("DTAZ", "StopTAZ")]
    
    # Calculate total travel diversion
    tempDT[, T_ikj := T_ik + T_kj]
    tempDT[, D_ikj := D_ik + D_kj]
    tempDT[, DeltaD_k := D_ikj - D_ij]
    
    # Remove options that stretch the rubber band too far
    # Retain at least one option for each stop even if it is greater than the threshold
    tempDT[, below_dev_thresh := ifelse(DeltaD_k < deviance.threshold,1,0)]
    
    # Check the remaining number of acceptable locations and filter
    tempDT[, num_below_dev_thresh := sum(below_dev_thresh), by = .(BusID, Vehicle, TourID, TripID, IntID)]
    tempDT <- tempDT[below_dev_thresh == 1 | num_below_dev_thresh == 0]
    
    # Get zone attributes
    tempDT[TAZLandUseCVTM[,.(DTAZ = TAZ, NEmp_Leisure, NEmp_Retail, NEmp_Total)],
           c("Leisure", "Retail", "TotalEmp") := .(i.NEmp_Leisure, i.NEmp_Retail, i.NEmp_Total),
           on = "DTAZ"]
    
    # Calculate attraction factor
    tempDT[intstop.deviations, deviation.dist := i.deviation.dist, on = "Vehicle"]
    
    # Apply the attraction model (manually, not using apollo::prediction)
    tempDT[, u := intStop.attr["b_emp"] * TotalEmp/1000 +
                  (intStop.attr["b_dist"] + 
                     intStop.attr["b_dist_vs"] * (Activity == "Vehicle Service")) * DeltaD_k/0.25 + 
                  intStop.attr["b_retail"] * Retail/1000 +
                  intStop.attr["b_leisure_ot"] * (Activity == "Other") * Leisure/1000]
    tempDT[, eu := exp(u)]
    tempDT[, p := eu/sum(eu), by = .(BusID, Vehicle, TourID, TripID, IntID)]
    
    # If all attraction probabilities are 0, make them 1
    setkey(tempDT, BusID, Vehicle, TourID, TripID, IntID)
    tempDT[tempDT[, .(allZeroes = .N == sum(p == 0)), 
                  by = .(BusID, Vehicle, TourID, TripID, IntID)][allZeroes == TRUE], 
           p := 1]
    
    # Sample zones for each intermediate stop
    set.seed(BASE_SEED_VALUE)
    
    sampleDTAZ <- function(SD){
      if(length(SD$DTAZ) == 1) { #assign DTAZ
        DTAZ <- SD$DTAZ
      } else { #select DTAZ by weighted sampling
        DTAZ <- sample(SD$DTAZ, size = 1, prob = SD$p)
      }
    }
    
    SelectedZones <- tempDT[, .(DTAZ = sampleDTAZ(.SD)), 
                            by = .(BusID, Vehicle, TourID, TripID, IntID), 
                            .SDcols = c("DTAZ", "p")]
    
    setkey(SelectedZones, BusID, Vehicle, TourID, TripID, IntID)
    
    # Assign intermediate stop zone
    intStopZone[SelectedZones, DTAZ := as.numeric(SelectedZones[["DTAZ"]])]
    
    # Assign origin of next intermediate stop as the destination of the current intermediate stop
    intStopZone[intStopZone[IntID == cc + 1L & NintStops > cc, 
                            .(BusID, Vehicle, TourID, TripID, IntID)],
                OTAZ := intStopZone[IntID == cc][NintStops > cc][["DTAZ"]]]
    
    cc <- cc + 1L
    
  }
  
  progressUpdate(subtaskprogress = 0.75, subtask = "Intermediate Stops", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Join intermediate trip details to full table of trips
  allTrips[intStopZone, 
           c("OTAZ", "DTAZ") := .(i.OTAZ, i.DTAZ), 
           on = .(BusID, Vehicle, TourID, TripID, IntID)]
  
  # Resequence TripID
  setorder(allTrips, BusID, Vehicle, TourID, TripID, IntID, na.last = TRUE)
  allTrips[, TripID := 1:.N, by = .(BusID, Vehicle, TourID)]
  allTrips[, StopTAZ := NULL]
  
  # Assign origin of scheduled stops as the destination of the previous intermediate stop, if applicable
  setkey(allTrips, BusID, Vehicle, TourID, TripID, IntID)
  allTrips[Scheduled == 1L & NintStops > 0L,
           OTAZ := allTrips[allTrips[Scheduled == 1L][NintStops > 0L, 
                                                      .(BusID, Vehicle, TourID, 
                                                        TripID = TripID - 1)]][["DTAZ"]]]
  allTrips[, IntID := NULL]
  
  ### Update trip departures and arrivals given new intermediate stops
  # Arrival time at first scheduled stop is the only known time
  allTrips[Scheduled == 1L, firstScheduled := 1L*(TripID == min(TripID)), by = .(BusID, Vehicle, TourID)]
  allTrips[is.na(firstScheduled), firstScheduled := 0L]
  
  ### Re-time tours
  # Put skims in extra-long format -- select just the records required based on the trip ods we are simulating
  tod.ranges <- attr(skims_tod, "tod.ranges")
  tod.right <- attr(skims_tod, "tod.right")
  
  od <- unique(allTrips[,.(OTAZ, DTAZ)])
  setkey(od, OTAZ, DTAZ)
  setkey(skims_tod, OTAZ, DTAZ)
  skims_tod_od <- skims_tod[od]
  skims.long <- meltSkimTableByTOD(skims_tod_od, tod_names = names(tod.ranges))
  
  # Some tour have intermediate stops before their first scheduled stop
  # These need to be separated and timed "backwards" at first
  allTrips[, HasEarlyIntermediateStops := 
             if (TripID[as.logical(firstScheduled)] != 1L) {TRUE} else {FALSE}, 
           by = .(BusID, Vehicle, TourID)]
  
  allTrips.HasEarlyStops <- allTrips[HasEarlyIntermediateStops == TRUE,  
                                     !"HasEarlyIntermediateStops", 
                                     with = FALSE]
  
  allTrips.NoEarlyStops  <- allTrips[HasEarlyIntermediateStops == FALSE, 
                                     !"HasEarlyIntermediateStops", 
                                     with = FALSE]
  
  # TOD and timing depends on whether times are based off start, middle, or end of trip
  timefactor <- c(0, 0.5, 1)[match(BASE_TIME_PERIOD_TRIP_POINT, c("START", "MIDDLE", "END"))]
  
  # Correct the travel times and distances for the first trip to the scheduled stop for the no early stops table
  allTrips.NoEarlyStops[TripID == 1, TOD := ifelse(MAMArrive - (1 - timefactor) * TravelTime >= 0,
                                            getSkimTOD(time = MAMArrive - (1 - timefactor) * TravelTime, 
                                                       tod.ranges = tod.ranges, 
                                                       tod.right = tod.right),
                                            getSkimTOD(time = MAMArrive - (1 - timefactor) * TravelTime + 1440, 
                                                       tod.ranges = tod.ranges, 
                                                       tod.right = tod.right))]
  
  allTrips.NoEarlyStops[skims.long[, .(TripID = 1, OTAZ, DTAZ, TOD, dist, time)],
                         c("TravelTime", "Distance", "MAMDepart") := .(i.time, i.dist, MAMArrive - i.time),
                         on = c("TripID", "OTAZ", "DTAZ", "TOD")]
  
  # Trips that have intermediate stops before their first scheduled stop
  if (allTrips.HasEarlyStops[, .N] > 0) {
    
    # How many trips back do we need to go?
    MaxEarlyTrips <- allTrips.HasEarlyStops[, .(NumEarlyTrips = TripID[firstScheduled == 1L] - 1L),
                                            by = .(BusID, Vehicle, TourID)][, max(NumEarlyTrips)]
    
    # Create a "backwards" TripID with the first scheduled trip as trip 0
    allTrips.HasEarlyStops[, BackTripID := max(TripID) - TripID + 1L, 
                           by = .(BusID, Vehicle, TourID)]
    
    allTrips.HasEarlyStops[, BackTripID := BackTripID - BackTripID[firstScheduled == 1L], 
                           by = .(BusID, Vehicle, TourID)]
    
    for (i in 1:MaxEarlyTrips) {
      
      allTrips.HasEarlyStops[allTrips.HasEarlyStops[BackTripID == i - 1L, .(BusID, Vehicle, TourID,
                                                                            BackTripID = BackTripID + 1L,
                                                                            OTAZ, DTAZ,
                                                                            NextStopDepart = MAMDepart)],
                             MAMArrive := i.NextStopDepart - StopDuration,
                             on = c("BusID", "Vehicle", "TourID", "BackTripID")]
      
      # get initial estimate of TOD based on MAMArrive
      allTrips.HasEarlyStops[BackTripID == i, TOD := getSkimTOD(time = MAMArrive, 
                                                                tod.ranges = tod.ranges, 
                                                                tod.right = tod.right)]
      
      allTrips.HasEarlyStops[skims.long[, .(BackTripID = i, OTAZ, DTAZ, TOD, dist, time)],
                             c("TravelTime", "Distance", "MAMDepart") := .(i.time, i.dist, MAMArrive - i.time),
                             on = c("BackTripID", "OTAZ", "DTAZ", "TOD")]
      
      # Adjust TOD based on BASE_TIME_PERIOD_TRIP_POINT now that we have an estimate of the travel time
      allTrips.HasEarlyStops[BackTripID == i, TOD := ifelse(MAMArrive - (1 - timefactor) * TravelTime >= 0,
                                                            getSkimTOD(time = MAMArrive - (1 - timefactor) * TravelTime, 
                                                                       tod.ranges = tod.ranges, 
                                                                       tod.right = tod.right),
                                                            getSkimTOD(time = MAMArrive - (1 - timefactor) * TravelTime + 1440, 
                                                                       tod.ranges = tod.ranges, 
                                                                       tod.right = tod.right))]
      
      allTrips.HasEarlyStops[skims.long[, .(BackTripID = i, OTAZ, DTAZ, TOD, dist, time)],
                             c("TravelTime", "Distance", "MAMDepart") := .(i.time, i.dist, MAMArrive - i.time),
                             on = c("BackTripID", "OTAZ", "DTAZ", "TOD")]
      
      
    }
    
    allTrips.HasEarlyStops[, BackTripID := NULL]
    
    allTrips <- rbind(allTrips.NoEarlyStops, allTrips.HasEarlyStops)
    setkey(allTrips, TourID, TripID)
  }
  
  # Calculate arrival and depart times for subsequent stops
  for (i in 2:allTrips[, max(TripID)]) {
    allTrips[allTrips[TripID == i - 1L, .(BusID, Vehicle, TourID, TripID = i,
                                          OTAZ, DTAZ,
                                          LastStopMAMArrive = MAMArrive,
                                          LastStopDuration = StopDuration)],
             MAMDepart := i.LastStopMAMArrive + i.LastStopDuration,
             on = c("BusID", "Vehicle", "TourID", "TripID")]
    
    # get initial estimate of TOD based on MAMDepart
    allTrips[TripID == i, TOD := getSkimTOD(time = MAMDepart, 
                                            tod.ranges = tod.ranges, 
                                            tod.right = tod.right)]
    
    allTrips[skims.long[, .(TripID = i, OTAZ, DTAZ, TOD, dist, time)],
             c("TravelTime", "Distance", "MAMArrive") := .(i.time, i.dist, MAMDepart + i.time),
             on = c("TripID", "OTAZ", "DTAZ", "TOD")]
    
    # Adjust TOD based on BASE_TIME_PERIOD_TRIP_POINT now that we have an estimate of the travel time
    allTrips[TripID == i, TOD := ifelse(MAMDepart + timefactor * TravelTime < 1440,
           getSkimTOD(time = MAMDepart + timefactor * TravelTime, 
                      tod.ranges = tod.ranges, 
                      tod.right = tod.right),
           getSkimTOD(time = MAMDepart + timefactor * TravelTime - 1440, 
                      tod.ranges = tod.ranges, 
                      tod.right = tod.right))]
    
    allTrips[skims.long[, .(TripID = i, OTAZ, DTAZ, TOD, dist, time)],
             c("TravelTime", "Distance", "MAMArrive") := .(i.time, i.dist, MAMDepart + i.time),
             on = c("TripID", "OTAZ", "DTAZ", "TOD")]
    
  }
  
  tidyDataTable(allTrips, select = c("BusID", "Vehicle", "TourID", "TripID",
                                     "Scheduled", "OTAZ", "DTAZ", "Activity",
                                     "MAMDepart", "MAMArrive", "TravelTime",
                                     "Distance", "StopDuration", "TOD",
                                     "TourType", "TourStartEndLoc"),
                key = c("BusID", "Vehicle", "TourID", "TripID"))
  
  progressUpdate(subtaskprogress = 1, subtask = "Intermediate Stops", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  return(allTrips)

}
