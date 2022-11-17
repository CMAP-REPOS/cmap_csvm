
# Function for weighted hierarchical clustering
getTours <- function(dist.mat, weights, branch.limit, ...) {
  
  n <- ncol(dist.mat)
  colnames(dist.mat) <- rownames(dist.mat) <- 1:n
  
  # Vector to hold the final assignments
  final.assignments <- rep(NA, n)
  last.class <- 0
  
  # Initialize with a single branch
  branches <- list(1:n)
  
  # Trim branches until they are under-weight
  go <- TRUE
  cc <- 1
  while (go) {
    
    # Is each branch sufficient? If not, but consists of just one leaf, let it through
    branches.okay <- unlist(lapply(X = branches, FUN = function(ids) sum(weights[ids]) <= branch.limit | length(ids) == 1))
    
    # Assign tours for branches that are done
    if (any(branches.okay)) lapply(X = branches[branches.okay], FUN = function(ids) {final.assignments[ids] <<- last.class + 1; last.class <<- last.class + 1})
    
    # If all remaining branches were okay, stop
    if (all(branches.okay)) break
    
    # Cluster each branch that is over-weight
    clusters <- lapply(X = branches[!branches.okay], FUN = function(ids) hclust(as.dist(dist.mat[ids, ids]), ...))
    
    # Split clusters in two
    clusters <- lapply(X = clusters, FUN = function(cluster) cutree(cluster, 2))
    
    # Get new branches
    branches.next <- list()
    for (cluster in clusters) {
      branches.next <- c(branches.next, list(as.numeric(names(cluster)[cluster == 1])))
      branches.next <- c(branches.next, list(as.numeric(names(cluster)[cluster == 2])))
    }
    
    # Prepare next iteration
    branches <- branches.next
    cc <- cc + 1
    
    if (cc > n) go <- FALSE
    
  }

  if (any(is.na(final.assignments))) warning("Some stops not assigned to tours! Check data.")
  return(as.integer(final.assignments))
  
}

# Cycles a vector left by 'shift' places
shiftleft <- function(x, shift) c(x, x)[(1 + shift):(length(x) + shift)]

# Function for routing a tour
getTourSequence <- function(stop.TAZs, firm.TAZ, time.mat) {
  
  # If there's only one stop, the solution is obvious
  if (length(stop.TAZs) == 1) return(2L)
  
  # Asymmetric distance matrix (time)
  idx <- c(as.character(firm.TAZ), as.character(stop.TAZs))
  time.mat.subset <- time.mat[idx, idx, drop = FALSE]
  
  # Traveling salesman solution
  set.seed(BASE_SEED_VALUE)
  TSP_solution <- solve_TSP(ATSP(time.mat.subset))
  TSP_solution <- as.numeric(TSP_solution)
      
  # Shift the solution so that stop 1 (the establishment) is first
  TSP_solution <- shiftleft(TSP_solution, which(TSP_solution == 1) - 1)
  
  return(order(TSP_solution)[-1])
  
}

# Cluster and sequence stops into tours
cv_sim_tours <- function(firmStopsVehDur, firms, branch.limit, skims, model) {

  progressUpdate(subtaskprogress = 0, subtask = "Tour Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Add firm details
  firmStopsVehDur[firms, c("EmpCatName", "EmpCatGroupedName", "TAZ") := .(as.character(i.EmpCatName), i.EmpCatGroupedName, i.TAZ), on = "BusID"]
  
  # Add branch limits distributions and sample
  branch.limit[, EmpCatName := as.character(EmpCatName)]
  firmStopsVehDur[branch.limit, c("brlim.mu", "brlim.sd") := .(i.brlim.mu, i.brlim.sd), on = "EmpCatName"]
  set.seed(BASE_SEED_VALUE)
  firmStopsVehDur[, branch.limit := qlnorm(runif(1), meanlog = brlim.mu, sdlog = brlim.sd)*60, by = EmpCatName]
  
  # Convert skims to a matrix
  time.mat <- as.matrix(dcast.data.table(data = skims, 
                                         formula = OTAZ~DTAZ, 
                                         value.var = "time")[, -1])
  rownames(time.mat) <- colnames(time.mat)
  
  # Cluster stops into tours based on distance with a total tour stop duration limit of branch.limit hours
  set.seed(BASE_SEED_VALUE)
  firmStopsVehDur[, TourID := getTours(dist.mat = time.mat[as.character(DTAZ), as.character(DTAZ), drop = FALSE],
                                weights = StopDuration, 
                                branch.limit = branch.limit[1]),
                  by = .(BusID, Vehicle)]
  
  progressUpdate(subtaskprogress = 1/3, subtask = "Tour Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Determine the stop sequence for a given tour
  firmStopsVehDur[, SequenceID := getTourSequence(stop.TAZs = DTAZ, 
                                           firm.TAZ = TAZ[1], 
                                           time.mat = time.mat),
                  by = .(BusID, Vehicle, TourID)]
  
  setkey(firmStopsVehDur, BusID, Vehicle, TourID, SequenceID)
  
  progressUpdate(subtaskprogress = 2/3, subtask = "Tour Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Add a tour type to each tour to determine whether it start and ends in the same place
  # and whether start and end location is the firm location 
  
  # Identify each tour as either a single scheduled stop or a multistop
  firmStopsVehDur[, Stops := .N, by = .(BusID, Vehicle, TourID)]
  
  # Create database -- one row for each tour, with characteristics required for model application
  database <- firmStopsVehDur[SequenceID == 2, .(BusID, Vehicle, TourID, EmpCatGroupedName, Stops)]
  
  # Recode vehicle type categories
  database[, is_med_veh := 1 * (Vehicle == "Medium")]
  database[, is_hvy_veh := 1 * (Vehicle == "Heavy")]
  
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
  
  # Code availability
  database[, av_bbm := ifelse(Stops > 1, 1, 0)]
  database[, av_bbs := ifelse(Stops == 1, 1, 0)]
  database[, av_bnm := ifelse(Stops > 1, 1, 0)]
  database[, av_bns := ifelse(Stops == 1, 1, 0)]
  database[, av_nbm := ifelse(Stops > 1, 1, 0)]
  database[, av_nb0 := 0] # Not creating zero stop tours
  database[, av_nbs := ifelse(Stops == 1, 1, 0)]
  database[, av_nnm := ifelse(Stops > 1, 1, 0)]
  
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
    

      V[['bbm']] = asc_bbm + asc_bbm_industry_retail * industry_retail + asc_bbm_industry_wholesale * industry_wholesale + asc_bbm_industry_construction * industry_construction + asc_bbm_industry_transport_industry * industry_transport_industry + asc_bbm_industry_admin_support_waste * industry_admin_support_waste + asc_bbm_industry_ed_health_social_public * industry_ed_health_social_public + asc_bbm_industry_service_other * industry_service_other + asc_bbm_industry_office_professional * industry_office_professional + asc_bbm_industry_service_foodDrink * industry_service_foodDrink + asc_bbm_is_med_veh * is_med_veh + asc_bbm_is_hvy_veh * is_hvy_veh
      V[['bbs']] = asc_bbs + asc_bbs_industry_retail * industry_retail + asc_bbs_industry_wholesale * industry_wholesale + asc_bbs_industry_construction * industry_construction + asc_bbs_industry_transport_industry * industry_transport_industry + asc_bbs_industry_admin_support_waste * industry_admin_support_waste + asc_bbs_industry_ed_health_social_public * industry_ed_health_social_public + asc_bbs_industry_service_other * industry_service_other + asc_bbs_industry_office_professional * industry_office_professional + asc_bbs_industry_service_foodDrink * industry_service_foodDrink + asc_bbs_is_med_veh * is_med_veh + asc_bbs_is_hvy_veh * is_hvy_veh
      V[['bnm']] = asc_bnm + asc_bnm_industry_retail * industry_retail + asc_bnm_industry_wholesale * industry_wholesale + asc_bnm_industry_construction * industry_construction + asc_bnm_industry_transport_industry * industry_transport_industry + asc_bnm_industry_admin_support_waste * industry_admin_support_waste + asc_bnm_industry_ed_health_social_public * industry_ed_health_social_public + asc_bnm_industry_service_other * industry_service_other + asc_bnm_industry_office_professional * industry_office_professional + asc_bnm_industry_service_foodDrink * industry_service_foodDrink + asc_bnm_is_med_veh * is_med_veh + asc_bnm_is_hvy_veh * is_hvy_veh
      V[['bns']] = asc_bns + asc_bns_industry_retail * industry_retail + asc_bns_industry_wholesale * industry_wholesale + asc_bns_industry_construction * industry_construction + asc_bns_industry_transport_industry * industry_transport_industry + asc_bns_industry_admin_support_waste * industry_admin_support_waste + asc_bns_industry_ed_health_social_public * industry_ed_health_social_public + asc_bns_industry_service_other * industry_service_other + asc_bns_industry_office_professional * industry_office_professional + asc_bns_industry_service_foodDrink * industry_service_foodDrink + asc_bns_is_med_veh * is_med_veh + asc_bns_is_hvy_veh * is_hvy_veh
      V[['nbm']] = asc_nbm + asc_nbm_industry_retail * industry_retail + asc_nbm_industry_wholesale * industry_wholesale + asc_nbm_industry_construction * industry_construction + asc_nbm_industry_transport_industry * industry_transport_industry + asc_nbm_industry_admin_support_waste * industry_admin_support_waste + asc_nbm_industry_ed_health_social_public * industry_ed_health_social_public + asc_nbm_industry_service_other * industry_service_other + asc_nbm_industry_office_professional * industry_office_professional + asc_nbm_industry_service_foodDrink * industry_service_foodDrink + asc_nbm_is_med_veh * is_med_veh + asc_nbm_is_hvy_veh * is_hvy_veh
      V[['nb0']] = asc_nb0 + asc_nb0_industry_retail * industry_retail + asc_nb0_industry_wholesale * industry_wholesale + asc_nb0_industry_construction * industry_construction + asc_nb0_industry_transport_industry * industry_transport_industry + asc_nb0_industry_admin_support_waste * industry_admin_support_waste + asc_nb0_industry_ed_health_social_public * industry_ed_health_social_public + asc_nb0_industry_service_other * industry_service_other + asc_nb0_industry_office_professional * industry_office_professional + asc_nb0_industry_service_foodDrink * industry_service_foodDrink + asc_nb0_is_med_veh * is_med_veh + asc_nb0_is_hvy_veh * is_hvy_veh
      V[['nbs']] = asc_nbs + asc_nbs_industry_retail * industry_retail + asc_nbs_industry_wholesale * industry_wholesale + asc_nbs_industry_construction * industry_construction + asc_nbs_industry_transport_industry * industry_transport_industry + asc_nbs_industry_admin_support_waste * industry_admin_support_waste + asc_nbs_industry_ed_health_social_public * industry_ed_health_social_public + asc_nbs_industry_service_other * industry_service_other + asc_nbs_industry_office_professional * industry_office_professional + asc_nbs_industry_service_foodDrink * industry_service_foodDrink + asc_nbs_is_med_veh * is_med_veh + asc_nbs_is_hvy_veh * is_hvy_veh
      V[['nnm']] = asc_nnm + asc_nnm_industry_retail * industry_retail + asc_nnm_industry_wholesale * industry_wholesale + asc_nnm_industry_construction * industry_construction + asc_nnm_industry_transport_industry * industry_transport_industry + asc_nnm_industry_admin_support_waste * industry_admin_support_waste + asc_nnm_industry_ed_health_social_public * industry_ed_health_social_public + asc_nnm_industry_service_other * industry_service_other + asc_nnm_industry_office_professional * industry_office_professional + asc_nnm_industry_service_foodDrink * industry_service_foodDrink + asc_nnm_is_med_veh * is_med_veh + asc_nnm_is_hvy_veh * is_hvy_veh
      
      ### Define settings for MNL model component
      mnl_settings = list(
        alternatives = c(
          bbm = 1,
          bbs = 2,
          bnm = 3,
          bns = 4,
          nbm = 5,
          nb0 = 6,
          nbs = 7,
          nnm = 8),
        avail = 
          list(
            bbm = av_bbm,
            bbs = av_bbs,
            bnm = av_bnm,
            bns = av_bns,
            nbm = av_nbm,
            nb0 = av_nb0,
            nbs = av_nbs,
            nnm = av_nnm),
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
  
  database[, TourType := cols[choice]]
  
  database[, TourStartEndLoc := ifelse(substr(TourType,1,1) == substr(TourType,2,2), "SameTAZ", "DiffTAZ")]
  database[, TourStartEndType := substr(TourType,1,2)]
  
  # Add the TourType back to firmStopsVehDur
  firmStopsVehDur[database, 
                  c("TourType", "TourStartEndLoc", "TourStartEndType")  := 
                    .(i.TourType, TourStartEndLoc, TourStartEndType),
                  on = .(BusID, Vehicle, TourID)]
  
  # Expand table to include tour start and stop locations consistent with the tour type
  # for base-base, same TAZ add a start and end stop at the firm location
  # for notbase-base, diff TAZ, add a start stop in the same TAZ as the first scheduled stop and an end stop at the firm location.
  # for notbase-notbase, same TAZ add a start stop and an end stop at the first stop TAZ
  # for base-notbase, diff TAZ add a first stop at the firm location and a end stop in the same TAZ at the last scheduled stop
  # for notbase-notbase, diff TAZ, add a start stop in the same TAZ as the first scheduled stop and and a end stop in the same TAZ at the last scheduled stop
  
  firmStopsVehDur[, Repeat := ifelse(.N == 1, 3, 1), by = .(BusID, Vehicle, TourID)]
  firmStopsVehDur[Repeat == 1, Repeat := c(2, rep(1, .N - 2), 2), by = .(BusID, Vehicle, TourID)]
  firmTourSequence <- firmStopsVehDur[rep(1:.N, times = Repeat)]
  firmTourSequence[, SequenceID := 1:.N, by = .(BusID, Vehicle, TourID)]
  
  # For cases where first sequence record is a tour start
  firmTourSequence[SequenceID == 1, c("StopID", "StopDuration", "Activity") := .(NA, 0, "Start Tour")]
  firmTourSequence[SequenceID == 1, DTAZ := ifelse(TourStartEndType %in% c("bb", "bn"), as.numeric(TAZ), as.numeric(DTAZ))]
  
  # Last sequence record is a tour end
  firmTourSequence[, maxSeq := .N, by = .(BusID, Vehicle, TourID)]
  firmTourSequence[SequenceID == maxSeq, c("StopID", "StopDuration", "Activity") := .(NA, 0, "Return")]
  firmTourSequence[firmTourSequence[SequenceID ==2], fsTAZ := i.DTAZ, on = c("BusID", "Vehicle", "TourID")]
  firmTourSequence[SequenceID == maxSeq, DTAZ := ifelse(TourStartEndType %in% c("bb", "nb"), as.numeric(TAZ), 
                                                        ifelse(TourStartEndType == "nn" & TourStartEndLoc == "SameTAZ", as.numeric(fsTAZ), as.numeric(DTAZ)))]
  # Order and set key
  firmTourSequence <- firmTourSequence[, .(BusID, Vehicle, TourID, SequenceID, StopTAZ = DTAZ, Activity, StopDuration, TourType, TourStartEndLoc)]
  setkey(firmTourSequence, BusID, Vehicle, TourID, SequenceID)
  
  progressUpdate(subtaskprogress = 1, subtask = "Tour Generation", prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  return(firmTourSequence)

}





