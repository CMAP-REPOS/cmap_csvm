
# Master function for executing the synthesis of firms.
firm_sim <- function(cbp) {
  
  # Begin progress tracking
  progressStart(action = "Simulating...", task = "Firms", dir = SCENARIO_LOG_PATH, subtasks = FALSE)
  
  # Different approach in base and future scenarios: base year start from start,
  # future year build on base year scaled firm list
  if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
    
    cat("Creating Base Year Establishment List", "\n")
    
    # Process and enumerate the CBP data
    progressUpdate(prop = 1/4, dir = SCENARIO_LOG_PATH)
    FirmsDomestic <- firm_synthesis_enumerate(cbp = cbp,
                                              c_cbp_mz = c_cbp_mz,
                                              EmpBounds = EmpBounds,
                                              emp_control_taz = emp_control_taz,
                                              cbp_ag = cbp_ag)
    
    # Allocate from counties to mesozones
    progressUpdate(prop = 2/4, dir = SCENARIO_LOG_PATH)
    FirmsDomestic <- firm_synthesis_mesozones(Firms = FirmsDomestic)
    
    # Scale the employment to TAZ controls
    progressUpdate(prop = 3/4, dir = SCENARIO_LOG_PATH)
    FirmsDomestic <- firm_synthesis_scaling(Firms = FirmsDomestic,
                                            emp_control_taz = emp_control_taz,
                                            c_cbp_mz = c_cbp_mz,
                                            c_taz_mz = c_taz_mz,
                                            EmpBounds = EmpBounds)
    
  } else {
    
    # Future year/alternative scenario
    if(file.exists(SCENARIO_BASEFIRMS)){
      
      cat("Updating Base Year Establishment List with Future Control Data", "\n")
      
      # Load the output from the base year firm synthesis model
      progressUpdate(prop = 1/4, dir = SCENARIO_LOG_PATH)
      
      load(SCENARIO_BASEFIRMS)
      FirmsDomestic <- firm_sim_results$ScenarioFirms
      rm(firm_sim_results)
      
      # Scale the emplyoment
      progressUpdate(prop = 3/4, dir = SCENARIO_LOG_PATH)
      FirmsDomestic <- firm_synthesis_scaling(Firms = FirmsDomestic,
                                              emp_control_taz = emp_control_taz,
                                              c_cbp_mz = c_cbp_mz,
                                              c_taz_mz = c_taz_mz,
                                              EmpBounds = EmpBounds)
      
    } else {
      
      stop("No Base Scenario outputs available. Please run the Base Scenario first.")
      
    }
  }
  
  # Add employment classifications
  progressUpdate(prop = 4/4, dir = SCENARIO_LOG_PATH)
  cat("Adding Employment Group Variables", "\n")
  
  FirmsDomestic[UEmpCats[, .(n2 = EmpCatName, EmpCatGroupedName)], 
                EmpCatGroupedName := i.EmpCatGroupedName,
                on = "n2"]
  
  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)
  
  # Return results
  return(list(ScenarioFirms = FirmsDomestic, 
              TAZLandUseCVTM = TAZLandUseCVTM))
  
}
