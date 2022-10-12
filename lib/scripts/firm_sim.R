
# Master function for executing the synthesis of firms.
firm_sim <- function(cbp) {
  
  # Begin progress tracking
  progressStart(action = "Simulating...", task = "Firms", dir = SCENARIO_LOG_PATH, subtasks = FALSE)
  
  # Define run_steps if it is not already in the environment (default to running all steps)
  if(!exists("run_step")) run_step <- rep(TRUE, 2)
  
  if(run_step[1]){
   
    # TAZLandUseCVTM is is in the environment, produced by firm_sim_process_inputs
     
  }
  
  if(run_step[2]){
  
    # Different approach in base and future scenarios: base year start from start,
    # future year build on base year scaled firm list
    if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
      
      cat("Creating Base Year Establishment List", "\n")
      
      # Process and enumerate the CBP data
      progressUpdate(prop = 1/4, dir = SCENARIO_LOG_PATH)
      ScenarioFirms <- firm_synthesis_enumerate(cbp = cbp,
                                                EmpBounds = EmpBounds,
                                                emp_control_taz = emp_control_taz,
                                                cbp_ag = cbp_ag)
      
      # Allocate from counties to mesozones
      progressUpdate(prop = 2/4, dir = SCENARIO_LOG_PATH)
      ScenarioFirms <- firm_synthesis_mesozones(Firms = ScenarioFirms,
                                                mzemp = mzemp)
      
      # Scale the employment to TAZ controls
      progressUpdate(prop = 3/4, dir = SCENARIO_LOG_PATH)
      ScenarioFirms <- firm_synthesis_scaling(Firms = ScenarioFirms,
                                              emp_control_taz = emp_control_taz,
                                              TAZ_System = TAZ_System,
                                              EmpBounds = EmpBounds)
      
    } else {
      
      # Future year/alternative scenario
      if(file.exists(SCENARIO_BASEFIRMS)){
        
        cat("Updating Base Year Establishment List with Future Control Data", "\n")
        
        # Load the output from the base year firm synthesis model
        progressUpdate(prop = 1/4, dir = SCENARIO_LOG_PATH)
        
        load(SCENARIO_BASEFIRMS)
        ScenarioFirms <- firm_sim_results$ScenarioFirms
        rm(firm_sim_results)
        
        # Scale the emplyoment
        progressUpdate(prop = 3/4, dir = SCENARIO_LOG_PATH)
        ScenarioFirms <- firm_synthesis_scaling(Firms = ScenarioFirms,
                                                emp_control_taz = emp_control_taz,
                                                TAZ_System = TAZ_System,
                                                EmpBounds = EmpBounds)
        
      } else {
        
        stop("No Base Scenario outputs available. Please run the Base Scenario first.")
        
      }
    }
    
    # Add employment classifications
    progressUpdate(prop = 4/4, dir = SCENARIO_LOG_PATH)
    cat("Adding Employment Group Variables", "\n")
    
    ScenarioFirms[UEmpCats[, .(n2 = EmpCatName, EmpCatGroupedName)], 
                  EmpCatGroupedName := i.EmpCatGroupedName,
                  on = "n2"]
    
  } # end run step 2
  
  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)
  
  # Return results
  if(USER_RUN_MODE == "Calibration"){
    return(get(submodel_results_name))
  } else {
    return(list(ScenarioFirms = ScenarioFirms, 
              TAZLandUseCVTM = TAZLandUseCVTM))
  }
  
}
