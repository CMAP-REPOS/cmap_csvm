
# Master function for executing the synthesis of firms.
firm_sim <- function(Establishments) {
  
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
      
      # Process and enumerate the Establishment data
      progressUpdate(prop = 1/3, dir = SCENARIO_LOG_PATH)
      ScenarioFirms <- firm_synthesis_enumerate(Establishments = Establishments,
                                                EstSizeCategories = EstSizeCategories,
                                                TAZEmployment = TAZEmployment,
                                                mzemp = mzemp)

      # Scale the employment to TAZ controls
      progressUpdate(prop = 2/3, dir = SCENARIO_LOG_PATH)
      ScenarioFirms <- scaleEstablishmentsTAZEmployment(RegionFirms = ScenarioFirms, 
                                       TAZEmployment = TAZEmployment, 
                                       NewFirmsProportion = BASE_NEW_FIRMS_PROP,
                                       MaxBusID = max(ScenarioFirms$BusID),
                                       EstSizeCategories = EstSizeCategories,
                                       TAZEmploymentShape = "LONG")
      
    } else {
      
      # Future year/alternative scenario
      if(file.exists(SCENARIO_BASEFIRMS)){
        
        cat("Updating Base Year Establishment List with Future Control Data", "\n")
        
        # Load the output from the base year firm synthesis model
        progressUpdate(prop = 1/3, dir = SCENARIO_LOG_PATH)
        
        load(SCENARIO_BASEFIRMS)
        ScenarioFirms <- firm_sim_results$ScenarioFirms
        rm(firm_sim_results)
        
        # Convert to an alternative (grouped) see of employment categories if necessary
        if(1==1){}
        
        # Scale the emplyoment
        progressUpdate(prop = 2/3, dir = SCENARIO_LOG_PATH)
        ScenarioFirms <- scaleEstablishmentsTAZEmployment(RegionFirms = ScenarioFirms, 
                                                          TAZEmployment = TAZEmployment, 
                                                          NewFirmsProportion = BASE_NEW_FIRMS_PROP,
                                                          MaxBusID = max(ScenarioFirms$BusID),
                                                          EstSizeCategories = EstSizeCategories,
                                                          TAZEmploymentShape = "LONG")
        
      } else {
        
        stop("No Base Scenario outputs available. Please run the Base Scenario first.")
        
      }
    }
    
    
    # Add employment classifications and spatial fields
    progressUpdate(prop = 3/3, dir = SCENARIO_LOG_PATH)
    cat("Adding Employment Group and Spatial Variables", "\n")
    
    ScenarioFirms[UEmpCats, 
                  EmpCatGroupedName := i.EmpCatGroupedName,
                  on = "EmpCatName"]
    
    ScenarioFirms[TAZ_System, 
                  c("Mesozone", "CountyFIPS") := .(i.Mesozone, i.CountyFIPS), 
                  on = "TAZ"]
    
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
