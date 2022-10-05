
# Master function for executing the synthesis of firms.
firm_sim <- function(cbp) {
  
  # Begin progress tracking
  progressStart(action = "Simulating...", task = "Firms", dir = SCENARIO_LOG_PATH, subtasks = FALSE)
  
  # Different approach in base and future scenarios: base year start from start,
  # future year build on base year scaled firm list
  if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
    
    cat("Creating Base Year Establishment List", "\n")
  
    # Run steps
    progressUpdate(prop = 1/12, dir = SCENARIO_LOG_PATH)
    FirmsDomestic <- firm_synthesis_enumerate(cbp = cbp,
                                              c_n6_n6io_sctg = c_n6_n6io_sctg,
                                              EmpBounds = EmpBounds,
                                              cbp_ag = cbp_ag)
    
    progressUpdate(prop = 2/12, dir = SCENARIO_LOG_PATH)
    FirmsDomestic <- firm_synthesis_commodities(Firms = FirmsDomestic)
    
    progressUpdate(prop = 3/12, dir = SCENARIO_LOG_PATH)
    FirmsDomestic <- firm_synthesis_mesozones(Firms = FirmsDomestic)
    
    progressUpdate(prop = 4/12, dir = SCENARIO_LOG_PATH)
    # For summaries prior to scaling
    FirmsDomesticUnscaled <- copy(FirmsDomestic)
    
    # Scale the emplyoment
    FirmsDomestic <- firm_synthesis_scaling(Firms = FirmsDomestic,
                                            emp_control = emp_control,
                                            emp_control_taz = emp_control_taz,
                                            c_cbp_faf = c_cbp_faf,
                                            c_cbp_mz = c_cbp_mz,
                                            c_taz_mz = c_taz_mz,
                                            EmpBounds = EmpBounds)
    
    # save complete firms  list after scaling
    save(FirmsDomestic, file = file.path(SCENARIO_OUTPUT_PATH, "BaseYearDomesticFirms.Rdata"))
   
  } else {
    
    # Future year/alternative scenario
    
    if(file.exists(file.path(SCENARIO_BASE_OUTPUT_PATH, "BaseYearDomesticFirms.Rdata"))){
      
      # Load the output from the base year firm synthesis model
      cat("Loading Base Year Establishment List", "\n")
      
      load(file.path(SCENARIO_BASE_OUTPUT_PATH, "BaseYearDomesticFirms.Rdata"))
      
      progressUpdate(prop = 4/12, dir = SCENARIO_LOG_PATH)
      # For summaries prior to scaling
      FirmsDomesticUnscaled <- copy(FirmsDomestic)
      
      # Scale the emplyoment
      FirmsDomestic <- firm_synthesis_scaling(Firms = FirmsDomestic,
                                              emp_control = emp_control,
                                              emp_control_taz = emp_control_taz,
                                              c_cbp_faf = c_cbp_faf,
                                              c_cbp_mz = c_cbp_mz,
                                              c_taz_mz = c_taz_mz,
                                              EmpBounds = EmpBounds)
      
      # save complete firms  list after scaling
      save(FirmsDomestic, file = file.path(SCENARIO_OUTPUT_PATH, "ScenarioDomesticFirms.Rdata"))
      
    } else {
      
      stop("No Base Scenario outputs available. Please run the Base Scenario first.")
      
    }
  }
  
  cat("Adding Employment Group Variables", "\n")
  
  # Add employment classifications
  RegionFirms[UEmpCats[, .(n2 = EmpCatName, EmpCatGroupedName)], 
                 EmpCatGroupedName := i.EmpCatGroupedName,
                 on = "n2"]
  
  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)
  
  # Return results
  return(list(RegionFirms = RegionFirms, 
              TAZLandUseCVTM = TAZLandUseCVTM))
  
}
