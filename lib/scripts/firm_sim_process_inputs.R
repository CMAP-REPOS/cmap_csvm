
# This function loads all necessary inputs into envir, after any needed transformations
firm_sim_process_inputs <- function(envir) {
  
  ### Load project input files
  project.files <- c( c_n2_empcats         = file.path(SYSTEM_DATA_PATH, "corresp_naics2_empcats.csv"),    #Correspondence between NAICS2 groups and aggregated employment groups
                      cbp                  = file.path(SYSTEM_DATA_PATH, "data_emp_cbp.csv"),         #CBP data file
                      cbp_ag               = file.path(SYSTEM_DATA_PATH, "data_emp_cbp_ag.csv"),           #CBP data file -- Agriculture records generated seperately
                      mzemp                = file.path(SYSTEM_DATA_PATH, "data_mesozone_emprankings.csv"), #Industry rankings data by mesozone based on employment
                      TAZ_System           = file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"), # TAZ system 
                      firm_sim_enumerate         = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_enumerate.R"),
                      firm_sim_mesozones         = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_mesozones.R"),
                      firm_sim_scaling           = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_scaling.R"))
  
  loadInputs(files = project.files, envir = envir)
  
  ### Process project input files
  envir[["UEmpCats"]]  <- unique(envir[["c_n2_empcats"]][,.(EmpCatID, EmpCatName, EmpCatDesc, EmpCatGroupedName)])
  
  ### Load scenario input files
  scenario.files <- c(emp_control_taz      = file.path(SCENARIO_INPUT_PATH, "data_emp_control_taz.csv"),     # Control totals for emmployment by TAZ
                      data_hh              = file.path(SCENARIO_INPUT_PATH, "data_hh.csv"))                   # HHs summarized at the TAZ level
  
  loadInputs(files = scenario.files, envir = envir)
  
  ### Process scenario input files
  
  # Create a summarized version of the employment data with employment grouping categories in wide format
  envir[["TAZLandUseCVTM"]] <- add_totals(dcast.data.table(merge(envir[["emp_control_taz"]][, .(TAZ = Zone17, Mesozone, CountyFIPS, 
                                                         EmpCatName = NAICS, Employment)],
                                     envir[["UEmpCats"]],
                                     by = "EmpCatName"),
                                     TAZ + Mesozone + CountyFIPS ~ EmpCatGroupedName,
                                     fun.aggregate = sum,
                                     value.var = "Employment"),
                                     idcols = 3L,
                                     coltotal = FALSE)
  setnames(envir[["TAZLandUseCVTM"]], 
           names(envir[["TAZLandUseCVTM"]])[4:ncol(envir[["TAZLandUseCVTM"]])],
           paste("NEmp", names(envir[["TAZLandUseCVTM"]])[4:ncol(envir[["TAZLandUseCVTM"]])], sep = "_"))
                                     
  envir[["TAZLandUseCVTM"]][envir[["data_hh"]][,.(TAZ = Zone17, HH)], HH := i.HH, on = "TAZ"]
  
  ### Define additional variables
  
  # Employment ranges: assume upper bound for the largest size (>5000) is 10,000 
  # to conform to earlier assumption of midpoint being 7,500
  envir[["EmpBounds"]] <- c(1, 20, 100, 250, 500, 1000, 2500, 5000, 10000)
  
  ### Return the cbp table
  cbp <- envir[["cbp"]]
  rm(cbp, envir = envir)
  
  return(cbp)
  
}
