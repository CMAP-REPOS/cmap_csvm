
# This function loads all necessary inputs into envir, after any needed transformations
firm_sim_process_inputs <- function(envir) {
  
  ### Load project input files
  project.files <- c( c_n6_n6io_sctg       = file.path(SYSTEM_DATA_PATH, "corresp_naics6_n6io_sctg.csv"),  #Correspondence between NAICS 6-digit, I/O NAICS, and SCTG
                      c_cbp_faf            = file.path(SYSTEM_DATA_PATH, "corresp_fafzone_cbpzone.csv"),   #Correspondence between CBP and FAF zone systems
                      c_cbp_mz             = file.path(SYSTEM_DATA_PATH, "corresp_mesozone_cbpzone.csv"),  #Correspondence between CBP and Mesozone zone systems
                      c_mz_faf_reg         = file.path(SYSTEM_DATA_PATH, "corresp_meso_faf3_region.csv"),  #Correspondence between Mesozones, FAF3, and census regions for summaries
                      c_n6_labels          = file.path(SYSTEM_DATA_PATH, "corresp_naics2007_labels.csv"),  #Correspondence NAICS 2007 at different levels of detail and industry name labels
                      c_n2_empcats         = file.path(SYSTEM_DATA_PATH, "corresp_naics2_empcats.csv"),    #Correspondence between NAICS2 groups and aggregated employment groups
                      cbp                  = file.path(SYSTEM_DATA_PATH, "data_emp_cbp_2017.csv"),         #CBP data file
                      cbp_ag               = file.path(SYSTEM_DATA_PATH, "data_emp_cbp_ag.csv"),           #CBP data file -- Agriculture records generated seperately
                      mzemp                = file.path(SYSTEM_DATA_PATH, "data_mesozone_emprankings.csv"), #Industry rankings data by mesozone based on employment
                      firm_sim_commodities       = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_commodities.R"),
                      firm_sim_enumerate         = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_enumerate.R"),
                      firm_sim_mesozones         = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_mesozones.R"),
                      firm_sim_scaling           = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_scaling.R"))
  
  loadInputs(files = project.files, envir = envir)
  
  ### Process project input files
  envir[["UEmpCats"]]  <- unique(envir[["c_n2_empcats"]][,.(EmpCatID, EmpCatName, EmpCatDesc, EmpCatGroupedName)])
  
  ### Load scenario input files
  scenario.files <- c(emp_control          = file.path(SCENARIO_INPUT_PATH, "data_emp_control_mz.csv"),       #Control totals for emmployment by Mesozone
                      emp_control_taz      = file.path(SCENARIO_INPUT_PATH, "data_emp_control_2017.csv"))     #Control totals for emmployment by TAZ
                                          
  
  loadInputs(files = scenario.files, envir = envir)
  
  ### Process scenario input files
  
  # Employment targets: replace the within CMAP portion in MZ with values rolled up from TAZ
  envir[["emp_control"]] <- rbind(envir[["emp_control_taz"]][,.(Employment = sum(Employment, na.rm = TRUE)), keyby = .(Mesozone, NAICS)],
                                  envir[["emp_control"]][Mesozone >= 150])
  
  # Create a summarized version of the employment data with employment grouping categories in wide format
  envir[["TAZLandUseCVTM"]] <- dcast.data.table(merge(envir[["emp_control_taz"]][, .(TAZ = Zone17, Mesozone, CountyFIPS, 
                                                         EmpCatName = NAICS, Employment)],
                                     envir[["UEmpCats"]],
                                     by = "EmpCatName"),
                                     TAZ + Mesozone + CountyFIPS ~ EmpCatGroupedName,
                                     fun.aggregate = sum,
                                     value.var = "Employment")
  
  ### TEMP add HH field, zeros for now, 
  ### TODO replace with data from HH_IN.TXT, https://github.com/CMAP-REPOS/cmap_csvm/issues/14
  envir[["TAZLandUseCVTM"]][, TotalHHs := 0]
  
  ### Define additional variables
  
  # Correspondence between TAZ and MZ based on employment data
  envir[["c_taz_mz"]] <- unique(envir$emp_control_taz[,.(TAZ = Zone17, Mesozone)])
  
  # Employment ranges: assume upper bound for the largest size (>5000) is 10,000 
  # to conform to earlier assumption of midpoint being 7,500
  envir[["EmpBounds"]] <- c(1, 20, 100, 250, 500, 1000, 2500, 5000, 10000)
  
  ### Return the cbp table
  cbp <- envir[["cbp"]]
  rm(cbp, envir = envir)
  
  return(cbp)
  
}
