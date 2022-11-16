
# This function loads all necessary inputs into envir, after any needed transformations
firm_sim_process_inputs <- function(envir) {
  
  ### Load project input files
  project.files <- c( c_n2_empcats         = file.path(SYSTEM_DATA_PATH, "corresp_naics2_empcats.csv"),    # Correspondence between NAICS2 groups and aggregated employment groups
                      cbp                  = file.path(SYSTEM_DATA_PATH, "data_emp_cbp.csv"),              # CBP data file
                      cbp_ag               = file.path(SYSTEM_DATA_PATH, "data_emp_cbp_ag.csv"),           # CBP data file -- Agriculture records generated seperately
                      mzemp                = file.path(SYSTEM_DATA_PATH, "data_mesozone_emprankings.csv"), # Industry rankings data by mesozone based on employment
                      EstSizeCategories    = file.path(SYSTEM_DATA_PATH, "data_est_size_categories.csv"),  # Establishment size categories and labels
                      TAZ_System           = file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"),                # TAZ system 
                      firm_sim_enumerate   = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_enumerate.R"))
  
  loadInputs(files = project.files, envir = envir)
  
  ### Process project input files
  envir[["UEmpCats"]]  <- unique(envir[["c_n2_empcats"]][,.(EmpCatID, EmpCatName = as.character(EmpCatName), EmpCatDesc, EmpCatGroupedName)])
  
  # For the base scenario need to process the input CBP data into the format required by the model
  if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
    # Combine Domestic Non Ag Firm Records with the Ag Firm Records
    # Remove any ag records in cbp and replacing with cbp_ag  
    cbp <- rbind(envir[["cbp"]][Industry_NAICS6_CBP >= 113110],
                 envir[["cbp_ag"]])
    
    # Aggregate by zones, NAICS, and firm size category
    # Extract just the region data for the 21 county CMAP region
    # For the 21 counties, CBPZONE == county FIPS code
    # Remove records with missing zones and NAICS codes
    cbp <- cbp[CBPZONE %in% BASE_FIPS_INTERNAL & !is.na(FAFZONE) & !is.na(Industry_NAICS6_CBP),
              .(e1 = sum(e1), e2 = sum(e2), e3 = sum(e3), e4 = sum(e4),
                e5 = sum(e5), e6 = sum(e6), e7 = sum(e7), e8 = sum(e8)),
              by = .(NAICS6 = Industry_NAICS6_CBP, CountyFIPS = CBPZONE)]
    
    # Add 2 digit NAICS which is the EmpCatName used in the model
    cbp[, EmpCatName := substr(NAICS6, 1, 2)]
    
    # Melt to create separate rows for each firm size category
    cbp <- melt.data.table(cbp,
                           measure.vars = paste0("e",1:8),
                           variable.name ="esizecat",
                           value.name = "est")
    
    # Convert esizecat to an integer (1:8)
    cbp[, esizecat := as.integer(esizecat)]
  
  } else {
    
    # for alternative scenarios, will not be using the CBP data, instead updating base year firms
    cbp <- NULL
    
  }
      
  # remove the CBP data from the environment
  rm(cbp, cbp_ag, envir = envir)

  ### Load scenario input files
  scenario.files <- c(TAZEmployment      = file.path(SCENARIO_INPUT_PATH, "data_emp_control_taz.csv"),     # Control totals for emmployment by TAZ
                      TAZHH              = file.path(SCENARIO_INPUT_PATH, "data_hh.csv"))                  # HHs summarized at the TAZ level
  
  loadInputs(files = scenario.files, envir = envir)
  
  ### Process scenario input files
  
  # Update fieldnames/datatypes for consistency
  
  # Naming and data type of control employment data
  setnames(envir[["TAZEmployment"]], 
           c("Zone17", "NAICS", "Employment"), 
           c("TAZ", "EmpCatName", "Employees.SE"))
  
  envir[["TAZEmployment"]][, EmpCatName := as.character(EmpCatName)]
  
  # Create a summarized version of the employment data with employment grouping categories in wide format
  envir[["TAZLandUseCVTM"]] <- add_totals(dcast.data.table(merge(envir[["TAZEmployment"]][, .(TAZ, Mesozone, CountyFIPS, 
                                                         EmpCatName, Employees.SE)],
                                     envir[["UEmpCats"]],
                                     by = "EmpCatName"),
                                     TAZ + Mesozone + CountyFIPS ~ EmpCatGroupedName,
                                     fun.aggregate = sum,
                                     value.var = "Employees.SE"),
                                     idcols = 3L,
                                     coltotal = FALSE)
  
  setnames(envir[["TAZLandUseCVTM"]], 
           names(envir[["TAZLandUseCVTM"]])[4:ncol(envir[["TAZLandUseCVTM"]])],
           paste("NEmp", names(envir[["TAZLandUseCVTM"]])[4:ncol(envir[["TAZLandUseCVTM"]])], sep = "_"))
                                     
  envir[["TAZLandUseCVTM"]][envir[["TAZHH"]][,.(TAZ = Zone17, HH)], HH := i.HH, on = "TAZ"]
  
  ### Return the cbp table
  return(cbp)
  
}
