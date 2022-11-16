
# This function loads all necessary inputs into envir, after any needed transformations
firm_sim_process_inputs <- function(envir) {
  
  ### Load project input files
  project.files <- c( c_n2_empcats         = file.path(SYSTEM_DATA_PATH, "corresp_naics2_empcats.csv"),    # Correspondence between NAICS2 groups and aggregated employment groups
                      cbp                  = file.path(SYSTEM_DATA_PATH, "data_emp_cbp.csv"),              # CBP data file
                      cbp_ag               = file.path(SYSTEM_DATA_PATH, "data_emp_cbp_ag.csv"),           # CBP data file -- Agriculture records generated seperately
                      mzemp                = file.path(SYSTEM_DATA_PATH, "data_mesozone_emprankings.csv"), # Industry rankings data by mesozone based on employment
                      EstSizeCategories    = file.path(SYSTEM_DATA_PATH, "data_est_size_categories.csv"),  # Establishment size categories and labels
                      TAZ_System           = file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"),                # TAZ system 
                      firm_sim_enumerate   = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_enumerate.R"),
                      firm_sim_mesozones   = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_mesozones.R"),
                      firm_sim_scaling     = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_scaling.R"))
  
  loadInputs(files = project.files, envir = envir)
  
  ### Process project input files
  envir[["UEmpCats"]]  <- unique(envir[["c_n2_empcats"]][,.(EmpCatID, EmpCatName, EmpCatDesc, EmpCatGroupedName)])
  
  # Combine Domestic Non Ag Firm Records with the Ag Firm Records
  # Remove any ag records in cbp and replacing with cbp_ag  
  # Extract just the region data for the 21 county CMAP region
  # For the 21 counties, CBPZONE == county FIPS code
  cbp <- rbind(envir[["cbp"]][Industry_NAICS6_CBP >= 113110],
               envir[["cbp_ag"]])[CBPZONE %in% BASE_FIPS_INTERNAL]
  rm(cbp, cbp_ag, envir = envir)

  ### Load scenario input files
  scenario.files <- c(TAZEmployment      = file.path(SCENARIO_INPUT_PATH, "data_emp_control_taz.csv"),     # Control totals for emmployment by TAZ
                      TAZHH              = file.path(SCENARIO_INPUT_PATH, "data_hh.csv"))                  # HHs summarized at the TAZ level
  
  loadInputs(files = scenario.files, envir = envir)
  
  ### Process scenario input files
  
  # Update fieldnames/datatypes for consistency
  setnames(envir[["TAZEmployment"]], 
           c("Zone17", "NAICS"), 
           c("TAZ", "EmpCatName"))
  
  # Create a wide version of the employment data for rFreight scaling function
  TAZEmploymentWide <- dcast.data.table(envir[["TAZEmployment"]],
                                    TAZ ~ EmpCatName,
                                    fun.aggregate = sum,
                                    value.var = "Employment")
  
  # Create a summarized version of the employment data with employment grouping categories in wide format
  envir[["TAZLandUseCVTM"]] <- add_totals(dcast.data.table(merge(envir[["TAZEmployment"]][, .(TAZ, Mesozone, CountyFIPS, 
                                                         EmpCatName, Employment)],
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
                                     
  envir[["TAZLandUseCVTM"]][envir[["TAZHH"]][,.(TAZ = Zone17, HH)], HH := i.HH, on = "TAZ"]
  
  ### Return the cbp table
  return(cbp)
  
}
