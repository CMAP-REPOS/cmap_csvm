
# This function loads all necessary inputs
tt_build_process_inputs <- function(envir){
  
  ### Load project input files
  
  ### Load inputs/outputs from earlier steps
  load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_CVTM_OUTPUTNAME))
  
  # extract the lookup from the skims for use in the trip tables
  envir[["zone_number"]] <- readLookupOMX(file.path(SCENARIO_INPUT_PATH, 
                                                    cv_inputs$skims_names[1]$file_name), 
                                          LookupName = "zone_number")$Lookup
  
  return(cv_sim_results$cv_trips)
  
}