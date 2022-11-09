# User options to control outputs, hardware use, and run mode
USER_PROCESSOR_CORES <- 4L # How many processors should be used for parallel components?
USER_PROCESS_SKIMS <- TRUE # Should the cv model process skims (if FALSE will if possible skip skim processing and use an existing processed skims file if present)
USER_RUN_MODE <- "Application" # What type of run is being done? 
                               # Options are "Application" or "Calibration" 
                               # where calibration triggers certain model components to run iterative adjustments
