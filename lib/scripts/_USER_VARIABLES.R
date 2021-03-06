# User options to control outputs, hardware use, and run mode
USER_GENERATE_DASHBOARD <- TRUE # Should the model generate the output dashboard? (TRUE/FALSE)
USER_GENERATE_DASHBOARD_MAPS <- TRUE # Should the output dashboard inclde maps? (TRUE/FALSE)
USER_PROCESSOR_CORES <- 1L # How many processors should be used for parallel components?
USER_RUN_MODE <- "Application" # What type of run is being done? 
                               # Options are "Application" or "Calibration" 
                               # where calibration triggers certain model components to run iterative adjustments