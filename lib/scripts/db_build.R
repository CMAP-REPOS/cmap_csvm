
# Master function for preparing and rendering the model dashboard.
db_build <- function(data = NULL){
  # Null data argument is present for compatibility with rFreight's run_sim
  # function
  
  # Begin progress tracking
  progressStart(action = "Writing...", task = "Dashboard", dir = SCENARIO_LOG_PATH, subtasks = FALSE)
  
  # Pandoc location inside the model
  rmarkdown::find_pandoc(dir = SYSTEM_PANDOC_PATH)
  
  # Render the dashboard into HTML
  db_build_render()

  # Create summary spreadsheet using model outputs including assignment flow tables
  if(SCENARIO_DB_SPREADSHEET){
    db_build_spreadsheet()
  }
  
  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)
  
  # Return dashboard address
  return(file.path(SCENARIO_OUTPUT_PATH, "ReportDashboard.html"))
}


