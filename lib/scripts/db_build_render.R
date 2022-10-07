
db_build_render <- function() {
  # Renders the dashboard using Rmarkdown, then post-processes the HTML file to
  # change display settings.
  
  dashboardName <- "ReportDashboard.html"
  
  # Generate dashboard
  rmarkdown::render(file.path(SYSTEM_SCRIPTS_PATH, "db_markdown", 
                              "ReportDashboard.Rmd"),
                    output_dir = SCENARIO_OUTPUT_PATH,
                    output_file = dashboardName,
                    quiet = TRUE)
  
  # The following lines change the rendering engine from SVG to Canvas, which
  # speeds up map rendering considerably.
  dashboardHTML <- readLines(file.path(SCENARIO_OUTPUT_PATH, dashboardName))
  idx <- which(dashboardHTML == "window.FlexDashboardComponents = [];")[1]
  dashboardHTML <- append(dashboardHTML, "L_PREFER_CANVAS = true;", after = idx)
  writeLines(dashboardHTML, file.path(SCENARIO_OUTPUT_PATH, dashboardName))
  
}