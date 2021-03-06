# packages for model and report dashboard
SYSTEM_PKGS <- c("data.table", "rFreight", "rhdf5", "rgdal", "rgeos", "future", 
                 "namedCapture", "bit64", "reshape")

SYSTEM_REPORT_PKGS <- c("DT", "flexdashboard", "leaflet", "geojsonio", "htmltools", "htmlwidgets",
                        "knitr", "mapview", "plotly", "RColorBrewer", "rgdal", "rgeos",
                        "rmarkdown", "scales", "stringr", "jsonlite", "pander")

SYSTEM_DEV_PKGS <- c("sf", "lwgeom", "dplyr", "ggrepel",
                     "ggspatial", "bookdown", "leaps")

# combine lists so all install if needed on call to initializeApp
SYSTEM_PKGS <- c(SYSTEM_PKGS, SYSTEM_REPORT_PKGS[!SYSTEM_REPORT_PKGS %in% SYSTEM_PKGS])

SYSTEM_APP_PATH       <- getwd()
SYSTEM_RFREIGHT_PATH  <- file.path(SYSTEM_APP_PATH, "lib", "rFreight_0.1-32.zip")
SYSTEM_PKGS_PATH      <- file.path(SYSTEM_APP_PATH, "lib", "pkgs", "library")
# add the library folder to the library search paths -- required for proper install and loading, 
# especially with future
.libPaths(c(SYSTEM_PKGS_PATH, .libPaths())) 
Sys.setenv(R_LIBS = paste(SYSTEM_PKGS_PATH, Sys.getenv("R_LIBS"), sep=.Platform$path.sep))

SYSTEM_DATA_PATH      <- file.path(SYSTEM_APP_PATH, "lib", "data")
SYSTEM_TEMPLATES_PATH <- file.path(SYSTEM_APP_PATH, "lib", "templates")
SYSTEM_SCRIPTS_PATH   <- file.path(SYSTEM_APP_PATH, "lib", "scripts")
SYSTEM_DEV_PATH       <- file.path(SYSTEM_APP_PATH, "dev")
SYSTEM_DOCS_PATH      <- file.path(SYSTEM_APP_PATH, "docs")


SYSTEM_FIRMSYN_OUTPUTNAME <- "1.Firms.RData"
SYSTEM_CVTM_OUTPUTNAME    <- "2.CommercialVehileTrips.RData"
SYSTEM_TT_OUTPUTNAME      <- "3.TripTables.RData"
SYSTEM_DB_OUTPUTNAME      <- "4.DashboardTables.RData"

# location of pandoc install, required for rmarkdown/dashboard
# this should already be set with rstudio installation
# Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")