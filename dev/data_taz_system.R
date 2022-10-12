# CMAP CVTM
# dev script: data_taz_system.R
#
# Purpose:
# Create TAZ correspodence file
#
# Outputs:
# Model inputs in lib\data:
# TAZ_System.csv
#
# use init_dev.R to run here instead of sourcing from _Master_Dev.R
source("./dev/init_dev.R")

### READ INPUT FILES ==================================================

emp_control <- fread("./scenarios/base/inputs/data_emp_control_taz.csv")
zone17 <- read_sf("./lib/data/zones17.shp")

# US Counties
county <- st_read(file.path(SYSTEM_DEV_DATA_PATH, 
                            "USCounty", 
                            "tl_2010_us_county10.shp"))
st_crs(county)

### CREATE COMPLETE CORRESPONDENCE ==============

c_taz_mz <- unique(emp_control[,.(TAZ = Zone17, Mesozone)])

TAZ_System <- as.data.table(zone17)
TAZ_System <- TAZ_System[,.(TAZ = zone17, cbd, chicago, cmap, CountyFIPS = county_fip, county = county_nam, state, township = township_r, sqmi)]

TAZ_System[c_taz_mz, Mesozone := i.Mesozone, on = "TAZ"]
TAZ_System[, county_state := paste(county, state, sep = ", ")]

### CREATE SHAPE FILE VERSION ===================================

TAZ_System_Shape <- st_transform(zone17, crs = st_crs(county))
TAZ_System_Shape$TAZ <- TAZ_System$zone17

### SAVE FINAL CORRESPONDENCE ===================================

# write the file to lib/data in the application
fwrite(TAZ_System, file.path("lib", 
                                "data", 
                                "TAZ_System.csv"))

# write the shapefile version 
st_write(TAZ_System_Shape, 
         dsn = file.path("lib", 
                         "data", 
                         "TAZ_System_Shape.shp"), 
         delete_layer = TRUE)