# CMAP CVTM
# dev script: data_gps.R
#
# Purpose:
# Review and process GPS data
#
# Outputs:
# Model inputs in lib\data:
# <list here>
#
# use init_dev.R to run here instead of sourcing from _Master_Dev.R
source("./dev/init_dev.R")

### READ INPUT FILES ==================================================# Review GPS

# data
### TODO need skims that are consistent with the zone17 system
skims <- fread(file.path(SCENARIO_INPUT_PATH, "cmap_data_zone_skims.csv"))
max(skims$Origin)
max(skims$Destination)

# TAZ
taz <- read_sf(dsn = file.path(SYSTEM_DEV_DATA_PATH, "TAZ", "zones17.shp"))
max(taz$zone17) #3632

sz <- read_sf(dsn = file.path(SYSTEM_DEV_DATA_PATH, "TAZ", "subzones17.shp"))
max(sz$subzone17) #17418

# GPS
gps <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "CVGPS", "TripsMediumLight_OD_ISPE83_SZPlusCounties_ValuesOnly_forRSG.xlsx")))
head(gps)
gps[,.N]                  
length(unique(gps$DeviceId))
length(unique(gps$Tour.ID))

# check the zones covered
unique(gps$Origin_SZ17PlusCounties)

# Vehicle type
gps[,.N, by = VehicleWeightClass_1 ] # Almost all medium (Medium Duty Trucks / Vans: ranges from 14001-26000 lb), a small number of light
gps[,.N, by = ProviderDrivingProfile_1] # Field Service / Local Delivery Fleets 

# Create a shapefile with trip counts
# Also do tour start counts
# Mid tour stop counts

# Filter out bad/missing location trips
gps[,.N, by = `Reporting.Concern.Indicated.by."1"`]

unique(taz$county_fip)
unique(taz$zone17)
range(taz$zone17) # 1 - 3632

gps[is.na(Origin_SZ17PlusCounties),.N]
gps[is.na(Destination_SZ17PlusCounties),.N]
gps[is.na(Destination_SZ17PlusCounties) & is.na(Origin_SZ17PlusCounties),.N]

gps[, OTAZ_TYPE := ifelse(as.integer(substr(Origin_SZ17PlusCounties,1,1)) == 9 & nchar(Origin_SZ17PlusCounties) == 6, "COUNTY", "TAZ")]
gps[, DTAZ_TYPE := ifelse(as.integer(substr(Destination_SZ17PlusCounties,1,1)) == 9  & nchar(Destination_SZ17PlusCounties) == 6, "COUNTY", "TAZ")]

gps[,.N, by = OTAZ_TYPE]
gps[,.N, by = DTAZ_TYPE]

gps[, TRIP_TYPE := ifelse(OTAZ_TYPE == "TAZ" & DTAZ_TYPE == "TAZ", "II",
                          ifelse(OTAZ_TYPE == "TAZ" & DTAZ_TYPE == "COUNTY", "IX",
                                 ifelse(OTAZ_TYPE == "COUNTY" & DTAZ_TYPE == "TAZ", "XI", "XX")))]
gps[,.N, by = TRIP_TYPE]
gps[is.na(TRIP_TYPE)]
gps[,.(range(Origin_SZ17PlusCounties)), by = OTAZ_TYPE]

gps[, TRIPS_KEEP := ifelse(is.na(TRIP_TYPE) | `Reporting.Concern.Indicated.by."1"` == 1, 0, 1)]
gps[,.N, by = TRIPS_KEEP]

gps[TRIPS_KEEP == 1,.N, by = TRIP_TYPE]

gps_summary <- gps[TRIPS_KEEP == 1,.(TRIP_ORIGINS = .N), by = .(SZ17PlusCounties = Origin_SZ17PlusCounties)]
gps_summary <- merge(gps_summary,
                     gps[TRIPS_KEEP == 1,.(TRIP_DESTINATIONS = .N), by = .(SZ17PlusCounties = Destination_SZ17PlusCounties)],
                     by = "SZ17PlusCounties",
                     all = TRUE)
gps_summary <- merge(gps_summary,
                     gps[TRIPS_KEEP == 1 & `Begin.Tour.-.After.Error.Checking` == TRUE, .(TOUR_STARTS = .N), by = .(SZ17PlusCounties = Origin_SZ17PlusCounties)],
                     by = "SZ17PlusCounties",
                     all = TRUE)

# add TAZ and out geographies to summary
sz_dt <- as.data.table(sz)
gps_summary <- merge(gps_summary, sz_dt[,.(SZ17PlusCounties = subzone17 , zone17, capzone17, cbd, chicago, cmap, county_fip, county_nam, state)],
                     by = "SZ17PlusCounties", all.x = TRUE)
gps_summary[SZ17PlusCounties <= 17418, subzone17 := SZ17PlusCounties]
gps_summary[SZ17PlusCounties > 17418, county_fip := substr(SZ17PlusCounties,2,7)]

gps_summary_sz <- gps_summary[!is.na(subzone17),.(TRIP_ORIGINS = sum(TRIP_ORIGINS, na.rm = TRUE),
                                 TRIP_DESTINATIONS = sum(TRIP_DESTINATIONS, na.rm = TRUE), 
                                 TOUR_STARTS = sum(TOUR_STARTS, na.rm = TRUE)),
                              by = .(subzone17, zone17, capzone17, cbd, chicago, cmap, county_fip, county_nam, state)]
gps_summary_zone <- gps_summary[!is.na(subzone17),.(TRIP_ORIGINS = sum(TRIP_ORIGINS, na.rm = TRUE),
                                                    TRIP_DESTINATIONS = sum(TRIP_DESTINATIONS, na.rm = TRUE), 
                                                    TOUR_STARTS = sum(TOUR_STARTS, na.rm = TRUE)),
                                by = .(zone17, capzone17, cbd, chicago, cmap, county_fip, county_nam, state)] 
gps_summary_county <- gps_summary[,.(TRIP_ORIGINS = sum(TRIP_ORIGINS, na.rm = TRUE),
                                                         TRIP_DESTINATIONS = sum(TRIP_DESTINATIONS, na.rm = TRUE), 
                                                         TOUR_STARTS = sum(TOUR_STARTS, na.rm = TRUE)),
                                     by = .(cmap, county_fip, county_nam, state)] 
fwrite(gps_summary,file.path(SYSTEM_DEV_DATA_PATH, "CVGPS", "GPS_Summary_SZ17PlusCounties.csv")) 
fwrite(gps_summary_sz,file.path(SYSTEM_DEV_DATA_PATH, "CVGPS", "GPS_Summary_SZ_Internal_Only.csv")) 
fwrite(gps_summary_zone,file.path(SYSTEM_DEV_DATA_PATH, "CVGPS", "GPS_Summary_Zone_Internal_Only.csv"))
fwrite(gps_summary_county,file.path(SYSTEM_DEV_DATA_PATH, "CVGPS", "GPS_Summary_Zone_Internal_External.csv"))

# join the totals to sz and taz shapefiles for mapping
sz <- merge(sz, 
            gps_summary_sz[,.(subzone17, 
                              triporig = TRIP_ORIGINS, 
                              tripdest = TRIP_DESTINATIONS, 
                              tourstarts = TOUR_STARTS)],
            by = "subzone17",
            all.x = TRUE)

taz <- merge(taz, 
             gps_summary_zone[,.(zone17, 
                               triporig = TRIP_ORIGINS, 
                               tripdest = TRIP_DESTINATIONS, 
                               tourstarts = TOUR_STARTS)],
             by = "zone17",
             all.x = TRUE)

write_sf(sz, 
         dsn = file.path(SYSTEM_DEV_DATA_PATH, "CVGPS", "GPS_Summary_SZ_Internal_Only.shp"))
write_sf(taz, 
         dsn = file.path(SYSTEM_DEV_DATA_PATH, "CVGPS", "GPS_Summary_Zone_Internal_Only.shp"))
