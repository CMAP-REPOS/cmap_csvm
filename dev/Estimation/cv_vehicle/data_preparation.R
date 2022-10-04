library(data.table)
library(readxl)
library(magrittr)

# locations of data files
# it is all relative
base_loc = "dev/Data_Processed/SEMCOG_Data"

cvs_location  = file.path(base_loc,"SEMCOG_CV_20181128_Submitted_ForDistribution.xlsx")
skim_location = file.path(base_loc,"skims_avg.RDS")
taz_centroids_location = file.path(base_loc, "TAZ_Centroids.csv") # not used
zip_centroids_locations = file.path(base_loc, "CVS_Unique_Stop_ZIP_Centroids.csv") # not used
emp_cats_loc = file.path(base_loc, "NAICS3_SEMCOGEmpCats_CMAP.csv")
taz_system_location = file.path(base_loc, "TAZ_System.csv")

# load data
cvs_establishment = read_xlsx(cvs_location, sheet = "ESTABLISHMENT")
cvs_trip          = read_xlsx(cvs_location, sheet = "TRIP")
cvs_tour          = read_xlsx(cvs_location, sheet = "TOUR")

taz_system = fread(taz_system_location)

emp_cats = fread(emp_cats_loc)

skims_avg = readRDS(skim_location)

# useful function
get_distance_meters = function(location_1, location_2){
  
  distance_meters = 
    geosphere::distHaversine(
      matrix(location_1, ncol = 2), 
      matrix(location_2, ncol = 2))
  
  return(distance_meters) 
}


# turn into data.tables
setDT(cvs_establishment)
setDT(cvs_tour)
setDT(cvs_trip)
setDT(skims_avg)

cvs_establishment[, .N, SITEID][N > 1]

# data needed for modeling
cvs_establishment = 
  cvs_establishment[SITE_TAZID != 9999 & !is.na(ESTABLISHMENT_WGHT_FCTR), 
    .(SITEID, 
      SITE_TAZID,
      INDUSTRY, NAICS2, NAICS3)]

cvs_trip =
  cvs_trip[,
    .(SITEID,
      VEHNUM,
      STOP_SEQ,
      SRID,
      FINAL_FACTOR)]

cvs_tour = 
  cvs_tour[, 
    .(SITEID,
      VEHNUM, 
      TOUR_NUM,
      STOP_SEQ, 
      STOP_ACTIVITY, 
      VEH_CLASS,
      TOUR_STOP_TAZID, 
      STOP_ZIP)]

cvs_tour = cvs_trip[cvs_tour, on = .(SITEID, VEHNUM, STOP_SEQ)]

choice_data = 
  cvs_tour[cvs_establishment, on = .(SITEID)]

# 1=Passenger Car
# 2=Pick-up Truck (4 wheels)
# 3=Van (Cargo or Minivan) (4 wheels)
# 4=Sport Utility Vehicle (SUV) (4 wheels)
# 5=Single Unit 2-axle (6 wheels)
# 6=Single Unit 3-axle (10 wheels)
# 7=Single Unit 4-axle (14 wheels)
# 8=Semi (all Tractor-Trailer combinations)
# 888=Other

choice_data[, .N, VEH_CLASS][order(VEH_CLASS)]

choice_data[VEH_CLASS %in% 1:4, veh_choice := 1]
choice_data[VEH_CLASS %in% 5:7, veh_choice := 2]
choice_data[VEH_CLASS %in% 8,   veh_choice := 3]

choice_data = choice_data[!is.na(FINAL_FACTOR)] # remove unweighted stops

choice_data[, .N,  veh_choice]

choice_data = choice_data[!is.na(veh_choice)] # removing 7 "Other" vehicle_class records

# some data recoding to use as independent variables
# no health or other
# replaced with mining and education

choice_data[, .N, .(NAICS2, NAICS3)]
choice_data[, NAICS2 := as.integer(NAICS2)]
choice_data[, NAICS3 := as.integer(NAICS3)]

emp_cats

choice_data[
  emp_cats[, .(NAICS2 = NAICSn2n3, EmpCatName, EmpCatDesc, CMAPGroup)],
  model_emp_cat_n2 := i.CMAPGroup,
  on = "NAICS2"]

choice_data[
  emp_cats[, .(NAICS3 = NAICSn2n3, EmpCatName, EmpCatDesc, CMAPGroup)],
  model_emp_cat_n3 := i.CMAPGroup,
  on = "NAICS3"]

choice_data[, 
  model_emp_cat := 
    ifelse(!is.na(model_emp_cat_n2), model_emp_cat_n2, model_emp_cat_n3)]

choice_data[, NAICS2 := NULL]
choice_data[, NAICS3 := NULL]

#changed these definitions, continue to see what else needs changing
choice_data[, industry_retail := 1 * (model_emp_cat %in% c('Retail'))]
choice_data[, industry_wholesale := 1 * (model_emp_cat %in% c('Wholesale'))]
choice_data[, industry_construction := 1 * (model_emp_cat %in% c("Construction"))]
choice_data[, industry_transport_industry := 1 * (model_emp_cat %in% c("Transport_Industry"))]
choice_data[, industry_admin_support_waste := 1 * (model_emp_cat %in% c("Admin_Support_Waste"))]
choice_data[, industry_ed_health_socialservices := 1 * (model_emp_cat %in% c("Ed_Health_SocialServices"))]
choice_data[, industry_service_other := 1 * (model_emp_cat %in% c("Service_Other"))]
choice_data[, industry_service_public := 1 * (model_emp_cat %in% c("Service_Public"))]
choice_data[, industry_office_professional := 1 * (model_emp_cat %in% c("Office_Professional"))]
choice_data[, industry_service_foodDrink := 1 * (model_emp_cat %in% c("Service_FoodDrink"))]

choice_data[, 
  .N,
  .(industry_retail,
    industry_wholesale,
    industry_construction,
    industry_transport_industry,
    industry_admin_support_waste,
    industry_ed_health_socialservices,
    industry_service_other,
    industry_service_public,
    industry_office_professional,
    industry_service_foodDrink)]

choice_data[, .N, STOP_ACTIVITY]

# 1=Returning to Base Location
# 2=Vehicle Maintenance (fuel, oil, etc)
# 3=Driver Needs (lunch, restroom, etc)
# 4=Deadhead/Drop Trailer/Bobtail
# 5=Delivering cargo
# 6=Picking up cargo
# 7=Getting Government Related Services
# 8=Providing Installation / Maintenance / Repair Services
# 9=Making a sales call
# 10=Providing professional services (legal, medical, financial)
# 11=Shopping for Business
# 888=Other"

choice_data[, activity_return := 1 * (STOP_ACTIVITY == 1)]
choice_data[, activity_driver_needs := 1 * (STOP_ACTIVITY == 3)]
choice_data[, activity_deliver := 1 * (STOP_ACTIVITY == 5)]
choice_data[, activity_pickup := 1 * (STOP_ACTIVITY == 6)]
choice_data[, activity_service := 1 * (STOP_ACTIVITY %in% c(8, 10))]
choice_data[, activity_other := 1 * (STOP_ACTIVITY %in% c(2, 4, 7, 9, 11, 888))]

# only modeling scheduled stops
# That would only be stops for pickup, drop, and service. 
# We'll deal with driver needs, other, return stops in other ways.

choice_data = choice_data[STOP_ACTIVITY %in% c(5, 6, 8, 10)]

choice_data = skims_avg[, .(OTAZ, DTAZ, dist = dist.avg)][choice_data, on = c(OTAZ = "SITE_TAZID", DTAZ = "TOUR_STOP_TAZID")]

# exclude any long distance tours that will be modeled elsewhere
buffer_zips = unique(taz_system[TAZ_TYPE == "BUFFER", ZCTA5CE10])

tours_to_exclude = choice_data[is.na(dist) & !STOP_ZIP %in% buffer_zips, .N, .(SITEID, VEHNUM, TOUR_NUM)]

choice_data = 
   choice_data[!tours_to_exclude, on = .(SITEID, VEHNUM, TOUR_NUM)]

choice_data[is.na(dist), .N, .(DTAZ)]

# appending distance from establishment from centroids
taz_centroids = fread(taz_centroids_location)

taz_centroids[, .N, TAZ][N > 1]

choice_data[, SITE_TAZID := OTAZ]

choice_data = 
  taz_centroids[, .(SITE_TAZID = TAZ, site_lon = X, site_lat = Y)] %>%
    .[choice_data, on = .(SITE_TAZID)]

# using zip centroids for external stops
zip_centroids = fread(zip_centroids_locations)

choice_data = 
  zip_centroids[, .(STOP_ZIP = GEOID10, stop_lon = INTPTLON10, stop_lat = INTPTLAT10)] %>%
    .[choice_data, on = .(STOP_ZIP)]

choice_data[DTAZ == 9999 & is.na(dist), dist := get_distance_meters(c(site_lon, site_lat), c(stop_lon, stop_lat)) / 1609.34]

choice_data[is.na(dist), .N, .(DTAZ)]

choice_data[, dist_00_02 := 1 * (dist >= 0  & dist < 2)]
choice_data[, dist_02_05 := 1 * (dist >= 2  & dist < 5)]
choice_data[, dist_05_10 := 1 * (dist >= 5  & dist < 10)]
choice_data[, dist_10_20 := 1 * (dist >= 10 & dist < 20)]
choice_data[, dist_20_p  := 1 * (dist >= 20)]

# save the table before simplyfying 
model_loc = "dev/Estimation/cv_vehicle/"
saveRDS(choice_data[order(SITEID, STOP_SEQ)], file.path(model_loc,"cv_vehicle_processed_data.rds"))

names(choice_data)

names_to_keep = 
  c("SITEID", "STOP_SEQ",
    "veh_choice", 
    "industry_retail",
    "industry_wholesale",
    "industry_construction",
    "industry_transport_industry",
    "industry_admin_support_waste",
    "industry_ed_health_socialservices",
    "industry_service_other",
    "industry_service_public",
    "industry_office_professional",
    "industry_service_foodDrink",
    "activity_deliver", "activity_pickup", "activity_service", 
    "dist_00_02", "dist_02_05", "dist_05_10", "dist_10_20", "dist_20_p")

# some cleanup on variables not needed for modeling
choice_data = choice_data[, names_to_keep, with = FALSE]

choice_data = choice_data[order(SITEID, STOP_SEQ)]


saveRDS(choice_data, file.path(model_loc, "estimation_data.rds"))



# ---------------------------------------------------------
# extra unused stuff. saving for time being
stop("Unused stuff below")
cvs_establishment = 
  cvs_establishment[SITE_TAZID != 9999, 
    .(SITEID, 
      SITE_TAZID,
      INDUSTRY, NAICS2, NAICS3,
      TOTAL_VEH_OWNED_OR_LEASED,
      OWNED_SINGLE_UNIT,
      OWNED_COMBO_TRACTOR_TRAILER,
      OWNED_PASSENGER_CAR_OR_SUV,
      OWNED_PICKUP_TRUCK,
      OWNED_VAN,
      OWNED_OTHER_VEH,
      TOTAL_VEH_NOT_OWNED,
      VEH_NOT_OWNED_SU,
      VEH_NOT_OWNED_CU,
      VEH_NOT_OWNED_PCSUV,
      VEH_NOT_OWNED_PICKUP,
      VEH_NOT_OWNED_VAN,
      VEH_NOT_OWNED_OTHER)]

# availability
# OWNED_SINGLE_UNIT           Number of Cargo transport vehicles (single unit) owned/leased
# OWNED_COMBO_TRACTOR_TRAILER	Number of Cargo transport vehicles (combo unit/tractor-trailers) owned/leased
# OWND_PASSENGER_CAR_OR_SUV	  Number of Passenger car or sport utility vehicle owned/leased
# OWNED_PICKUP_TRUCK	        Number of Pickup trucks owned/leased
# OWNED_VAN	                  Number of Vans owned/leased
# OWNED_OTHER_VEH	            Number of Other vehicles used for cargo delivery or pickup owned/leased
# VEH_NOT_OWNED_SU	          Number of Cargo transport vehicles (single unit) NOT owned/leased
# VEH_NOT_OWNED_CU	          Number of Cargo transport vehicles (combo unit/tractor-trailers) NOT owned/leased
# VEH_NOT_OWNED_PCSUV	        Number of Passenger car or sport utility vehicle NOT owned/leased
# VEH_NOT_OWNED_PICKUP	      Number of Pickup trucks NOT owned/leased
# VEH_NOT_OWNED_VAN	          Number of Vans NOT owned/leased
# VEH_NOT_OWNED_OTHER	        Number of Other vehicles used for cargo delivery or pickup NOT owned/leased

choice_data[, av_light := 0]
choice_data[OWNED_PASSENGER_CAR_OR_SUV > 0, av_light := 1]
choice_data[OWNED_PICKUP_TRUCK > 0, av_light := 1]
choice_data[OWNED_VAN > 0, av_light := 1]
choice_data[VEH_NOT_OWNED_PCSUV > 0, av_light := 1]
choice_data[VEH_NOT_OWNED_PICKUP > 0, av_light := 1]
choice_data[VEH_NOT_OWNED_VAN > 0, av_light := 1]

# since we don't know what these are, assume all are available
choice_data[OWNED_OTHER_VEH > 0, av_light := 1]
choice_data[VEH_NOT_OWNED_OTHER > 0, av_light := 1]

choice_data[, av_medium := 0]
choice_data[OWNED_SINGLE_UNIT > 0, av_medium := 1]
choice_data[VEH_NOT_OWNED_SU > 0, av_medium := 1]

choice_data[, av_heavy := 0]
choice_data[OWNED_COMBO_TRACTOR_TRAILER > 0, av_heavy := 1]
choice_data[VEH_NOT_OWNED_CU > 0, av_heavy := 1]

# since we don't know what these are, assume all are available
choice_data[OWNED_OTHER_VEH > 0, av_light := 1]
choice_data[VEH_NOT_OWNED_OTHER > 0, av_light := 1]
choice_data[OWNED_OTHER_VEH > 0, av_medium := 1]
choice_data[VEH_NOT_OWNED_OTHER > 0, av_medium := 1]
choice_data[OWNED_OTHER_VEH > 0, av_heavy := 1]
choice_data[VEH_NOT_OWNED_OTHER > 0, av_heavy := 1]

# validation
choice_data[, .N, .(veh_choice, av_light)][order(av_light, veh_choice)]
choice_data[, .N, .(veh_choice, av_medium)][order(av_medium, veh_choice)]
choice_data[, .N, .(veh_choice, av_heavy)][order(av_heavy, veh_choice)]



zip_centroids[, .N, GEOID10]



choice_data[TRIP_STOP_TAZID == 9999, .(stop_lon, stop_lat, stop_lon_2, stop_lat_2)][order(stop_lon)]



summary(choice_data[, dist])

# 6 trips with problematic
choice_data[is.na(dist), .(STOP_ZIP, TRIP_STOP_TAZID, SITE_TAZID) ]

zip_centroids[GEOID10 %in% c('04930', '88888', '48552')]