library(data.table)
library(readxl)
library(magrittr)
library(lubridate)

# locations of data files
# it is all relative
base_loc = "dev/Data_Processed/SEMCOG_Data"
cvs_location  = file.path(base_loc, "SEMCOG_CV_20181128_Submitted_ForDistribution.xlsx")
skim_location = file.path(base_loc, "skims_avg.RDS")
taz_centroids_location = file.path(base_loc, "TAZ_Centroids.csv") # not used
zip_centroids_locations = file.path(base_loc, "CVS_Unique_Stop_ZIP_Centroids.csv") # not used
emp_cats_loc = file.path(base_loc, "NAICS3_SEMCOGEmpCats_CMAP.csv")
taz_system_location = file.path(base_loc, "TAZ_System.csv")

# load data
cvs_establishment = read_xlsx(cvs_location, sheet = "ESTABLISHMENT")
cvs_tour          = read_xlsx(cvs_location, sheet = "TOUR")
cvs_trip          = read_xlsx(cvs_location, sheet = "TRIP")

taz_system = fread(taz_system_location)

emp_cats = fread(emp_cats_loc)

skims_avg = readRDS(skim_location)

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
      INDUSTRY, 
      NAICS2, 
      NAICS3)]

cvs_trip =
  cvs_trip[,
    .(SITEID,
      VEHNUM,
      STOP_SEQ,
      SRID,
      FINAL_FACTOR)]

cvs_tour = cvs_trip[cvs_tour, on = .(SITEID, VEHNUM, STOP_SEQ)]

cvs_establishment[, .N, .(NAICS2, NAICS3)]
cvs_establishment[, NAICS2 := as.integer(NAICS2)]
cvs_establishment[, NAICS3 := as.integer(NAICS3)]

emp_cats

cvs_establishment[
  emp_cats[, .(NAICS2 = NAICSn2n3, EmpCatName, EmpCatDesc, CMAPGroup)],
  model_emp_cat_n2 := i.CMAPGroup,
  on = "NAICS2"]

cvs_establishment[
  emp_cats[, .(NAICS3 = NAICSn2n3, EmpCatName, EmpCatDesc, CMAPGroup)],
  model_emp_cat_n3 := i.CMAPGroup,
  on = "NAICS3"]

cvs_establishment[, 
  model_emp_cat := 
    ifelse(!is.na(model_emp_cat_n2), model_emp_cat_n2, model_emp_cat_n3)]

cvs_establishment[, NAICS2 := NULL]
cvs_establishment[, NAICS3 := NULL]

cvs_establishment[, industry_retail := 1 * (model_emp_cat %in% c('Retail'))]
cvs_establishment[, industry_wholesale := 1 * (model_emp_cat %in% c('Wholesale'))]
cvs_establishment[, industry_construction := 1 * (model_emp_cat %in% c("Construction"))]
cvs_establishment[, industry_transport_industry := 1 * (model_emp_cat %in% c("Transport_Industry"))]
cvs_establishment[, industry_admin_support_waste := 1 * (model_emp_cat %in% c("Admin_Support_Waste"))]
cvs_establishment[, industry_ed_health_social_public := 1 * (model_emp_cat %in% c("Ed_Health_Social_Public"))]
cvs_establishment[, industry_service_other := 1 * (model_emp_cat %in% c("Service_Other"))]
cvs_establishment[, industry_office_professional := 1 * (model_emp_cat %in% c("Office_Professional"))]
cvs_establishment[, industry_service_foodDrink := 1 * (model_emp_cat %in% c("Service_FoodDrink"))]

# types of tours
# 01 - base to base, single stop
# 02 - base to base, multiple stops
# 03 - not_base to base, no stops
# 04 - not_base to base, single stop
# 05 - not_base to base, multiple stops
# 06 - base to not_base, no stops
# 07 - base to not_base, single stop
# 08 - base to not_base, multiple stops

# counting number of stops as stops not at base and not the starting location
cvs_tour[, 
  num_stops := sum(STOP_BASE_NOTBASE == 2 & STOP_SEQ > 1), 
  .(SITEID, TOUR_NUM, VEHNUM)]

cvs_tour[, .N, num_stops][order(num_stops)]

cvs_tour[, 
  last_stop := max(STOP_SEQ), 
  .(SITEID, TOUR_NUM, VEHNUM)]

# exclude any long distance tours that will be modeled elsewhere
buffer_zips = unique(taz_system[TAZ_TYPE == "BUFFER", ZCTA5CE10])

tours_to_exclude = cvs_tour[TOUR_STOP_TAZID == 9999 & !STOP_ZIP %in% buffer_zips, .N, .(SITEID, VEHNUM, TOUR_NUM)]

cvs_tour = 
   cvs_tour[!tours_to_exclude, on = .(SITEID, VEHNUM, TOUR_NUM)]

tour_types = 
  dcast(
    cvs_tour[
      STOP_SEQ == 1 | STOP_SEQ == last_stop, 
      .(SITEID, 
        TOUR_NUM,
        VEHNUM, 
        VEH_CLASS,
        TOUR_STOP_TYPE = 
          fcase(
            num_stops == 0, "NO_STOP",
            num_stops == 1, "SINGLE_STOP",
            num_stops  > 1, "MULTIPLE_STOP"),
        last_stop = c("BEGIN_LOC", "END_LOC")[1 + 1 * (last_stop == STOP_SEQ)], 
        STOP_BASE_NOTBASE = c("BASE", "NOT_BASE")[STOP_BASE_NOTBASE])],
    SITEID + TOUR_NUM + VEHNUM + VEH_CLASS + TOUR_STOP_TYPE ~ last_stop,
    value.var = "STOP_BASE_NOTBASE")

tour_types[, .N, .(BEGIN_LOC, END_LOC, TOUR_STOP_TYPE)][order(BEGIN_LOC, END_LOC, TOUR_STOP_TYPE)]

tour_types

# # removing some unwanted tour types
# tour_types =
#   tour_types[
#     !((BEGIN_LOC == 'NOT_BASE' & 
#        END_LOC == 'NOT_BASE' & 
#        TOUR_STOP_TYPE == 'SINGLE_STOP') |
#       (BEGIN_LOC == 'BASE' & 
#        END_LOC == 'BASE' & 
#        TOUR_STOP_TYPE == 'NO_STOP')),]

tour_types[, .N, .(BEGIN_LOC, END_LOC, TOUR_STOP_TYPE)][order(BEGIN_LOC, END_LOC, TOUR_STOP_TYPE)]


# excluding establishments outside the area
tour_types =
  cvs_establishment[tour_types, on = .(SITEID), nomatch = FALSE]

tour_types[VEH_CLASS %in% 1:4, veh_choice := 1]
tour_types[VEH_CLASS %in% 5:7, veh_choice := 2]
tour_types[VEH_CLASS %in% 8,   veh_choice := 3]

tour_types[, .N, .(VEH_CLASS, veh_choice)]

# similar to the vehicle choice model, we are removing "Other"
tour_types = tour_types[VEH_CLASS != 888, ]

tour_types[, .N, .(BEGIN_LOC, END_LOC, TOUR_STOP_TYPE)][order(BEGIN_LOC, END_LOC, TOUR_STOP_TYPE)]

tour_types[,
  tour_type_choice := 
    fcase(
      BEGIN_LOC == 'BASE'     & END_LOC == 'BASE'     & TOUR_STOP_TYPE == 'MULTIPLE_STOP', 1,
      BEGIN_LOC == 'BASE'     & END_LOC == 'BASE'     & TOUR_STOP_TYPE == 'SINGLE_STOP',   2,
      BEGIN_LOC == 'BASE'     & END_LOC == 'NOT_BASE' & TOUR_STOP_TYPE == 'MULTIPLE_STOP', 3,
      BEGIN_LOC == 'BASE'     & END_LOC == 'NOT_BASE' & TOUR_STOP_TYPE == 'SINGLE_STOP',   4,
      BEGIN_LOC == 'NOT_BASE' & END_LOC == 'BASE'     & TOUR_STOP_TYPE == 'MULTIPLE_STOP', 5,
      BEGIN_LOC == 'NOT_BASE' & END_LOC == 'BASE'     & TOUR_STOP_TYPE == 'NO_STOP',       6,
      BEGIN_LOC == 'NOT_BASE' & END_LOC == 'BASE'     & TOUR_STOP_TYPE == 'SINGLE_STOP',   7,
      BEGIN_LOC == 'NOT_BASE' & END_LOC == 'NOT_BASE' & TOUR_STOP_TYPE == 'MULTIPLE_STOP', 8)]

tour_types[, .N, .(tour_type_choice, BEGIN_LOC, END_LOC, TOUR_STOP_TYPE)][order(BEGIN_LOC, END_LOC, TOUR_STOP_TYPE)]

# here we are removing likely commute trips so they are not modeled
possible_commute_trips = 
  tour_types[
    tour_type_choice == 6, 
    .(SITEID, TOUR_NUM, VEHNUM)] %>% 
  .[cvs_tour, on = .(SITEID, TOUR_NUM, VEHNUM), nomatch = 0]

possible_commute_trips[
  STOP_SEQ == 1 &
    (STOP_PLACENAME %like% 'HOUSE' |
        STOP_PLACENAME %like% 'HOME' |
        STOP_PLACENAME %like% 'RESIDENCE'),
  exclude := 1]

tour_types = 
  possible_commute_trips[
    STOP_SEQ == 1, 
    .(SITEID, VEHNUM, TOUR_NUM, exclude)] %>%
    .[tour_types, on = .(SITEID, VEHNUM, TOUR_NUM)] %>% 
    .[is.na(exclude)]

tour_types

# add the total weight for each tour for producing weighted tabulations of the data
tour_types[cvs_tour[,.(FINAL_FACTOR = sum(FINAL_FACTOR)), by = .(SITEID, VEHNUM, TOUR_NUM)],
           FINAL_FACTOR := i.FINAL_FACTOR, 
           on = .(SITEID, VEHNUM, TOUR_NUM)]

# add total stops, total stop duration for producing tour level targets
# that can be affected by the clustering algorithm
tour_types[cvs_tour[STOP_BASE_NOTBASE == 2 & STOP_SEQ > 1,
                    .(num_stops = .N), 
                    by = .(SITEID, TOUR_NUM, VEHNUM)],
           num_stops := i.num_stops,
           on = .(SITEID, VEHNUM, TOUR_NUM)]

# calculating duration
cvs_tour[!is.na(DEPART_TIME), DEPART_TIME_adj := as.POSIXct(paste0('1899-12-31 ', DEPART_TIME))]
cvs_tour[!is.na(ARRIVE_TIME), ARRIVE_TIME_adj := as.POSIXct(paste0('1899-12-31 ', ARRIVE_TIME))]
cvs_tour[DEPART_TIME_adj < ARRIVE_TIME_adj, DEPART_TIME_adj := DEPART_TIME_adj + 60 * 60 * 24]
cvs_tour[, duration := as.numeric(DEPART_TIME_adj - ARRIVE_TIME_adj)]
cvs_tour[STOP_BASE_NOTBASE == 1, duration := 0] # not counting the time spent at base
cvs_tour[is.na(duration), duration := 0]

tour_types[cvs_tour[, .(total_stop_duration = sum(duration, na.rm = TRUE)), 
                    by = .(SITEID, TOUR_NUM, VEHNUM)],
           total_stop_duration := i.total_stop_duration,
           on = .(SITEID, VEHNUM, TOUR_NUM)]

# save the table before simplyfying 
model_loc = 'dev/Estimation/cv_tours'
saveRDS(tour_types[order(SITEID, TOUR_NUM, VEHNUM)], file.path(model_loc, "cv_tours_processed_data.rds"))

choice_data = copy(tour_types)

choice_data = choice_data[!is.na(tour_type_choice)]

names(choice_data)

names_to_keep = 
  c("SITEID", "TOUR_NUM", "VEHNUM",
    "veh_choice",
    "tour_type_choice",
    "industry_retail",
    "industry_wholesale",
    "industry_construction",
    "industry_transport_industry",
    "industry_admin_support_waste",
    "industry_ed_health_social_public",
    "industry_service_other",
    "industry_office_professional",
    "industry_service_foodDrink")

# some cleanup on variables not needed for modeling
choice_data = choice_data[, names_to_keep, with = FALSE]

choice_data = choice_data[order(SITEID, TOUR_NUM, VEHNUM)]

saveRDS(choice_data, file.path(model_loc, "estimation_data.rds"))

# ---------------------------------------------------------
# extra unused stuff. saving for time being
stop("Unused code")
choice_data[, .N, tour_type_choice][order(tour_type_choice)]

possible_commute_trips = 
  choice_data[
    tour_type_choice == 6, 
    .(SITEID, TOUR_NUM, VEHNUM)] %>% 
  .[cvs_tour, on = .(SITEID, TOUR_NUM, VEHNUM), nomatch = 0]

possible_commute_trips[
  STOP_SEQ == 1, 
  .(SITEID, 
    TOUR_NUM, 
    VEHNUM, 
    STOP_SEQ, 
    DEPART_TIME, 
    STOP_ACTIVITY, 
    STOP_ACTIVITY_OTHER)] %>% 
  .[, .N, .(STOP_ACTIVITY, STOP_ACTIVITY_OTHER)] %>%
.[order(STOP_ACTIVITY, STOP_ACTIVITY_OTHER)]

possible_commute_trips[
  STOP_SEQ == 2, 
  .(SITEID, 
    TOUR_NUM, 
    VEHNUM, 
    STOP_SEQ, 
    DEPART_TIME, 
    STOP_ACTIVITY, 
    STOP_ACTIVITY_OTHER)] %>% 
  .[, .N, .(STOP_ACTIVITY, STOP_ACTIVITY_OTHER)] %>%
.[order(STOP_ACTIVITY, STOP_ACTIVITY_OTHER)]
      
possible_commute_trips[
  STOP_SEQ == 1,
  .N,
  .(hour_of_departure = substr(DEPART_TIME, 1, 2))][order(hour_of_departure)]

possible_commute_trips[
  STOP_SEQ == 1,
  .N,
  .(STOP_PLACENAME)]

