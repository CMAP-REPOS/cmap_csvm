library(data.table)
library(readxl)
library(magrittr)


# locations of data files
# it is all relative
base_loc = "dev/Data_Processed/SEMCOG_Data"
model_loc = "dev/Estimation/cv_intermediate"

cvs_location  = file.path(base_loc, "SEMCOG_CV_20181128_Submitted_ForDistribution.xlsx")
skim_location = file.path(base_loc, "skims_avg.RDS") # not used
taz_centroids_location = file.path(base_loc, "TAZ_Centroids.csv")
zip_centroids_locations = file.path(base_loc, "CVS_Unique_Stop_ZIP_Centroids.csv")
emp_cats_loc = file.path(base_loc, 'NAICS3_SEMCOGEmpCats_CMAP.csv')
taz_system_location = file.path(base_loc, "TAZ_System.csv")

# load data
cvs_establishment = read_xlsx(cvs_location, sheet = "ESTABLISHMENT")
cvs_tour          = read_xlsx(cvs_location, sheet = "TOUR")
cvs_trip          = read_xlsx(cvs_location, sheet = "TRIP")

skims_avg = readRDS(skim_location)
taz_system = fread(taz_system_location)

setDT(cvs_establishment)
setDT(cvs_tour)
setDT(cvs_trip)
setDT(skims_avg)

setnames(skims_avg, "OTAZ", "otaz")
setnames(skims_avg, "DTAZ", "dtaz")

land_use_path = file.path(base_loc, "TAZSocioEconomics.csv")

land_use_data = fread(land_use_path)

# from Colin
# That has the full set of TAZs in it 
# (1:2811 internal, 2812:2899 external stations, and 
# then 2900-3638 for a set of external buffer zones 
# that we added using the MDOT statewide model. There are 
# values in the external station records but they are meaningless

land_use_data[TAZ <= 2811]

land_use_data[HH > POP]

land_use_data[HH == 0]
land_use_data[is.na(e01_nrm)]

land_use_data[
  HH == 0 & 
    e01_nrm + e02_constr +  
    e03_manuf + e04_whole + e05_retail + e06_trans + e07_utility + 
    e08_infor + e09_finan + e10_pstsvc + e11_compmgt + e12_admsvc + 
    e13_edusvc + e14_medfac + e15_hospit + e16_leisure + e17_othsvc +  
    e18_pubadm == 0]

land_use_data[TAZ <= 2811, .(sum(HH), sum(POP), sum(POP) / sum(HH))]

# Var_Name	  Sector_Name	                                    CVTM_Cat
# e01_nrm	    Natural Resources & Mining	                    Industrial
# e07_utility	Utilities	                                      Industrial
# e02_constr	Construction	                                  Industrial
# e03_manuf	  Manufacturing	                                  Industrial
# e04_whole	  Wholesale Trade	                                Retail
# * e05_retail	Retail Trade	                                  Retail
# e06_trans	  Transportation & Warehousing	                  Industrial
# e08_infor	  Information	                                    Office
# e09_finan	  Financial Activities	                          Office
# e10_pstsvc	Professional, Scientific, & Technical Services	Office
# e11_compmgt	Management of Companies & Enterprises	          Office
# e12_admsvc	Administrative, Support, & Waste Services	      Office
# e13_edusvc	Education Services	                            Education
# * e16_leisure	Leisure & Hospitality	                          Office
# e17_othsvc	Other Services	                                Office
# e18_pubadm	Public Administration	                          Office
# e14_medfac	Medical Facilities	                            Medical Services
# e15_hospit	Hospitals	                                      Medical Services

# total employment
cvs_tour[cvs_trip, FINAL_FACTOR := i.FINAL_FACTOR, on = .(SITEID, VEHNUM, STOP_SEQ)]

# data needed for modeling
cvs_establishment = 
  cvs_establishment[SITE_TAZID != 9999 & !is.na(ESTABLISHMENT_WGHT_FCTR), 
    .(SITEID, SITE_TAZID)]

# recoding other trips 
#View(cvs_tour[STOP_ACTIVITY == 888, .N, STOP_ACTIVITY_OTHER][order(-N)])
cvs_tour[
  grepl("KID", STOP_ACTIVITY_OTHER) | 
    grepl("CHILD", STOP_ACTIVITY_OTHER) |
    grepl("PERSONAL", STOP_ACTIVITY_OTHER) |
    grepl("DAUGHTER", STOP_ACTIVITY_OTHER) |
    grepl("FRIEND", STOP_ACTIVITY_OTHER) |
    grepl("PARTY", STOP_ACTIVITY_OTHER) |
    grepl("FOOTBALL", STOP_ACTIVITY_OTHER) |
    grepl("GUESTS", STOP_ACTIVITY_OTHER) |
    grepl("RECREATIONAL", STOP_ACTIVITY_OTHER) |
    grepl("GROCERIES", STOP_ACTIVITY_OTHER) |
    STOP_ACTIVITY_OTHER %in% 
      c("GOING HOME", 
        "HOME", 
        "HOME FOR EVENING",
        "CHURCH MTG",
        "SOCIAL", 
        "WORK OUT", 
        "WORKING OUT", 
        "DROP OFF BOOKS", 
        "DROP OFF PERSON AT SHELTER"), 
  .N, ]

cvs_tour[
  !is.na(STOP_ACTIVITY_OTHER),
  personal_other := 0]

cvs_tour[
  grepl("KID", STOP_ACTIVITY_OTHER) | 
    grepl("CHILD", STOP_ACTIVITY_OTHER) |
    grepl("PERSONAL", STOP_ACTIVITY_OTHER) |
    grepl("DAUGHTER", STOP_ACTIVITY_OTHER) |
    grepl("FRIEND", STOP_ACTIVITY_OTHER) |
    grepl("PARTY", STOP_ACTIVITY_OTHER) |
    grepl("FOOTBALL", STOP_ACTIVITY_OTHER) |
    grepl("GUESTS", STOP_ACTIVITY_OTHER) |
    grepl("RECREATIONAL", STOP_ACTIVITY_OTHER) |
    grepl("GROCERIES", STOP_ACTIVITY_OTHER) |
    STOP_ACTIVITY_OTHER %in% 
      c("GOING HOME", 
        "HOME", 
        "HOME FOR EVENING",
        "CHURCH MTG",
        "SOCIAL", 
        "WORK OUT", 
        "WORKING OUT", 
        "DROP OFF BOOKS", 
        "DROP OFF PERSON AT SHELTER"), 
  personal_other := 1, ]

#View(cvs_tour[personal_other == 0, .N, STOP_ACTIVITY_OTHER][order(-N)])

cvs_tour[, .N, personal_other]

# these were removed elsewhere as well
cvs_tour = 
  cvs_tour[
    !(VEHNUM %in% cvs_tour[STOP_ACTIVITY_OTHER %in% c("HOME", "HOME FOR EVENING"), VEHNUM] & 
      TOUR_NUM %in% c(1, 3))]
  
# exclude any long distance tours that will be modeled elsewhere
buffer_zips = unique(taz_system[TAZ_TYPE == "BUFFER", ZCTA5CE10])

tours_to_exclude = cvs_tour[TOUR_STOP_TAZID == 9999 & !STOP_ZIP %in% buffer_zips, .N, .(SITEID, VEHNUM, TOUR_NUM)]

cvs_tour = 
   cvs_tour[!tours_to_exclude, on = .(SITEID, VEHNUM, TOUR_NUM)]

# 1=Returning to Base Location
# -> 2=Vehicle Maintenance (fuel, oil, etc)
# -> 3=Driver Needs (lunch, restroom, etc)
# 4=Deadhead/Drop Trailer/Bobtail
# 5=Delivering cargo
# 6=Picking up cargo
# -> 7=Getting Government Related Services
# 8=Providing Installation / Maintenance / Repair Services
# 9=Making a sales call
# 10=Providing professional services (legal, medical, financial)
# -> 11=Shopping for Business
# -> 888=Other

cvs_tour[, 
  intermediate_stop_type :=
    fcase(
      !STOP_ACTIVITY %in% c(2, 3, 7, 11, 888), 1L, # no stop
      (STOP_ACTIVITY == 3L | (STOP_ACTIVITY == 888L & personal_other == 1L)), 2L, # Driver needs
      STOP_ACTIVITY == 2L, 3L, # vehicle service stop
      STOP_ACTIVITY %in% c(7L, 11L) | (STOP_ACTIVITY == 888L & personal_other == 0L), 4L
  )] # other

# excluding establishments outside the area
cvs_tour =
  cvs_establishment[cvs_tour, on = .(SITEID), nomatch = FALSE]

cvs_tour =
  cvs_tour[!is.na(intermediate_stop_type)]

#land_use_data = 
#  land_use_data[
#    cvs_tour[, .(TAZ = TOUR_STOP_TAZID, intermediate_stop_type)],
#    on = .(TAZ)]

#land_use_data[TAZ != 9999, ][order(HH)]

#land_use_data[TAZ != 9999, .N, TAZ][order(TAZ)]

#land_use_data[
#  TAZ != 9999, 
#  .(leisure = mean(e16_leisure), 
#    retail = mean(e05_retail),
#    POP = mean(POP),
#    HH = mean(HH)), 
#  .(intermediate_stop_type = 
#      factor(
#        intermediate_stop_type, 
#        levels = 1:4,
#        labels = c("No stop", "Driver needs", "Vehicle", "Other")))][order(intermediate_stop_type)]

land_use_data

cv_intermediate_deviation = readRDS("cv_intermediate_deviations.rds")

cv_intermediate_deviation

# generating choice set
choice_set = skims_avg[dist.avg < max(cv_intermediate_deviation[, deviation.dist]) & otaz <= 2811 & dtaz <= 2811]

choice_set[, c("time.avg", "dist.rev", "time.rev", "OREGION", "DREGION") := NULL]

# tazs with 1 and only 1 alternative
choice_set = choice_set[!otaz %in% choice_set[, .N, otaz][N == 1, otaz],]

choice_set[, .N, otaz][order(-N)]

choice_set[, TAZ := dtaz]
choice_set[, dtaz := NULL]
setnames(choice_set, "otaz", "intermediate_taz")

setnames(land_use_data, "TAZ", "intermediate_taz")

choice_set = land_use_data[choice_set, on = .(intermediate_taz)]

intermediate_stops = 
  cvs_tour[
    TOUR_STOP_TAZID <= 2811 & intermediate_stop_type != 1, 
    .(SITEID, 
      VEHNUM,
      TOUR_NUM,
      STOP_SEQ,
      TAZ = TOUR_STOP_TAZID, 
      intermediate_stop_type)]

intermediate_stops[, next_stop := STOP_SEQ + 1]

intermediate_stops =
  intermediate_stops[
    cvs_tour, 
    on = c(next_stop = "STOP_SEQ", "SITEID", "VEHNUM", "TOUR_NUM")]

intermediate_stops = 
  intermediate_stops[
    i.intermediate_stop_type == 1, #this is only looking at a single intermediate stop before a non-intermediate stop
    .(SITEID, 
      VEHNUM, 
      TOUR_NUM, 
      STOP_SEQ = next_stop, 
      TAZ = TOUR_STOP_TAZID, # where non-intermediate stop occurred
      intermediate_taz = TAZ, # where intermediate stop occurred
      intermediate_stop_type)]

intermediate_stops[is.na(intermediate_stop_type), .N, intermediate_taz]
intermediate_stops[, .N, intermediate_stop_type]

intermediate_stops[is.na(intermediate_stop_type), intermediate_stop_type := 1]

intermediate_stops[, .N, .(TAZ, VEHNUM, SITEID, STOP_SEQ, TOUR_NUM)][N != 1]

intermediate_stops = intermediate_stops[!is.na(intermediate_taz)]

choice_data = choice_set[intermediate_stops, on = .(TAZ), nomatch = 0, allow.cartesian = TRUE]

choice_data[order(SITEID, VEHNUM, TOUR_NUM, STOP_SEQ)]

choice_data[, choice := 1 * (intermediate_taz == i.intermediate_taz)]

choice_data[, .N, choice]

choice_data[, .N, .(SITEID, VEHNUM, TOUR_NUM, STOP_SEQ, choice)][, .N, .(SITEID, VEHNUM, TOUR_NUM, STOP_SEQ)][N != 2]
choices_within_deviation = 
  choice_data[, .N, .(SITEID, VEHNUM, TOUR_NUM, STOP_SEQ, choice)][, .N, .(SITEID, VEHNUM, TOUR_NUM, STOP_SEQ)][N == 2]

choice_data = choice_data[choices_within_deviation, on = .(SITEID, VEHNUM, TOUR_NUM, STOP_SEQ), nomatch = 0]

choice_data[SITEID == 100710 & VEHNUM == 41267 & TOUR_NUM == 1 & STOP_SEQ == 2]

choice_data[, num_alts := .N, .(SITEID, VEHNUM, TOUR_NUM, STOP_SEQ)]

choice_data[, .N, num_alts][order(num_alts)]

choice_data[num_alts == 1, .(TAZ, intermediate_taz)]

choice_data[, 
  EMP := e01_nrm + e02_constr + e03_manuf + e04_whole + e05_retail +            
    e06_trans + e07_utility + e08_infor + e09_finan + e10_pstsvc + 
    e11_compmgt + e12_admsvc + e13_edusvc + e14_medfac + e15_hospit + 
    e16_leisure + e17_othsvc + e18_pubadm]


# save the table before simplyfying 
saveRDS(choice_data[order(SITEID, TOUR_NUM, VEHNUM, STOP_SEQ)], "cv_intermediate_attraction_processed_data.rds")

keep_cols = 
  c("SITEID", "VEHNUM", "TOUR_NUM", "STOP_SEQ", 
    "EMP", "HH", "POP", "e05_retail", "e16_leisure", "dist.avg",                   
    "intermediate_stop_type", "num_alts", "choice")

choice_data = choice_data[num_alts > 1, keep_cols, with = FALSE]

setnames(choice_data, "dist.avg", "dist")

choice_data = choice_data[order(SITEID, VEHNUM, TOUR_NUM, STOP_SEQ)]

saveRDS(choice_data, "estimation_data_attraction.RDS")


