# This script contains the funtions which perform the processing 
# of data for the summary spreadsheet

# Function to create TAZ levels summaries for Model Region from component outputs
createTAZSummaries <- function(scenario, scenario_output_path, load_db_workspace = FALSE){ 
  
  # Load the db workspace if this is an alternative scenario
  if(load_db_workspace){
    load(file.path(scenario_output_path, SYSTEM_DB_OUTPUTNAME))
    ScenarioFirms <- db_inputs$ScenarioFirms
    TAZSocioEconomics <- db_inputs$TAZSocioEconomics
    ld_trips <- db_inputs$ld_trips
    cv_trips <- db_inputs$cv_trips
    TripTable <- db_inputs$TripTable
  }
  
  # Create TAZ level summaries
  
  # Firm synthesis: firms and employment, compared with SE data
  firm_sim_taz <- ScenarioFirms[,.(Firms = .N, Emp = sum(Employees)), 
                                                 keyby = .(EmpCatID, EmpCatName , TAZ)]
  
  firm_sim_se <- TAZSocioEconomics
  firm_sim_se <- melt.data.table(firm_sim_se,
                                 id.vars = c("TAZ", "HH", "POP"),
                                 variable.name = "EmpCatName",
                                 value.name = "SE")
  firm_sim_taz <- merge(firm_sim_taz,
                        firm_sim_se[,.(TAZ, EmpCatName, SE)],
                        by = c("TAZ", "EmpCatName"),
                        all = TRUE)
  firm_sim_taz[is.na(firm_sim_taz)] <- 0
  
  firm_sim_taz[, Diff := Emp - SE]
  
  firm_sim_taz_w <- dcast.data.table(firm_sim_taz,
                                     TAZ ~ EmpCatName,
                                     fun.aggregate = sum,
                                     value.var = c("Firms", "Emp", "SE", "Diff"))
  
  # Long distance model
  ld_taz_o_trips <- ld_trips[,.(NumTrips = sum(Trips, na.rm = TRUE)),
                             keyby = .(TAZ = OTAZ, Vehicle, Movement.Type)]
  
  ld_taz_d_trips <- ld_trips[,.(NumTrips = sum(Trips, na.rm = TRUE)),
                             keyby = .(TAZ =DTAZ, Vehicle, Movement.Type)]
  
  ld_taz_trips <- merge(ld_taz_o_trips[, .(oTripsLD = sum(NumTrips)), by = .(TAZ, Vehicle)],
                        ld_taz_d_trips[, .(dTripsLD = sum(NumTrips)), by = .(TAZ, Vehicle)],
                        by = c("TAZ", "Vehicle"),
                        all = TRUE)
  
  ld_taz_trips[is.na(ld_taz_trips)] <- 0
  
  ld_taz_trips_w <- dcast.data.table(ld_taz_trips,
                                     TAZ ~ Vehicle,
                                     fun.aggregate = sum,
                                     value.var = c("oTripsLD", "dTripsLD"))
  
  # Commercial vehicle touring model: trips
  cv_taz_o_trips <- cv_trips[,.(NumTrips = .N),
                              keyby = .(TAZ = OTAZ, Vehicle, Scheduled)]
  
  cv_taz_d_trips <- cv_trips[,.(NumTrips = .N),
                              keyby = .(TAZ =DTAZ, Vehicle, Scheduled, Activity)]
  
  cv_taz_trips <- merge(cv_taz_o_trips[, .(oTripsCV = sum(NumTrips)), by = .(TAZ, Vehicle)],
                        cv_taz_d_trips[, .(dTripsCV = sum(NumTrips)), by = .(TAZ, Vehicle)],
                        by = c("TAZ", "Vehicle"),
                        all = TRUE)
  
  cv_taz_trips[is.na(cv_taz_trips)] <- 0
  
  cv_taz_trips_w <- dcast.data.table(cv_taz_trips,
                                     TAZ ~ Vehicle,
                                     fun.aggregate = sum,
                                     value.var = c("oTripsCV", "dTripsCV"))
  
  # Trip tables: trips
  tt_taz_o_trips <- TripTable[,.(NumTrips = .N),
                               keyby = .(TAZ = OTAZ, Vehicle)]
  
  tt_taz_d_trips <- TripTable[,.(NumTrips = .N),
                               keyby = .(TAZ = DTAZ, Vehicle)]
  
  tt_taz_trips <- merge(tt_taz_o_trips[, .(oTripsAll = sum(NumTrips)), by = .(TAZ, Vehicle)],
                        tt_taz_d_trips[, .(dTripsAll = sum(NumTrips)), by = .(TAZ, Vehicle)],
                        by = c("TAZ", "Vehicle"),
                        all = TRUE)
  
  tt_taz_trips[is.na(tt_taz_trips)] <- 0
  
  tt_taz_trips_w <- dcast.data.table(tt_taz_trips,
                                     TAZ ~ Vehicle,
                                     fun.aggregate = sum,
                                     value.var = c("oTripsAll", "dTripsAll"))
  
  # Return a list of all of the tables created
  returnlist <- list(firm_sim_taz = firm_sim_taz,
                     firm_sim_taz_w = firm_sim_taz_w,
                     cv_taz_o_trips = cv_taz_o_trips,
                     cv_taz_d_trips = cv_taz_d_trips,
                     cv_taz_trips = cv_taz_trips,
                     cv_taz_trips_w = cv_taz_trips_w,
                     ld_taz_o_trips = ld_taz_o_trips,
                     ld_taz_d_trips = ld_taz_d_trips,
                     ld_taz_trips = ld_taz_trips,
                     ld_taz_trips_w = ld_taz_trips_w,
                     tt_taz_o_trips = tt_taz_o_trips,
                     tt_taz_d_trips = tt_taz_d_trips,
                     tt_taz_trips = tt_taz_trips,
                     tt_taz_trips_w = tt_taz_trips_w)
  
  returnlist$tabletitles <- names(returnlist)
  returnlist$scenario <- scenario
  
  return(returnlist)
  
}

# Function to create truck trip summaries from trip tables
createTripTableSummaries <- function(scenario, scenario_output_path, load_db_workspace = FALSE){
  
  # Load the db workspace if this is an alternative scenario
  if(load_db_workspace){
    load(file.path(scenario_output_path, SYSTEM_DB_OUTPUTNAME))
    # ScenarioFirms <- db_inputs$ScenarioFirms
    # TAZSocioEconomics <- db_inputs$TAZSocioEconomics
    # ld_trips <- db_inputs$ld_trips
    # cv_trips <- db_inputs$cv_trips
    TripTable <- db_inputs$TripTable
    tmh_vtods_wide <- db_inputs$tmh_vtods_wide
  }
  
  # Daily tt (aggregate over time of day)
  ttd <- TripTable[,.(trips = sum(trips), 
               time = mean(time), 
               distance = mean(dist)), 
            by = .(OTAZ, DTAZ, Vehicle, ODSegment, OSummaryGeog, DSummaryGeog)]
  
  # Time of Day (simplify to remove OTAZ and DTAZ)
  tttod <- TripTable[,.(trips = sum(trips), 
                 time = sum(trips*time)/sum(trips), 
                 distance = sum(dist*trips)/sum(trips)), 
              by = .(TOD, Vehicle, ODSegment, OSummaryGeog, DSummaryGeog)]
  
  # Daily summaries by vehicle type, OD segment, region
  daily.sum.veh <- add_totals(ttd[,.(Trips = round(sum(trips)), 
                                     VMT = round(sum(trips * distance)), 
                                     VHT = round(sum(trips * time/60))), 
                                  keyby = .(Vehicle)], rowtotal = FALSE)
  
  daily.sum.od <- add_totals(ttd[,.(Trips = round(sum(trips)), 
                                    VMT = round(sum(trips * distance)), 
                                    VHT = round(sum(trips * time/60))), 
                                 keyby = .(ODSegment)],
                             rowtotal = FALSE)
  
  daily.sum.veh.od <- add_totals(ttd[,.(Trips = round(sum(trips)), 
                                        VMT = round(sum(trips * distance)), 
                                        VHT = round(sum(trips * time/60))), 
                                     keyby = .(Vehicle, ODSegment)],
                                 idcols = 2L,
                                 rowtotal = FALSE)
  
  daily.sum.veh.odreg <- add_totals(ttd[,.(Trips = round(sum(trips)), 
                                           VMT = round(sum(trips * distance)), 
                                           VHT = round(sum(trips * time/60))), 
                                        keyby = .(Vehicle, ODSegment, OSummaryGeog, DSummaryGeog)],
                                    idcols = 4L,
                                    rowtotal = FALSE)
  
  xt.mh.reg.trips <- add_totals(dcast.data.table(daily.sum.veh.odreg[Vehicle != "Light"], 
                                                 OSummaryGeog~DSummaryGeog, 
                                                 fun.aggregate = sum, 
                                                 value.var = "Trips"))
  
  xt.mh.reg.vmt <- add_totals(dcast.data.table(daily.sum.veh.odreg[Vehicle != "Light"], 
                                               OSummaryGeog~DSummaryGeog, 
                                               fun.aggregate = sum, 
                                               value.var = "VMT"))
  # Trips crossing cordons
  ttd[, ExternalCordon := trips * c(0,1,1,2)[match(ODSegment, c("II", "IX", "XI", "XX"))]]
  
  # tag externals in Canada
  international_ext <- TAZ_System[StationGroup == "Ontario"]$TAZ
  ttd[, InternationalCordon := trips * ifelse((OTAZ %in% international_ext & !DTAZ %in% international_ext)|
                                       (DTAZ %in% international_ext & !OTAZ %in% international_ext),1,0)]
  
  # Summarize cordon trips
  xt.mh.cordon.external <- add_totals(dcast.data.table(ttd, ODSegment~Vehicle, fun.aggregate = sum, value.var = "ExternalCordon"))
  xt.mh.cordon.international <- add_totals(dcast.data.table(ttd, OSummaryGeog+DSummaryGeog~Vehicle, fun.aggregate = sum, value.var = "InternationalCordon"),
                                   idcols = 1:2)[Total>0]
  
  # Summarise external trips by station
  ext.in <- add_totals(dcast.data.table(ttd[ODSegment %in% c("XI", "XX"), .(Trips = sum(trips)), keyby = .(TAZ = as.character(OTAZ), ODSegment, Vehicle)],
                                        TAZ ~ ODSegment + Vehicle, fun.aggregate = sum, value.var = "Trips"))
  setnames(ext.in, names(ext.in)[2:8], paste0("In_", names(ext.in)[2:8]))
  ext.out <- add_totals(dcast.data.table(ttd[ODSegment %in% c("IX", "XX"), .(Trips = sum(trips)), keyby = .(TAZ = as.character(DTAZ), ODSegment, Vehicle)],
                                         TAZ ~ ODSegment + Vehicle, fun.aggregate = sum, value.var = "Trips"))
  setnames(ext.out, names(ext.out)[2:8], paste0("Out_", names(ext.out)[2:8]))
  
  ext.sum <- merge(TAZ_System[EXTERNAL == 1, .(TAZ = as.character(TAZ), StationGroup, StationName, StationType)],
                   ext.in,
                   by = "TAZ") 
  
  ext.sum <- merge(ext.sum,
                   ext.out,
                   by = "TAZ")
  
  ext.sum <- add_totals(ext.sum,
                        idcols = 4L,
                        rowtotal = FALSE)
  
  # Create a vector of tabletitles (pretty names for the table to be used in the spreadsheet)
  tabletitles = c("Daily Trip Summary by Vehicle Type",
                  "Daily Trip Summary by OD Segment",
                  "Daily Trip Summary by Vehicle Type and OD Segment",
                  "Daily Trip Summary by Vehicle Type, OD Segment, and OD Regions",
                  "Tabulation of Total (Medium + Heavy Trucks) Daily Trips by OD Regions",
                  "Tabulation of Total (Medium + Heavy Trucks) Daily VMT by OD Regions",
                  "Daily Trips Crossing the External Cordon by Vehicle Type and OD Segment",
                  "Daily Trips Crossing the International Cordon by Vehicle Type and OD Regions",
                  "Daily Trips by External Station, Direction, Vehicle Type, and OD Segment")
  
  # Return a list of all of the tables created, with those for the spreadsheet first
  return(list(daily.sum.veh = daily.sum.veh,
              daily.sum.od = daily.sum.od,
              daily.sum.veh.od = daily.sum.veh.od,
              daily.sum.veh.odreg = daily.sum.veh.odreg,
              xt.mh.reg.trips = xt.mh.reg.trips,
              xt.mh.reg.vmt = xt.mh.reg.vmt,
              xt.mh.cordon.external = xt.mh.cordon.external,
              xt.mh.cordon.international = xt.mh.cordon.international,
              ext.sum = ext.sum,
              tttod = tttod,
              tabletitles = tabletitles,
              scenario = scenario))
  
}

# Function to create comparions of TAZ summaries
compareTAZ <- function(scenario_1_taz_list, scenario_2_taz_list){
  
  # Compare the firm synthesis outputs
  # Compare difference in scenarios to difference in SE
  
  firm_sim_taz_1 <- copy(scenario_1_taz_list[["firm_sim_taz"]])
  firm_sim_taz_2 <- copy(scenario_2_taz_list[["firm_sim_taz"]])
  
  setnames(firm_sim_taz_1, 
           c("TAZ", "EmpCatName", paste("Ref", 
                                          names(firm_sim_taz_1)[3:ncol(firm_sim_taz_1)],
                                          sep = "_")))
  
  setnames(firm_sim_taz_2, 
           c("TAZ", "EmpCatName", paste("Alt", 
                                          names(firm_sim_taz_2)[3:ncol(firm_sim_taz_2)],
                                          sep = "_")))
  
  firm_sim_taz <- merge(firm_sim_taz_1,
                        firm_sim_taz_2,
                        by = c("TAZ", "EmpCatName"),
                        all = TRUE)
  
  firm_sim_taz[is.na(firm_sim_taz)] <- 0
  
  firm_sim_taz[, c("Diff_Firms","Diff_Emp", "Diff_SE") := .(Alt_Firms - Ref_Firms,
                                                            Alt_Emp - Ref_Emp,
                                                            Alt_SE - Ref_SE)]
  
  firm_sim_taz[, Diff_Diff := Diff_Emp - Diff_SE]
  
  cols <- names(firm_sim_taz)[3:ncol(firm_sim_taz)]
  firm_sim_taz_sum <- firm_sim_taz[,lapply(.SD, sum), by = TAZ, .SDcols = cols]
  
  # Compare the long distance model outputs
  ld_taz_trips_1 <- copy(scenario_1_taz_list[["ld_taz_trips"]])
  ld_taz_trips_2 <- copy(scenario_2_taz_list[["ld_taz_trips"]])
  
  setnames(ld_taz_trips_1, 
           c("TAZ", "Vehicle", paste("Ref", 
                                     names(ld_taz_trips_1)[3:ncol(ld_taz_trips_1)],
                                     sep = "_")))
  
  setnames(ld_taz_trips_2, 
           c("TAZ", "Vehicle", paste("Alt", 
                                     names(ld_taz_trips_2)[3:ncol(ld_taz_trips_2)],
                                     sep = "_")))
  
  ld_taz_trips <- merge(ld_taz_trips_1,
                        ld_taz_trips_2,
                        by = c("TAZ", "Vehicle"),
                        all = TRUE)
  
  ld_taz_trips[is.na(ld_taz_trips)] <- 0
  
  ld_taz_trips[, c("Diff_oTripsLD", "Diff_dTripsLD") := .(Alt_oTripsLD - Ref_oTripsLD,
                                                          Alt_dTripsLD - Ref_dTripsLD)]
  
  ld_taz_trips_w <- dcast.data.table(ld_taz_trips,
                                     TAZ ~ Vehicle,
                                     fun.aggregate = sum,
                                     value.var = c("Ref_oTripsLD", "Ref_dTripsLD", 
                                                   "Alt_oTripsLD", "Alt_dTripsLD", 
                                                   "Diff_oTripsLD", "Diff_dTripsLD"))
  
  # Compare the commercial vehicle touring outputs
  cv_taz_trips_1 <- copy(scenario_1_taz_list[["cv_taz_trips"]])
  cv_taz_trips_2 <- copy(scenario_2_taz_list[["cv_taz_trips"]])
  
  setnames(cv_taz_trips_1, 
           c("TAZ", "Vehicle", paste("Ref", 
                                     names(cv_taz_trips_1)[3:ncol(cv_taz_trips_1)],
                                     sep = "_")))
  
  setnames(cv_taz_trips_2, 
           c("TAZ", "Vehicle", paste("Alt", 
                                     names(cv_taz_trips_2)[3:ncol(cv_taz_trips_2)],
                                     sep = "_")))
  
  cv_taz_trips <- merge(cv_taz_trips_1,
                        cv_taz_trips_2,
                        by = c("TAZ", "Vehicle"),
                        all = TRUE)
  
  cv_taz_trips[is.na(cv_taz_trips)] <- 0
  
  cv_taz_trips[, c("Diff_oTripsCV", "Diff_dTripsCV") := .(Alt_oTripsCV - Ref_oTripsCV,
                                                          Alt_dTripsCV - Ref_dTripsCV)]
  
  cv_taz_trips_w <- dcast.data.table(cv_taz_trips,
                                     TAZ ~ Vehicle,
                                     fun.aggregate = sum,
                                     value.var = c("Ref_oTripsCV", "Ref_dTripsCV", 
                                                   "Alt_oTripsCV", "Alt_dTripsCV", 
                                                   "Diff_oTripsCV", "Diff_dTripsCV"))
  
  # Compare the trip table outputs
  tt_taz_trips_1 <- copy(scenario_1_taz_list[["tt_taz_trips"]])
  tt_taz_trips_2 <- copy(scenario_2_taz_list[["tt_taz_trips"]])
  
  setnames(tt_taz_trips_1, 
           c("TAZ", "Vehicle", paste("Ref", 
                                     names(tt_taz_trips_1)[3:ncol(tt_taz_trips_1)],
                                     sep = "_")))
  
  setnames(tt_taz_trips_2, 
           c("TAZ", "Vehicle", paste("Alt", 
                                     names(tt_taz_trips_2)[3:ncol(tt_taz_trips_2)],
                                     sep = "_")))
  
  tt_taz_trips <- merge(tt_taz_trips_1,
                        tt_taz_trips_2,
                        by = c("TAZ", "Vehicle"),
                        all = TRUE)
  
  tt_taz_trips[is.na(tt_taz_trips)] <- 0
  
  tt_taz_trips[, c("Diff_oTripsAll", "Diff_dTripsAll") := .(Alt_oTripsAll - Ref_oTripsAll,
                                                            Alt_dTripsAll - Ref_dTripsAll)]
  
  tt_taz_trips_w <- dcast.data.table(tt_taz_trips,
                                     TAZ ~ Vehicle,
                                     fun.aggregate = sum,
                                     value.var = c("Ref_oTripsAll", "Ref_dTripsAll", 
                                                   "Alt_oTripsAll", "Alt_dTripsAll", 
                                                   "Diff_oTripsAll", "Diff_dTripsAll"))
  
  # Create a unified summary
  taz_sum <- merge(firm_sim_taz_sum,
                   ld_taz_trips_w,
                   by = "TAZ",
                   all = TRUE)
  
  taz_sum <- merge(taz_sum,
                   cv_taz_trips_w,
                   by = "TAZ",
                   all = TRUE)
  
  taz_sum <- merge(taz_sum,
                   tt_taz_trips_w,
                   by = "TAZ",
                   all = TRUE)
  
  taz_sum[is.na(taz_sum)] <- 0
  
  # Table titles
  tabletitles = c("Comparison by TAZ",
                  "Firm Synthesis by TAZ and Employment Category",
                  "Firm Synthesis by TAZ",
                  "Long Distance Truck Trips by TAZ and Vehicle",
                  "Long Distance Truck Trips by TAZ and Vehicle (Wide)",
                  "Commercial Vehicle Trips by TAZ and Vehicle",
                  "Commercial Vehicle Trips by TAZ and Vehicle (Wide)",
                  "Total Trips by TAZ and Vehicle",
                  "Total Trips by TAZ and Vehicle (Wide)")
  
  # Return tables, titles, and scenario names/years
  return(list(taz_sum = taz_sum,
              firm_sim_taz = firm_sim_taz,
              firm_sim_taz_sum = firm_sim_taz_sum,
              ld_taz_trips = ld_taz_trips,
              ld_taz_trips_w = ld_taz_trips_w,
              cv_taz_trips = cv_taz_trips,
              cv_taz_trips_w = cv_taz_trips_w,
              tt_taz_trips = tt_taz_trips,
              tt_taz_trips_w = tt_taz_trips_w,
              tabletitles = tabletitles,
              scenario_1 = scenario_1_taz_list$scenario,
              scenario_2 = scenario_2_taz_list$scenario))
  
}

# Function to create comparisons of truck trip summaries
compareTripTable <- function(scenario_1_tt_list, scenario_2_tt_list){
  
  # Tables required:
  
  # daily.sum.veh -- scenario 1, scenario 2, diff (2-1), pct change (2-1)/1
  
  daily.sum.veh_1 <- copy(scenario_1_tt_list[["daily.sum.veh"]])
  daily.sum.veh_2 <- copy(scenario_2_tt_list[["daily.sum.veh"]])
  
  setnames(daily.sum.veh_1, 
           c("Vehicle", paste("Ref", 
                              names(daily.sum.veh_1)[2:ncol(daily.sum.veh_1)],
                              sep = "_")))
  
  setnames(daily.sum.veh_2, 
           c("Vehicle", paste("Alt", 
                              names(daily.sum.veh_2)[2:ncol(daily.sum.veh_2)],
                              sep = "_")))
  
  daily.sum.veh <- merge(daily.sum.veh_1,
                         daily.sum.veh_2,
                         by = "Vehicle")
  
  daily.sum.veh[, c("Diff_Trips","Diff_VMT", "Diff_VHT") := .(Alt_Trips - Ref_Trips,
                                                              Alt_VMT - Ref_VMT,
                                                              Alt_VHT - Ref_VHT)]
  
  daily.sum.veh[, c("PctCh_Trips","PctCh_VMT", "PctCh_VHT") := .(Diff_Trips/Ref_Trips,
                                                                 Diff_VMT/Ref_VMT,
                                                                 Diff_VHT/Ref_VHT)]
  
  daily.sum.veh[is.na(daily.sum.veh)] <- 0
  
  # daily.sum.od -- scenario 1, scenario 2, diff (2-1), pct change (2-1)/1
  daily.sum.od_1 <- copy(scenario_1_tt_list[["daily.sum.od"]])
  daily.sum.od_2 <- copy(scenario_2_tt_list[["daily.sum.od"]])
  
  setnames(daily.sum.od_1, 
           c("ODSegment", paste("Ref", 
                                names(daily.sum.od_1)[2:ncol(daily.sum.od_1)],
                                sep = "_")))
  
  setnames(daily.sum.od_2, 
           c("ODSegment", paste("Alt", 
                                names(daily.sum.od_2)[2:ncol(daily.sum.od_2)],
                                sep = "_")))
  
  daily.sum.od <- merge(daily.sum.od_1,
                        daily.sum.od_2,
                        by = "ODSegment")
  
  daily.sum.od[, c("Diff_Trips","Diff_VMT", "Diff_VHT") := .(Alt_Trips - Ref_Trips,
                                                             Alt_VMT - Ref_VMT,
                                                             Alt_VHT - Ref_VHT)]
  
  daily.sum.od[, c("PctCh_Trips","PctCh_VMT", "PctCh_VHT") := .(Diff_Trips/Ref_Trips,
                                                                Diff_VMT/Ref_VMT,
                                                                Diff_VHT/Ref_VHT)]
  
  daily.sum.od[is.na(daily.sum.od)] <- 0
  
  # daily.sum.veh.od -- scenario 1, scenario 2, diff (2-1), pct change (2-1)/1
  daily.sum.veh.od_1 <- copy(scenario_1_tt_list[["daily.sum.veh.od"]])
  daily.sum.veh.od_2 <- copy(scenario_2_tt_list[["daily.sum.veh.od"]])
  
  setnames(daily.sum.veh.od_1, 
           c("Vehicle", "ODSegment", paste("Ref", 
                                           names(daily.sum.veh.od_1)[3:ncol(daily.sum.veh.od_1)],
                                           sep = "_")))
  
  setnames(daily.sum.veh.od_2, 
           c("Vehicle", "ODSegment", paste("Alt", 
                                           names(daily.sum.veh.od_2)[3:ncol(daily.sum.veh.od_2)],
                                           sep = "_")))
  
  
  daily.sum.veh.od_1[is.na(Vehicle), Vehicle := "Total"]
  daily.sum.veh.od_2[is.na(Vehicle), Vehicle := "Total"]
  
  daily.sum.veh.od <- merge(daily.sum.veh.od_1,
                            daily.sum.veh.od_2,
                            by = c("Vehicle", "ODSegment"))
  
  daily.sum.veh.od[, c("Diff_Trips","Diff_VMT", "Diff_VHT") := .(Alt_Trips - Ref_Trips,
                                                                 Alt_VMT - Ref_VMT,
                                                                 Alt_VHT - Ref_VHT)]
  
  daily.sum.veh.od[, c("PctCh_Trips","PctCh_VMT", "PctCh_VHT") := .(Diff_Trips/Ref_Trips,
                                                                    Diff_VMT/Ref_VMT,
                                                                    Diff_VHT/Ref_VHT)]
  
  daily.sum.veh.od[is.na(daily.sum.veh.od)] <- 0
  daily.sum.veh.od[Vehicle == "Total", Vehicle := NA]
  
  # Table titles
  tabletitles = c("Comparison of Daily Trips, VMT, and VHT by Vehicle Type",
                  "Comparison of Daily Trips, VMT, and VHT by OD Segment",
                  "Comparison of Daily Trips, VMT, and VHT by Vehicle Type and OD Segment")
  
  # Return tables, titles, and scenario names/years
  return(list(daily.sum.veh = daily.sum.veh,
              daily.sum.od = daily.sum.od,
              daily.sum.veh.od = daily.sum.veh.od,
              tabletitles = tabletitles,
              scenario_1 = scenario_1_tt_list$scenario,
              scenario_2 = scenario_2_tt_list$scenario))
  
}

# Function to add a sheet and a list of summary tables
addScenarioSummarySheet <- function(wb, sheetname, tableslist, 
                                    tabletitles = names(tableslist), 
                                    introtext = list(sheetname),
                                    fmtlist = NULL){
  
  # styles and options used in the spreadsheet
  options("openxlsx.numFmt" = "#,##0") # 0 decimal cases formating, comma seperator for default numeric style
  hs1 <- createStyle(fgFill = "#DCE6F1", halign = "CENTER", border = "TopBottomLeftRight") # header
  st1 <- createStyle(fgFill = "#DCE6F1", halign = "LEFT", fontSize = 16) # sheet title
  nfcur <- createStyle(numFmt = "$ #,##0") # currency format for any dollar value fields
  nf3d <- createStyle(numFmt = "#,##0.000")# 3 decimal places for any fractional values
  nfpct <- createStyle(numFmt = "0%") # percentage format for any growth ratios
  
  addWorksheet(wb, sheetname)
  setColWidths(wb, sheetname, cols = 1:100, widths = "auto")
  addStyle(wb, sheetname, style = st1, rows = 1, cols = 1:100)
  
  for(i in 1:length(introtext)){
    writeData(wb, sheetname, introtext[[i]], startRow = i)
  }
  
  endrow <- length(introtext) 
  
  # add every table with its names as title
  for(i in 1:length(tableslist)){
    
    # Write the title and table
    writeData(wb, sheetname, tabletitles[i], startRow = endrow + 2)  
    writeData(wb, sheetname, tableslist[[i]], startRow = endrow + 3, borders = "all", headerStyle = hs1)
    
    # add any column formatting by column
    if(!is.null(fmtlist)){
      if(nrow(fmtlist[table == i])>0){
        for(j in 1:nrow(fmtlist[table == i])){ 
          addStyle(wb, 
                   sheetname,
                   get(fmtlist[table == i]$format[j]),
                   rows = (endrow + 4):(endrow + 3 + nrow(tableslist[[i]])),
                   cols = fmtlist[table == i]$col[j],
                   gridExpand = TRUE,
                   stack = TRUE)
        }
      }
    }
    
    # increment the endrow
    endrow <- endrow + 4 + nrow(tableslist[[i]])
    
  }
  
  return(wb)
}

# Function to create assignment summaries from time period and daily flow table
createAssignmentSummaries <- function(scenario, model_flows){

  # add the volumes from the network flows
  model_flows[is.na(model_flows)] <- 0
  model_flows[, TOT_FLOW_PASS_CAR := AB_FLOW_SOV + BA_FLOW_SOV + AB_FLOW_HOV2 + BA_FLOW_HOV2 + AB_FLOW_HOV3 + BA_FLOW_HOV3]
  model_flows[, TOT_FLOW_LIGHT_TRUCK := `AB_FLOW_LIGHT TRUCK` + `BA_FLOW_LIGHT TRUCK`]
  model_flows[, TOT_FLOW_LIGHT_VEHICLE := TOT_FLOW_PASS_CAR + TOT_FLOW_LIGHT_TRUCK]
  model_flows[, TOT_FLOW_MEDIUM_TRUCK := `AB_FLOW_MEDIUM TRUCK` + `BA_FLOW_MEDIUM TRUCK`]
  model_flows[, TOT_FLOW_HEAVY_TRUCK := `AB_FLOW_HEAVY TRUCK` + `BA_FLOW_HEAVY TRUCK`]
  model_flows[, TOT_FLOW_TRUCK := TOT_FLOW_MEDIUM_TRUCK + TOT_FLOW_HEAVY_TRUCK]
  model_flows[, TOT_FLOW_CHECK := TOT_FLOW_LIGHT_VEHICLE + TOT_FLOW_MEDIUM_TRUCK + TOT_FLOW_HEAVY_TRUCK]
  
  model_flow_summary <- model_flows[,.(LINKSERIAL = ID1, TOD, TOT_FLOW_PASS_CAR, TOT_FLOW_LIGHT_TRUCK, TOT_FLOW_LIGHT_VEHICLE,
                                       TOT_FLOW_MEDIUM_TRUCK, TOT_FLOW_HEAVY_TRUCK, TOT_FLOW_TRUCK, TOT_FLOW_VEHICLE = TOT_FLOW)]
  
  # # Join on link distances for VMT summary from flows
  # model_flow_summary[hwy_shp_dt, c("FCLASS", "LENGTH") := .(i.FCLASS, i.LENGTH), on = "LINKSERIAL"]
  # model_flow_summary[, VMT_Pass_Car := TOT_FLOW_PASS_CAR * LENGTH]
  # model_flow_summary[, VMT_Light_Truck := TOT_FLOW_LIGHT_TRUCK * LENGTH]
  # model_flow_summary[, VMT_Light_Vehicle := TOT_FLOW_LIGHT_VEHICLE * LENGTH]
  # model_flow_summary[, VMT_Medium_Truck := TOT_FLOW_MEDIUM_TRUCK * LENGTH]
  # model_flow_summary[, VMT_Heavy_Truck := TOT_FLOW_HEAVY_TRUCK * LENGTH]
  # model_flow_summary[, VMT_Truck := TOT_FLOW_TRUCK * LENGTH]
  # model_flow_summary[, VMT_Vehicle := TOT_FLOW_VEHICLE * LENGTH]
  
  # compare with the complete set of daily counts
  daily_comp_all <- merge(counts_daily_class,
                          model_flow_summary[TOD == "DY"
                                             ,.(LINKSERIAL, TOT_FLOW_PASS_CAR, TOT_FLOW_LIGHT_TRUCK, TOT_FLOW_LIGHT_VEHICLE,
                                                TOT_FLOW_MEDIUM_TRUCK, TOT_FLOW_HEAVY_TRUCK, TOT_FLOW_TRUCK, TOT_FLOW_VEHICLE)],
                          by = "LINKSERIAL",
                          all.x = TRUE)
  
  daily_count_vol_comp <- c(Counts = nrow(daily_comp_all), colSums(daily_comp_all[,c(5:9, 21:26)], na.rm = TRUE))
  daily_count_vol_comp["TOT_FLOW_LIGHT_VEHICLE"]/daily_count_vol_comp["AADT_CAR"]
  daily_count_vol_comp["TOT_FLOW_MEDIUM_TRUCK"]/daily_count_vol_comp["AADT_SUT"]
  daily_count_vol_comp["TOT_FLOW_HEAVY_TRUCK"]/daily_count_vol_comp["AADT_MUT"]
  
  # compare time period flows with the time period counts
  counts_time_period[, TOD := substr(TOD, 1,2)]
  
  tod_comp_all <- merge(counts_time_period,
                        model_flow_summary[TOD != "DY"
                                           ,.(LINKSERIAL, TOD = as.character(TOD),
                                              TOT_FLOW_MEDIUM_TRUCK, TOT_FLOW_HEAVY_TRUCK, TOT_FLOW_TRUCK)],
                        by = c("LINKSERIAL", "TOD"),
                        all.x = TRUE)
  
  tod_comp_all[, TRUCK := SUT + MUT]
  
  # Calculate error at link level
  daily_comp_all[, c("Error_LV", "Error_SUT", "Error_MUT", "Error_Truck", "Error_All") :=
                   .(TOT_FLOW_LIGHT_VEHICLE - AADT_CAR,
                     TOT_FLOW_MEDIUM_TRUCK - AADT_SUT,
                     TOT_FLOW_HEAVY_TRUCK - AADT_MUT,
                     TOT_FLOW_TRUCK - (AADT_SUT + AADT_MUT),
                     TOT_FLOW_VEHICLE - AADT)]
  
  tod_comp_all[, c("Error_SUT", "Error_MUT", "Error_Truck") :=
                 .(TOT_FLOW_MEDIUM_TRUCK - SUT,
                   TOT_FLOW_HEAVY_TRUCK - MUT,
                   TOT_FLOW_TRUCK - TRUCK)]
  
  # Compare by county, functional class, etc
  daily_comp_all_fclass <- add_totals(daily_comp_all[,.(NumCounts = .N,
                                                        AADT_CAR = sum(AADT_CAR), 
                                                        AADT_SUT = sum(AADT_SUT),
                                                        AADT_MUT = sum(AADT_MUT),
                                                        AADT_TRUCK = sum(AADT_SUT + AADT_MUT),
                                                        AADT = sum(AADT),
                                                        TOT_FLOW_PASS_CAR = round(sum(TOT_FLOW_PASS_CAR)), 
                                                        TOT_FLOW_LIGHT_TRUCK = round(sum(TOT_FLOW_LIGHT_TRUCK)), 
                                                        TOT_FLOW_LIGHT_VEHICLE = round(sum(TOT_FLOW_LIGHT_VEHICLE)),
                                                        TOT_FLOW_MEDIUM_TRUCK = round(sum(TOT_FLOW_MEDIUM_TRUCK)), 
                                                        TOT_FLOW_HEAVY_TRUCK = round(sum(TOT_FLOW_HEAVY_TRUCK)), 
                                                        TOT_FLOW_TRUCK = round(sum(TOT_FLOW_TRUCK)), 
                                                        TOT_FLOW_VEHICLE = round(sum(TOT_FLOW_VEHICLE)),
                                                        Error_LV = round(sum(Error_LV)),
                                                        Error_SUT = round(sum(Error_SUT)), 
                                                        Error_MUT = round(sum(Error_MUT)), 
                                                        Error_Truck = round(sum(Error_Truck)), 
                                                        Error_All = round(sum(Error_All)),
                                                        SumSqEr_LV = sum(Error_LV^2),
                                                        SumSqEr_SUT = sum(Error_SUT^2),
                                                        SumSqEr_MUT = sum(Error_MUT^2),
                                                        SumSqEr_Truck = sum(Error_Truck^2),
                                                        SumSqEr_All = sum(Error_All^2)),
                                                     keyby = .(FCLASS, FCLASS_LABEL)],
                                      idcols = 2L, rowtotal = FALSE)
  
  daily_comp_all_fclass[, c("Ratio_LV", "Ratio_SUT", "Ratio_MUT", "Ratio_Truck", "Ratio_All") :=
                          .(round(TOT_FLOW_LIGHT_VEHICLE/AADT_CAR,2), round(TOT_FLOW_MEDIUM_TRUCK/AADT_SUT,2), 
                            round(TOT_FLOW_HEAVY_TRUCK/AADT_MUT,2), round(TOT_FLOW_TRUCK/AADT_TRUCK,2), 
                            round(TOT_FLOW_VEHICLE/AADT,2))]
  
  daily_comp_all_fclass[, PctRMSE_LV := round(sqrt(SumSqEr_LV/NumCounts)/(AADT_CAR/NumCounts)*100,2)]
  daily_comp_all_fclass[, PctRMSE_SUT := round(sqrt(SumSqEr_SUT/NumCounts)/(AADT_SUT/NumCounts)*100,2)]
  daily_comp_all_fclass[, PctRMSE_MUT := round(sqrt(SumSqEr_MUT/NumCounts)/(AADT_MUT/NumCounts)*100,2)]
  daily_comp_all_fclass[, PctRMSE_Truck := round(sqrt(SumSqEr_Truck/NumCounts)/(AADT_TRUCK/NumCounts)*100,2)]
  daily_comp_all_fclass[, PctRMSE_All := round(sqrt(SumSqEr_All/NumCounts)/(AADT/NumCounts)*100,2)]
  
  daily_comp_all_county <- add_totals(daily_comp_all[,.(NumCounts = .N,
                                                        AADT_CAR = sum(AADT_CAR), 
                                                        AADT_SUT = sum(AADT_SUT),
                                                        AADT_MUT = sum(AADT_MUT),
                                                        AADT_TRUCK = sum(AADT_SUT + AADT_MUT),
                                                        AADT = sum(AADT),
                                                        TOT_FLOW_PASS_CAR = round(sum(TOT_FLOW_PASS_CAR)), 
                                                        TOT_FLOW_LIGHT_TRUCK = round(sum(TOT_FLOW_LIGHT_TRUCK)), 
                                                        TOT_FLOW_LIGHT_VEHICLE = round(sum(TOT_FLOW_LIGHT_VEHICLE)),
                                                        TOT_FLOW_MEDIUM_TRUCK = round(sum(TOT_FLOW_MEDIUM_TRUCK)), 
                                                        TOT_FLOW_HEAVY_TRUCK = round(sum(TOT_FLOW_HEAVY_TRUCK)), 
                                                        TOT_FLOW_TRUCK = round(sum(TOT_FLOW_TRUCK)), 
                                                        TOT_FLOW_VEHICLE = round(sum(TOT_FLOW_VEHICLE)),
                                                        Error_LV = round(sum(Error_LV)),
                                                        Error_SUT = round(sum(Error_SUT)), 
                                                        Error_MUT = round(sum(Error_MUT)), 
                                                        Error_Truck = round(sum(Error_Truck)), 
                                                        Error_All = round(sum(Error_All)),
                                                        SumSqEr_LV = sum(Error_LV^2),
                                                        SumSqEr_SUT = sum(Error_SUT^2),
                                                        SumSqEr_MUT = sum(Error_MUT^2),
                                                        SumSqEr_Truck = sum(Error_Truck^2),
                                                        SumSqEr_All = sum(Error_All^2)),
                                                     keyby = .(COUNTY, CountyName)],
                                      idcols = 2L, rowtotal = FALSE)
  
  daily_comp_all_county[, c("Ratio_LV", "Ratio_SUT", "Ratio_MUT", "Ratio_Truck", "Ratio_All") :=
                          .(round(TOT_FLOW_LIGHT_VEHICLE/AADT_CAR,2), round(TOT_FLOW_MEDIUM_TRUCK/AADT_SUT,2), 
                            round(TOT_FLOW_HEAVY_TRUCK/AADT_MUT,2), round(TOT_FLOW_TRUCK/AADT_TRUCK,2), 
                            round(TOT_FLOW_VEHICLE/AADT,2))]
  
  daily_comp_all_county[, PctRMSE_LV := round(sqrt(SumSqEr_LV/NumCounts)/(AADT_CAR/NumCounts)*100,2)]
  daily_comp_all_county[, PctRMSE_SUT := round(sqrt(SumSqEr_SUT/NumCounts)/(AADT_SUT/NumCounts)*100,2)]
  daily_comp_all_county[, PctRMSE_MUT := round(sqrt(SumSqEr_MUT/NumCounts)/(AADT_MUT/NumCounts)*100,2)]
  daily_comp_all_county[, PctRMSE_Truck := round(sqrt(SumSqEr_Truck/NumCounts)/(AADT_TRUCK/NumCounts)*100,2)]
  daily_comp_all_county[, PctRMSE_All := round(sqrt(SumSqEr_All/NumCounts)/(AADT/NumCounts)*100,2)]
  
  daily_comp_all_areatype <- add_totals(daily_comp_all[,.(NumCounts = .N,
                                                          AADT_CAR = sum(AADT_CAR), 
                                                          AADT_SUT = sum(AADT_SUT),
                                                          AADT_MUT = sum(AADT_MUT),
                                                          AADT_TRUCK = sum(AADT_SUT + AADT_MUT),
                                                          AADT = sum(AADT),
                                                          TOT_FLOW_PASS_CAR = round(sum(TOT_FLOW_PASS_CAR)), 
                                                          TOT_FLOW_LIGHT_TRUCK = round(sum(TOT_FLOW_LIGHT_TRUCK)), 
                                                          TOT_FLOW_LIGHT_VEHICLE = round(sum(TOT_FLOW_LIGHT_VEHICLE)),
                                                          TOT_FLOW_MEDIUM_TRUCK = round(sum(TOT_FLOW_MEDIUM_TRUCK)), 
                                                          TOT_FLOW_HEAVY_TRUCK = round(sum(TOT_FLOW_HEAVY_TRUCK)), 
                                                          TOT_FLOW_TRUCK = round(sum(TOT_FLOW_TRUCK)), 
                                                          TOT_FLOW_VEHICLE = round(sum(TOT_FLOW_VEHICLE)),
                                                          Error_LV = round(sum(Error_LV)),
                                                          Error_SUT = round(sum(Error_SUT)), 
                                                          Error_MUT = round(sum(Error_MUT)), 
                                                          Error_Truck = round(sum(Error_Truck)), 
                                                          Error_All = round(sum(Error_All)),
                                                          SumSqEr_LV = sum(Error_LV^2),
                                                          SumSqEr_SUT = sum(Error_SUT^2),
                                                          SumSqEr_MUT = sum(Error_MUT^2),
                                                          SumSqEr_Truck = sum(Error_Truck^2),
                                                          SumSqEr_All = sum(Error_All^2)),
                                                       keyby = .(AREATYPE, AREATYPE_LABEL)],
                                        idcols = 2L, rowtotal = FALSE)
  
  daily_comp_all_areatype[, c("Ratio_LV", "Ratio_SUT", "Ratio_MUT", "Ratio_Truck", "Ratio_All") :=
                            .(round(TOT_FLOW_LIGHT_VEHICLE/AADT_CAR,2), round(TOT_FLOW_MEDIUM_TRUCK/AADT_SUT,2), 
                              round(TOT_FLOW_HEAVY_TRUCK/AADT_MUT,2), round(TOT_FLOW_TRUCK/AADT_TRUCK,2), 
                              round(TOT_FLOW_VEHICLE/AADT,2))]
  
  daily_comp_all_areatype[, PctRMSE_LV := round(sqrt(SumSqEr_LV/NumCounts)/(AADT_CAR/NumCounts)*100,2)]
  daily_comp_all_areatype[, PctRMSE_SUT := round(sqrt(SumSqEr_SUT/NumCounts)/(AADT_SUT/NumCounts)*100,2)]
  daily_comp_all_areatype[, PctRMSE_MUT := round(sqrt(SumSqEr_MUT/NumCounts)/(AADT_MUT/NumCounts)*100,2)]
  daily_comp_all_areatype[, PctRMSE_Truck := round(sqrt(SumSqEr_Truck/NumCounts)/(AADT_TRUCK/NumCounts)*100,2)]
  daily_comp_all_areatype[, PctRMSE_All := round(sqrt(SumSqEr_All/NumCounts)/(AADT/NumCounts)*100,2)]
  
  daily_comp_all_volgrp <- add_totals(daily_comp_all[,.(NumCounts = .N,
                                                        AADT_CAR = sum(AADT_CAR), 
                                                        AADT_SUT = sum(AADT_SUT),
                                                        AADT_MUT = sum(AADT_MUT),
                                                        AADT_TRUCK = sum(AADT_SUT + AADT_MUT),
                                                        AADT = sum(AADT),
                                                        TOT_FLOW_PASS_CAR = round(sum(TOT_FLOW_PASS_CAR)), 
                                                        TOT_FLOW_LIGHT_TRUCK = round(sum(TOT_FLOW_LIGHT_TRUCK)), 
                                                        TOT_FLOW_LIGHT_VEHICLE = round(sum(TOT_FLOW_LIGHT_VEHICLE)),
                                                        TOT_FLOW_MEDIUM_TRUCK = round(sum(TOT_FLOW_MEDIUM_TRUCK)), 
                                                        TOT_FLOW_HEAVY_TRUCK = round(sum(TOT_FLOW_HEAVY_TRUCK)), 
                                                        TOT_FLOW_TRUCK = round(sum(TOT_FLOW_TRUCK)), 
                                                        TOT_FLOW_VEHICLE = round(sum(TOT_FLOW_VEHICLE)),
                                                        Error_LV = round(sum(Error_LV)),
                                                        Error_SUT = round(sum(Error_SUT)), 
                                                        Error_MUT = round(sum(Error_MUT)), 
                                                        Error_Truck = round(sum(Error_Truck)), 
                                                        Error_All = round(sum(Error_All)),
                                                        SumSqEr_LV = sum(Error_LV^2),
                                                        SumSqEr_SUT = sum(Error_SUT^2),
                                                        SumSqEr_MUT = sum(Error_MUT^2),
                                                        SumSqEr_Truck = sum(Error_Truck^2),
                                                        SumSqEr_All = sum(Error_All^2)),
                                                     keyby = .(VOLUME_GROUP_TRUCK)],
                                      idcols = 1L, rowtotal = FALSE)
  
  daily_comp_all_volgrp[, c("Ratio_LV", "Ratio_SUT", "Ratio_MUT", "Ratio_Truck", "Ratio_All") :=
                          .(round(TOT_FLOW_LIGHT_VEHICLE/AADT_CAR,2), round(TOT_FLOW_MEDIUM_TRUCK/AADT_SUT,2), 
                            round(TOT_FLOW_HEAVY_TRUCK/AADT_MUT,2), round(TOT_FLOW_TRUCK/AADT_TRUCK,2), 
                            round(TOT_FLOW_VEHICLE/AADT,2))]
  
  daily_comp_all_volgrp[, PctRMSE_LV := round(sqrt(SumSqEr_LV/NumCounts)/(AADT_CAR/NumCounts)*100,2)]
  daily_comp_all_volgrp[, PctRMSE_SUT := round(sqrt(SumSqEr_SUT/NumCounts)/(AADT_SUT/NumCounts)*100,2)]
  daily_comp_all_volgrp[, PctRMSE_MUT := round(sqrt(SumSqEr_MUT/NumCounts)/(AADT_MUT/NumCounts)*100,2)]
  daily_comp_all_volgrp[, PctRMSE_Truck := round(sqrt(SumSqEr_Truck/NumCounts)/(AADT_TRUCK/NumCounts)*100,2)]
  daily_comp_all_volgrp[, PctRMSE_All := round(sqrt(SumSqEr_All/NumCounts)/(AADT/NumCounts)*100,2)]
  
  # Time period
  tod_comp_all_tod <- add_totals(tod_comp_all[!is.na(TOT_FLOW_MEDIUM_TRUCK) & !is.na(TOT_FLOW_HEAVY_TRUCK),
                                              .(NumCounts = .N,
                                                SUT = sum(SUT),
                                                MUT = sum(MUT),
                                                TRUCK = sum(SUT + MUT),
                                                TOT_FLOW_MEDIUM_TRUCK = round(sum(TOT_FLOW_MEDIUM_TRUCK)), 
                                                TOT_FLOW_HEAVY_TRUCK = round(sum(TOT_FLOW_HEAVY_TRUCK)), 
                                                TOT_FLOW_TRUCK = round(sum(TOT_FLOW_TRUCK)), 
                                                Error_SUT = round(sum(Error_SUT)), 
                                                Error_MUT = round(sum(Error_MUT)), 
                                                Error_Truck = round(sum(Error_Truck)), 
                                                SumSqEr_SUT = sum(Error_SUT^2),
                                                SumSqEr_MUT = sum(Error_MUT^2),
                                                SumSqEr_Truck = sum(Error_Truck^2)),
                                              keyby = .(TOD)],
                                 rowtotal = FALSE)
  
  tod_comp_all_tod[, c("Ratio_SUT", "Ratio_MUT", "Ratio_Truck") :=
                     .(round(TOT_FLOW_MEDIUM_TRUCK/SUT,2), 
                       round(TOT_FLOW_HEAVY_TRUCK/MUT,2), round(TOT_FLOW_TRUCK/TRUCK,2))]
  
  tod_comp_all_tod[, PctRMSE_SUT := round(sqrt(SumSqEr_SUT/NumCounts)/(SUT/NumCounts)*100,2)]
  tod_comp_all_tod[, PctRMSE_MUT := round(sqrt(SumSqEr_MUT/NumCounts)/(MUT/NumCounts)*100,2)]
  tod_comp_all_tod[, PctRMSE_Truck := round(sqrt(SumSqEr_Truck/NumCounts)/(TRUCK/NumCounts)*100,2)]
  
  tod_comp_all_fclass_tod <- add_totals(tod_comp_all[!is.na(TOT_FLOW_MEDIUM_TRUCK) & !is.na(TOT_FLOW_HEAVY_TRUCK),
                                                     .(NumCounts = .N,
                                                       SUT = round(sum(SUT)),
                                                       MUT = round(sum(MUT)),
                                                       TRUCK = round(sum(SUT + MUT)),
                                                       TOT_FLOW_MEDIUM_TRUCK = round(sum(TOT_FLOW_MEDIUM_TRUCK)), 
                                                       TOT_FLOW_HEAVY_TRUCK = round(sum(TOT_FLOW_HEAVY_TRUCK)), 
                                                       TOT_FLOW_TRUCK = round(sum(TOT_FLOW_TRUCK)), 
                                                       Error_SUT = round(sum(Error_SUT)), 
                                                       Error_MUT = round(sum(Error_MUT)), 
                                                       Error_Truck = round(sum(Error_Truck)), 
                                                       SumSqEr_SUT = sum(Error_SUT^2),
                                                       SumSqEr_MUT = sum(Error_MUT^2),
                                                       SumSqEr_Truck = sum(Error_Truck^2)),
                                                     keyby = .(FCLASS, FCLASS_LABEL, TOD)],
                                        idcols = 3L,
                                        rowtotal = FALSE)
  
  tod_comp_all_fclass_tod[, c("Ratio_SUT", "Ratio_MUT", "Ratio_Truck") :=
                            .(round(TOT_FLOW_MEDIUM_TRUCK/SUT,2), 
                              round(TOT_FLOW_HEAVY_TRUCK/MUT,2), round(TOT_FLOW_TRUCK/TRUCK,2))]
  
  tod_comp_all_fclass_tod[, PctRMSE_SUT := round(sqrt(SumSqEr_SUT/NumCounts)/(SUT/NumCounts)*100,2)]
  tod_comp_all_fclass_tod[, PctRMSE_MUT := round(sqrt(SumSqEr_MUT/NumCounts)/(MUT/NumCounts)*100,2)]
  tod_comp_all_fclass_tod[, PctRMSE_Truck := round(sqrt(SumSqEr_Truck/NumCounts)/(TRUCK/NumCounts)*100,2)]
  
  # Collect together summaries and write out
  assignment_validation_list <- list(daily_comp_all = daily_comp_all,
                                    tod_comp_all = tod_comp_all,
                                    daily_comp_all_fclass = daily_comp_all_fclass,
                                    daily_comp_all_county = daily_comp_all_county,
                                    daily_comp_all_areatype = daily_comp_all_areatype,
                                    daily_comp_all_volgrp = daily_comp_all_volgrp,
                                    tod_comp_all_tod = tod_comp_all_tod,
                                    tod_comp_all_fclass_tod = tod_comp_all_fclass_tod)
  
  assignment_validation_list$tabletitles <- names(assignment_validation_list)
  assignment_validation_list$scenario <- scenario
  
  return(assignment_validation_list)

}
