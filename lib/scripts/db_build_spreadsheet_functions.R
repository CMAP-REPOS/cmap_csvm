# This script contains the funtions which perform the processing 
# of data for the summary spreadsheet

# Function to create TAZ levels summaries for Model Region from component outputs
createTAZSummaries <- function(scenario, scenario_output_path, ref_scenario = FALSE){ 
  
  # Use the reference version of objects if this is a reference scenario
  if(ref_scenario){
    ScenarioFirms <- db_inputs$ref_ScenarioFirms
    TAZLandUseCVTM <- db_inputs$ref_TAZLandUseCVTM
    cv_trips <- db_inputs$ref_cv_trips
    TripTable <- db_inputs$ref_TripTable
  }
  
  # Create TAZ level summaries
  
  # Firm synthesis: firms and employment, compared with SE data
  firm_sim_taz <- ScenarioFirms[,.(Firms = .N, Emp = sum(Employees)), 
                                                 keyby = .(EmpCatGroupedName, TAZ)]
  
  firm_sim_se <- TAZLandUseCVTM
  firm_sim_se <- melt.data.table(firm_sim_se,
                                 id.vars = c("TAZ", "Mesozone", "CountyFIPS", "HH", "NEmp_Total"),
                                 variable.name = "EmpCatGroupedName",
                                 value.name = "SE")
  firm_sim_se[, EmpCatGroupedName := sub("NEmp_","",EmpCatGroupedName) ]
  firm_sim_taz <- merge(firm_sim_taz,
                        firm_sim_se[,.(TAZ, EmpCatGroupedName, SE)],
                        by = c("TAZ", "EmpCatGroupedName"),
                        all = TRUE)
  firm_sim_taz[is.na(firm_sim_taz)] <- 0
  
  firm_sim_taz[, Diff := Emp - SE]
  
  firm_sim_taz_w <- dcast.data.table(firm_sim_taz,
                                     TAZ ~ EmpCatGroupedName,
                                     fun.aggregate = sum,
                                     value.var = c("Firms", "Emp", "SE", "Diff"))
  
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
  tt_taz_o_trips <- TripTable[,.(NumTrips = sum(trips)),
                               keyby = .(TAZ = OTAZ, Vehicle)]
  
  tt_taz_d_trips <- TripTable[,.(NumTrips = sum(trips)),
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
                     tt_taz_o_trips = tt_taz_o_trips,
                     tt_taz_d_trips = tt_taz_d_trips,
                     tt_taz_trips = tt_taz_trips,
                     tt_taz_trips_w = tt_taz_trips_w)
  
  returnlist$tabletitles <- names(returnlist)
  returnlist$scenario <- scenario
  
  return(returnlist)
  
}

# Function to create truck trip summaries from trip tables
createTripTableSummaries <- function(scenario, scenario_output_path, ref_scenario = FALSE){
  
  # Load the db workspace if this is an alternative scenario
  if(ref_scenario){
    TripTable <- db_inputs$ref_TripTable
    tmh_vtods_wide <- db_inputs$ref_tmh_vtods_wide
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
  
  xt.reg.trips <- add_totals(dcast.data.table(daily.sum.veh.odreg[!is.na(Vehicle)], 
                                                 OSummaryGeog~DSummaryGeog, 
                                                 fun.aggregate = sum, 
                                                 value.var = "Trips"))
  
  xt.reg.vmt <- add_totals(dcast.data.table(daily.sum.veh.odreg[!is.na(Vehicle)], 
                                               OSummaryGeog~DSummaryGeog, 
                                               fun.aggregate = sum, 
                                               value.var = "VMT"))
  
  # Create a vector of tabletitles (pretty names for the table to be used in the spreadsheet)
  tabletitles = c("Daily Trip Summary by Vehicle Type",
                  "Daily Trip Summary by OD Segment",
                  "Daily Trip Summary by Vehicle Type and OD Segment",
                  "Daily Trip Summary by Vehicle Type, OD Segment, and OD Regions",
                  "Tabulation of Total Daily Trips by OD Regions",
                  "Tabulation of Total Daily VMT by OD Regions")
  
  # Return a list of all of the tables created, with those for the spreadsheet first
  return(list(daily.sum.veh = daily.sum.veh,
              daily.sum.od = daily.sum.od,
              daily.sum.veh.od = daily.sum.veh.od,
              daily.sum.veh.odreg = daily.sum.veh.odreg,
              xt.reg.trips = xt.reg.trips,
              xt.reg.vmt = xt.reg.vmt,
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
           c("TAZ", "EmpCatGroupedName", paste("Ref", 
                                          names(firm_sim_taz_1)[3:ncol(firm_sim_taz_1)],
                                          sep = "_")))
  
  setnames(firm_sim_taz_2, 
           c("TAZ", "EmpCatGroupedName", paste("Alt", 
                                          names(firm_sim_taz_2)[3:ncol(firm_sim_taz_2)],
                                          sep = "_")))
  
  firm_sim_taz <- merge(firm_sim_taz_1,
                        firm_sim_taz_2,
                        by = c("TAZ", "EmpCatGroupedName"),
                        all = TRUE)
  
  firm_sim_taz[is.na(firm_sim_taz)] <- 0
  
  firm_sim_taz[, c("Diff_Firms","Diff_Emp", "Diff_SE") := .(Alt_Firms - Ref_Firms,
                                                            Alt_Emp - Ref_Emp,
                                                            Alt_SE - Ref_SE)]
  
  firm_sim_taz[, Diff_Diff := Diff_Emp - Diff_SE]
  
  cols <- names(firm_sim_taz)[3:ncol(firm_sim_taz)]
  firm_sim_taz_sum <- firm_sim_taz[,lapply(.SD, sum), by = TAZ, .SDcols = cols]
  
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
                  "Commercial Vehicle Trips by TAZ and Vehicle",
                  "Commercial Vehicle Trips by TAZ and Vehicle (Wide)",
                  "Total Trips by TAZ and Vehicle",
                  "Total Trips by TAZ and Vehicle (Wide)")
  
  # Return tables, titles, and scenario names/years
  return(list(taz_sum = taz_sum,
              firm_sim_taz = firm_sim_taz,
              firm_sim_taz_sum = firm_sim_taz_sum,
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

