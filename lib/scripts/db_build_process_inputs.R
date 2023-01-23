
# This function loads all necessary inputs into envir, after any needed
# transformations
db_build_process_inputs <- function(envir){
  
  ### Load project input files
  project.files <- list(db_build_graphics_functions    = file.path(SYSTEM_SCRIPTS_PATH, "db_build_graphics_functions.R"),
                        db_build_render                = file.path(SYSTEM_SCRIPTS_PATH, "db_build_render.R"),
                        db_build_spreadsheet           = file.path(SYSTEM_SCRIPTS_PATH, "db_build_spreadsheet.R"),
                        db_build_spreadsheet_functions = file.path(SYSTEM_SCRIPTS_PATH, "db_build_spreadsheet_functions.R"),
                        c_n2_empcats                   = file.path(SYSTEM_DATA_PATH, "corresp_naics2_empcats.csv"),
                        equity_zones                   = file.path(SYSTEM_DATA_PATH, "data_equity_zones.csv"),
                        TAZ_System                     = file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"))
  
  loadInputs(files = project.files, envir = envir)
  
  ### Load inputs/outputs from earlier steps
  # Preparations for all loadings
  if(any(SCENARIO_DB_FIRMSYN, SCENARIO_DB_CVTM, SCENARIO_DB_TT)) {
    TAZ_System <- envir[["TAZ_System"]]
    if(!(BASE_DASHBOARD_GEOGRAPHY %in% colnames(TAZ_System))){
      stop("BASE_DASHBOARD_GEOGRAPHY must be a column name from the TAZ_System dataset!")
    }
    
    # Convert the DistrictName into a factor
    district_labels <- unique(TAZ_System[, .(DistrictNum, DistrictName)])[order(DistrictNum)]
    district_labels_vec <- district_labels$DistrictName
    TAZ_System[, DistrictName := factor(DistrictName, levels = district_labels_vec)]
    
    # Adjust the order of the counties
    county_order <- unique(TAZ_System[,.(county_state, CountyFIPS, cmap)])[order(-cmap, CountyFIPS)]
    county_order_vec <- unique(county_order$county_state)
    TAZ_System[, CountyName:=factor(county_state, levels = county_order_vec)]
    envir[["TAZ_System"]] <- TAZ_System
    
    #Transform the employment category mapping
    envir[["c_n2_empcats"]] <- unique(envir[["c_n2_empcats"]][,.(EmpCatName, EmpCatDesc,
                                                                           EmpCatGroupedName)])
    setorder(envir[["c_n2_empcats"]],EmpCatName)
    
    # Load the skims for use in the processing steps below
    skims <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"))
    # Put skims in extra-long format
    tod.ranges <- attr(skims, "tod.ranges")
    skims.long <- meltSkimTableByTOD(skims, 
                                     tod_names = names(tod.ranges))
    
  }
  
  if(SCENARIO_DB_FIRMSYN){
    load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FIRMSYN_OUTPUTNAME))
    ScenarioFirms <- firm_sim_results$ScenarioFirms
    
    # Add dashboard geography
    ScenarioFirms <- merge(ScenarioFirms, TAZ_System[,.(TAZ, Region = get(BASE_DASHBOARD_GEOGRAPHY))],
                           by.x = "TAZ", by.y = "TAZ")
    
    ScenarioFirms[, TAZ_TYPE := ifelse(TAZ %in% BASE_TAZ_CMAP, "CMAP MPO Area", "Non-CMAP Part of Model Region")]
    
    # Convert size to labels
    ScenarioFirms[, esizecat := factor(firm_inputs$EstSizeCategories$Label[esizecat], 
                                       levels = firm_inputs$EstSizeCategories$Label)]
    
    setnames(ScenarioFirms, "Emp", "Employees")
    envir[["ScenarioFirms"]] <- ScenarioFirms
    
    # SE data for validation of firm synthesis
    envir[["TAZLandUseCVTM"]] <- firm_inputs$TAZLandUseCVTM
    
  }
  
  if(SCENARIO_DB_CVTM){
    load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_CVTM_OUTPUTNAME))
    
    # Add dashboard geography
    cv_trips <- cv_sim_results$cv_trips
    cv_trips <- merge(cv_trips, TAZ_System[,.(TAZ, Region.Origin = get(BASE_DASHBOARD_GEOGRAPHY))],
                      by.x = "OTAZ", by.y = "TAZ")
    cv_trips <- merge(cv_trips, TAZ_System[,.(TAZ, Region.Destination = get(BASE_DASHBOARD_GEOGRAPHY))],
                      by.x = "DTAZ", by.y = "TAZ")
    cv_trips <- cv_trips[cv_trips[TripID == 1, .(TourID, TAZ.Start = OTAZ, Region.Start = Region.Origin)], 
                     c("TAZ.Start", "Region.Start") := .(i.TAZ.Start, i.Region.Start), on = "TourID"]
    
    # add tour start to stop destination distance
    cv_trips[skims.long[,.(TAZ.Start = OTAZ, DTAZ, TOD, dist)], 
              Distance.Start := i.dist, 
              on = c("TAZ.Start", "DTAZ", "TOD")]
    
    # add od segment
    cv_trips[, Movement.Type := add_od_segment(origins = OTAZ, 
                                               destinations = DTAZ, 
                                               external_taz = NULL)]
    
    envir[["cv_trips"]] <- cv_trips
    
    # calculate the average cluster distance 
    # for multistop tours: distance between all stop combinations excluding the tour start/end
    # In this case include intermediate stops added later to match with the validation data
    # in which scheduled and intermediate stops cannot be distinguished
    cv_trips_cluster <- cv_trips[Activity != "Return",.(TourID, TripID, Region.Start, OTAZ, DTAZ)]
    cv_trips_cluster <- cv_trips_cluster[, NumStops := .N, by = TourID][NumStops > 1]
    # Stops are all DTAZs on the tour
    clusterMeanDist <- function(stop.TAZs, dist.mat){
      idx <- as.character(stop.TAZs)
      dist.mat.subset <- dist.mat[idx, idx, drop = FALSE]
      diag(dist.mat.subset) <- NA
      return(mean(dist.mat.subset, na.rm = TRUE))
    }
    
    # Convert skims to a matrix
    dist.mat <- as.matrix(dcast.data.table(data = skims[,.(OTAZ, DTAZ, distance = dist.avg)], 
                                           formula = OTAZ~DTAZ, 
                                           value.var = "distance")[, -1])
    rownames(dist.mat) <- colnames(dist.mat)
    
    cv_trips_cluster[, MeanDist := clusterMeanDist(DTAZ, dist.mat), by = TourID]
    cv_trips_cluster <- cv_trips_cluster[TripID == 1, .(Region.Start, TourID, NumStops, MeanDist)]
    envir[["cv_trips_cluster"]] <- cv_trips_cluster
    
    # add a debug summary to look for outliers in the stops by activity by stop to check for high values
    # in absolute terms and per HH/per Emp
    cv_trips_act <- cv_trips[, .(Trips = .N), keyby = .(TAZ = DTAZ, Activity, Scheduled)]
    cv_trips_taz <- dcast.data.table(cv_trips_act,
                                     TAZ ~ Activity,
                                     fun.aggregate = sum,
                                     value.var = "Trips")
    setnames(cv_trips_taz, "Break/Meal","BreakMeal")
    setnames(cv_trips_taz, "Vehicle Service","VehicleService")
    cv_trips_taz[, Scheduled := Goods + Service]
    cv_trips_taz[, Intermediate := BreakMeal + VehicleService + Other]
    cv_trips_taz[, Stops := Scheduled + Intermediate + Return]
    cv_trips_taz <- merge(cv_trips_taz,
                          firm_inputs$TAZLandUseCVTM,
                          by = "TAZ",
                          all.x = TRUE)
    
    cv_trips_taz[, PctIntermediate := Intermediate/Stops]
    cv_trips_taz[, StopPerHH := ifelse(HH > 0, Stops/HH, 0)]
    cv_trips_taz[, StopPerEmp := ifelse(NEmp_Total > 0, Stops/NEmp_Total, 0)]
    
    envir[["cv_trips_taz"]] <- cv_trips_taz
  }
  
  if(SCENARIO_DB_TT){
    
    load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_TT_OUTPUTNAME))
    
    # Create a trip gen summary table
    # extent is all model region TAZs (CMAP MPO and rest of model region)
    tab_template <- data.table(ID = rep(BASE_TAZ_INTERNAL,6),
                               Vehicle = rep(c("Light", "Medium", "Heavy"), 
                                             each = length(BASE_TAZ_INTERNAL) * 2),
                               Direction = rep(c(rep("O",length(BASE_TAZ_INTERNAL)), 
                                                 rep("D", length(BASE_TAZ_INTERNAL))),3))
    
    tab_template[, Vehicle := factor(Vehicle, levels = c("Light", "Medium", "Heavy"))]
    
    truck_trip_gen <- rbind(tt_list$TripTable[OTAZ %in% BASE_TAZ_INTERNAL,
                                              .(trips = sum(trips), Direction = "O"), 
                                              keyby = .(ID = OTAZ, Vehicle)],
                            tt_list$TripTable[DTAZ %in% BASE_TAZ_INTERNAL,
                                              .(trips = sum(trips), Direction = "D"), 
                                              keyby = .(ID = DTAZ, Vehicle)])
    
    truck_trip_gen <- merge(tab_template, 
                            truck_trip_gen,
                            by = c("ID", "Vehicle", "Direction"),
                            all = TRUE)
    
    truck_trip_gen[is.na(trips), trips := 0]
    truck_trip_gen[, Direction := factor(Direction, levels = c("O", "D"))]
    
    envir[["truck_trip_gen"]] <- truck_trip_gen
    
    # write a wide version of this table
    truck_trip_gen[, PA := factor(Direction, labels = c("p", "a"))]
    
    truck_trip_gen_wide <- dcast.data.table(truck_trip_gen,
                     ID ~ Vehicle + PA,
                     fun.aggregate = sum,
                     value.var = "trips",
                     fill = 0)
    
    setnames(truck_trip_gen_wide,
             names(truck_trip_gen_wide),
             gsub("_", "Truck", names(truck_trip_gen_wide)))
    
    fwrite(truck_trip_gen_wide,
           file.path(SCENARIO_OUTPUT_PATH, "CV_Trip_Generation_Summary.csv"))
    
    # Create a trip and VMT by OD segment, geography, summary
    
    # Extract TripTable and add time, distance
    TripTable <- tt_list$TripTable[,.(trips = sum(trips)), 
                                   by = .(OTAZ, DTAZ, Vehicle, TOD)]
    setkey(TripTable, OTAZ, DTAZ, Vehicle, TOD)
    
    TripTable[skims.long, 
              c("dist", "time") := .(i.dist, i.time), 
              on = c("OTAZ", "DTAZ", "TOD")]
    
    # Calculate VMT and VHT
    TripTable[, c("VMT", "VHT") := .(trips * dist, trips * time / 60)]
    
    # Add grouping variabes (ODSegment, Summary Geography)
    TripTable[, ODSegment := add_od_segment(OTAZ, DTAZ, external_taz = NULL)]
    
    TripTable <- add_od_fields(TripTable, TAZ_System,  
                               fieldsToAdd = c("county_state", "DistrictName"))
    
    TripTable[, OSummaryGeog := ODistrictName]
    TripTable[, DSummaryGeog := DDistrictName]
    
    envir[["TripTable"]] <- TripTable
              
    # Trips, VMT, VHT by Vehicle, TOD, ODSegment
    tmh_vtods <- TripTable[,.(Trips = sum(trips, na.rm = TRUE), 
                              MeanDist = sum(VMT, na.rm = TRUE)/sum(trips, na.rm = TRUE), 
                              MeanTime = (sum(VHT, na.rm = TRUE) * 60)/sum(trips, na.rm = TRUE), # in minutes
                              MeanSpeed = sum(VMT, na.rm = TRUE)/sum(VHT, na.rm = TRUE), # in miles/hour
                              VMT = sum(VMT, na.rm = TRUE), 
                              VHT = sum(VHT, na.rm = TRUE)),
                           keyby = .(Vehicle, TOD, ODSegment)]
    
    tab_template <- data.table(expand.grid(Vehicle = c("Light", "Medium", "Heavy"),
                                           TOD = names(BASE_TOD_RANGES),
                                           ODSegment = c("II", "IX", "XI", "XX")))
    
    tab_template[, Vehicle := factor(Vehicle, levels = c("Light", "Medium", "Heavy"))]
    tab_template[, TOD := factor(TOD, levels = names(BASE_TOD_RANGES))]
    
    # add some labels for the time periods
    tod_labels <- data.table(TOD = names(BASE_TOD_RANGES),
                             Hours = c("8pm-6am","6am-7am", "7am-9am",
                                            "9am-10am", "10am-2pm", "2pm-4pm", 
                                            "4pm-6pm", "6pm-8pm"))
    
    tab_template[tod_labels, Hours := i.Hours, on = "TOD"]
    
    tmh_vtods <- merge(tab_template, 
                       tmh_vtods,
                       by = c("Vehicle", "TOD", "ODSegment"),
                       all = TRUE)
    
    tmh_vtods[is.na(Trips), c("Trips", "MeanDist", "MeanTime", "MeanSpeed", "VMT", "VHT") := 0]
    
    envir[["tmh_vtods"]] <- tmh_vtods
    envir[["tod_labels"]] <- tod_labels
    
    # Write a wide version of this table
    # Filtered to just II
    tmh_vtods_wide <- dcast.data.table(tmh_vtods,
                                      Vehicle + TOD + ODSegment ~ .,
                                      fun.aggregate = sum,
                                      value.var = c("Trips", "VMT", "VHT", "MeanDist", "MeanTime", "MeanSpeed"),
                                      fill = 0)[ODSegment == "II"]
    
    envir[["tmh_vtods_wide"]] <- tmh_vtods_wide
    
    fwrite(tmh_vtods_wide,
           file.path(SCENARIO_OUTPUT_PATH, "CV_Trip_VMT_VHT_Summary.csv"))
    
  }
  
  if(SCENARIO_DB_SPREADSHEET){
    
    # Add code here for processing inputs required specifically to build the summary spreadsheet
    # Currently no additional code required beyond that in other sections of this script
   
  }
  
  if(SCENARIO_DB_CALIBRATION){
    
    # Import the calibration results
    cal_filepaths <- list.files(SYSTEM_CALIBRATION_PATH, full.names = TRUE)
    cal_results <- lapply(cal_filepaths,readRDS)
    names(cal_results) <- tools::file_path_sans_ext(list.files(SYSTEM_CALIBRATION_PATH))
    envir[["cal_results"]] <- cal_results
    
  }
  
  if(SCENARIO_DB_REFERENCE){
    
    # Import the base year dashboard tabulations (or other run scenario)
    # to create comparisons between the current scenario and the reference year scenarios
    REFERENCE_OUTPUT <- file.path(SYSTEM_APP_PATH, "scenarios", SCENARIO_REFERENCE_NAME, "outputs", SYSTEM_DB_OUTPUTNAME)
    
    if(file.exists(REFERENCE_OUTPUT)){
      
      db_reference <- new.env()  
      load(REFERENCE_OUTPUT, envir = db_reference)
      
      #extract some of the reference tables and remove the other inputs, functions, etc, that are in the env.
      ref_obj <- ls(db_reference$db_inputs, all.names=TRUE)
      ref_obj <- ref_obj[grep("db_tab_", ref_obj)]
      ref_obj <- c("ScenarioFirms", "TAZLandUseCVTM", "cv_trips", "cv_trips_cluster",  "TripTable", "tmh_vtods", "truck_trip_gen", "tmh_vtods_wide", ref_obj)
      for(n in ref_obj) assign(paste0("ref_", n), get(n, db_reference$db_inputs), envir)
      
    } else {
      
      SCENARIO_DB_REFERENCE <- FALSE
      
    }
    
  }
  
  ### Create files for mapping
  # Open geographic file of TAZs
  shp <- readOGR(file.path(SYSTEM_DATA_PATH, "TAZ_System_Shape.shp"), layer = "TAZ_System_Shape", verbose = FALSE)
  
  # Add the grouping variable and order it
  shp$DistrictName <- TAZ_System$DistrictName[match(shp$zone17, TAZ_System$TAZ)]
  shp$DistrictName <- factor(shp$DistrictName, levels = district_labels_vec)
  envir[["shp"]] <- shp
  
  # Generate the bounding box for CMAP region
  envir[["CMAP_BBOX"]] <- shp %>% bbox()
  
  ### Generate basemaps
  prepTAZPolygons <- envir[["prepTAZPolygons"]]
  prepTAZList <- prepTAZPolygons(shp = shp, group.by = BASE_DASHBOARD_GEOGRAPHY)
  envir[["TAZ.polys"]] <- prepTAZList$shp
  if(exists("district_labels_vec")){
    envir[["TAZ.polys"]]$DistrictName <- factor(envir[["TAZ.polys"]]$DistrictName, levels=district_labels_vec)
    envir[["TAZ.polys"]]$Group <- factor(envir[["TAZ.polys"]]$Group, levels=district_labels_vec)
  }
  envir[["colorFun"]] <- prepTAZList$colorFun

  ### Initialize variables to store common values
  # Create time of day vectors with labels for both 30 and 60-minute divisions
  tod_breaks30 <- seq(from = 0, to = 1440, by = 30)
  tod_labels30 <- c("12:00am - 12:29am", "12:30am - 12:59am", "1:00am - 1:29am", 
                    "1:30am - 1:59am", "2:00am - 2:29am", "2:30am - 2:59am",
                    "3:00am - 3:29am", "3:30am - 3:59am", "4:00am - 4:29am",
                    "4:30am - 4:59am", "5:00am - 5:29am", "5:30am - 5:59am",
                    "6:00am - 6:29am", "6:30am - 6:59am", "7:00am - 7:29am",
                    "7:30am - 7:59am", "8:00am - 8:29am", "8:30am - 8:59am",
                    "9:00am - 9:29am", "9:30am - 9:59am", "10:00am - 10:29am",
                    "10:30am - 10:59am", "11:00am - 11:29am", "11:30am - 11:59am",
                    "12:00pm - 12:29pm", "12:30pm - 12:59pm", "1:00pm - 1:29pm",
                    "1:30pm - 1:59pm", "2:00pm - 2:29pm", "2:30pm - 2:59pm",
                    "3:00pm - 3:29pm", "3:30pm - 3:59pm", "4:00pm - 4:29pm",
                    "4:30pm - 4:59pm", "5:00pm - 5:29pm", "5:30pm - 5:59pm",
                    "6:00pm - 6:29pm", "6:30pm - 6:59pm", "7:00pm - 7:29pm",
                    "7:30pm - 7:59pm", "8:00pm - 8:29pm", "8:30pm - 8:59pm",
                    "9:00pm - 9:29pm", "9:30pm - 9:59pm", "10:00pm - 10:29pm",
                    "10:30pm - 10:59pm", "11:00pm - 11:29pm", "11:30pm - 11:59pm")
  
  tod_breaks60 <- seq(from = 0, to = 1440, by = 60)
  tod_labels60 <- c("12:00am - 12:59am", "1:00am - 1:59am", "2:00am - 2:59am",
                    "3:00am - 3:59am", "4:00am - 4:59am", "5:00am - 5:59am",
                    "6:00am - 6:59am", "7:00am - 7:59am", "8:00am - 8:59am",
                    "9:00am - 9:59am", "10:00am - 10:59am", "11:00am - 11:59am",
                    "12:00pm - 12:59pm", "1:00pm - 1:59pm", "2:00pm - 2:59pm",
                    "3:00pm - 3:59pm", "4:00pm - 4:59pm", "5:00pm - 5:59pm",
                    "6:00pm - 6:59pm", "7:00pm - 7:59pm", "8:00pm - 8:59pm",
                    "9:00pm - 9:59pm", "10:00pm - 10:59pm", "11:00pm - 11:59pm")
  
  envir[["tod_breaks30"]] <- tod_breaks30
  envir[["tod_labels30"]] <- tod_labels30
  envir[["tod_breaks60"]] <- tod_breaks60
  envir[["tod_labels60"]] <- tod_labels60
  
  envir[["theme_db"]] <- theme_db <- theme_bw() + theme(plot.margin = unit(c(10,10,20,10),"pt"))
  
  envir[["rsgcolordf"]] <- data.frame(red=c(246,0,99,186,117,255,82),
                                    green=c(139,111,175,18,190,194,77),
                                    blue=c(31,161,94,34,233,14,133),
                                    colornames=c("orange","marine","leaf","cherry","sky","sunshine","violet"))
  
  # Density value for dot density maps
  envir[["k"]] <- 25
  
  
}