# SEMCOG CVTM
# dev script: data_cvs.R
#
# Purpose:
# Process SEMCOG CVS data
# 1. Processes and analyses the CVS data in more detail than the brief Data_Review_CVS.R sript
# 2. Produces the tables, charts, and maps included in the data processing memo
# 3. Produces estimation, calibration, and validation datasets that are based on the CVS data
#
# Outputs:
#
# In dev/Data_Processed/Survey/Outputs:
# semcog_cvs_est.RDS
# semcog_cvs_veh.RDS
# semcog_cvs_trip.RDS
# semcog_cvs_tour.RDS
#
# In dev\Data_Processed\_Documentation
# charts and maps of the CVS data

# use init_dev.R to run here instead of sourcing from _Master_Dev.R
# source("./dev/init_dev.R")

### READ INPUT FILES AND DEFINE VARIABLES ----------------------------------

# data descriptor
descr <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "Survey", "Item_1_2017CVS/SEMCOG_CV_20181128_Submitted_ForDistribution.xlsx"),
                              sheet = "DATA DICTIONARY"))

# Establishment data
est <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "Survey", "Item_1_2017CVS/SEMCOG_CV_20181128_Submitted_ForDistribution.xlsx"),
                            sheet = "ESTABLISHMENT"))

# Truck vehicle and trip data
veh <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "Survey", "Item_1_2017CVS/SEMCOG_CV_20181128_Submitted_ForDistribution.xlsx"),
                            sheet = "VEHICLE"))

trip <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "Survey", "Item_1_2017CVS/SEMCOG_CV_20181128_Submitted_ForDistribution.xlsx"),
                             sheet = "TRIP"))

tour <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "Survey", "Item_1_2017CVS/SEMCOG_CV_20181128_Submitted_ForDistribution.xlsx"),
                             sheet = "TOUR"))

gps <- data.table(read.xlsx(file.path(SYSTEM_DEV_DATA_PATH, "Survey", "Item_1_2017CVS/SEMCOG_CV_20181128_Submitted_ForDistribution.xlsx"),
                            sheet = "GPS_TRIP"))

# Other supporting files required for review and processing
# Crosswalk between NAICS and SEMCOG employment categories
empcats <- fread("./lib/data/NAICS3_to_EmpCats.csv")
emp_cat_labels <- unique(empcats[,.(EmpCatName, EmpCatDesc)])
emp_cat_labels[, EmpLabel := paste0(EmpCatName, " (", EmpCatDesc, ")")]

# TAZ SE data by year
taz_se_yr_dt <- fread(file = file.path(SYSTEM_DEV_DATA_PATH, "Landuse", "Outputs", "TAZSocioEconomicsAllYears.csv"))

# Establishments seed data
Establishments <- fread(file = file.path(SYSTEM_DEV_DATA_PATH, "Landuse", "Outputs", "Establishments.csv"))

### Establishments ------------------------------------------------------------------

# Label survey industry category
IndustryCats <- unlist(strsplit(descr[TABLE == "Establishment" & FIELD.NAME == "INDUSTRY"]$CODE.VALUES, "\n"))
est[, IndName := IndustryCats[INDUSTRY]]

# SEMCOG empcat
# est has establishment NAICS2 and NAICS3
est[, NAICS2 := as.integer(NAICS2)]
est[, NAICS3 := as.integer(NAICS3)]

est[empcats[,.(NAICS2 = NAICSn2n3, EmpCatName)],
    EmpCatNameN2 := i.EmpCatName,
    on = "NAICS2"]

est[empcats[,.(NAICS3 = NAICSn2n3, EmpCatName)],
    EmpCatNameN3 := i.EmpCatName,
    on = "NAICS3"]

est[, EmpCatName := ifelse(!is.na(EmpCatNameN2), EmpCatNameN2, EmpCatNameN3)]
est[is.na(EmpCatName),.N] #should be zero

# samples by industry
est.ind <- est[,.(Establishments = .N,
                  EstWeighted = sum(ESTABLISHMENT_WGHT_FCTR)), 
                  keyby = IndName]
est.ind.m <- melt.data.table(est.ind,
                             id.vars = "IndName",
                             variable.name = "Data",
                             value.name = "NumEst")
est.ind.m[, PctEst := NumEst/sum(NumEst), by = "Data"]

p_est_industry <- ggplot(data = est.ind, aes(x = factor(IndName, levels = rev(IndustryCats)), y = Establishments)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Establishments", subtitle = "By Industry, Unweighted", caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
  xlab("Industry Category") + scale_y_continuous(name = "Number of Establishments", labels = scales::comma) + coord_flip() 

p_est_industry_wght <- ggplot(data = est.ind, aes(x = factor(IndName, levels = rev(IndustryCats)), y = EstWeighted)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Establishments", subtitle = "By Industry, Weighted", caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
  xlab("Industry Category") + scale_y_continuous(name = "Number of Establishments", labels = scales::comma) + coord_flip() 

p_est_industry_comp <- ggplot(data = est.ind.m, aes(x = factor(IndName, levels = rev(IndustryCats)), y = PctEst, fill = Data)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("Measure", values = rgb(rsgcolordf[2:3,],maxColorValue = 255), labels = c("Unweighted", "Weighted")) +
  scale_y_continuous(name = "Percentage of Establishments", labels = scales::percent) + 
  xlab("Industry Category") + 
  labs(title = "Establishments in the CVS Sample", 
       subtitle = "By Industry, Weighted and Unweighted", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey") +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  coord_flip() 

ggsave(p_est_industry_comp, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, 
                            "_Documentation", 
                            "data_proc_cvs_p_est_industry_comp.png"), 
       width = 6.5, 
       height = 4) 

# Compare weighted establishment distribution with the SEMCOG region employment data
tazempbase <- melt.data.table(taz_se_yr_dt[Year == 2015 & TAZ %in% BASE_TAZ_MODEL_REGION],
                              id.vars = c("TAZ","Year"),
                              variable.name = "EmpCatName",
                              value.name = "Employment")[!EmpCatName %in% c("POP", "HH")]
empbase <- tazempbase[,.(Number = sum(Employment), Data = "EmpWeighted", Source = "Model SE Data"), keyby = EmpCatName]
empbase[, Percent := Number/sum(Number), by = "Data"]

Establishments[empcats, EmpCatName := i.EmpCatName, on = "EmpCatID"]
estbase <- Establishments[TAZ %in% BASE_TAZ_MODEL_REGION, 
                          .(Number = .N, Data = "EstWeighted", Source = "Model SE Data"), 
                          keyby = EmpCatName] 
estbase[, Percent := Number/sum(Number), by = "Data"]

est.empcat <- est[,.(Establishments = .N,
                     Employment = sum(TOTAL_EMPLOYEES),
                     EstWeighted = sum(ESTABLISHMENT_WGHT_FCTR),
                     EmpWeighted = sum(ESTABLISHMENT_WGHT_FCTR * TOTAL_EMPLOYEES)), 
                  keyby = EmpCatName]
est.empcat.m <- melt.data.table(est.empcat,
                             id.vars = "EmpCatName",
                             variable.name = "Data",
                             value.name = "Number")
est.empcat.m[, Percent := Number/sum(Number), by = "Data"]
est.empcat.m[, Source := "CV Survey"]
est.empcat.m <- rbind(est.empcat.m[Data %in% c("EmpWeighted", "EstWeighted")],
                      empbase,
                      estbase)
est.empcat.m[, EmpCatName := factor(as.character(EmpCatName))]
est.empcat.m[, Measure := ifelse(Data == "EmpWeighted", "Employment", "Establishements")]
est.empcat.m[, Measure := factor(Measure, levels = c("Establishements", "Employment"))]
est.empcat.m[, IndustryLabel := emp_cat_labels$EmpLabel[match(EmpCatName, emp_cat_labels$EmpCatName)]]

p_est_empcat_comp <- ggplot(data = est.empcat.m, aes(x = IndustryLabel, y = Percent, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("Measure", values = rgb(rsgcolordf[2:3,],maxColorValue = 255)) +
  scale_y_continuous(name = "Percentage of Establishments or Employment", labels = scales::percent) + 
  xlab("Employment Category") + 
  labs(title = "Establishments and Employment in the CVS Sample", 
       subtitle = "By Employment Category, Compared with Model Inputs", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey (Weighted), SEMCOG Establishment Data, SEMCOG E7 Model") +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  facet_wrap(~Measure) +
  coord_flip() 

ggsave(p_est_industry_comp, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, 
                            "_Documentation", 
                            "data_proc_cvs_p_est_empcat_comp.png"), 
       width = 8, 
       height = 5) 

# What other information about the establishments is interesting?
descr[TABLE == "Establishment"]

# Vehicle ownership and use
# OWN_LEASE_BUS_VEH	
# Does your company own or lease any vehicles (cars, vans, trucks, or large vehicles) as part of the business	
# 1=Yes; 2=No

est.own_lease <- est[,.(Establishments = .N,
                  EstWeighted = sum(ESTABLISHMENT_WGHT_FCTR)), 
               keyby = OWN_LEASE_BUS_VEH]
est.own_lease.m <- melt.data.table(est.own_lease,
                             id.vars = "OWN_LEASE_BUS_VEH",
                             variable.name = "Data",
                             value.name = "NumEst")
est.own_lease.m[, PctEst := NumEst/sum(NumEst), by = "Data"]
est.own_lease.m

# USE_PERSNL_VEH	
# Does anyone in your company use personal vehicles for company business	
# 1=Yes; 2=No; 99 = Not Provided
est.pers_veh <- est[,.(Establishments = .N,
                        EstWeighted = sum(ESTABLISHMENT_WGHT_FCTR)), 
                     keyby = USE_PERSNL_VEH]
est.pers_veh.m <- melt.data.table(est.pers_veh,
                                   id.vars = "USE_PERSNL_VEH",
                                   variable.name = "Data",
                                   value.name = "NumEst")
est.pers_veh.m[, PctEst := NumEst/sum(NumEst), by = "Data"]
est.pers_veh.m

# have 1 or more other vehicle
est[, veh_not_own := ifelse(TOTAL_VEH_NOT_OWNED > 0,1,2)]

est.veh_not_own <- est[,.(Establishments = .N,
                             EstWeighted = sum(ESTABLISHMENT_WGHT_FCTR)), 
                          keyby = veh_not_own]
est.veh_not_own.m <- melt.data.table(est.veh_not_own,
                                        id.vars = "veh_not_own",
                                        variable.name = "Data",
                                        value.name = "NumEst")
est.veh_not_own.m[, PctEst := NumEst/sum(NumEst), by = "Data"]
est.veh_not_own.m

# either own/leave or personal vehicle or have 1 or more other vehicle
est[, own_lease_pers := ifelse(OWN_LEASE_BUS_VEH == 1 | USE_PERSNL_VEH == 1 | TOTAL_VEH_NOT_OWNED > 0,1,2)]

est.own_lease_pers <- est[,.(Establishments = .N,
                       EstWeighted = sum(ESTABLISHMENT_WGHT_FCTR)), 
                    keyby = own_lease_pers]
est.own_lease_pers.m <- melt.data.table(est.own_lease_pers,
                                  id.vars = "own_lease_pers",
                                  variable.name = "Data",
                                  value.name = "NumEst")
est.own_lease_pers.m[, PctEst := NumEst/sum(NumEst), by = "Data"]
est.own_lease_pers.m

# What types of vehicle and how many
# TOTAL_VEH_OWNED_OR_LEASED	    Total number of vehicles owned or leased by the company at this location
# OWNED_SINGLE_UNIT	            Number of Cargo transport vehicles (single unit) owned/leased
# OWNED_COMBO_TRACTOR_TRAILER	  Number of Cargo transport vehicles (combo unit/tractor-trailers) owned/leased
# OWNED_PASSENGER_CAR_OR_SUV	    Number of Passenger car or sport utility vehicle owned/leased
# OWNED_PICKUP_TRUCK	          Number of Pickup trucks owned/leased
# OWNED_VAN       	            Number of Vans owned/leased
# OWNED_OTHER_VEH	              Number of Other vehicles used for cargo delivery or pickup owned/leased

# TOTAL_VEH_NOT_OWNED	          Total number of vehicles other vehicles NOT owned/leased by the company that are regularly used for commercial purposes
# VEH_NOT_OWNED_SU	            Number of Cargo transport vehicles (single unit) NOT owned/leased
# VEH_NOT_OWNED_CU	            Number of Cargo transport vehicles (combo unit/tractor-trailers) NOT owned/leased
# VEH_NOT_OWNED_PCSUV	          Number of Passenger car or sport utility vehicle NOT owned/leased
# VEH_NOT_OWNED_PICKUP	        Number of Pickup trucks NOT owned/leased
# VEH_NOT_OWNED_VAN	            Number of Vans NOT owned/leased
# VEH_NOT_OWNED_OTHER	          Number of Other vehicles used for cargo delivery or pickup NOT owned/leased

# check consistency
est[OWN_LEASE_BUS_VEH == 1 & TOTAL_VEH_OWNED_OR_LEASED == 0, .N] #0
est[OWN_LEASE_BUS_VEH == 2 & TOTAL_VEH_OWNED_OR_LEASED > 0, .N] #0

est[TOTAL_VEH_NOT_OWNED == 0, .N] #0
est[TOTAL_VEH_NOT_OWNED > 0, .N] #0

# number of vehicles by type
# calculated weight average number of vehicles of each type
av_veh   <- rbind(
   est[OWN_LEASE_BUS_VEH == 1,
    .(total_veh = sum(TOTAL_VEH_OWNED_OR_LEASED * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      `Single unit truck` = sum(OWNED_SINGLE_UNIT * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      `Combo tractor trailer` = sum(OWNED_COMBO_TRACTOR_TRAILER * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      `Car or SUV` = sum(OWNED_PASSENGER_CAR_OR_SUV * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      `Pickup truck` = sum(OWNED_PICKUP_TRUCK * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      `Van` = sum(OWNED_VAN * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      `Other vehicle` = sum(OWNED_OTHER_VEH * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      Ownership = "Owned or Leased")],
   est[TOTAL_VEH_NOT_OWNED > 0,
    .(total_veh = sum(TOTAL_VEH_NOT_OWNED * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      `Single unit truck` = sum(VEH_NOT_OWNED_SU * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      `Combo tractor trailer` = sum(VEH_NOT_OWNED_CU * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      `Car or SUV` = sum(VEH_NOT_OWNED_PCSUV * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      `Pickup truck` = sum(VEH_NOT_OWNED_PICKUP * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      `Van` = sum(VEH_NOT_OWNED_VAN * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      `Other vehicle` = sum(VEH_NOT_OWNED_OTHER * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
      Ownership = "Not Owned")])

av_veh_m <- melt.data.table(data = av_veh,
                            id.vars = "Ownership",
                            variable.name = "Vehicle_Type",
                            value.name = "Number_Vehicles")

# simple chart of average number of vehicles y type
p_est_av_veh <- ggplot(data = av_veh_m[Vehicle_Type != "total_veh"], aes(x = factor(Vehicle_Type, levels = rev(levels(Vehicle_Type))), y = Number_Vehicles, fill = Ownership)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = rgb(rsgcolordf[2:3,],maxColorValue = 255)) +
  scale_y_continuous(name = "Number of Vehicles", labels = scales::comma) + 
  xlab("Vehicle Type") + 
  labs(title = "Average Number of Vehicles Available to Establishments", 
       subtitle = "By Vehicle Type and Ownership", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey (Weighted)") +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  coord_flip() 

ggsave(p_est_av_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, 
                            "_Documentation", 
                            "data_proc_cvs_p_est_av_veh.png"), 
       width = 8, 
       height = 5) 

# Truck trips by establishment and employee
est[, .(VehTo = sum(NUM_VEH_DLVR_CARGO_OR_SVCS_HERE_AVG_WKDY),
        VehFrom = sum(NUM_VEH_DLVR_CARGO_OR_SVCS_AWAY_AVG_WKDY),
        Emp = sum(TOTAL_EMPLOYEES),
        Est = .N), 
    by = INDUSTRY]

est[, TrucksToPerEmp := NUM_VEH_DLVR_CARGO_OR_SVCS_HERE_AVG_WKDY/TOTAL_EMPLOYEES]
est[, TrucksFromPerEmp := NUM_VEH_DLVR_CARGO_OR_SVCS_AWAY_AVG_WKDY/TOTAL_EMPLOYEES]

est_trips_to_from <- rbind(est[,.(IndName, TrucksPerEmp = TrucksToPerEmp, Direction = "TO")],
                           est[,.(IndName, TrucksPerEmp = TrucksFromPerEmp, Direction = "FROM")])

p_est_trucks_to_from <- ggplot(data = est_trips_to_from, aes(x = factor(IndName, levels = IndustryCats), y = TrucksPerEmp, fill = Direction)) +
  geom_boxplot() + 
  scale_fill_manual(values = rgb(rsgcolordf[2:3,],maxColorValue = 255)) +
  labs(title = "Daily Trucks Trips To and From Establishments", subtitle = "Per Employee by Industry", caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Unweighted") +
  xlab("Industry Category") + scale_y_continuous(name = "Number of Daily Truck Trips per Employee", labels = scales::comma) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  coord_flip()

ggsave(p_est_trucks_to_from, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_est_trucks_to_from.png"), 
       width = 8, height = 5) 

av_trips <- est[, .(To = sum(TrucksToPerEmp * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR),
                    From = sum(TrucksFromPerEmp * ESTABLISHMENT_WGHT_FCTR)/sum(ESTABLISHMENT_WGHT_FCTR)),
             by = EmpCatName]

av_trips[, IndustryLabel := emp_cat_labels$EmpLabel[match(EmpCatName, emp_cat_labels$EmpCatName)]]

av_trips_m <- melt.data.table(av_trips,
                              id.vars = c("EmpCatName", "IndustryLabel"),
                              variable.name = "Direction",
                              value.name = "DailyTrucks")

p_est_av_trucks_day <- ggplot(data = av_trips_m, aes(x = IndustryLabel, y = DailyTrucks, fill = Direction)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("Direction", values = rgb(rsgcolordf[2:3,],maxColorValue = 255)) +
  scale_y_continuous(name = "Daily Trucks", labels = scales::comma) + 
  xlab("Industry Category") + 
  labs(title = "Average Daily Trucks per Employee", 
       subtitle = "By Employment Category", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey (Weighted)") +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  coord_flip() 

ggsave(p_est_av_trucks_day, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, 
                            "_Documentation", 
                            "data_proc_cvs_p_est_av_trucks_day.png"), 
       width = 8, 
       height = 5)

# Save the establishments table with added fields
saveRDS(est, file.path(SYSTEM_DEV_DATA_PATH, "Survey", "Outputs", "semcog_cvs_est.RDS"))

### Vehicles  ------------------------------------------------------------------

# vehicle class, vehicle use
dcast.data.table(veh[!TTL_DIARY_STOPS %in% c(0,999)], VEH_PRIMARY_USE~ VEH_CLASS)

veh_class_use <- veh[!TTL_DIARY_STOPS %in% c(0,999), 
                     .(VehiclesUnweighted = .N, Vehicles = sum(FINAL_FACTOR)), 
                     by = .(VEH_PRIMARY_USE, VEH_CLASS)]

veh_class_use <- veh_class_use[VEH_PRIMARY_USE != 88 & VEH_CLASS != 888]

VehPrimaryUseCats <- unlist(strsplit(descr[TABLE == "Vehicle" & FIELD.NAME == "VEH_PRIMARY_USE"]$CODE.VALUES, "\n"))
VehPrimaryUseCats <- c("Cargo", "Service", "Service & Cargo")
VehClassCats <- unlist(strsplit(descr[TABLE == "Vehicle" & FIELD.NAME == "VEH_CLASS"]$CODE.VALUES, "\n"))

veh_class_use[, VehClassName := VehClassCats[VEH_CLASS]]
veh_class_use[, VehPrimaryUseName := VehPrimaryUseCats[VEH_PRIMARY_USE]]
veh_class_use[, PctVehicles := Vehicles/sum(Vehicles), by = VehClassName]

p_veh_class <- ggplot(data = veh_class_use[, .(Vehicles = sum(Vehicles)), by = VehClassName], aes(x = VehClassName, y = Vehicles)) +
  geom_bar(stat = "identity", position = "dodge", fill = rgb(rsgcolordf[2,],maxColorValue = 255)) +
  labs(title = "Number of Vehicles", 
       subtitle = "by Vehicle Class", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Vehicle Class") + scale_y_continuous(name = "Vehicles", labels = scales::comma) +
  coord_flip()

ggsave(p_veh_class, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_veh_class.png"), 
       width = 8, height = 5) 

p_veh_class_use <- ggplot(data = veh_class_use, aes(x = VehClassName, y = PctVehicles, fill = VehPrimaryUseName)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  scale_fill_manual("Primary Use", values = rgb(rsgcolordf[2:4,],maxColorValue = 255)) +
  labs(title = "Vehicle Primary Use", 
       subtitle = "By Vehicle Class", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Vehicle Class") + scale_y_continuous(name = "Vehicles", labels = scales::percent)  +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  coord_flip() 

ggsave(p_veh_class_use, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_veh_class_use.png"), 
       width = 8, height = 5) 

veh[, VehClassName := VehClassCats[VEH_CLASS]]

p_veh_trips <- ggplot(data = veh[!TTL_DIARY_STOPS %in% c(0,999) & VEH_CLASS != 888], aes(x = VehClassName, y = TTL_DIARY_STOPS)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Reported Stops on Diary Day", 
       subtitle = "By Vehicle Class", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Unweighted") +
  xlab("Vehicle Class") + scale_y_continuous(name = "Stops", labels = scales::comma) 

ggsave(p_veh_trips, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_veh_trips.png"), 
       width = 8, height = 5) 

veh[!TTL_DIARY_STOPS %in% c(0,999) & VEH_CLASS != 888, .(median(TTL_DIARY_STOPS)), by = VEH_CLASS]
veh[!TTL_DIARY_STOPS %in% c(0,999) & VEH_CLASS != 888, .(Veh = .N, MeanStops = mean(TTL_DIARY_STOPS)), by = VEH_CLASS][order(VEH_CLASS)]

fwrite(veh[, .N, keyby = .(VEH_CLASS, VEHICLE_TYPE_EXPANSION)],
       file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_veh_n.csv"))

# grouped classes
VehClassLMH <- c(rep("Light", 4), rep("Medium", 3), "Heavy", NA)
veh[, VehClassLMH := VehClassLMH[VEH_CLASS]]

# Save the vehicles table with added fields
saveRDS(veh, file.path(SYSTEM_DEV_DATA_PATH, "Survey", "Outputs", "semcog_cvs_veh.RDS"))

### Trips ------------------------------------------------------------------------------------

trip[,.(StopsUnweighted = .N,
       Stops = sum(FINAL_FACTOR))]

# Stops by activity 
stop_act <- trip[,.(StopsUnweighted = .N,
                    Stops = sum(FINAL_FACTOR)), 
                 keyby = STOP_ACTIVITY]

ActivityCats <- unlist(strsplit(descr[TABLE == "Trip" & FIELD.NAME == "STOP_ACTIVITY"]$CODE.VALUES, "\n"))

stop_act[, ActivityName := ifelse(STOP_ACTIVITY <= 11, 
                                  ActivityCats[STOP_ACTIVITY], 
                                  ActivityCats[12]) ]

# Add a stop type grouping the stop activity 
# to the trips table to identify trips as service, goods, or other
stop_act[, StopPurpose := c("Return/Deadhead/Other", 
                            "Intermediate", 
                            "Intermediate", 
                            "Return/Deadhead/Other", 
                            "Goods", 
                            "Goods",
                            "Return/Deadhead/Other", 
                            "Service", 
                            "Service", 
                            "Service", 
                            "Goods", 
                            "Return/Deadhead/Other")]

stop_act[, StopPurpose := factor(StopPurpose, levels = c("Goods", "Service", "Intermediate", "Return/Deadhead/Other"))]

trip[stop_act, 
     c("StopPurpose", "ActivityName") := .(i.StopPurpose, i.ActivityName),
     on = "STOP_ACTIVITY"]

setkey(stop_act, Stops)

p_stop_activity <- ggplot(data = stop_act, aes(x = factor(ActivityName, levels = stop_act$ActivityName), y = Stops, fill = StopPurpose)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Stop Purpose", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Number of Stops", 
       subtitle = "By Detailed Stop Activity and Stop Purpose", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Stop Activity") + scale_y_continuous(name = "Number of Stops", labels = scales::comma) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
  coord_flip() 

ggsave(p_stop_activity, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_stop_activity.png"), 
       width = 8, height = 5) 

# Stop duration
# Calculate stop duration, including correction for stops over midnight
trip[!is.na(ARRIVE_TIME) & !is.na(DEPART_TIME), 
     StopDuration := ifelse(DEPART_TIME - ARRIVE_TIME > 0, 
                            DEPART_TIME - ARRIVE_TIME, 
                            DEPART_TIME - ARRIVE_TIME + 1) ]

# Convert to minutes
trip[, StopDuration := StopDuration * 24 * 60]

# Calculate median and mean activity duration by activity (weighted)
median_act <- trip[!is.na(ARRIVE_TIME) & !is.na(DEPART_TIME) & STOP_ACTIVITY != 1, 
                   .(MedianDuration = median(StopDuration), 
                     MeanDurationUnweighted = mean(StopDuration),
                     MeanDuration = sum(StopDuration * FINAL_FACTOR)/sum(FINAL_FACTOR)), 
                   by = .(ActivityName, StopPurpose)][order(MeanDuration)]

p_stop_duration <- ggplot(data = median_act, aes(x = factor(ActivityName, levels = median_act$ActivityName), y = MeanDuration, fill = StopPurpose)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Stop Purpose", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Average Stop Duration", 
       subtitle = "By Detailed Stop Activity and Stop Purpose", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Activity") + scale_y_continuous(name = "Stop Duration (Minutes)", labels = scales::comma) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
  coord_flip()

ggsave(p_stop_duration, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_stop_duration.png"), 
       width = 8, height = 5) 

# Trip summary by vehicle type, including estimates of VMT, weighted
trip[veh, VehClassLMH := i.VehClassLMH, , on = "VEHNUM"]
trip[, VehClassLMH := factor(VehClassLMH, levels = c("Light", "Medium", "Heavy"))]

# Stops by vehicle type
stop_act_veh <- trip[!is.na(VehClassLMH), 
                     .(StopsUnweighted = .N,
                         Stops = sum(FINAL_FACTOR)), 
                      keyby = .(STOP_ACTIVITY, ActivityName, StopPurpose, VehClassLMH)]

stop_act_veh[, PctStops := Stops/sum(Stops), by = VehClassLMH]
setkey(stop_act_veh, STOP_ACTIVITY)

p_stop_activity_veh <- ggplot(data = stop_act_veh, aes(x = factor(ActivityName, levels = rev(ActivityCats)), y = PctStops, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Percentage of Stops", 
       subtitle = "By Detailed Stop Activity and Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Stop Activity") + scale_y_continuous(name = "Percentage of Stops", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  coord_flip() 

ggsave(p_stop_activity_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_stop_activity_vehicle.png"), 
       width = 8, height = 5) 

# Stop duration by vehicle type
median_act_veh <- trip[!is.na(ARRIVE_TIME) & !is.na(DEPART_TIME) & !is.na(VehClassLMH) 
                       & StopPurpose == "Goods" & ActivityName != "11=Shopping for Business", 
                   .(MedianDuration = median(StopDuration), 
                     MeanDurationUnweighted = mean(StopDuration),
                     MeanDuration = sum(StopDuration * FINAL_FACTOR)/sum(FINAL_FACTOR)), 
                   by = .(ActivityName, StopPurpose, VehClassLMH)]

p_stop_duration_veh <- ggplot(data = median_act_veh, aes(x = ActivityName, y = MeanDuration, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Average Stop Duration", 
       subtitle = "For Goods Stop Types by Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Activity") + scale_y_continuous(name = "Stop Duration (Minutes)", labels = scales::comma) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  coord_flip()

ggsave(p_stop_duration_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_goods_stop_duration_veh.png"), 
       width = 8, height = 4) 

# Trips: distance and travel time vehicle type
trip[is.na(TRAVEL_DISTANCE) & is.na(TRAVEL_MINUTES), .N, by = STOP_SEQ]
trip[!is.na(TRAVEL_DISTANCE) & !is.na(TRAVEL_MINUTES)]

median_distance_time_veh <- rbind(trip[!is.na(TRAVEL_DISTANCE) & !is.na(TRAVEL_MINUTES) & !is.na(VehClassLMH), 
                       .(NumberTrips = .N,
                         ExpandedTrips = sum(FINAL_FACTOR),
                         MedianDistanceUnweighted = round(median(TRAVEL_DISTANCE),1), 
                         MeanDistanceUnweighted = round(mean(TRAVEL_DISTANCE),1),
                         MeanDistance = round(sum(TRAVEL_DISTANCE * FINAL_FACTOR)/sum(FINAL_FACTOR),1),
                         MedianTimeUnweighted = round(median(TRAVEL_MINUTES),1), 
                         MeanTimeUnweighted = round(mean(TRAVEL_MINUTES),1),
                         MeanTime = round(sum(TRAVEL_MINUTES * FINAL_FACTOR)/sum(FINAL_FACTOR),1)), 
                       keyby = .(VehClassLMH)],
                       trip[!is.na(TRAVEL_DISTANCE) & !is.na(TRAVEL_MINUTES) & !is.na(VehClassLMH), 
                            .(VehClassLMH = "All Vehicles",
                              NumberTrips = .N,
                              ExpandedTrips = sum(FINAL_FACTOR),
                              MedianDistanceUnweighted = round(median(TRAVEL_DISTANCE),1), 
                              MeanDistanceUnweighted = round(mean(TRAVEL_DISTANCE),1),
                              MeanDistance = round(sum(TRAVEL_DISTANCE * FINAL_FACTOR)/sum(FINAL_FACTOR),1),
                              MedianTimeUnweighted = round(median(TRAVEL_MINUTES),1), 
                              MeanTimeUnweighted = round(mean(TRAVEL_MINUTES),1),
                              MeanTime = round(sum(TRAVEL_MINUTES * FINAL_FACTOR)/sum(FINAL_FACTOR),1))])

fwrite(median_distance_time_veh,
       file = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_median_distance_time_veh.csv"))

#Bins the time and distances and plot frequencies
trip[!is.na(TRAVEL_DISTANCE) & !is.na(TRAVEL_MINUTES), 
     .(MinDist = min(TRAVEL_DISTANCE), MaxDist = max(TRAVEL_DISTANCE),
       MinTime = min(TRAVEL_MINUTES), MaxTime = max(TRAVEL_MINUTES))]
dist_bins <- seq(0,200, by = 10)
time_bins <- seq(0,240, by = 10)

trip[, DistBin := dist_bins[findInterval(TRAVEL_DISTANCE, vec = dist_bins)]]
trip[, TimeBin := time_bins[findInterval(TRAVEL_MINUTES, vec = time_bins)]]

trip_dist_bin_veh <- trip[!is.na(DistBin) & !is.na(VehClassLMH),
     .(TripsUnweighted = .N, 
       Trips = sum(FINAL_FACTOR)), 
     keyby = .(DistBin, VehClassLMH)]

trip_time_bin_veh <- trip[!is.na(TimeBin) & !is.na(VehClassLMH),
     .(TripsUnweighted = .N, 
       Trips = sum(FINAL_FACTOR)), 
     keyby = .(TimeBin, VehClassLMH)]

trip_dist_bin_veh[, PctTrips := Trips/sum(Trips), by = VehClassLMH]
trip_time_bin_veh[, PctTrips := Trips/sum(Trips), by = VehClassLMH]

p_trip_dist_bin_veh <- ggplot(data = trip_dist_bin_veh, aes(x = factor(DistBin), y = PctTrips, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Trip Distance Distribution", 
       subtitle = "By Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Trip Distance (Bin Lower Bound, Miles)") + scale_y_continuous(name = "Percent Trips", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom")

ggsave(p_trip_dist_bin_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_distance_dist_10_veh.png"), 
       width = 8, height = 4) 

p_trip_time_bin_veh <- ggplot(data = trip_time_bin_veh, aes(x = factor(TimeBin), y = PctTrips, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Trip Travel Time Distribution", 
       subtitle = "By Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Travel Time (Bin Lower Bound, Minutes)") + scale_y_continuous(name = "Percent Trips", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom")

ggsave(p_trip_time_bin_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_time_dist_10_veh.png"), 
       width = 8, height = 4)

trip[, DistRound := floor(TRAVEL_DISTANCE)]
trip[, TimeRound := floor(TRAVEL_MINUTES)]

trip_dist_round_veh <- trip[!is.na(DistRound) & !is.na(VehClassLMH),
                            .(TripsUnweighted = .N, 
                              Trips = sum(FINAL_FACTOR)), 
                            keyby = .(DistRound, VehClassLMH)]

trip_time_round_veh <- trip[!is.na(TimeRound) & !is.na(VehClassLMH),
                            .(TripsUnweighted = .N, 
                              Trips = sum(FINAL_FACTOR)), 
                            keyby = .(TimeRound, VehClassLMH)]

trip_dist_round_veh[, PctTrips := Trips/sum(Trips), by = VehClassLMH]
trip_time_round_veh[, PctTrips := Trips/sum(Trips), by = VehClassLMH]

p_trip_dist_round_veh <- ggplot(data = trip_dist_round_veh[DistRound < 100], aes(x = DistRound, y = PctTrips, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Trip Distance Distribution", 
       subtitle = "By Vehicle Type, Excluding Trips Over 100 Miles", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Trip Distance (Miles)") + scale_y_continuous(name = "Percent Trips", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  facet_wrap(~VehClassLMH, ncol = 1)

ggsave(p_trip_dist_round_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_distance_dist_detailed_veh.png"), 
       width = 8, height = 8) 

p_trip_time_round_veh <- ggplot(data = trip_time_round_veh[TimeRound < 120], aes(x = TimeRound, y = PctTrips, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Trip Travel Time Distribution", 
       subtitle = "By Vehicle Type, Excluding Trips Over 120 Minutes", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Travel Time (Minutes)") + scale_y_continuous(name = "Percent Trips", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  facet_wrap(~VehClassLMH, ncol = 1)

ggsave(p_trip_time_round_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_time_dist_detailed_veh.png"), 
       width = 8, height = 8)

# VMT and VHT by vehicle type
trips_vmt_summary <- rbind(trip[!is.na(TRAVEL_DISTANCE) & !is.na(TRAVEL_MINUTES) & !is.na(VehClassLMH), 
                          .(TripsUnweighted = .N, 
                            Trips = sum(FINAL_FACTOR), 
                            VMT = sum(FINAL_FACTOR * TRAVEL_DISTANCE, na.rm = TRUE),
                            VHT = sum(FINAL_FACTOR * TRAVEL_MINUTES/60, na.rm = TRUE)), 
                          keyby = VehClassLMH],
                          trip[!is.na(TRAVEL_DISTANCE) & !is.na(TRAVEL_MINUTES) & !is.na(VehClassLMH), 
                               .(VehClassLMH = "All Vehicles",
                                 TripsUnweighted = .N, 
                                 Trips = sum(FINAL_FACTOR), 
                                 VMT = sum(FINAL_FACTOR * TRAVEL_DISTANCE, na.rm = TRUE),
                                 VHT = sum(FINAL_FACTOR * TRAVEL_MINUTES/60, na.rm = TRUE))])

trips_vmt_summary[VehClassLMH != "All Vehicles", 
                  c("PctTrips", "PctVMT", "PctVHT") := .(round(Trips/sum(Trips),3), 
                                                           round(VMT/sum(VMT),3),
                                                           round(VHT/sum(VHT),3))]
trips_vmt_summary[VehClassLMH == "All Vehicles", 
                  c("PctTrips", "PctVMT", "PctVHT") := 1.000]

fwrite(trips_vmt_summary,
       file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trip_vmt_veh.csv"))

# Spatial distribution of stops and trips
# Stops: Inside the SEMCOG region, outside the region
# Trips: II/XI/IX/external (not necessarily XX, could be not traversing the region at all)

# TRIP_STOP_TAZID has a value of 9999 for outside of SEMCOG region
trip[, .N, by = TRIP_STOP_TAZID]
trip[, IntExtStop := ifelse(TRIP_STOP_TAZID == 9999, "External", "Internal")]
trip[, .(Unweighted = .N, Weighted = sum(FINAL_FACTOR)), by = IntExtStop]

# Cross tab of number of stops by vehicle class and type and Internal/External
# by detailed vehicle class
trip.ie.veh <- dcast.data.table(trip, 
                                VEH_CLASS + VehClassLMH ~ IntExtStop,
                                fun.aggregate = sum,
                                value.var = "FINAL_FACTOR")

trip.ie.veh[, PctExt := External/(Internal+External)]

# by vehicle type
trip.ie.veh.lmh <- dcast.data.table(trip[!is.na(VehClassLMH)], 
                                VehClassLMH ~ IntExtStop,
                                fun.aggregate = sum,
                                value.var = "FINAL_FACTOR")

trip.ie.veh.lmh[, PctExt := External/(Internal+External)]

# Cross tab of number of weighted stops by purpose and Internal/External
# by detailed activity type and stop purpose
trip.ie.act <- dcast.data.table(trip, 
                                ActivityName + StopPurpose ~ IntExtStop,
                                fun.aggregate = sum,
                                value.var = "FINAL_FACTOR")

trip.ie.act[, PctExt := External/(Internal+External)]

# by stop purpose
trip.ie.purp <- dcast.data.table(trip, 
                                StopPurpose ~ IntExtStop,
                                fun.aggregate = sum,
                                value.var = "FINAL_FACTOR")

trip.ie.purp[, PctExt := External/(Internal+External)]

# By vehicle type and stop purpose
trip.ie.veh.lmh.purp <- dcast.data.table(trip[!is.na(VehClassLMH)], 
                                         VehClassLMH + StopPurpose ~ IntExtStop,
                                         fun.aggregate = sum,
                                         value.var = "FINAL_FACTOR")

trip.ie.veh.lmh.purp[, PctExt := External/(Internal+External)]

p_ext_stops <- ggplot(data = trip.ie.veh.lmh.purp, aes(x = StopPurpose, y = PctExt, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Proportion of Stops External to SEMCOG Region", 
       subtitle = "By Stop Purpose and Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Stop Purpose") + scale_y_continuous(name = "Percentage External Stops", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  coord_flip()

ggsave(p_ext_stops, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_externals_stops.png"), 
       width = 8, height = 5) 

# Similar charts for trips, by II/IX/XI/external by vehicle type
# Add the stop location (internal or external) for the previous stop
# Will be NA for the stops that are stop_seq = 1, the departing stop at the beginning of a vehicle's diary
trip[trip[, .(SITEID, VEHNUM, STOP_SEQ = STOP_SEQ + 1, IntExtStop)], 
     IntExtPrevStop := i.IntExtStop,
     on = c("SITEID", "VEHNUM", "STOP_SEQ")]

trip[!is.na(VehClassLMH), ODSegment := ifelse(IntExtPrevStop == "Internal" & IntExtStop == "Internal", "Internal-Internal",
                                                    ifelse(IntExtPrevStop == "Internal" & !IntExtStop == "Internal", "Internal-External",
                                                           ifelse(!IntExtPrevStop == "Internal" & IntExtStop == "Internal", "External-Internal","External to Region")))]
trip[, ODSegment := factor(ODSegment, levels = c("Internal-Internal", "Internal-External", "External-Internal", "External to Region")) ]
trip[,.(Unwieghted = .N, Weighted = sum(FINAL_FACTOR)), by = ODSegment]

# By vehicle type and od segment
trip.od.veh.lmh <- trip[!is.na(VehClassLMH) & !is.na(ODSegment), .(Trips = sum(FINAL_FACTOR)), by = .(ODSegment, VehClassLMH)]
trip.od.veh.lmh[, PctODSegment := Trips/sum(Trips), by = VehClassLMH]

p_trips_odsegment_veh <- ggplot(data = trip.od.veh.lmh, aes(x = ODSegment, y = PctODSegment, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Proportion of Trips by OD Segment", 
       subtitle = "By Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("OD Segment") + scale_y_continuous(name = "Percentage Trips", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  coord_flip()

ggsave(p_trips_odsegment_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_odsegment_veh.png"), 
       width = 8, height = 5) 

# redo the distance distribution by vehicle type showing the OD Segment
trip[!is.na(ODSegment), ODGroup := ifelse(ODSegment == "Internal-Internal", "Internal Trip", "External Trip")]

trip_dist_bin_veh_ie <- trip[!is.na(DistBin) & !is.na(VehClassLMH) & !is.na(ODGroup),
                            .(TripsUnweighted = .N, 
                              Trips = sum(FINAL_FACTOR)), 
                            keyby = .(DistBin, VehClassLMH, ODGroup)]

trip_dist_bin_veh_ie[, PctTrips := Trips/sum(Trips), by = .(VehClassLMH, ODGroup)]
trip_dist_bin_veh_ie[, ODGroup := factor(ODGroup, levels = c("Internal Trip", "External Trip"))]

p_trip_dist_bin_veh_ie <- ggplot(data = trip_dist_bin_veh_ie, aes(x = DistBin, y = PctTrips, fill = ODGroup)) +
  geom_bar(stat = "identity") +
  scale_fill_manual("Internal or External", values = rgb(rsgcolordf[c(2,3),],maxColorValue = 255)) +
  labs(title = "Trip Distance Distribution", 
       subtitle = "By Vehicle Type and Internal or External Trip", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Trip Distance (Miles)") + scale_y_continuous(name = "Percent Trips", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  facet_grid(VehClassLMH~ODGroup)

ggsave(p_trip_dist_bin_veh_ie, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_distance_dist_int_ext_veh.png"), 
       width = 8, height = 8) 

# Summary of travel times and distance by Vehicle and Int/Ext
mean_distance_time_veh_ie <- rbind(trip[!is.na(TRAVEL_DISTANCE) & !is.na(TRAVEL_MINUTES) & !is.na(VehClassLMH) & !is.na(ODGroup), 
                                       .(NumberTrips = .N,
                                         ExpandedTrips = sum(FINAL_FACTOR),
                                         MeanDistance = round(sum(TRAVEL_DISTANCE * FINAL_FACTOR)/sum(FINAL_FACTOR),1),
                                         MeanTime = round(sum(TRAVEL_MINUTES * FINAL_FACTOR)/sum(FINAL_FACTOR),1)), 
                                       keyby = .(VehClassLMH, ODGroup)],
                                  trip[!is.na(TRAVEL_DISTANCE) & !is.na(TRAVEL_MINUTES) & !is.na(VehClassLMH) & !is.na(ODGroup), 
                                       .(VehClassLMH = "All Vehicles",
                                         NumberTrips = .N,
                                         ExpandedTrips = sum(FINAL_FACTOR),
                                         MeanDistance = round(sum(TRAVEL_DISTANCE * FINAL_FACTOR)/sum(FINAL_FACTOR),1),
                                         MeanTime = round(sum(TRAVEL_MINUTES * FINAL_FACTOR)/sum(FINAL_FACTOR),1)),
                                       keyby = ODGroup])

mean_distance_time_veh_ie[, IntExt := factor(ifelse(ODGroup == "Internal Trip", "Int", "Ext"), levels = c("Int", "Ext"))]

mean_distance_time_veh_ie <- dcast.data.table(mean_distance_time_veh_ie,
                                              VehClassLMH ~ IntExt,
                                              fun.aggregate = sum,
                                              value.var = c("NumberTrips", "ExpandedTrips", "MeanDistance", "MeanTime"))
setcolorder(mean_distance_time_veh_ie, c("VehClassLMH", "NumberTrips_Int", "ExpandedTrips_Int", "MeanDistance_Int", "MeanTime_Int",
                                         "NumberTrips_Ext", "ExpandedTrips_Ext", "MeanDistance_Ext", "MeanTime_Ext"))

fwrite(mean_distance_time_veh_ie,
       file = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_mean_distance_time_veh_ie.csv"))

# Diurnal profile of trip start times
# DEPART_TIME is the departure time from the stop, so use all trips from stop_seq == 1
# Final trip of the diary is NA, no departure time, so filter for NA DEPART_TIME
# Values are decimal day, 0-1
range(trip$DEPART_TIME, na.rm = TRUE)

trip[, DEPART_HOUR := floor(DEPART_TIME * 24)]
dep_hour_veh <- trip[!is.na(DEPART_HOUR) & !is.na(VehClassLMH),
                .(NumberTrips = .N,
                  ExpandedTrips = sum(FINAL_FACTOR)),
                keyby = .(DEPART_HOUR, VehClassLMH)]
dep_hour_veh[, PctTrips := ExpandedTrips/sum(ExpandedTrips), by = VehClassLMH]

p_trip_dep_hour_veh <- ggplot(data = dep_hour_veh, aes(x = factor(DEPART_HOUR), y = PctTrips, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Trip Departure Time Distribution", 
       subtitle = "By Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Departure Time (Hour)") + scale_y_continuous(name = "Percent Trips", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom")

ggsave(p_trip_dep_hour_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_depart_time_veh.png"), 
       width = 8, height = 5)

# Allocate the trips into the SEMCOG model time periods
todranges <- c(NT = 0, AM = 390, MD = 540, PM = 870, EV = 1110, NT = 1320)

trip[, DEPART_TOD := names(todranges)[findInterval(DEPART_TIME * 1440, vec = todranges)]]
trip[, DEPART_TOD := factor(DEPART_TOD, levels = names(todranges[2:6]))]
trip[, DEPART_TOD := factor(DEPART_TOD, labels = c("AM (6:30 AM - 8:59 AM)",
                                                   "MD (9:00 AM - 2:29 PM)",
                                                   "PM (2:30 PM - 6:29 PM)",
                                                   "EV (6:30 PM - 9:59 PM)",
                                                   "NT (10:00 PM - 6:29 AM)"))]
trip[!is.na(DEPART_TOD),.N, keyby = DEPART_TOD]

# chart the percent of trips by time period, vehicle, and OD group
trip_depart_tod_veh_ie <- trip[!is.na(DEPART_TOD) & !is.na(VehClassLMH) & !is.na(ODGroup),
                             .(TripsUnweighted = .N, 
                               Trips = sum(FINAL_FACTOR)), 
                             keyby = .(DEPART_TOD, VehClassLMH, ODGroup)]

trip_depart_tod_veh_ie[, PctTrips := Trips/sum(Trips), by = .(VehClassLMH, ODGroup)]
trip_depart_tod_veh_ie[, ODGroup := factor(ODGroup, levels = c("Internal Trip", "External Trip"))]

p_trip_depart_tod_veh_ie <- ggplot(data = trip_depart_tod_veh_ie, aes(x = DEPART_TOD, y = PctTrips, fill = VehClassLMH)) +
  geom_bar(stat = "identity",  position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Trip Departure Time Period Distribution", 
       subtitle = "By Vehicle Type and Internal or External Trip", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Time Period") + scale_y_continuous(name = "Percent Trips", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  facet_wrap(~ODGroup, ncol = 1)

ggsave(p_trip_depart_tod_veh_ie, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_tod_dist_int_ext_veh.png"), 
       width = 8, height = 5) 

# distribution of stops by tod and stop purpose
# based on arrival time at stops
trip[, ARRIVE_HOUR := floor(ARRIVE_TIME * 24)]
trip[, ARRIVE_TOD := names(todranges)[findInterval(ARRIVE_TIME * 1440, vec = todranges)]]
trip[, ARRIVE_TOD := factor(ARRIVE_TOD, levels = names(todranges[2:6]))]
trip[, ARRIVE_TOD := factor(ARRIVE_TOD, labels = c("AM (6:30 AM - 8:59 AM)",
                                                   "MD (9:00 AM - 2:29 PM)",
                                                   "PM (2:30 PM - 6:29 PM)",
                                                   "EV (6:30 PM - 9:59 PM)",
                                                   "NT (10:00 PM - 6:29 AM)"))]

trip[!is.na(ARRIVE_TOD),.N, keyby = ARRIVE_TOD]

# chart the percent of stops by time period, vehicle, stop purpse
trip_arrive_tod_veh_purp <- trip[!is.na(ARRIVE_TOD) & !is.na(VehClassLMH) & !is.na(StopPurpose),
                               .(TripsUnweighted = .N, 
                                 Trips = sum(FINAL_FACTOR)), 
                               keyby = .(ARRIVE_TOD, VehClassLMH, StopPurpose)]

trip_arrive_tod_veh_purp[, PctTrips := Trips/sum(Trips), by = .(VehClassLMH, StopPurpose)]

p_trip_arrive_tod_veh_purp <- ggplot(data = trip_arrive_tod_veh_purp, aes(x = ARRIVE_TOD, y = PctTrips, fill = VehClassLMH)) +
  geom_bar(stat = "identity",  position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Stop Arrival Time Period Distribution", 
       subtitle = "By Vehicle Type and Stop Purpose", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Time Period") + scale_y_continuous(name = "Percent Stops", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  facet_wrap(~StopPurpose, ncol = 1)

ggsave(p_trip_arrive_tod_veh_purp, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_tod_dist_stop_purp_veh.png"), 
       width = 8, height = 8) 

# tabulate VMT by time period and vehicle type, and OD Type
# Summary of travel times and distance by Vehicle and Int/Ext
vmt_tod_veh_ie <- trip[!is.na(TRAVEL_DISTANCE) & !is.na(ARRIVE_TOD) & !is.na(VehClassLMH) & !is.na(ODGroup), 
                             .(NumberTrips = .N,
                               ExpandedTrips = sum(FINAL_FACTOR),
                               VMT = round(sum(TRAVEL_DISTANCE * FINAL_FACTOR))), 
                             keyby = .(ARRIVE_TOD, VehClassLMH, ODGroup)]

vmt_tod_veh_ie[, PctVMT := VMT/sum(VMT), by = .(VehClassLMH, ODGroup)]
vmt_tod_veh_ie[, IntExt := factor(ifelse(ODGroup == "Internal Trip", "Int", "Ext"), levels = c("Int", "Ext"))]

vmt_tod_veh_ie <- add_totals(dcast.data.table(vmt_tod_veh_ie,
                                              ARRIVE_TOD ~ IntExt + VehClassLMH,
                                              fun.aggregate = sum,
                                              value.var = "PctVMT"),
                             rowtotal = FALSE)

fwrite(vmt_tod_veh_ie,
       file = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_trips_vmt_tod_veh_ie.csv"))

# save the trips table
saveRDS(trip, file.path(SYSTEM_DEV_DATA_PATH, "Survey", "Outputs", "semcog_cvs_trip.RDS"))

### Tours -----------------------------------------------------------------------------------

# add the additional fields added to the trip table
tour <- merge(tour, 
              trip[,.(USID, FINAL_FACTOR, StopPurpose, 
                      ActivityName, StopDuration, VehClassLMH, 
                      DistBin, TimeBin, DistRound, TimeRound, IntExtStop,
                      IntExtPrevStop, ODSegment, ODGroup, DEPART_HOUR,
                      DEPART_TOD, ARRIVE_HOUR, ARRIVE_TOD)],
              by = c("USID"),
              all.x = TRUE)

setkey(tour, SITEID, VEHNUM, TOUR_NUM, STOP_SEQ)

# All of the STOP_SEQ == 1 stops, including those added in a new tour start stops
# should be NA for StopDuration, DistBin, TimeBin DistRound TimeRound, IntExtPrevStop, ODSegment, ODGroup, ARRIVE_TOD
tour[STOP_SEQ == 1, c("StopDuration", "DistBin", "TimeBin",
                      "DistRound", "TimeRound", "IntExtPrevStop", 
                      "ODSegment", "ODGroup", "ARRIVE_TOD") := NA]
# Also make NA the fields in the table for distance, minutes, and arrival time to avoid duplication
# They describe the last trip of the previous tour if there are values in them which is confusing
tour[STOP_SEQ == 1, c("TRAVEL_DISTANCE", "TRAVEL_MINUTES", "ARRIVE_TIME") := NA]

# Add a field to identify the stop as the tour departure stop
tour[, TOUR_START := ifelse(STOP_SEQ == 1, TRUE, FALSE)]
tour[, TOUR_STOPS := max(STOP_SEQ) - 1, by = .(VEHNUM, TOUR_NUM)] # -1 to not count the departure as a stop
tour[, TOUR_END := ifelse(STOP_SEQ == TOUR_STOPS + 1, TRUE, FALSE)]

# For TOUR END stops, if the depart fields have values in them they describe the end of an intertour break
# Leave the values in them but need to treat carefully when calculating tour total times, etc.

# Identify all stops that are not Return stops
tour[, STOP_NOT_RETURN := ifelse(STOP_SEQ != 1 & STOP_ACTIVITY != 1, TRUE, FALSE)]

# For tabulation purposes, calculate a set of tour weights that can be used to weight each tabulation by tour
# This should be the mean of final factors for all of the trips in the tour
tour_weight <- tour[STOP_SEQ != 1, .(Trips = .N, Weight = mean(FINAL_FACTOR)), by = .(VEHNUM, TOUR_NUM)]

# Number of tours per vehicle and stops per tour
tour_sum <- tour[STOP_ACTIVITY != 1, .(Stops = .N), by = .(VEHNUM, TOUR_NUM, VehClassLMH)]
tour_sum[, Tours := max(TOUR_NUM), by = VEHNUM]
tour_sum[, ToursFact := ifelse(Tours < 3, as.character(Tours),"3+")]
tour_sum[tour_weight, Weight := i.Weight, on = c("VEHNUM", "TOUR_NUM")]
tour_sum_stops <- tour_sum[, .(UnweightedTours = .N, Tours = sum(Weight)), keyby = .(ToursFact, Stops)]
tour_sum_stops[, PctTours := Tours/sum(Tours)]

tour_sum_stops[Stops <= 12, sum(PctTours)*100] #1% over 12 stops

p_tour_sum <- ggplot(data = tour_sum_stops, aes(x = factor(Stops), y = PctTours, fill = ToursFact)) +
  geom_bar(stat = "identity",  position = "stack") +
  scale_fill_manual("Number of Tours per Day", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Stops per Tour Distribution", 
       subtitle = "By Number of Tours per Day", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Stops per Tour") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom")

ggsave(p_tour_sum, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_stops_tour_day.png"), 
       width = 8, height = 5) 

# Tour stop distribution by vehicle type
tour_sum_stops_veh <- tour_sum[!is.na(VehClassLMH), 
                               .(UnweightedTours = .N, Tours = sum(Weight)), 
                               keyby = .(Stops, VehClassLMH)]

tour_sum_stops_veh[, PctTours := Tours/sum(Tours), by = VehClassLMH]

p_tour_sum_veh <- ggplot(data = tour_sum_stops_veh, aes(x = factor(Stops), y = PctTours, fill = VehClassLMH)) +
  geom_bar(stat = "identity",  position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Stops per Tour Distribution", 
       subtitle = "By Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Stops per Tour") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom")

ggsave(p_tour_sum_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_stops_veh.png"), 
       width = 8, height = 5) 

# Categorize tours as all goods, all service, both goods and service stops, no goods or service stops
tour_purp <- tour[STOP_ACTIVITY != 1, .(Stops = .N), by = .(VEHNUM, TOUR_NUM, StopPurpose, VehClassLMH)]
tour_purp[StopPurpose %in% c("Intermediate", "Return/Deadhead/Other"), StopPurpose := "Other"]
tour_purp[,.(Tours = sum(Stops)), by = StopPurpose]
tour_purp <- dcast.data.table(tour_purp, 
                              VEHNUM + TOUR_NUM + VehClassLMH ~ StopPurpose,
                              fun.aggregate = sum,
                              value.var = "Stops")
tour_purp[, TourPurpose := ifelse(Goods > 0 & Service == 0, "Goods",
                                  ifelse(Service > 0 & Goods == 0, "Service",
                                         ifelse(Service > 0 & Goods > 0, "Goods and Service", "Other")))]

tour_purp[tour_weight, Weight := i.Weight, on = c("VEHNUM", "TOUR_NUM")]
tour_purp_veh <- tour_purp[!is.na(VehClassLMH), .(UnweightedTours = .N, Tours = sum(Weight)), keyby = .(TourPurpose, VehClassLMH)]
tour_purp_veh[, PctTours := Tours/sum(Tours), by = VehClassLMH]
tour_purp_veh[, TourPurpose := factor(TourPurpose, levels = c("Goods", "Service", "Goods and Service", "Other"))]

p_tour_purp_veh <- ggplot(data = tour_purp_veh, aes(x = TourPurpose, y = PctTours, fill = VehClassLMH)) +
  geom_bar(stat = "identity",  position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Tour Purpose Distribution", 
       subtitle = "By Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Tour Purpose") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom")

ggsave(p_tour_purp_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_purp_veh.png"), 
       width = 8, height = 5) 

# Number of stops distribution by vehicle type and tour purpose
tour_purp[tour_sum, Stops := i.Stops, on = c("VEHNUM", "TOUR_NUM")]
tour_purp_stops_veh <- tour_purp[!is.na(VehClassLMH), .(UnweightedTours = .N, Tours = sum(Weight)), keyby = .(TourPurpose, Stops, VehClassLMH)]
tour_purp_stops_veh[, PctTours := Tours/sum(Tours), by = .(TourPurpose, VehClassLMH)]
tour_purp_stops_veh[, TourPurpose := factor(TourPurpose, levels = c("Goods", "Service", "Goods and Service", "Other"))]

p_tour_purp_stops_veh <- ggplot(data = tour_purp_stops_veh[(TourPurpose == "Goods")|(TourPurpose == "Service" & VehClassLMH %in% c("Light", "Medium"))], aes(x = factor(Stops), y = PctTours, fill = VehClassLMH)) +
  geom_bar(stat = "identity",  position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Stops per Tour Distribution", 
       subtitle = "By Vehicle Type and Tour Purpose", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Stops per Tour") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  facet_wrap(~TourPurpose, ncol = 1)

ggsave(p_tour_purp_stops_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_stops_veh_purp.png"), 
       width = 8, height = 5) 

# Analysis of tour start and end locations
tour_start_end <- tour[(TOUR_START), 
                       .(TOUR_START_TAZID = TOUR_STOP_TAZID, START_BASE_NOTBASE = STOP_BASE_NOTBASE), 
                       keyby = .(VEHNUM, TOUR_NUM)]

tour_start_end[tour[(TOUR_END),
                    .(TOUR_END_TAZID = TOUR_STOP_TAZID, END_BASE_NOTBASE = STOP_BASE_NOTBASE), 
                    by = .(VEHNUM, TOUR_NUM)],
               c("TOUR_END_TAZID", "END_BASE_NOTBASE") := 
                 .(i.TOUR_END_TAZID, i.END_BASE_NOTBASE), 
               on = c("VEHNUM", "TOUR_NUM")]

tour_start_end[, START_END_SAME_TAZ := ifelse(TOUR_START_TAZID == TOUR_END_TAZID, 1, 2)]

# Create a tour summary table and then summarize by vehicle-day to create a daily typology
# Include location and type of tour start and end
# Add number of stops, weight, vehicle and tour purpose from earlier analysis
tour_summary <- merge(tour_start_end,
                      tour_purp,
                      by = c("VEHNUM", "TOUR_NUM"))

# Add some fields characterizing the tours
# start-end combination type:
# 1 = base to base, 2 = base to not base, 3 = not base to base, 4 = not base to not base
tour_summary[, START_END_TYPE := ifelse(START_BASE_NOTBASE == 1 & END_BASE_NOTBASE == 1, 1,
                                        ifelse(START_BASE_NOTBASE == 1 & END_BASE_NOTBASE == 2, 2,
                                               ifelse(START_BASE_NOTBASE == 2 & END_BASE_NOTBASE == 1, 3, 4)))]

# Number of stops type:
# 1 = Single mid tour stop or non-return to base stop, 
# 2 = 2 or more mid tour stops
tour_summary[, STOP_NUM_TYPE := ifelse(Stops == 1, 1, 2)]

# Combine the start and end location, the start and end type, and the stop num type to a single tour type
tour_summary[,.N, keyby = .(START_END_SAME_TAZ, START_END_TYPE, STOP_NUM_TYPE)]
tour_summary[, TOUR_TYPE := paste(START_END_SAME_TAZ, START_END_TYPE, STOP_NUM_TYPE, sep = "_")]

tour_summary[, START_END_SAME_TAZ_TEXT := factor(START_END_SAME_TAZ, labels = c("SameTAZ", "DiffTAZ"))]
tour_summary[, START_END_TYPE_TEXT := factor(START_END_TYPE, labels = c("Base-Base", "Base-NotBase", "NotBase-Base", "NotBase-NotBase"))]
tour_summary[, STOP_NUM_TYPE_TEXT := factor(STOP_NUM_TYPE, labels = c("SingleStop", "MultiStop"))]

tour_summary[, TOUR_TYPE_TEXT := paste(START_END_SAME_TAZ_TEXT, START_END_TYPE_TEXT, STOP_NUM_TYPE_TEXT, sep = "_")]

# Summaries
# Total
tour_summary_totals <- tour_summary[, .(UnweightedTours = .N, NumberTours = sum(Weight)), 
                                    keyby = .(START_END_SAME_TAZ_TEXT, START_END_TYPE_TEXT, STOP_NUM_TYPE_TEXT, TOUR_TYPE_TEXT)]

tour_summary_totals[, PctTours := NumberTours/sum(NumberTours)]

fwrite(tour_summary_totals, 
       file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_summary.csv"))

# By vehicle class (LMH)
tour_summary_veh <- tour_summary[, .(UnweightedTours = .N, NumberTours = sum(Weight)), 
                                 keyby = .(START_END_SAME_TAZ_TEXT, START_END_TYPE_TEXT, STOP_NUM_TYPE_TEXT, TOUR_TYPE_TEXT, VehClassLMH)]

tour_summary_veh_xt <- dcast.data.table(tour_summary_veh[!is.na(VehClassLMH)], 
                                        TOUR_TYPE_TEXT ~ VehClassLMH,
                                        fun.aggregate = sum,
                                        value.var = "NumberTours", fill = 0)[order(-Light)]

tour_summary_veh_xt[, c("PctLight", "PctMedium", "PctHeavy") := 
                      .(Light/sum(Light), Medium/sum(Medium), Heavy/sum(Heavy))]

fwrite(tour_summary_veh_xt, 
       file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_summary_veh.csv"))

# Create a daily summary to see how the tour types are chained together over the course of the day
daily_tours <- tour_summary[, .(TOURS = max(TOUR_NUM)), by = VEHNUM]
daily_tours[order(-TOURS)]

tour_summary_comb <- tour_summary[,.(VEHNUM, TOUR_NUM, TOUR_TYPE_TEXT, STOPS = Stops, VehClassLMH, Weight)]
tour_summary_vehnum <- dcast.data.table(tour_summary_comb, VEHNUM + VehClassLMH ~ TOUR_NUM, value.var = "TOUR_TYPE_TEXT", fill = "")
setnames(tour_summary_vehnum, c("VEHNUM", "VehClassLMH", paste0("TOUR", 1:9)))
tour_summary_vehnum[daily_tours, TOURS := i.TOURS, on = "VEHNUM"]
tour_summary_vehnum[tour_summary[, .(Weight = sum(Weight)), by = VEHNUM],
                    Weight := i.Weight, on = "VEHNUM"]

# groups by same daily patters
daily_summary <- tour_summary_vehnum[, .(UnweightedVehicles = .N, NumberVehicles = sum(Weight)), 
                                     keyby = .(TOURS, TOUR1, TOUR2, TOUR3, TOUR4, TOUR5, TOUR6, TOUR7, TOUR8, TOUR9)]

daily_summary[, PctVehicles := NumberVehicles/sum(NumberVehicles)]
setcolorder(daily_summary, c("UnweightedVehicles", "NumberVehicles", "PctVehicles", "TOURS", paste0("TOUR", 1:9)))
fwrite(daily_summary, file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_daily_summary.csv"))

# groups by same daily patterns by vehicle class
daily_summary_veh <- tour_summary_vehnum[!is.na(VehClassLMH), 
                                         .(UnweightedVehicles = .N, NumberVehicles = sum(Weight)), 
                                         keyby = .(VehClassLMH, TOURS, TOUR1, TOUR2, TOUR3, TOUR4, TOUR5, TOUR6, TOUR7, TOUR8, TOUR9)]

daily_summary_veh[, PctVehicles := NumberVehicles/sum(NumberVehicles), by = VehClassLMH]
setcolorder(daily_summary_veh, c("VehClassLMH", "UnweightedVehicles", "NumberVehicles", "PctVehicles", "TOURS", paste0("TOUR", 1:9)))
fwrite(daily_summary_veh, file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_daily_summary_veh.csv"))

# Create some summary tables/charts for memo with just top entries and others grouped.
# 1. Prevalance of open tours by vehicle type
tour_summary_veh_open <- tour_summary_veh[!is.na(VehClassLMH), .(NumberTours = sum(NumberTours)), by = .(START_END_SAME_TAZ_TEXT, VehClassLMH)]
tour_summary_veh_open[, PctTours := NumberTours/sum(NumberTours), by = VehClassLMH]

p_tour_veh_open <- ggplot(data = tour_summary_veh_open, aes(x = VehClassLMH, y = PctTours, fill = START_END_SAME_TAZ_TEXT)) +
  geom_bar(stat = "identity",  position = "stack") +
  scale_fill_manual("Tour Start/End", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Complete and Incomplete Tours", 
       subtitle = "By Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Vehicle Type") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") + coord_flip()

ggsave(p_tour_veh_open, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_veh_open.png"), 
       width = 8, height = 4) 

#2. Tour patterns
# overall
top_tour_summary <- tour_summary_totals[PctTours >= 0.01][order(NumberTours)]

p_tour_patterns <- ggplot(data = top_tour_summary, aes(x = factor(TOUR_TYPE_TEXT, levels = top_tour_summary$TOUR_TYPE_TEXT), y = PctTours, fill = factor(START_END_SAME_TAZ_TEXT))) +
  geom_bar(stat = "identity") +
  scale_fill_manual("Tour Start/End", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Tour Patterns", 
       subtitle = "Patterns in >1% Tours", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Tour Pattern") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") + coord_flip()

ggsave(p_tour_patterns, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_patterns.png"), 
       width = 8, height = 5) 

# by vehicle
top_tour_summary_veh <- tour_summary_veh[, PctTours := NumberTours/sum(NumberTours), by = VehClassLMH][PctTours >= 0.01 & !is.na(VehClassLMH)][order(VehClassLMH, NumberTours)]
fwrite(top_tour_summary_veh, file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_review_cvs_top_tour_summary_veh.csv"))

p_tour_patterns_veh <- ggplot(data = top_tour_summary_veh, aes(x = factor(TOUR_TYPE_TEXT, levels = top_tour_summary_veh[!duplicated(TOUR_TYPE_TEXT)][order(PctTours)]$TOUR_TYPE_TEXT), y = PctTours, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Tour Patterns", 
       subtitle = "Patterns in >1% Tours by Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Tour Pattern") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") + coord_flip()

ggsave(p_tour_patterns_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_patterns_veh.png"), 
       width = 8, height = 5) 

#3. Daily summary
# overall
daily_summary[, WeightVehicles := NumberVehicles/TOURS]
daily_summary[, PctDaily := WeightVehicles/sum(WeightVehicles)]
setorder(daily_summary, -PctDaily)
daily_summary[, CumPctDaily := cumsum(PctDaily)]
daily_summary[1:13]

top_daily_summary <- daily_summary[CumPctDaily < .87][order(PctDaily)]
top_daily_summary[, paste0("TOUR", 4:9) := NULL]
top_daily_summary[, TOUR1 := gsub("TAZ", "", TOUR1)]
top_daily_summary[, TOUR1 := gsub("Base", "B", TOUR1)]
top_daily_summary[, TOUR1 := gsub("Not", "N", TOUR1)]
top_daily_summary[, TOUR1 := gsub("Stop", "", TOUR1)]
top_daily_summary[, TOUR2 := gsub("TAZ", "", TOUR2)]
top_daily_summary[, TOUR2 := gsub("Base", "B", TOUR2)]
top_daily_summary[, TOUR2 := gsub("Not", "N", TOUR2)]
top_daily_summary[, TOUR2 := gsub("Stop", "", TOUR2)]
top_daily_summary[, TOUR3 := gsub("TAZ", "", TOUR3)]
top_daily_summary[, TOUR3 := gsub("Base", "B", TOUR3)]
top_daily_summary[, TOUR3 := gsub("Not", "N", TOUR3)]
top_daily_summary[, TOUR3 := gsub("Stop", "", TOUR3)]
top_daily_summary[, TOUR1 := paste("1.", TOUR1)]
top_daily_summary[TOUR2 != "", TOUR2 := paste("/2.", TOUR2)]
top_daily_summary[TOUR3 != "", TOUR3 := paste("/3.", TOUR3)]
top_daily_summary[, TOURS_TEXT := paste(TOUR1, TOUR2, TOUR3)]

p_daily_patterns <- ggplot(data = top_daily_summary, aes(x = factor(TOURS_TEXT, levels = top_daily_summary$TOURS_TEXT), y = PctDaily, fill = factor(TOURS))) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Number of Tours in the Day", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Daily Tour Patterns", 
       subtitle = "Patterns by >1% Vehicles", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Daily Pattern") + scale_y_continuous(name = "Percent Vehicles", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") + coord_flip()

ggsave(p_daily_patterns, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_daily_patterns.png"), 
       width = 8, height = 5) 

# Tours with start or end points external to the region
# by vehicle class (LMH)
tour_summary[, TourStartIntExt:= ifelse(TOUR_START_TAZID == 9999, "External", "Internal")]
tour_summary[, TourEndIntExt:= ifelse(TOUR_END_TAZID == 9999, "External", "Internal")]
tour_summary_veh_ie <- tour_summary[, .(UnweightedNumberTours = .N, NumberTours = sum(Weight)), keyby = .(START_END_SAME_TAZ_TEXT, START_END_TYPE_TEXT, STOP_NUM_TYPE_TEXT, TOUR_TYPE_TEXT, VehClassLMH, TourStartIntExt, TourEndIntExt)]
tour_summary_veh_ie_open <- tour_summary_veh_ie[!is.na(VehClassLMH), .(NumberTours = sum(NumberTours)), keyby = .(START_END_SAME_TAZ_TEXT, VehClassLMH, TourStartIntExt, TourEndIntExt)]
tour_summary_veh_ie_open[, PctTours := NumberTours/sum(NumberTours), by = VehClassLMH]
tour_summary_veh_ie_open[TourStartIntExt == "External"| TourEndIntExt == "External", sum(PctTours), keyby = .(START_END_SAME_TAZ_TEXT, VehClassLMH)]
tour_summary_veh_ie_open[TourStartIntExt == "Internal" & TourEndIntExt == "Internal", sum(PctTours), keyby = .(START_END_SAME_TAZ_TEXT, VehClassLMH)]

# Tour length (travel time, stop duration, total tour time)
# Tour length (distance traveled)
# By vehicle type, by tour purpose
tour_durations <- tour[,.(StopDuration = sum(StopDuration, na.rm = TRUE),
                         TRAVEL_MINUTES = sum(TRAVEL_MINUTES, na.rm = TRUE),
                         TRAVEL_DISTANCE = sum(TRAVEL_DISTANCE, na.rm = TRUE)),
                       keyby = .(VEHNUM, TOUR_NUM)]
tour_durations[, TourTime := StopDuration + TRAVEL_MINUTES]

tour_summary <- merge(tour_summary,
                      tour_durations,
                      by = c("VEHNUM", "TOUR_NUM"),
                      all.x = TRUE)

tour_summary[, PctStopped := StopDuration/TourTime]

# Arrival time at the first scheduled stop, i.e., the first stop on the tour that is a goods or service stop
tour_first_stop <- tour[StopPurpose %in% c("Goods", "Service") & !is.na(ARRIVE_TIME),
                        .(First_Stop_Seq = min(STOP_SEQ)), 
                        keyby = .(VEHNUM, TOUR_NUM)]
tour[tour_first_stop, 
     First_Sched_Stop := i.First_Stop_Seq, 
     on = c("VEHNUM", "TOUR_NUM")]
# tour[,.N, keyby = FirsT_Sched_Stop]
# tour[FirsT_Sched_Stop == 12]
tour_arrive <- tour[STOP_SEQ == First_Sched_Stop,
                    .(VEHNUM, TOUR_NUM, FS_Seq = First_Sched_Stop, 
                      FS_Arrive_Time = ARRIVE_TIME, FS_Arrive_Hour = ARRIVE_HOUR,
                      FS_Arrive_TOD = ARRIVE_TOD, 
                      FS_StopPurpose = StopPurpose, FS_IntExtStop = IntExtStop, 
                      FS_ODSegment = ODSegment)]

tour_summary <- merge(tour_summary,
                      tour_arrive,
                      by = c("VEHNUM", "TOUR_NUM"),
                      all.x = TRUE)

# Create summary charts for the tour durations and arrival times
# total tour duration (time and distance)
# bin times into hour increments and plot the distribution
max(tour_summary$TourTime)
max(tour_summary$StopDuration)
max(tour_summary$TRAVEL_MINUTES)
tour_time_bins <- 0:12

tour_summary[, TourTotalTimeBin := tour_time_bins[findInterval(TourTime/60, vec = tour_time_bins)]]
tour_total_time <- tour_summary[,.(TimeType = "Total", UnweightedTours = .N, NumberTours = sum(Weight)),
              keyby = .(TimeBin = TourTotalTimeBin)]

tour_summary[, TourStopDurationBin := tour_time_bins[findInterval(StopDuration/60, vec = tour_time_bins)]]
tour_stopped_time <- tour_summary[,.(TimeType = "Stopped", UnweightedTours = .N, NumberTours = sum(Weight)),
             keyby = .(TimeBin = TourStopDurationBin)]

tour_summary[, TourTravelTimeBin := tour_time_bins[findInterval(TRAVEL_MINUTES/60, vec = tour_time_bins)]]
tour_travel_time <- tour_summary[,.(TimeType = "Travel", UnweightedTours = .N, NumberTours = sum(Weight)),
             keyby = .(TimeBin = TourTravelTimeBin)]

tour_time_summary <- rbind(tour_total_time, tour_stopped_time, tour_travel_time)
tour_time_summary[, PctTours := NumberTours/sum(NumberTours), by = TimeType]

p_tour_time_dist <- ggplot(data = tour_time_summary, aes(x = factor(TimeBin), y = PctTours, fill = factor(TimeType, levels = c("Travel", "Stopped","Total")))) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Time Category", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Tour Time Distribution", 
       subtitle = "Travel Time, Stopped Time, and Total Time per Tour, Binned by Hour", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Time in Hours (Lower Bound)") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") 

ggsave(p_tour_time_dist, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_total_stopped_travel_time_dist.png"), 
       width = 8, height = 5) 

# times by vehicle using more detailed distributions
tour_time_bins_10 <- seq(0, 720, by = 10)

tour_summary[, TourTotalTimeBin10 := tour_time_bins_10[findInterval(TourTime, vec = tour_time_bins_10)]]
tour_total_time_10 <- tour_summary[!is.na(VehClassLMH),.(TimeType = "Total", UnweightedTours = .N, NumberTours = sum(Weight)),
                                keyby = .(TimeBin = TourTotalTimeBin10, VehClassLMH)]

tour_summary[, TourStopDurationBin10 := tour_time_bins_10[findInterval(StopDuration, vec = tour_time_bins_10)]]
tour_stopped_time_10 <- tour_summary[!is.na(VehClassLMH),.(TimeType = "Stopped", UnweightedTours = .N, NumberTours = sum(Weight)),
                                  keyby = .(TimeBin = TourStopDurationBin10, VehClassLMH)]

tour_summary[, TourTravelTimeBin10 := tour_time_bins_10[findInterval(TRAVEL_MINUTES, vec = tour_time_bins_10)]]
tour_travel_time_10 <- tour_summary[!is.na(VehClassLMH),.(TimeType = "Travel", UnweightedTours = .N, NumberTours = sum(Weight)),
                                 keyby = .(TimeBin = TourTravelTimeBin10, VehClassLMH)]

tour_time_summary_10 <- rbind(tour_total_time_10, tour_stopped_time_10, tour_travel_time_10)
tour_time_summary_10[, PctTours := NumberTours/sum(NumberTours), by = .(TimeType, VehClassLMH)]

tour_time_summary_10[, TimeType := factor(TimeType, levels = c("Travel", "Stopped","Total"))]

p_tour_time_dist_veh <- ggplot(data = tour_time_summary_10, aes(x = TimeBin, y = PctTours, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Tour Time Distribution", 
       subtitle = "Travel Time, Stopped Time, and Total Time per Tour (10 Minute Bins, Over 720 Minutes Grouped)", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Time in Minutes (Lower Bound)") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  facet_grid(VehClassLMH~TimeType)

ggsave(p_tour_time_dist_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_total_stopped_travel_time_dist_veh.png"), 
       width = 8, height = 6) 

# Chart showing split between travel time and stopped time by vehicle type
tour_time_mean_veh <- tour_summary[!is.na(VehClassLMH), .(TravelTime = sum(TRAVEL_MINUTES * Weight)/sum(Weight),
                 StopDuration = sum(StopDuration * Weight)/sum(Weight),
                 TourTime = sum(TourTime * Weight)/sum(Weight)),
             keyby = VehClassLMH]

tour_time_mean_veh <- melt.data.table(tour_time_mean_veh,
                                        id.vars = c("VehClassLMH", "TourTime"),
                                        variable.name = "TimeType",
                                        value.name = "Time")
tour_time_mean_veh[, PctTime := Time/TourTime]
tour_time_mean_veh <- melt.data.table(tour_time_mean_veh,
                                       id.vars = c("VehClassLMH", "TourTime", "TimeType"),
                                       variable.name = "Units",
                                       value.name = "Time")

tour_time_mean_veh[Units == "PctTime", Units := "Percent Time"]

p_tour_time_av_pct_veh <- ggplot(data = tour_time_mean_veh, aes(x = VehClassLMH, y = Time, fill = TimeType)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Time Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Tour Time Averages", 
       subtitle = "Travel Time and Stopped Time by Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Vehicle Type") + scale_y_continuous(name = "Time (Minutes or Percent Total)", labels = scales::comma) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  facet_wrap(~Units, nrow = 1, scales = "free_y")

ggsave(p_tour_time_av_pct_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_stopped_travel_time_av_veh.png"), 
       width = 8, height = 4) 

# Summary for travel time component averages for goods and service tours by vehicle type
tour_time_mean_veh_purp <- tour_summary[!is.na(VehClassLMH) & TourStartIntExt == "Internal" & TourEndIntExt == "Internal", 
                                        .(TravelTime = sum(TRAVEL_MINUTES * Weight)/sum(Weight),
                                                            StopDuration = sum(StopDuration * Weight)/sum(Weight),
                                                            TourTime = sum(TourTime * Weight)/sum(Weight)),
                                     keyby = .(VehClassLMH, TourPurpose)]

tour_time_mean_veh_purp <- melt.data.table(tour_time_mean_veh_purp,
                                        id.vars = c("VehClassLMH", "TourPurpose", "TourTime"),
                                        variable.name = "TimeType",
                                        value.name = "Time")

p_tour_time_av_veh_purp <- ggplot(data = tour_time_mean_veh_purp[(TourPurpose == "Goods")|(TourPurpose == "Service" & VehClassLMH %in% c("Light", "Medium"))], aes(x = VehClassLMH, y = Time, fill = TimeType)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Time Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Tour Time Averages for Internal Tours", 
       subtitle = "Travel Time and Stopped Time by Vehicle Type and Tour Purpose", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Vehicle Type") + scale_y_continuous(name = "Time (Minutes)", labels = scales::comma) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  facet_wrap(~TourPurpose, nrow = 1)

ggsave(p_tour_time_av_veh_purp, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_stopped_travel_time_av_veh_purp.png"), 
       width = 8, height = 4) 

# Distance distribution
tour_distance_bins <- seq(0, 300, by = 10)

tour_summary[, TourDistanceBin := tour_distance_bins[findInterval(TRAVEL_DISTANCE, vec = tour_distance_bins)]]
tour_summary[,.N, keyby = TourDistanceBin]
tour_distance_veh <- tour_summary[!is.na(VehClassLMH),
                              .(UnweightedTours = .N, NumberTours = sum(Weight)),
                                keyby = .(DistanceBin = TourDistanceBin, VehClassLMH)]

tour_distance_veh[, PctTours := NumberTours/sum(NumberTours), by = VehClassLMH]

p_tour_distance_veh <- ggplot(data = tour_distance_veh, aes(x = DistanceBin, y = PctTours, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Tour Distance Distribution", 
       subtitle = "By Vehicle Type in 10 Miles Bins, Greater than 300 Miles Grouped", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Distance in Miles (Lower Bound)") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") 

ggsave(p_tour_distance_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_total_distance_veh.png"), 
       width = 8, height = 5) 

# Time of day of first stop
# Arrival hour and time period for the first scheduled stop in the tour
str(tour_summary)

arr_fs_hour_veh <- tour_summary[!is.na(FS_Arrive_Hour) & !is.na(VehClassLMH),
                     .(NumberTours = .N,
                       ExpandedTours = sum(Weight)),
                     keyby = .(FS_Arrive_Hour, VehClassLMH)]
arr_fs_hour_veh[, PctTours := ExpandedTours/sum(ExpandedTours), by = VehClassLMH]

p_tours_arr_fs_veh <- ggplot(data = arr_fs_hour_veh, aes(x = factor(FS_Arrive_Hour), y = PctTours, fill = VehClassLMH)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Arrival Time at First Stop on Tour Distribution", 
       subtitle = "By Vehicle Type", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Arrival Time (Hour)") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom")

ggsave(p_tours_arr_fs_veh, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_arrival_time_fs_veh.png"), 
       width = 8, height = 5)

# Look at differences by time period by vehicle type and tour purpose
arr_fs_tod_veh_purp <- tour_summary[!is.na(FS_Arrive_Hour) & !is.na(VehClassLMH) & 
                                      ((TourPurpose == "Goods")|(TourPurpose == "Service" & VehClassLMH %in% c("Light", "Medium"))),
                                .(NumberTours = .N,
                                  ExpandedTours = sum(Weight)),
                                keyby = .(FS_Arrive_TOD, VehClassLMH, TourPurpose)]
arr_fs_tod_veh_purp[, PctTours := ExpandedTours/sum(ExpandedTours), by = .(VehClassLMH, TourPurpose)]

p_tour_arrive_tod_veh_purp <- ggplot(data = arr_fs_tod_veh_purp, aes(x = FS_Arrive_TOD, y = PctTours, fill = VehClassLMH)) +
  geom_bar(stat = "identity",  position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Arrival Time Period at First Stop on Tour Distribution", 
       subtitle = "By Vehicle Type and Tour Purpose", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Time Period") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  facet_wrap(~TourPurpose, ncol = 1)

ggsave(p_tour_arrive_tod_veh_purp, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_arrival_tod_fs_veh_purp.png"), 
       width = 8, height = 5)

# Is there a relationship between tour length and TOD?
tour_summary[!is.na(TourTotalTimeBin), TourTotalTimeCat := ifelse(TourTotalTimeBin <3, "Short", 
                                          ifelse(TourTotalTimeBin <6, "Medium", "Long"))]
tour_summary[, TourTotalTimeCat := factor(TourTotalTimeCat, levels = c("Short", "Medium", "Long"))]

arr_fs_tod_veh_time <- tour_summary[!is.na(FS_Arrive_TOD) & !is.na(TourTotalTimeCat) & !is.na(VehClassLMH),
                                    .(NumberTours = .N,
                                      ExpandedTours = sum(Weight)),
                                    keyby = .(FS_Arrive_TOD, VehClassLMH, TourTotalTimeCat)]

arr_fs_tod_veh_time[, PctTours := ExpandedTours/sum(ExpandedTours), by = .(VehClassLMH, TourTotalTimeCat)]
arr_fs_tod_veh_time[, TourTotalTimeCat := factor(TourTotalTimeCat, labels = c("Short (<3 Hours", "Medium (3-6 Hours)", "Long (>6 Hours)"))]

p_tour_arrive_tod_veh_time <- ggplot(data = arr_fs_tod_veh_time, aes(x = FS_Arrive_TOD, y = PctTours, fill = VehClassLMH)) +
  geom_bar(stat = "identity",  position = position_dodge2(preserve = "single", padding = 0)) +
  scale_fill_manual("Vehicle Type", values = rgb(rsgcolordf[2:5,],maxColorValue = 255)) +
  labs(title = "Arrival Time Period at First Stop on Tour Distribution", 
       subtitle = "By Vehicle Type and Tour Length Category", 
       caption = "Source: SEMCOG 2017 Commercial Vehicle Survey, Weighted") +
  xlab("Time Period") + scale_y_continuous(name = "Percent Tours", labels = scales::percent) +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  facet_wrap(~TourTotalTimeCat, ncol = 1)

ggsave(p_tour_arrive_tod_veh_time, 
       filename = file.path(SYSTEM_DEV_DATA_PATH, "_Documentation", "data_proc_cvs_tours_arrival_tod_fs_veh_length.png"), 
       width = 8, height = 6)

# save the tour table
saveRDS(tour, file.path(SYSTEM_DEV_DATA_PATH, "Survey", "Outputs", "semcog_cvs_tour.RDS"))





