# CMAP CSVM
# dev script: Estimation_CV_Activities.R
# Develop a cross tabulation of establishments by the activities of their commercial vehicles

# The model application step for this model is cv_sim_activities, see cv_sim_activities.R
# Notes:
# The model selects from Goods, Service, and both Goods and Service for each business
# Segmentation should be updated to something more appropriate for the model employment categories
# CMAP model uses the NAICS2 codes, use those or sensible groupings of those codes


# Setup -------------------------------------------------------------------
source("./dev/init_dev.R")
library(readxl)
library(tidyverse)
THIS_MODEL_PATH <- file.path("./dev/Estimation/cv_activities/")
# use the NAICS2007 table built in to rFreight package to create a labels list
n2labels <- unique(NAICS2007[,.(n2 = NAICS2, Label2)])


# Load Example Output -----------------------------------------------------
cv_activities_model_old <- readRDS(file.path(THIS_MODEL_PATH, "_cv_activities_model_old.RDS"))
# Output format needs to be the same as this, except that industry is replaced with new employment group categroization.


## Load Output of Firm Synthesis (For Reference) ---------------------------
load(file.path("scenarios/base/outputs/1.Firms.RData"))
names(firm_sim_results)
firm_sim_results$ScenarioFirms
# n2 field is the NAICS 2 digit employment category
firm_sim_results$ScenarioFirms[,.N, keyby = n2]

# Load in the CMAP Establishment survey data
# Identify which business establishments operated commercial vehicles and made stops in each of the four categories:
# (1) good stops only (delivery or pickup), 
# (2) service stops only, 
# (30 both goods and service stops, 
# ($)or neither/only other types of stops

# create an up updated cross tabulation with row percentages of business establishments in each of the
# four categories () in each employment category


### TODO update old code from this point on...


# Load Survey Data --------------------------------------------------------
est <- read_excel('dev/Data_Processed/Survey/CMAP data for RSG 20220328 v1.0.xlsx', sheet = 1)


est_vars <- est %>% 
  select(EMP_telkey, AGEN_NAICS, AQ5, AQ6A_OPEN_A:AQ6C_OPEN_D, ExpansionWeight, NormalizedWeight) #add AQ5 to validate trip activity



# Prepare Employment Category ---------------------------------------------
est_rename <- est_vars %>% 
  mutate(EmpCatName = case_when(
    AGEN_NAICS %in% c(44,45) ~ 'Retail',
    AGEN_NAICS %in% c(22, 23) ~ 'Construction',
    AGEN_NAICS %in% c(53, 54) ~ 'Office_Professional',
    AGEN_NAICS %in% c(61,62,92) ~ 'Ed_Health_Social_Public',
    AGEN_NAICS %in% c(561, 562, 5617) ~ 'Admin_Support_Waste',
    #AGEN_NAICS == 92 ~ 'Service_Public',
    AGEN_NAICS == 72 ~ 'Service_FoodDrink',
    AGEN_NAICS == 81 ~ 'Service_Other',
    AGEN_NAICS == 42 ~ 'Wholesale',
  ))

est_rename %>% 
  group_by(EmpCatName) %>% 
  count()

# Create Trip Sums and Categorize ----------------------------------------------
#If Relevant columns are all NA, Goods or Services Trips = NA, otherwise they are summed, na.rm = T.
est_Q6_sums <- est_rename %>% 
  rowwise() %>% 
  mutate(Goods = ifelse((is.na(AQ6A_OPEN_A) & is.na(AQ6A_OPEN_B) & is.na(AQ6A_OPEN_C) & is.na(AQ6A_OPEN_D) &
                          is.na(AQ6B_OPEN_A) & is.na(AQ6B_OPEN_B) & is.na(AQ6B_OPEN_C) & is.na(AQ6B_OPEN_D)),
                        NA,
                        sum(across(AQ6A_OPEN_A:AQ6B_OPEN_D), na.rm = T)),
         Services = ifelse((is.na(AQ6C_OPEN_A) & is.na(AQ6C_OPEN_B) & is.na(AQ6C_OPEN_C) & is.na(AQ6C_OPEN_D)),
                           NA,
                           sum(across(AQ6C_OPEN_A:AQ6C_OPEN_D), na.rm = T))
         )
        
  
#Creates binary variables indicating whether they took trips, did not take trips, or had missing data for use in next step
#Same for Own vehicles to validate Q5 against Q6
est_Q6_sums <- est_Q6_sums %>% 
  mutate(Own_Vehicles = case_when(AQ5 > 0 ~ '1',
                               AQ5 ==0 ~ '0',
                               is.na(AQ5) ~ 'NA',),
         Goods_yesno = case_when(Goods >0 ~ '1',
                                 Goods ==0 ~ '0',
                                 is.na(Goods) ~ 'NA'),
         Services_yesno = case_when(Services >0 ~ '1',
                                 Services ==0 ~ '0',
                                 is.na(Services) ~ 'NA'))


#classifies firms based on binary variables above
firm_activity <- est_Q6_sums %>% 
  mutate(Activity = case_when(Goods_yesno == 1 & Services_yesno == 0 ~ 'Goods',
                              Goods_yesno == 0 & Services_yesno == 1 ~ 'Services',
                              Goods_yesno == 0 & Services_yesno == 0 ~ 'Other',
                              Goods_yesno == 1 & Services_yesno == 'NA' ~ 'Goods_NASerivces',
                              Goods_yesno == 'NA' & Services_yesno ==1 ~ 'Other',
                              Goods_yesno == 'NA' & Services_yesno == 'NA' ~ 'NoData',
                              Goods_yesno == 1 & Services_yesno == 1 ~ 'GoodsAndServices')) %>%
  mutate(Activity = case_when(Own_Vehicles == 0 & Activity == 'NoData' ~ 'Other',
                              T ~ Activity))



##to inspect observations with missing Q5 and Q6##
Missing_Q5Q6_List <- firm_activity %>% 
  filter(Own_Vehicles == 'NA' & Activity == 'NoData') %>% 
  select(EMP_telkey)
Missing_Q5Q6 <- est %>% 
  filter(EMP_telkey %in% Missing_Q5Q6_List$EMP_telkey)


#Get rid of observations with missing Q5 and Q6 data
firm_activity <- firm_activity %>% 
  filter(Own_Vehicles != 'NA' & Activity != 'NoData')

  




# For Exploring incongruent Data --------------------------------------
#Includes some logic to check for if AQ5 was >0, check if businesses said they have vehicles but didnt list any trips
#if own vehicles but didnt respond/entered no trips - 
#Checking Records where No/NA vehicles but positive number of trips
#Validating Records
#Logic Check AQ5-AQ6
firm_activity %>% 
  group_by(Own_Vehicles, Activity) %>% 
  summarise(n = n()) %>% 
  spread(Activity, n) %>% 
  select(Goods, GoodsAndServices, Services, Other)


#Exploring Cases where Q5 = 0 but trips is positive
NoVehciles_YesTrips_list <- firm_activity %>% 
  filter(Own_Vehicles == '0' & Activity == 'Services') %>% 
  select(EMP_telkey)

Extra_trips <- est %>% 
  filter(EMP_telkey %in% NoVehciles_YesTrips_list$EMP_telkey)





#Checking Records with both Missing Q5 and Q6
NATrips_NAVehcile_List <- firm_activity %>% 
  filter(Own_Vehicles == 'NA' & Activity == 'NoData') %>% 
  select(EMP_telkey)

MissingInfo <- est %>% 
  filter(EMP_telkey %in% NATrips_NAVehcile_List$EMP_telkey)
#those records with missing Q5 & Q6 are also missing a lot of other information,# 
#will remove from the final count#




#Results Summary
firm_activity_expansion <- firm_activity %>% 
  select(-(1:15)) %>% 
  filter(!is.na(Activity))

 


# Weighted Crosstabs ------------------------------------------------------
firm_activity_weighted_count <-  firm_activity_expansion %>% 
  group_by(EmpCatName, Activity) %>%
  tally(wt = ExpansionWeight) %>% 
  subset(select = c('EmpCatName', 'Activity', 'n'))%>% 
  spread(Activity, n)%>% 
  mutate(
    across(Goods:Services, ~replace_na(.x, 0))) %>% 
  mutate(
    across(Goods:Services, round, 5)) %>%
  select(EmpCatGroupedName = EmpCatName, 
         Goods, 
         GoodsAndService = GoodsAndServices,
         Other,
         Service = Services) %>%
  mutate(Total = sum(across(Goods:Service))) %>%
  mutate(across(Goods:Service, )) %>% 
  filter(!is.na(EmpCatGroupedName)) 

firm_activity_weighted_prop <- firm_activity_weighted_count
firm_activity_weighted_prop[,2:5] <- sweep(firm_activity_weighted_count[,2:5], 1, rowSums(firm_activity_weighted_count[,2:5]), FUN="/") 

firm_activity_weighted_prop <- firm_activity_weighted_prop %>% 
  mutate(Total = sum(across(Goods:Service))) 

firm_activity_weighted_prop_final <- firm_activity_weighted_prop %>%
  select(-Total) %>% 
  as.data.table()


saveRDS(firm_activity_weighted_prop_final, 'dev/Estimation/cv_activities/cv_activities_model.RDS')



# Summary Tables ----------------------------------------------------------
#Breakdown of NAICS2 Group for Activity = Other
firm_activity_expansion %>% 
  group_by(Activity) %>% 
  count(EmpCatName) %>% 
  filter(Activity == 'Other') %>% 
  as.data.table()

#Count of firms for each activity
firm_activity_expansion %>% 
  group_by(Activity) %>% 
  count() %>% 
  as.data.table()

#Raw Count of Activity for each firm by NAICS2 Groups
firm_activity_expansion %>% 
  group_by(EmpCatName, Activity) %>%
  count() %>% 
  subset(select = c('EmpCatName', 'Activity', 'n')) %>% 
  spread(Activity, n) %>% 
  filter(!is.na(EmpCatName)) %>% 
  as.data.table()
  







# # Example Code ------------------------------------------------------------
# # standardise naming in CVS processing
# # employment category should be called EmpCatName
# setnames(est, "Model_EmpCat", "EmpCatName")
# 
# est[trip[,.N,.(SITEID)],TotalStops:=i.N,on=.(SITEID)]
# est[is.na(TotalStops),TotalStops:=0]
# 
# est[,.(NAICS2 = list(sort(unique(NAICS2))),
#        NAICS3 = list(sort(unique(NAICS3)))),by=.(EmpCatName)][order(EmpCatName)]
# 
# empcat_to_empgrp <- c("e01_nrm" = "Production",
# "e02_constr" = "Industrial",
# "e03_manuf" = "Production",
# "e04_whole" = "Retail",
# "e05_retail" = "Retail",
# "e06_trans" = "Transportation",
# "e07_utility" = "Industrial",
# "e08_infor" = "Info_FIRE_Prof", #Private
# "e09_finan" = "Info_FIRE_Prof",
# "e10_pstsvc" = "Info_FIRE_Prof",
# "e11_compmgt" = "Info_FIRE_Prof",
# "e12_admsvc" = "Ed_Pub_Other_Ser", #Public Service
# "e13_edusvc" = "Ed_Pub_Other_Ser",
# "e14_medfac" = "Medical_Services",
# "e15_hospit" = "Medical_Services",
# "e16_leisure" = "Leisure", 
# "e17_othsvc" = "Ed_Pub_Other_Ser",
# "e18_pubadm" = "Ed_Pub_Other_Ser")
# 
# est[, EmpCatGroupedName := empcat_to_empgrp[EmpCatName]]
# 
# # Perform Unweighted Estimation
# # Data Prepartion
# # Assign model empcat to trips using establishment id (SITEID)
# trip[est, EmpCatName := i.EmpCatName, on = .(SITEID)]
# trip[est, EstWght := i.ESTABLISHMENT_WGHT_FCTR, on = .(SITEID)]
# 
# # Create new feature
# trip[,StopType:=ifelse(any(StopPurpose=="Goods") & any(StopPurpose=="Service"),
#                        "GoodsAndService", ifelse(any(StopPurpose=="Goods"), "Goods",
#                                                  ifelse(any(StopPurpose=="Service"),"Service","Other"))),
#      by=.(SITEID)]
# 
# # Classify firm by activity
# firm_activity = dcast(trip[,.N,.(EmpCatName, SITEID, StopPurpose ,EstWght)],
#                       EmpCatName+SITEID+EstWght~StopPurpose,
#                       value.var = "N", fill=0)
# firm_activity_unwgt = firm_activity[,.(Goods=sum(as.integer((Goods > 0) & (Service==0))),
#                                        Service=sum(as.integer((Service > 0) & (Goods == 0))),
#                                        Total=.N),
#                                     .(EmpCatName)]
# 
# firm_activity = dcast(trip[,.(EmpCatName, SITEID, StopPurpose, StopType, EstWght)], 
#                       EmpCatName+SITEID+EstWght~StopType, 
#                       value.var = "SITEID", fill=0, fun.aggregate=length)
# firm_activity[,EmpGrp:=empcat_to_empgrp[EmpCatName]]
# setkey(firm_activity, SITEID)
# 
# setkey(est, SITEID)
# est[firm_activity,c("Goods", "Service", "GoodsAndService", "Other"):=
#       .(i.Goods, i.Service, i.GoodsAndService, i.Other)]
# est[,c("Goods", "Service", "GoodsAndService", "Other"):=lapply(.SD, function(x) ifelse(is.na(x),0,x)),
#     .SDcols=c("Goods", "Service", "GoodsAndService", "Other")]
# 
# est[,TotalStops:=Goods+Service+GoodsAndService+Other]
# 
# # mpreddata = est[,.N,.(EmpCatGroupedName)]
# # mpreddata[, c("Goods", "Service", "GoodsAndService", "Other"):=NULL]
# # mpreddata[,N:=NULL]
# # mpreddata = melt(mpreddata, id.vars = "EmpCatGroupedName", variable.name = "Activity", value.name="Proportion",
# #                  variable.factor = FALSE, value.factor = FALSE)
# # 
# # ggplot(mpreddata,aes(x=EmpCatGroupedName, y=Proportion))+
# #   geom_label(data=m2preddata,aes(label=paste0(EstSize)))+
# #   geom_point(color="red")+facet_wrap(~Activity)+
# #   xlab("Employment Group")
# # ggsave("dev/Estimation/cv_activities/summaries/EmpGrpEstSizeProportion.png",
# #        width=16,height = 10)
# 
# saveRDS(dcast(mpreddata, EmpCatGroupedName~Activity, value.var = "Proportion"),
#         "dev/Estimation/cv_activities/cv_activities_model.RDS")


