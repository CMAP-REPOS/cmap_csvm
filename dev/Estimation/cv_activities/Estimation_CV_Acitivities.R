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
cv_activities_model_old <- readRDS(file.path(THIS_MODEL_PATH, "_cv_activities_model.RDS"))
# Output format needs to be the same as this, except that industry is replaced with new employment group categroization.


## Load Output of Firm Synthesis (For Reference) ---------------------------
load(file.path("scenarios/base/outputs/1.Firms.RData"))
names(firm_sim_results)
firm_sim_results$RegionFirms
# n2 field is the NAICS 2 digit employment category
firm_sim_results$RegionFirms[,.N, keyby = n2]

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
#driver <- read_excel('dev/Data_Processed/Survey/CMAP data for RSG 20220328 v1.0.xlsx', sheet = 2)
#trips <- read_excel('dev/Data_Processed/Survey/CMAP data for RSG 20220328 v1.0.xlsx', sheet = 3)
#not joining yet, using just est for now

est_vars <- est %>% 
  select(AGEN_NAICS, AQ6A_OPEN_A:AQ6C_OPEN_D, ExpansionWeight, NormalizedWeight)
# Prepare Employment Category ---------------------------------------------
est_rename <- est_vars %>% 
  mutate(EmpCatName = case_when(
    AGEN_NAICS %in% c(44,45) ~ 'Retail',
    AGEN_NAICS %in% c(22, 23) ~ 'Construction',
    AGEN_NAICS %in% c(53, 54) ~ 'Office_Professional',
    AGEN_NAICS %in% c(61,62) ~ 'Ed_Health_SocialServices',
    AGEN_NAICS %in% c(561, 562, 5617) ~ 'Admin_Support_Waste',
    AGEN_NAICS == 92 ~ 'Service_Public',
    AGEN_NAICS == 72 ~ 'Service_FoodDrink',
    AGEN_NAICS == 81 ~ 'Service_Other',
    AGEN_NAICS == 42 ~ 'Wholesale',
  ))


# Create Sums and Categorize ----------------------------------------------
colSums(!is.na(est_rename))


est_Q6_sums <- est_rename %>% 
  rowwise() %>% 
  mutate(Goods = sum(across(AQ6A_OPEN_A:AQ6B_OPEN_D), na.rm = T),
         Services = sum(across(AQ6C_OPEN_A:AQ6C_OPEN_D), na.rm = T),
         Q6Trip_Total = (Goods + Services))

firm_activity <- est_Q6_sums %>% 
  mutate(Activity = case_when(Goods >0 & Services == 0 ~ 'Goods',
                              Goods == 0 & Services > 0 ~ 'Services',
                              Goods > 0 & Services > 0 ~ 'GoodsAndServices',
                              Goods == 0 & Services == 0 ~ 'NoData',
                              T ~ 'NA'))

firm_activity_expansion <- firm_activity %>% 
  mutate(TotalTrips_Expansion = (Q6Trip_Total * ExpansionWeight)) %>% 
  select(-(1:13))
  





# Unweighted Crosstabs (Exploratory) --------------------------------------
#Distribution of Firm Activity Type by NAICS2 Group
#Inlcude some logic to check for if AQ5 was >0, check if businesses said they have vehicles but didnt list any trips
firm_activity_expansion %>% 
  group_by(EmpCatName, Activity) %>%
  summarise(n = n()) %>%
  #mutate(prop = n/sum(n)) %>% 
  subset(select = c('EmpCatName', 'Activity', 'prop')) %>% 
  spread(Activity, prop) %>% 
  mutate(
    across(Goods:Services, ~replace_na(.x, 0))) %>% 
  mutate(
    across(Goods:Services, round, 4)) %>%
  select(EmpCatName, Goods, GoodsAndServices, Services, NoData) %>% 
  as.data.table()

#Distribution of Trips by Firm Activity Type and NAICS2 Group
firm_activity_expansion %>% 
  group_by(EmpCatName, Activity) %>%
  summarise(n = sum(Q6Trip_Total))%>%
  mutate(prop = n/sum(n)) %>% 
  subset(select = c('EmpCatName', 'Activity', 'prop')) %>% 
  spread(Activity, prop) %>% 
  mutate(
    across(Goods:Services, ~replace_na(.x, 0))) %>% 
  mutate(
    across(Goods:Services, round, 4)) %>%
  select(EmpCatName, Goods, GoodsAndServices, Services, NoData) %>% 
  as.data.table()

#Number of firms by Firm Activity
firm_activity_expansion %>% 
  group_by(Activity) %>% 
  count()

#Breakdown of NAICS2 Employer Category for Firms with Insufficient Trips Count Data  
firm_activity_expansion %>% 
  group_by(Activity) %>% 
  count(EmpCatName) %>% 
  filter(Activity == 'NoData')
 


# Weighted Crosstabs ------------------------------------------------------
FirmActivity_Type_weight <- firm_activity_expansion %>% 
  group_by(EmpCatName, Activity) %>%
  tally(wt = ExpansionWeight) %>% 
  mutate(prop = n/sum(n)) %>% 
  subset(select = c('EmpCatName', 'Activity', 'prop')) %>% 
  spread(Activity, prop) %>% 
  mutate(
    across(Goods:Services, ~replace_na(.x, 0))) %>% 
  mutate(
    across(Goods:Services, round, 4)) %>%
  select(EmpCatName, Goods, GoodsAndServices, Services, NoData) %>% 
  as.data.table()
  
FirmActivity_Trips_weight <- firm_activity_expansion %>% 
  group_by(EmpCatName, Activity) %>%
  summarise(n = sum(TotalTrips_Expansion))%>%
  mutate(prop = n/sum(n)) %>% 
  subset(select = c('EmpCatName', 'Activity', 'prop')) %>% 
  spread(Activity, prop) %>% 
  mutate(
    across(Goods:Services, ~replace_na(.x, 0))) %>% 
  mutate(
    across(Goods:Services, round, 4)) %>%
  select(EmpCatName, Goods, GoodsAndServices, Services, NoData) %>% 
  as.data.table()















# Example Code ------------------------------------------------------------
# standardise naming in CVS processing
# employment category should be called EmpCatName
setnames(est, "Model_EmpCat", "EmpCatName")

est[trip[,.N,.(SITEID)],TotalStops:=i.N,on=.(SITEID)]
est[is.na(TotalStops),TotalStops:=0]

est[,.(NAICS2 = list(sort(unique(NAICS2))),
       NAICS3 = list(sort(unique(NAICS3)))),by=.(EmpCatName)][order(EmpCatName)]

empcat_to_empgrp <- c("e01_nrm" = "Production",
"e02_constr" = "Industrial",
"e03_manuf" = "Production",
"e04_whole" = "Retail",
"e05_retail" = "Retail",
"e06_trans" = "Transportation",
"e07_utility" = "Industrial",
"e08_infor" = "Info_FIRE_Prof", #Private
"e09_finan" = "Info_FIRE_Prof",
"e10_pstsvc" = "Info_FIRE_Prof",
"e11_compmgt" = "Info_FIRE_Prof",
"e12_admsvc" = "Ed_Pub_Other_Ser", #Public Service
"e13_edusvc" = "Ed_Pub_Other_Ser",
"e14_medfac" = "Medical_Services",
"e15_hospit" = "Medical_Services",
"e16_leisure" = "Leisure", 
"e17_othsvc" = "Ed_Pub_Other_Ser",
"e18_pubadm" = "Ed_Pub_Other_Ser")

est[, EmpCatGroupedName := empcat_to_empgrp[EmpCatName]]

# Perform Unweighted Estimation
# Data Prepartion
# Assign model empcat to trips using establishment id (SITEID)
trip[est, EmpCatName := i.EmpCatName, on = .(SITEID)]
trip[est, EstWght := i.ESTABLISHMENT_WGHT_FCTR, on = .(SITEID)]

# Create new feature
trip[,StopType:=ifelse(any(StopPurpose=="Goods") & any(StopPurpose=="Service"),
                       "GoodsAndService", ifelse(any(StopPurpose=="Goods"), "Goods",
                                                 ifelse(any(StopPurpose=="Service"),"Service","Other"))),
     by=.(SITEID)]

# Classify firm by activity
firm_activity = dcast(trip[,.N,.(EmpCatName, SITEID, StopPurpose ,EstWght)],
                      EmpCatName+SITEID+EstWght~StopPurpose,
                      value.var = "N", fill=0)
firm_activity_unwgt = firm_activity[,.(Goods=sum(as.integer((Goods > 0) & (Service==0))),
                                       Service=sum(as.integer((Service > 0) & (Goods == 0))),
                                       Total=.N),
                                    .(EmpCatName)]

firm_activity = dcast(trip[,.(EmpCatName, SITEID, StopPurpose, StopType, EstWght)], 
                      EmpCatName+SITEID+EstWght~StopType, 
                      value.var = "SITEID", fill=0, fun.aggregate=length)
firm_activity[,EmpGrp:=empcat_to_empgrp[EmpCatName]]
setkey(firm_activity, SITEID)

setkey(est, SITEID)
est[firm_activity,c("Goods", "Service", "GoodsAndService", "Other"):=
      .(i.Goods, i.Service, i.GoodsAndService, i.Other)]
est[,c("Goods", "Service", "GoodsAndService", "Other"):=lapply(.SD, function(x) ifelse(is.na(x),0,x)),
    .SDcols=c("Goods", "Service", "GoodsAndService", "Other")]

est[,TotalStops:=Goods+Service+GoodsAndService+Other]

# mpreddata = est[,.N,.(EmpCatGroupedName)]
# mpreddata[, c("Goods", "Service", "GoodsAndService", "Other"):=NULL]
# mpreddata[,N:=NULL]
# mpreddata = melt(mpreddata, id.vars = "EmpCatGroupedName", variable.name = "Activity", value.name="Proportion",
#                  variable.factor = FALSE, value.factor = FALSE)
# 
# ggplot(mpreddata,aes(x=EmpCatGroupedName, y=Proportion))+
#   geom_label(data=m2preddata,aes(label=paste0(EstSize)))+
#   geom_point(color="red")+facet_wrap(~Activity)+
#   xlab("Employment Group")
# ggsave("dev/Estimation/cv_activities/summaries/EmpGrpEstSizeProportion.png",
#        width=16,height = 10)

saveRDS(dcast(mpreddata, EmpCatGroupedName~Activity, value.var = "Proportion"),
        "dev/Estimation/cv_activities/cv_activities_model.RDS")


