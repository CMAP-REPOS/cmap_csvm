# CMAP CSVM
# dev script: Estimation_CV_Activities.R
# Develop a cross tabulation of establishments by the activities of their commercial vehicles

# The model application step for this model is cv_sim_activities, see cv_sim_activities.R
# Notes:
# The model selects from Goods, Service, and both Goods and Service for each business
# Segmentation should be updated to something more appropriate for the model employment categories
# CMAP model uses the NAICS2 codes, use those or sensible groupings of those codes

# # set up script
source("./dev/init_dev.R")

THIS_MODEL_PATH <- file.path("./dev/Estimation/cv_activities/")

# use the NAICS2007 table built in to rFreight package to create a labels list
n2labels <- unique(NAICS2007[,.(n2 = NAICS2, Label2)])

# Load the existing example tabulation
cv_activities_model_old <- readRDS(file.path(THIS_MODEL_PATH, "_cv_activities_model.RDS"))
# Output format needs to be the same as this, except that industry is replaced with new employment group categroization.

# Load the firm synthsis output which is the input to this model for reference
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

# Load the estimation datasets
est <- readRDS(file.path("dev", "Data_Processed", "outputs", "semcog_cvs_est.RDS"))
trip <- readRDS(file.path("dev", "Data_Processed", "outputs", "semcog_cvs_trip.RDS"))

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


