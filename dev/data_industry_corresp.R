# CMAP CVTM
# dev script: data_industry_corresp.R
#
# Purpose:
# Create correspondences between employment categories used in the model
#
# Outputs:
# Model inputs in lib\data:
# NAICS2_to_EmpCats.csv
#
# use init_dev.R to run here instead of sourcing from _Master_Dev.R
source("./dev/init_dev.R")

### READ INPUT FILES ==================================================

emp_control <- fread("./scenarios/base/inputs/data_emp_control_2017.csv")

### CREATE COMPLETE CORRESPONDENCES AT NAICS 2 LEVEL ==============

n2labels <- unique(NAICS2007[,.(n2 = NAICS2, Label2)])[order(n2)]
n2labels <- n2labels[n2 %in% unique(emp_control$NAICS)]
n2labels[, EmpCatID := .I]

setnames(n2labels, 
         c("n2", "Label2"),
         c("EmpCatName", "EmpCatDesc"))
setcolorder(n2labels, c("EmpCatID", "EmpCatName", "EmpCatDesc"))

### ADD EXTRA CORRESPODENCE TO MATCH THE CVTM MODEL CATS =======================

# These categories are used in several of the estimated models
n2labels[, EmpCatGroupedName := case_when(
  EmpCatName %in% c(44,45) ~ 'Retail',
  EmpCatName %in% c(22, 23) ~ 'Construction',
  EmpCatName %in% c(51, 52, 53, 54, 55) ~ 'Office_Professional',
  EmpCatName %in% c(61,62) ~ 'Ed_Health_SocialServices',
  EmpCatName == c(56) ~ 'Admin_Support_Waste',
  EmpCatName == 92 ~ 'Service_Public',
  EmpCatName %in% c(71, 72) ~ 'Service_FoodDrink',
  EmpCatName == 81 ~ 'Service_Other',
  EmpCatName == 42 ~ 'Wholesale',
  EmpCatName %in% c(11, 21, 31, 32, 33, 48, 49) ~ 'Transport_Industry')]


### SAVE FINAL CORRESPONDENCE ===================================

# write the file to lib/data in the application
fwrite(n2labels, file.path("lib", 
                                "data", 
                                "corresp_naics2_empcats.csv"))

