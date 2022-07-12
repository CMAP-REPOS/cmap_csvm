# Test Script Review CV Activities outputs

source("./dev/init_dev.R")

# ### code for inline running
# # global environment
# for(n in ls(cv_inputs, all.names=TRUE)) assign(n, get(n, cv_inputs), environment())

### code for review of results
load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_CVTM_OUTPUTNAME))

### TEMP outputs being saved as cv_trips initially
firmActivities <- cv_sim_results$cv_trips
cv_activities_model <- cv_inputs$cv_activities_model

# Compare the activity distributions by employment category

# taz_se <- emp_control_taz[,.(SE = sum(Employees.SE)), keyby = .(TAZ, n2)]
# taz_se[RegionFirms[, .(Emp = sum(Emp)), by = .(TAZ, n2)], Emp := i.Emp, on = c("TAZ", "n2")]
# 
# taz_se[is.na(Emp) & SE > 0]
# 
# taz_se[is.na(Emp), Emp := 0] 
# taz_se[is.na(SE), SE := 0] 
# 
# taz_se[, Diff := Emp - SE]
# taz_se_n2 <- taz_se[, .(Emp = sum(Emp), SE = sum(SE), Diff = sum(Diff)), keyby = n2]

n2labels <- unique(NAICS2007[,.(n2 = NAICS2, Label2)])
# taz_se_n2[n2labels, Label2 := i.Label2, on = "n2"]
# setcolorder(taz_se_n2, c("Label2", names(taz_se_n2)[1:4]))

fwrite(cv_activities_test, "./dev/Testing_CV_Activities.csv")