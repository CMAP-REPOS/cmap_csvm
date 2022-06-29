# Test Script Review Firm synthesis Outputs

### code for inline running
# global environment
for(n in ls(firm_inputs, all.names=TRUE)) assign(n, get(n, firm_inputs), environment())



### code for review of results
load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FIRMSYN_OUTPUTNAME))
RegionFirms <- firm_sim_results$RegionFirms
RegionFirms[is.na(TAZ)]


emp_control_taz <- firm_inputs$emp_control_taz
taz_se <- emp_control_taz[,.(SE = sum(Employees.SE)), keyby = .(TAZ, n2)]
taz_se[RegionFirms[, .(Emp = sum(Emp)), by = .(TAZ, n2)], Emp := i.Emp, on = c("TAZ", "n2")]

taz_se[is.na(Emp) & SE > 0]

taz_se[is.na(Emo)] <- 0