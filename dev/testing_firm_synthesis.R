# Test Script Review Firm synthesis Outputs

source("./dev/init_dev.R")

# ### code for inline running
# # global environment
# for(n in ls(firm_inputs, all.names=TRUE)) assign(n, get(n, firm_inputs), environment())

### code for review of results
load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FIRMSYN_OUTPUTNAME))
RegionFirms <- firm_sim_results$RegionFirms
RegionFirms[is.na(TAZ)]


emp_control_taz <- firm_inputs$emp_control_taz
taz_se <- emp_control_taz[,.(SE = sum(Employees.SE)), keyby = .(TAZ, n2)]
taz_se[RegionFirms[, .(Firms = .N, Emp = sum(Emp)), by = .(TAZ, n2)], c("Emp", "Firms") := .(i.Emp, i.Firms), on = c("TAZ", "n2")]

taz_se[is.na(Emp) & SE > 0]

taz_se[is.na(Emp), Emp := 0] 
taz_se[is.na(SE), SE := 0] 
taz_se[is.na(Firms), Firms := 0] 

taz_se[, Diff := Emp - SE]
taz_se_n2 <- taz_se[, .(Firms = sum(Firms), Emp = sum(Emp), SE = sum(SE), Diff = sum(Diff)), keyby = n2]

n2labels <- unique(NAICS2007[,.(n2 = NAICS2, Label2)])
taz_se_n2[n2labels, Label2 := i.Label2, on = "n2"]

setcolorder(taz_se_n2, c("Label2", names(taz_se_n2)[1:5]))

taz_se_n2[, n2 := as.character(n2)]
taz_se_n2 <- add_totals(taz_se_n2, rowtotal = FALSE, idcols = 2L)

fwrite(taz_se_n2, file.path(SYSTEM_DEV_TESTING_PATH, "Testing_Firm_Synthesis_Comparison_n2.csv"))