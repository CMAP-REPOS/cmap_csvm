# CMAP CSVM
# dev script: calibration_target_firm_sim.R
# 
# Purpose:
# Import calibration data and create targets for Firm Synthesis Model
#
# Outputs:
# Calibrations targets in dev/Calibration
# calibration_targets_firm_sim.RDS
#
# In dev\Calibration\_Documentation
# Charts documenting firm synthesis calibration targets
#
# Use init_dev.R to run here instead of sourcing from _Master_Dev.R
# source("./dev/init_dev.R")

### READ IN FIRM SIM MODEL INPUTS, SCENARIO INPUTS, AND SUPPORT DATA -------------------------------------

# Use model step process inputs script and load objects to global environment
source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_process_inputs.R"))
firm_inputs <- new.env()
Establishments <- firm_sim_process_inputs(envir = firm_inputs)
for(n in ls(firm_inputs, all.names=TRUE)) assign(n, get(n, firm_inputs), environment())

### DEVELOP LIST OF TARGET TABLES  ------------------------------------------------------------------------

model_step_targets_firm_sim <- list()

### firm_sim_taz_land_use ---------------------------------------------------------------------------------

# Calibration of the firm_sim_taz_land_use step:
# Confirm that the input SE data are imported and summarized correctly
# Create from the TAZSE data scenario
# Add category labeling and aggregrate geographies

targetdt <- melt.data.table(TAZLandUseCVTM[TAZ %in% BASE_TAZ_INTERNAL],
                            id.vars = c("TAZ", "Mesozone", "CountyFIPS"), 
                            variable.name = "Category", 
                            value.name = "Target")
targetdt[, c("CatType", "Units") := .("Land Use Category", "Employees_Households")]
targetdt[TAZ_System, c("SummaryGeog") := .(i.county_state), on = "TAZ"]
targetdt[, SummaryGeogName := "CMAP County"]
targetdt[, TAZ_TYPE := ifelse(TAZ %in% BASE_TAZ_CMAP, "CMAP 7 County Area", "Rest of Model Region")]

model_step_targets_firm_sim[["firm_sim_taz_land_use"]] <- targetdt

# charts of targets for cal-val plan
# 1. HHs and Employment by County (Do all CMAP counties)
firm_sim_taz_land_use <- targetdt[Category %in% c("NEmp_Total", "HH"), 
                                  .(Target = sum(Target)), 
                                  by = .(SummaryGeog, Category)]
firm_sim_taz_land_use[, Category := ifelse(Category == "NEmp_Total", "Employment", "Househholds")]

p_firm_sim_taz_land_use <- ggplot(data = firm_sim_taz_land_use, 
                                  aes(x = factor(SummaryGeog), 
                                      y = Target, 
                                      fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("Land Use Type", values = rgb(rsgcolordf[2:3,],maxColorValue = 255)) +
  labs(title = "CMAP Household and Employment Data", 
       subtitle = paste("By County, Scenario:", SCENARIO_NAME), 
       caption = "Source: CMAP 2019 Landuse Data") +
  xlab("County") + scale_y_continuous(name = "Employment or Households", labels = scales::comma) + 
  theme(axis.ticks = element_blank(), legend.position="bottom", plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) +
  coord_flip()

ggsave(filename = file.path(SYSTEM_DEV_CALIBRATION_DOC_PATH, "firm_sim_taz_land_use.png"), 
       plot = p_firm_sim_taz_land_use, 
       width = 6.5, 
       height = 4)

# 2. HHs and Employment by Employment Group and by SEMCOG/Buffer 

firm_sim_taz_lu_cmap_region <- dcast.data.table(targetdt[,.(Target = sum(Target)), by = .(Category, TAZ_TYPE)],
                                                  Category~TAZ_TYPE, fun.aggregate = sum, value.var = "Target")
firm_sim_taz_lu_cmap_region[, Category := sub("NEmp_", "", Category)]

firm_sim_taz_lu_cmap_region[, Category := factor(Category)]
setorder(firm_sim_taz_lu_cmap_region, Category)
setnames(firm_sim_taz_lu_cmap_region, "Category", "Land Use Type")
firm_sim_taz_lu_cmap_region[, Total := `CMAP 7 County Area` + `Rest of Model Region`]
firm_sim_taz_lu_cmap_region[, `Land Use Type`:= ifelse(`Land Use Type` == "TotalEmp", "Total Employment", as.character(`Land Use Type`))]
firm_sim_taz_lu_cmap_region[, `Land Use Type`:= ifelse(`Land Use Type` == "HH", "Households", as.character(`Land Use Type`))]

fwrite(x = firm_sim_taz_lu_cmap_region,
       file = file.path(SYSTEM_DEV_CALIBRATION_DOC_PATH, "firm_sim_taz_lu_cmap_region.csv"))

### firm_sim_scale_employees  ------------------------------------------------------------------------------

# Calibration of the firm_sim_scale_employees 
# Confirm that the TAZEmployment table is matched
# Create from the TAZSE data for the scenario
# Add category labeling and aggregrate geographies

targetdt <- emp_control_taz[Zone17 %in% BASE_TAZ_INTERNAL,
                            .(TAZ = Zone17, CountyFIPS, Category = NAICS, Target = Employment)]
targetdt[, c("CatType", "Units") := .("Employment Category", "Employees")]
targetdt[UEmpCats[,.(Category = EmpCatName, EmpCatDesc)], CatLabel := i.EmpCatDesc, on = "Category"]
targetdt[TAZ_System, c("TAZ_TYPE", "SummaryGeog") := 
           .(ifelse(i.cmap == 0, "CMAP 7 County Area", "Rest of Model Region"), i.county_state), on = "TAZ"]
targetdt[, SummaryGeogName := "CMAP County"]

# Add the targets to the list

model_step_targets_firm_sim[["firm_sim_scale_employees"]] <- list(taz_emp_target = targetdt)

# charts of targets for cal-val plan
# 1. Employment by region and employment category
firm_sim_empcat_cmap <- targetdt[, .(Target = sum(Target)), by = .(Category, TAZ_TYPE)]

firm_sim_empcat_cmap[UEmpCats[,.(Category = EmpCatName, EmpCatDesc)], 
                                   EmpCatDesc := i.EmpCatDesc, 
                                   on = "Category"]

firm_sim_empcat_cmap[, EmpLabel := paste0(Category, " (", EmpCatDesc, ")")]

p_firm_sim_empcat_cmap <- ggplot(data = firm_sim_empcat_cmap, aes(EmpLabel, Target, fill = TAZ_TYPE)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual("CMAP/Rest of region", values = rgb(rsgcolordf[2:3,],maxColorValue = 255)) +
  scale_y_continuous(name = "Employment", labels = scales::comma) +
  labs(title = "CMAP Employment Data", 
       subtitle = "By Employment Category, and CMAP Region TAZ", 
       caption = "CMAP 2019 Landuse Data") +
  xlab("Employment Category") +
  theme(axis.ticks = element_blank(), legend.position="bottom") +
  coord_flip()

ggsave(filename = file.path(SYSTEM_DEV_CALIBRATION_DOC_PATH, "firm_sim_empcat_cmap.png"), 
       plot = p_firm_sim_empcat_cmap, 
       width = 8, 
       height = 8)

### SAVE THE LIST OF TARGETS --------------------------------------------------------------------------------

# Save the model step targets, a list with tables of target results for each submodel
saveRDS(model_step_targets_firm_sim, 
        file = file.path(SYSTEM_DEV_CALIBRATION_PATH, 
                         "calibration_targets_firm_sim.RDS"))
