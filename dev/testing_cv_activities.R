# Test Script Review CV Activities outputs

source("./dev/init_dev.R")

# ### code for inline running
# # global environment
# for(n in ls(cv_inputs, all.names=TRUE)) assign(n, get(n, cv_inputs), environment())

### code for review of results
load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_CVTM_OUTPUTNAME))
load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FIRMSYN_OUTPUTNAME))

### TEMP outputs being saved as cv_trips initially
firmActivities <- cv_sim_results$cv_trips
cv_activities_model <- cv_inputs$cv_activities_model

ScenarioFirms <- firm_sim_results[["RegionFirms"]][!is.na(TAZ)]

# Compare the activity distributions by employment category
firmActivities <- dcast.data.table(firmActivities,
                                   BusID ~ Activity,
                                   fun.aggregate = length)
firmActivities[, GoodsAndService := ifelse(Goods == 1 & Service == 1, 1, 0)]
firmActivities[, Goods := ifelse(GoodsAndService == 1, 0, Goods)]
firmActivities[, Service := ifelse(GoodsAndService == 1, 0, Service)]

# Join back to the scenario firms table to see how many firms did not get any activities
ScenarioFirms[firmActivities, c("Goods", "Service", "GoodsAndService") := .(i.Goods, i.Service, i.GoodsAndService)]
ScenarioFirms[is.na(Goods), c("Goods", "Service", "GoodsAndService") := 0]
ScenarioFirms[, Other := ifelse(Goods + Service + GoodsAndService == 0, 1, 0)]

# Group by employment category
activities_test <- add_totals(ScenarioFirms[,.(GoodsMod = sum(Goods), 
                                               ServiceMod = sum(Service), 
                                               GoodsAndServiceMod = sum(GoodsAndService), 
                                               OtherMod = sum(Other)),
              keyby = EmpCatGroupedName], coltotal = FALSE)

# calculate difference between the application outputs and the inputs model distribution
activities_test[, c("GoodsPct", "ServicePct", "GoodsAndServicePct", "OtherPct") := 
                  .(GoodsMod/Total, ServiceMod/Total, GoodsAndServiceMod/Total, OtherMod/Total)]

activities_test[cv_activities_model, c("Goods", "Service", "GoodsAndService", "Other") := 
                  .(i.Goods, i.Service, i.GoodsAndService, i.Other)]  
activities_test[is.na(Goods), c("Goods", "Service", "GoodsAndService", "Other") := 0]

activities_test[, c("GoodsDiff", "ServiceDiff", "GoodsAndServiceDiff", "OtherDiff") := 
                  .(GoodsPct - Goods, ServicePct - Service,
                    GoodsAndServicePct - GoodsAndService, OtherPct - Other)]

# any deviation of more than 1%?
activities_test[abs(GoodsDiff) > 0.01 | abs(ServiceDiff) > 0.01 | abs(GoodsAndServiceDiff) > 0.01 | abs(OtherDiff) > 0.01]

fwrite(activities_test, "./dev/Testing_CV_Activities.csv")