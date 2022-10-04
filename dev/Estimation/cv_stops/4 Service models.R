library(data.table)
library(pscl)
library(splines)
library(glmmTMB)
library(foreach)
library(doParallel)
library(DHARMa)

model_loc = 'dev/Estimation/cv_stops/'

##########################
# Estimate Models

# Load data
load(file.path(model_loc, "Stop_Counts_Service.RData"))
source(file.path(model_loc, "0 helper functions.R"))

# Code employment size category
service_stop_counts[,Size:=cut(TOTAL_EMPLOYEES, c(0, 5, 15, 25, 50, 100, Inf), 
                            labels = c("1-5", "5-15", "15-25", "25-50", "50-100", "100+"))]

service_stop_counts[,ttime:=cut(time, c(0, 5, 15, 30, 60, 90, 120, Inf),
                             labels = c("0-5", "5-15", "15-30", "30-60", "60-90", "90-120", "120p"))]


IndustryCat = dcast.data.table(service_stop_counts,SITEID~IndustryCat,value.var = "IndustryCat", 
                    fun.aggregate = function(x) ifelse(length(x)>0,1,0))

service_stop_counts = IndustryCat[service_stop_counts, on=.(SITEID)]

service_stop_counts[, size_001_005 := 1 * (TOTAL_EMPLOYEES >= 1   & TOTAL_EMPLOYEES < 5)]
service_stop_counts[, size_005_015 := 1 * (TOTAL_EMPLOYEES >= 5   & TOTAL_EMPLOYEES < 15)]
service_stop_counts[, size_015_025 := 1 * (TOTAL_EMPLOYEES >= 15  & TOTAL_EMPLOYEES < 25)]
service_stop_counts[, size_025_050 := 1 * (TOTAL_EMPLOYEES >= 25  & TOTAL_EMPLOYEES < 50)]
service_stop_counts[, size_050_100 := 1 * (TOTAL_EMPLOYEES >= 50  & TOTAL_EMPLOYEES < 100)]
service_stop_counts[, size_100_250 := 1 * (TOTAL_EMPLOYEES >= 100 & TOTAL_EMPLOYEES < 250)]
service_stop_counts[, size_250_p    := 1 * (TOTAL_EMPLOYEES >= 250)]


service_stop_counts[, dist_00_02 := 1 * (dist >= 0  & dist < 2)]
service_stop_counts[, dist_02_05 := 1 * (dist >= 2  & dist < 5)]
service_stop_counts[, dist_05_10 := 1 * (dist >= 5  & dist < 10)]
service_stop_counts[, dist_10_20 := 1 * (dist >= 10 & dist < 20)]
service_stop_counts[, dist_20_40 := 1 * (dist >= 20 & dist < 40)]
service_stop_counts[, dist_40_p  := 1 * (dist >= 40)]

service_stop_counts[, time_000_005 := 1 * (time >= 0   & time < 5)]
service_stop_counts[, time_005_015 := 1 * (time >= 5   & time < 15)]
service_stop_counts[, time_015_030 := 1 * (time >= 15  & time < 30)]
service_stop_counts[, time_030_060 := 1 * (time >= 30  & time < 60)]
service_stop_counts[, time_060_090 := 1 * (time >= 60  & time < 90)]
service_stop_counts[, time_090_120 := 1 * (time >= 90  & time < 120)]
service_stop_counts[, time_120_p   := 1 * (time >= 120)]

service_stop_counts[, size_001_005 := 1 * (TOTAL_EMPLOYEES >= 1   & TOTAL_EMPLOYEES < 5)]
service_stop_counts[, size_005_015 := 1 * (TOTAL_EMPLOYEES >= 5   & TOTAL_EMPLOYEES < 15)]
service_stop_counts[, size_015_025 := 1 * (TOTAL_EMPLOYEES >= 15  & TOTAL_EMPLOYEES < 25)]
service_stop_counts[, size_025_050 := 1 * (TOTAL_EMPLOYEES >= 25  & TOTAL_EMPLOYEES < 50)]
service_stop_counts[, size_050_100 := 1 * (TOTAL_EMPLOYEES >= 50  & TOTAL_EMPLOYEES < 100)]
service_stop_counts[, size_100_250 := 1 * (TOTAL_EMPLOYEES >= 100 & TOTAL_EMPLOYEES < 250)]
service_stop_counts[, size_250_p    := 1 * (TOTAL_EMPLOYEES >= 250)]

service_stop_counts[, SITE_TAZIDF:=factor(SITE_TAZID)]

# # Check service_stop_counts
# service_stop_counts[,.N, keyby = STOPS][,NSTOPS := N*STOPS][]
# sum(service_stop_counts[,.N, keyby = STOPS][,NSTOPS := N*STOPS]$NSTOPS)
# sum(service_stop_counts$WEIGHTED_STOPS)
# length(unique(service_stop_counts$SITEID))
# service_stop_counts[,.(STOPS = sum(STOPS)), by = SITEID][,.N, keyby = STOPS]

# Calculate null log-likelihood
hurdle_ll <- sum(log(0.5)*nrow(service_stop_counts))
count_ll <- sum(dpois(service_stop_counts[, STOPS], mean(service_stop_counts[, STOPS]), log = TRUE))
hurdle_ll + count_ll

##########################
# Estimate Service Models

# ### Block 1
# # Model 1
# myFormula <- STOPS ~ I(HH/1000) | 1
# fit <- crossvalidate(myFormula, service_stop_counts)
# results <- appendResults(fit)
# 
# # Model 2
# myFormula <- STOPS ~ 1 | I(HH/1000)
# fit <- crossvalidate(myFormula, service_stop_counts)
# results <- appendResults(fit, results)
# 
# # Model 3
# myFormula <- STOPS ~ I(HH/1000)
# fit <- crossvalidate(myFormula, service_stop_counts)
# results <- appendResults(fit, results)
# 
# # Model 4
# myFormula <- STOPS ~ I(HH/1000) + I(TOTAL_EMPLOYEES/1000) | I(HH/1000)
# fit <- crossvalidate(myFormula, service_stop_counts)
# results <- appendResults(fit, results)
# 
# # Model 4 glmmtmb
# myFormula <- STOPS ~ I(HH/1000) + I(TOTAL_EMPLOYEES/1000)
# myZIFormula <- ~ I(HH/1000)
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, data = service_stop_counts,
#                              family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# # Model 5 glmmtmb
# myFormula <- STOPS ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time)
# myZIFormula <- ~log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Office) + log(dist)
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = service_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# # Model 6 glmmtmb
# myFormula <- STOPS ~ log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time) + log(TOTAL_EMPLOYEES)
# myZIFormula <- ~log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Office) + log(dist) + Industrial + Office + Retail + 
#   `Medical Services`
# 
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = service_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# # Model 7 glmmtmb
# myFormula <- STOPS ~ log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time) + log(TOTAL_EMPLOYEES)
# myZIFormula <- ~log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Office) + log(dist) + Industrial + Office + Retail + 
#   `Medical Services` + log(TOTAL_EMPLOYEES) 
# 
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = service_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# # Model 8 glmmtmb
# myFormula <- STOPS ~ log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time) + log(TOTAL_EMPLOYEES) + `Medical Services` + 
#   Education
# myZIFormula <- ~log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Office) + log(dist) + Industrial + Office + Retail + 
#   `Medical Services` + log(TOTAL_EMPLOYEES) 
# 
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = service_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# # Model 9 glmmtmb
# myFormula <- STOPS ~ log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time) + log(TOTAL_EMPLOYEES) + `Medical Services` + 
#   Education + log(time):`Medical Services` + log(time):Education
# myZIFormula <- ~log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Office) + log(dist) + Industrial + Office + Retail + 
#   `Medical Services` + log(TOTAL_EMPLOYEES) 
# 
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = service_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# # Model 10 glmmtmb
# myFormula <- STOPS ~ log1p(NEmp_Industrial) + log1p(NEmp_Production) + log1p(NEmp_Transportation) + 
#   log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time) + log(TOTAL_EMPLOYEES) + Medical_Services + 
#   Ed_Pub_Other_Ser + log(time):Medical_Services + log(time):Ed_Pub_Other_Ser + (1 | SITE_TAZID)
# myZIFormula <- ~log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Office) + log(dist) + Industrial + Office + Retail + 
#   `Medical Services` + log(TOTAL_EMPLOYEES) 
# 
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = service_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# # Model 11 glmmtmb
# myFormula <- STOPS ~ log1p(NEmp_Industrial) + log1p(NEmp_Production) + log1p(NEmp_Transportation) + 
#   log1p(NEmp_Medical_Services) +
#   log1p(NEmp_Retail) + log(time) + log(TOTAL_EMPLOYEES) + Medical_Services +
#   Ed_Pub_Other_Ser + log(time):Medical_Services + log(time):Ed_Pub_Other_Ser + (1 | SITE_TAZID) +
#     log1p(NEmp_Leisure) + Leisure + log(time):Leisure
# myZIFormula <- ~log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Production) + log1p(NEmp_Transportation) + 
#   log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Info_FIRE_Prof) + log1p(NEmp_Leisure) + log(dist) + Industrial + Production +
#   Transportation + Leisure + Info_FIRE_Prof + Retail + 
#   Medical_Services + log(TOTAL_EMPLOYEES) + (1 | SITE_TAZID)
# 
# NEmp_vars = grep("NEmp", names(service_stop_counts), value = TRUE)
# NEmp_vars = NEmp_vars[-9]
# Industry_Vars = unique(service_stop_counts$IndustryCat)
# Industry_Vars = Industry_Vars[Industry_Vars!="Production"]
# 
# myFormula2 = as.formula(paste0("STOPS ~ log1p(HH) +", paste0(paste0(" log1p(", NEmp_vars,") "),
#                                                   collapse = "+"),
#                                "+",
#                                paste0(paste0(" ", Industry_Vars, " "),
#                                       collapse = "+"),
#                                "+ log(time) + log(TOTAL_EMPLOYEES) +",
#                                paste0(paste0(" log(time):", Industry_Vars," "),
#                                       collapse = "+"),
#                                "+",
#                                paste0(paste0(" log(TOTAL_EMPLOYEES):", Industry_Vars," "),
#                                       collapse = "+"),
#                                "+ (1|SITE_TAZID)"))
# 
# 
# myFormula2 = as.formula(paste0("STOPS ~ log1p(HH) +", paste0(paste0(" log1p(", NEmp_vars,") "),
#                                                   collapse = "+"),
#                                "+",
#                                paste0(paste0(" ", Industry_Vars, " "),
#                                       collapse = "+"),
#                                "+ log(time) + log(TOTAL_EMPLOYEES) +",
#                                paste0(paste0(" log(time):", Industry_Vars," "),
#                                       collapse = "+"),
#                                "+",
#                                paste0(paste0(" log(TOTAL_EMPLOYEES):", Industry_Vars," "),
#                                       collapse = "+"),
#                                "+ (1|SITE_TAZID:SITEID)"))
# 
# myFormula2 = as.formula(paste0("STOPS ~ log1p(HH) +", paste0(paste0(" log1p(", NEmp_vars,") "),
#                                                   collapse = "+"),
#                                "+",
#                                paste0(paste0(" ", Industry_Vars, " "),
#                                       collapse = "+"),
#                                "+ log(time) + log(TOTAL_EMPLOYEES) +",
#                                paste0(paste0(" log(time):", Industry_Vars," "),
#                                       collapse = "+"),
#                                "+",
#                                paste0(paste0(" log(TOTAL_EMPLOYEES):", Industry_Vars," "),
#                                       collapse = "+"),
#                                "+ (1|SITE_TAZID/SITEID)"))
# 
# 
# 
# myZIFormula2 = as.formula(paste0("STOPF ~ log1p(HH) +", paste0(paste0(" log1p(", NEmp_vars,") "),
#                                                              collapse = "+"),
#                                "+",
#                                paste0(paste0(" ", Industry_Vars, " "),
#                                       collapse = "+"),
#                                "+ log(time) + log(TOTAL_EMPLOYEES) +",
#                                paste0(paste0(" log(time):", Industry_Vars," "),
#                                       collapse = "+"),
#                                "+",
#                                paste0(paste0(" log(TOTAL_EMPLOYEES):", Industry_Vars," "),
#                                       collapse = "+"),
#                                "+ (1|SITE_TAZID/SITEID)"))
# 
# 
# fitall <- glmmTMB(myFormula2,# ziformula = myZIFormula, 
#                data = service_stop_counts[STOPS>0],
#                family = truncated_poisson(link = "log"))
# 
# fitdrop = drop1(fitall, trace = 0)
# dropvar = attributes(fitdrop)$row.names[which.min(fitdrop$AIC)]
# cat("Dropping variable: ", dropvar, "\n")
# fitdropm = update(fitall, as.formula(paste0("~.-",dropvar)))
# continue = TRUE
# 
# while(continue){
#   fitdrop = drop1(fitdropm, trace = 0)
#   dropvar = attributes(fitdrop)$row.names[which.min(fitdrop$AIC)]
#   if(dropvar != "<none>"){
#     cat("Dropping variable: ", dropvar, "\n")
#     fitdropm = update(fitdropm, as.formula(paste0("~.-",dropvar)))
#   } else {
#     continue = FALSE
#   }
# }
# service_stop_counts[,STOPF:=STOPS>0]
# 
# fitziall = glmmTMB(myZIFormula2,# ziformula = myZIFormula, 
#                    data = service_stop_counts,
#                    family = binomial(link = "logit"),
#                    verbose = FALSE)
# 
# 
# fitdrop = drop1(fitziall, trace = 0)
# dropvar = attributes(fitdrop)$row.names[which.min(fitdrop$AIC)]
# fitdropm = update(fitziall, as.formula(paste0("~.-",dropvar)))
# continue = TRUE
# 
# while(continue){
#   fitdrop = drop1(fitdropm, trace = 0)
#   dropvar = attributes(fitdrop)$row.names[which.min(fitdrop$AIC)]
#   if(dropvar != "<none>"){
#     cat("Dropping variable: ", dropvar, "\n")
#     fitdropm = update(fitdropm, as.formula(paste0("~.-",dropvar)))
#   } else {
#     continue = FALSE
#   }
# }

# finalformula = STOPS ~ log1p(NEmp_Retail) + log1p(NEmp_Transportation) + log1p(NEmp_Ed_Pub_Other_Ser) + 
#   Industrial + Transportation + Medical_Services + log(time) + Industrial:log(time) + 
#   Transportation:log(time) + Medical_Services:log(time) + (1 | SITE_TAZID/SITEID)
# 
# finalziformula = ~ log1p(HH) + log1p(NEmp_Production) + 
#   log1p(NEmp_Retail) + 
#   log1p(NEmp_Info_FIRE_Prof) + log1p(NEmp_Medical_Services) + 
#   Ed_Pub_Other_Ser + Retail + Industrial + Transportation + 
#   Medical_Services + Leisure + Info_FIRE_Prof + log(time) + 
#   log(TOTAL_EMPLOYEES) + Ed_Pub_Other_Ser:log(TOTAL_EMPLOYEES) + 
#   Leisure:log(TOTAL_EMPLOYEES) + Info_FIRE_Prof:log(TOTAL_EMPLOYEES) + 
#   (1 | SITE_TAZID/SITEID)
# 
# 
# 
# final_model = glmmTMB(finalformula, 
#                       ziformula = ~ log1p(HH) + log1p(NEmp_Production) + 
#                         log1p(NEmp_Retail) + 
#                         log1p(NEmp_Info_FIRE_Prof) + log1p(NEmp_Medical_Services) + 
#                         Ed_Pub_Other_Ser + Retail + Industrial + Transportation + 
#                         Medical_Services + Leisure + Info_FIRE_Prof + log(time) + 
#                         log(TOTAL_EMPLOYEES) + Ed_Pub_Other_Ser:log(TOTAL_EMPLOYEES) + 
#                         Leisure:log(TOTAL_EMPLOYEES) + Info_FIRE_Prof:log(TOTAL_EMPLOYEES) + 
#                         (1 | SITE_TAZID/SITEID),
#                       data = service_stop_counts,
#                       family = truncated_poisson(link = "log"),
#                       verbose = TRUE,
#                       REML=TRUE)
# 
# # Model diagnostics for count model
# service_stops_filtered_count = service_stop_counts[STOPS > 0]
# fit_trunc <- glmmTMB(finalformula,
#                      data = service_stops_filtered_count,
#                      family = truncated_poisson(link = "log"),
#                      verbose = TRUE,
#                      REML = FALSE)
# 
# # Perform some simulated residual checks
# simres_service_model = simulateResiduals(final_model)
# simres_trunc_service_model = simulateResiduals(fit_trunc)
# 
# 
# # Plot residual plot
# png("final_models/service/Service Model Residual.png", width = 1000, height = 500)
# # plotResiduals(simres_service_model)
# plotResiduals(simres_trunc_service_model)
# dev.off()
# 
# # Plot dispersion test
# png("final_models/service/Service Model Dispersion.png", width = 1000, height = 500)
# testDispersion(simres_service_model)
# dev.off()
# 
# # Tests count
# png("final_models/service/Service Model Zero Count.png", width = 1000, height = 500)
# testGeneric(simres_service_model, function(x) sum(x==0))
# dev.off()
# 
# # Tests count 1
# png("final_models/service/Service Model One Count.png", width = 1000, height = 500)
# testGeneric(simres_service_model, function(x) sum(x==1))
# dev.off()
# 
# # Tests count 2
# png("final_models/service/Service Model Two Count.png", width = 1000, height = 500)
# testGeneric(simres_service_model, function(x) sum(x==2))
# dev.off()
# 
# # Tests count 3
# png("final_models/service/Service Model Three Count.png", width = 1000, height = 500)
# testGeneric(simres_service_model, function(x) sum(x==3))
# dev.off()
# 
# # Tests count >=4
# png("final_models/service/Service Model Four Plus Count.png", width = 1000, height = 500)
# testGeneric(simres_service_model, function(x) sum(x>=4))
# dev.off()
# 
# # Tests uniformity
# png("final_models/service/Service Model Uniformity Test.png", width = 1000, height = 500)
# testUniformity(simres_service_model)
# dev.off()
# 
# ### Final Model
# myFormula <- STOPS ~ log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time) + log(TOTAL_EMPLOYEES) + `Medical Services` + 
#   Education + log(time):`Medical Services` + log(time):Education | log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Office) + log(dist) + Industrial + Office + Retail + 
#   `Medical Services` + log(TOTAL_EMPLOYEES)

# myFormula <- STOPS ~ log1p(NEmp_Retail) + log1p(NEmp_Transportation) + log1p(NEmp_Ed_Pub_Other_Ser) + 
#   Industrial + Transportation + Medical_Services + log(time) + 
#   Industrial:log(time) + Transportation:log(time) + Medical_Services:log(time) |
#   log1p(HH) + log1p(NEmp_Production) + log1p(NEmp_Retail) + 
#   log1p(NEmp_Info_FIRE_Prof) + log1p(NEmp_Medical_Services) + 
#   Ed_Pub_Other_Ser + Retail + 
#   Industrial + Transportation + 
#   Medical_Services + Leisure + Info_FIRE_Prof + log(time) + 
#   log(TOTAL_EMPLOYEES) + Ed_Pub_Other_Ser:log(TOTAL_EMPLOYEES) + 
#   Leisure:log(TOTAL_EMPLOYEES) + Info_FIRE_Prof:log(TOTAL_EMPLOYEES)


#need to include wholsale
#some categories from semcog now are composed of several categories now in other employment groups
myFormula <- STOPS ~ log1p(NEmp_Retail) + log1p(NEmp_Wholesale)+ log1p(NEmp_Transport_Industry) + 
  log1p(NEmp_Service_Other) + log1p(NEmp_Service_Public) + 
  log1p(NEmp_Admin_Support_Waste) +
  Construction + Transport_Industry + Ed_Health_SocialServices + log(time) + 
  Construction:log(time) + Transport_Industry:log(time) + Ed_Health_SocialServices:log(time) |
  log1p(HH) + log1p(NEmp_Transport_Industry) + log1p(NEmp_Retail) + log1p(NEmp_Wholesale) +
  log1p(NEmp_Office_Professional) + log1p(NEmp_Ed_Health_SocialServices) + 
  Admin_Support_Waste + 
  Service_Other + 
  #Retail + removed to prevent errors resulting from overspecification,  decision informed by summary of employment category trip rates
  Wholesale + 
  Construction + 
  Transport_Industry + 
  Ed_Health_SocialServices + 
  Service_FoodDrink + 
  Office_Professional + 
  log(time) + log(TOTAL_EMPLOYEES) + 
  Admin_Support_Waste:log(TOTAL_EMPLOYEES) + Service_Other:log(TOTAL_EMPLOYEES) + 
  Service_FoodDrink:log(TOTAL_EMPLOYEES) + Office_Professional:log(TOTAL_EMPLOYEES)
  

service.fit <- hurdle(formula = myFormula, data = service_stop_counts,
                      dist="poisson",
                      zero.dist="binomial",
                      link="logit")
summary(service.fit)
service.fit.copy <- service.fit

# Calibration trick: estimate with separate time parameters but use the shared value from the previous model
# Copy population level estimate to hurdle model for prediction
# coef_names <- names(service.fit$coefficients$zero)
# zero_par <- length(service.fit$coefficients$zero)
# service.fit$coefficients$zero <- -final_model$fit$par[(seq_len(zero_par))]
# names(service.fit$coefficients$zero) <- coef_names
# 
# coef_names <- names(service.fit$coefficients$count)
# count_par <- length(service.fit$coefficients$count)
# service.fit$coefficients$count <- final_model$fit$par[(count_par + seq_len(count_par))]
# names(service.fit$coefficients$count) <- coef_names
# Only works when passing newdata to predict function.

summary(service.fit)

# Trim all the junk from the hurdle model object to save space
# residuals, model, weights, fitted.values
finalModel <- service.fit
finalModel$residuals <- NULL
finalModel$fitted.values <- NULL
finalModel$model <- NULL
finalModel$weights <- NULL
attr(finalModel$terms$count, ".Environment") <- NULL
attr(finalModel$terms$zero, ".Environment") <- NULL
attr(finalModel$terms$full, ".Environment") <- NULL

# Save final model
saveRDS(finalModel, file = file.path(model_loc, "new_models/services/cv_service_model.RDS"))

# Write results to csv
options(width = 10000)
sink(file = file.path(model_loc, "final_models/services/Service Model Results.txt"))
print(summary(service.fit.copy))
print(summary(final_model))
sink()
options(width = 137)
