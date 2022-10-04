library(data.table)
library(pscl)
library(ggplot2)
# library(countreg)
library(splines)
library(glmmTMB)
library(foreach)
library(doParallel)
library(DHARMa)

setwd("dev/Estimation/cv_stops/")

##########################
# Estimate Models

# Load data
load("Stop_Counts_Goods_test.RData")
# load("Counts_Meeting.RData")
# load("Counts_Service.RData")
source("0 helper functions.R")

# Code employment size category
good_stop_counts[,Size:=cut(TOTAL_EMPLOYEES, c(0, 5, 15, 25, 50, 100, Inf), 
                           labels = c("1-5", "5-15", "15-25", "25-50", "50-100", "100+"))]

good_stop_counts[,ttime:=cut(time, c(0, 5, 15, 30, 60, 90, 120, Inf),
                            labels = c("0-5", "5-15", "15-30", "30-60", "60-90", "90-120", "120p"))]


IndustryCat = dcast.data.table(good_stop_counts,SITEID~IndustryCat,value.var = "IndustryCat", 
                    fun.aggregate = function(x) ifelse(length(x)>0,1,0))

good_stop_counts = IndustryCat[good_stop_counts, on = .(SITEID)]

good_stop_counts[, size_001_005 := 1 * (TOTAL_EMPLOYEES >= 1   & TOTAL_EMPLOYEES < 5)]
good_stop_counts[, size_005_015 := 1 * (TOTAL_EMPLOYEES >= 5   & TOTAL_EMPLOYEES < 15)]
good_stop_counts[, size_015_025 := 1 * (TOTAL_EMPLOYEES >= 15  & TOTAL_EMPLOYEES < 25)]
good_stop_counts[, size_025_050 := 1 * (TOTAL_EMPLOYEES >= 25  & TOTAL_EMPLOYEES < 50)]
good_stop_counts[, size_050_100 := 1 * (TOTAL_EMPLOYEES >= 50  & TOTAL_EMPLOYEES < 100)]
good_stop_counts[, size_100_250 := 1 * (TOTAL_EMPLOYEES >= 100 & TOTAL_EMPLOYEES < 250)]
good_stop_counts[, size_250_p    := 1 * (TOTAL_EMPLOYEES >= 250)]


good_stop_counts[, dist_00_02 := 1 * (dist >= 0  & dist < 2)]
good_stop_counts[, dist_02_05 := 1 * (dist >= 2  & dist < 5)]
good_stop_counts[, dist_05_10 := 1 * (dist >= 5  & dist < 10)]
good_stop_counts[, dist_10_20 := 1 * (dist >= 10 & dist < 20)]
good_stop_counts[, dist_20_40 := 1 * (dist >= 20 & dist < 40)]
good_stop_counts[, dist_40_p  := 1 * (dist >= 40)]

good_stop_counts[, time_000_005 := 1 * (time >= 0   & time < 5)]
good_stop_counts[, time_005_015 := 1 * (time >= 5   & time < 15)]
good_stop_counts[, time_015_030 := 1 * (time >= 15  & time < 30)]
good_stop_counts[, time_030_060 := 1 * (time >= 30  & time < 60)]
good_stop_counts[, time_060_090 := 1 * (time >= 60  & time < 90)]
good_stop_counts[, time_090_120 := 1 * (time >= 90  & time < 120)]
good_stop_counts[, time_120_p   := 1 * (time >= 120)]

good_stop_counts[, size_001_005 := 1 * (TOTAL_EMPLOYEES >= 1   & TOTAL_EMPLOYEES < 5)]
good_stop_counts[, size_005_015 := 1 * (TOTAL_EMPLOYEES >= 5   & TOTAL_EMPLOYEES < 15)]
good_stop_counts[, size_015_025 := 1 * (TOTAL_EMPLOYEES >= 15  & TOTAL_EMPLOYEES < 25)]
good_stop_counts[, size_025_050 := 1 * (TOTAL_EMPLOYEES >= 25  & TOTAL_EMPLOYEES < 50)]
good_stop_counts[, size_050_100 := 1 * (TOTAL_EMPLOYEES >= 50  & TOTAL_EMPLOYEES < 100)]
good_stop_counts[, size_100_250 := 1 * (TOTAL_EMPLOYEES >= 100 & TOTAL_EMPLOYEES < 250)]
good_stop_counts[, size_250_p    := 1 * (TOTAL_EMPLOYEES >= 250)]

# # Check good_stop_counts
# good_stop_counts[,.N, keyby = STOPS][,NSTOPS := N*STOPS][]
# sum(good_stop_counts[,.N, keyby = STOPS][,NSTOPS := N*STOPS]$NSTOPS)
# sum(good_stop_counts$WEIGHTED_STOPS)
# length(unique(good_stop_counts$SITEID))
# good_stop_counts[,.(STOPS = sum(STOPS)), by = SITEID][,.N, keyby = STOPS]

# Calculate null log-likelihood
hurdle_ll <- sum(log(0.5)*nrow(good_stop_counts))
count_ll <- sum(dpois(good_stop_counts[, STOPS], mean(good_stop_counts[, STOPS]), log = TRUE))
hurdle_ll + count_ll

# Need to update expressions below to account for updated employment category grouping
# # Model 1 glmmtmb
# myFormula <- STOPS ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time)
# myZIFormula <- ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Education) + 
#   log1p(NEmp_Retail) + log(dist)
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = good_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# # Model 2 glmmtmb
# myFormula <- STOPS ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time)
# myZIFormula <- ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Education) + 
#   log1p(NEmp_Retail) + log(dist) + Industrial + Office + Retail
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = good_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# # Model 2 glmmtmb
# myFormula <- STOPS ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time)
# myZIFormula <- ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Education) + 
#   log1p(NEmp_Retail) + log(dist) + Industrial + Office + Retail + 
#   (Industrial + Office + Retail):log(dist)
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = good_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# 
# # Model 3 glmmtmb
# myFormula <- STOPS ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time)
# myZIFormula <- ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Education) + 
#   log1p(NEmp_Retail) + log(dist) + Industrial + Office + Retail + 
#   (Industrial):log(dist)
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = good_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# 
# # Model 4 glmmtmb
# myFormula <- STOPS ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time) + Industrial + Office + Retail
# myZIFormula <- ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Education) + 
#   log1p(NEmp_Retail) + log(dist) + Industrial + Office + Retail + 
#   (Industrial):log(dist)
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = good_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# 
# 
# # Model 5 glmmtmb
# myFormula <- STOPS ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time) + Industrial
# myZIFormula <- ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Education) + 
#   log1p(NEmp_Retail) + log(dist) + Industrial + Office + Retail + 
#   (Industrial):log(dist)
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = good_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# 
# # Model 6 glmmtmb
# myFormula <- STOPS ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time) + Industrial + log(TOTAL_EMPLOYEES)
# myZIFormula <- ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Education) + 
#   log1p(NEmp_Retail) + log(dist) + Industrial + Office + Retail + 
#   (Industrial):log(dist) + log(TOTAL_EMPLOYEES)
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = good_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)
# 
# # Model 7 glmmtmb
# myFormula <- STOPS ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Medical_Services) + 
#   log1p(NEmp_Retail) + log(time) + Industrial + log(TOTAL_EMPLOYEES) + (1 | SITE_TAZID)
# myZIFormula <- ~ log1p(HH) + log1p(NEmp_Industrial) + log1p(NEmp_Education) + 
#   log1p(NEmp_Retail) + log(dist) + Industrial + Office + Retail + 
#   (Industrial):log(dist) + log(TOTAL_EMPLOYEES)
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = good_stop_counts,
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# summary(fit)


# # Model 8 glmmtmb
# myFormula <- STOPS ~ log1p(HH) + log1p(NEmp_Production) + 
#   log1p(NEmp_Medical_Services) + log1p(NEmp_Transportation) +
#   log1p(NEmp_Retail) + log(time) + Industrial + Transportation + log(TOTAL_EMPLOYEES) + 
#   (1 | SITE_TAZID) + log(time):(Production) + Production
# myZIFormula <- STOPF ~ log1p(HH) + log1p(NEmp_Production) + 
#   log1p(NEmp_Retail) + log(dist) + Industrial + Production + Info_FIRE_Prof + Retail + Transportation +
#   (Industrial):log(dist) + (Transportation):log(dist) + (Production):log(dist) + (Info_FIRE_Prof):log(dist) + 
#   log(TOTAL_EMPLOYEES) + (1 | SITE_TAZID)
# fit <- glmmTMB(myFormula, ziformula = myZIFormula, 
#                data = good_stop_counts[STOPS>0],
#                family = truncated_poisson(link = "log"),
#                verbose = TRUE)
# good_stop_counts[,STOPF:=STOPS>0]
# fitzi <- glmmTMB(myZIFormula, 
#                data = good_stop_counts,
#                family = binomial(link = "logit"),
#                verbose = TRUE)
# fitzi2 <- glmmTMB(myZIFormula, 
#                data = good_stop_counts,
#                family = binomial(link = "cloglog"),
#                verbose = TRUE)
# summary(fit)
# 
# final_model <- fit
# 
# # Model diagnostics for count model
# good_stops_filtered_count = good_stop_counts[STOPS > 0]
# fit_trunc <- glmmTMB(myFormula,
#                      data = good_stops_filtered_count,
#                      family = truncated_poisson(link = "log"),
#                      verbose = TRUE)
# 
# ### Final Model
# # Perform some simulated residual checks
# simres_goods_model = simulateResiduals(final_model)
# simres_trunc_goods_model = simulateResiduals(fit_trunc)
# 
# # Plot residual plot
# png("final_models/goods/Goods Model Residual.png", width = 1000, height = 500)
# # testResiduals(simres_goods_model)
# plotResiduals(simres_trunc_goods_model)
# dev.off()
# 
# # Plot dispersion test
# png("final_models/goods/Goods Model Dispersion.png", width = 1000, height = 500)
# testDispersion(simres_goods_model)
# dev.off()
# 
# # Tests count
# png("final_models/goods/Goods Model Zero Count.png", width = 1000, height = 500)
# testGeneric(simres_goods_model, function(x) sum(x==0))
# dev.off()
# 
# # Tests count 1
# png("final_models/goods/Goods Model One Count.png", width = 1000, height = 500)
# testGeneric(simres_goods_model, function(x) sum(x==1))
# dev.off()
# 
# # Tests count 2
# png("final_models/goods/Goods Model Two Count.png", width = 1000, height = 500)
# testGeneric(simres_goods_model, function(x) sum(x==2))
# dev.off()
# 
# # Tests count 3
# png("final_models/goods/Goods Model Three Count.png", width = 1000, height = 500)
# testGeneric(simres_goods_model, function(x) sum(x==3))
# dev.off()
# 
# # Tests count >=4
# png("final_models/goods/Goods Model Four Plus Count.png", width = 1000, height = 500)
# testGeneric(simres_goods_model, function(x) sum(x>=4))
# dev.off()
# 
# # Tests uniformity
# png("final_models/goods/Goods Model Uniformity Test.png", width = 1000, height = 500)
# testUniformity(simres_trunc_goods_model)
# dev.off()

### Final Model
#SEMCOG Formula
# myFormula <- STOPS ~ log1p(HH) + log1p(NEmp_Production) + 
#   log1p(NEmp_Medical_Services) + log1p(NEmp_Transportation) +
#   log1p(NEmp_Retail) + log(time) + Industrial + Transportation + log(TOTAL_EMPLOYEES) +
#   log(time):(Production) + Production | log1p(HH) + log1p(NEmp_Production) + 
#   log1p(NEmp_Retail) + log(dist) + Industrial + Production + Info_FIRE_Prof + Retail + Transportation +
#   (Industrial):log(dist) + (Transportation):log(dist) + (Production):log(dist) + (Info_FIRE_Prof):log(dist) + 
#   log(TOTAL_EMPLOYEES)

#CMAP FORMULA
#due to grouped categories, we would have some repeats which i have commented out for comparison
myFormula <- STOPS ~ log1p(HH) + log1p(NEmp_Transport_Industry) + 
  log1p(NEmp_Ed_Health_SocialServices) + #log1p(NEmp_Transportation) +
  log1p(NEmp_Retail) + log1p(NEmp_Wholesale) + log(time) + Construction + Transport_Industry + log(TOTAL_EMPLOYEES) +
  log(time):(Transport_Industry) | #+ Transport_Industry
  log1p(HH) + log1p(NEmp_Transport_Industry) + 
  log1p(NEmp_Retail) + log1p(NEmp_Wholesale) + log(dist) + Construction + Transport_Industry + Office_Professional + Retail + #Transport_Industry +
  (Construction):log(dist) + (Transport_Industry):log(dist) + #(Transport_Industry):log(dist) + 
  (Office_Professional):log(dist) + 
  log(TOTAL_EMPLOYEES)

goods.fit <- hurdle(formula = myFormula, data = good_stop_counts,
                      dist="poisson",
                      zero.dist="binomial",
                      link="logit")
summary(goods.fit)
goods.fit.copy <- goods.fit

# # Test
# good_stop_counts[, STOPSPRED := predict(object = goods.fit)]
# good_stop_counts[, STOPSPREDMC := as.integer(montecarlo.predict(object = goods.fit, at = 0:100))]
# good_stop_counts[,.N, keyby = STOPS][,NSTOPS := N*STOPS][]
# good_stop_counts[,.N, keyby = STOPSPREDMC][,NSTOPS := N*STOPSPREDMC][]
# sum(good_stop_counts[,.N, keyby = STOPS][,NSTOPS := N*STOPS]$NSTOPS)
# sum(good_stop_counts[,.N, keyby = STOPSPREDMC][,NSTOPS := N*STOPSPREDMC]$NSTOPS)
# good_stop_counts[,.(STOPS = sum(STOPS)), by = SITEID][,.N, keyby = STOPS]
# good_stop_counts[,.(STOPS = sum(STOPSPREDMC)), by = SITEID][,.N, keyby = STOPS]


# Calibration trick: estimate with separate time parameters but use the shared value from the previous model
# Copy population level estimate to hurdle model for prediction
coef_names <- names(goods.fit$coefficients$count)
count_par <- length(goods.fit$coefficients$count)
goods.fit$coefficients$count <- final_model$fit$par[seq_len(count_par)]
names(goods.fit$coefficients$count) <- coef_names

coef_names <- names(goods.fit$coefficients$zero)
zero_par <- length(goods.fit$coefficients$zero)
goods.fit$coefficients$zero <- -final_model$fit$par[(count_par+seq_len(zero_par))]
names(goods.fit$coefficients$zero) <- coef_names
# Only works when passing newdata to predict function.

summary(goods.fit)


# Trim all the junk from the hurdle model object to save space
# residuals, model, weights, fitted.values
finalModel <- goods.fit
finalModel$residuals <- NULL
finalModel$fitted.values <- NULL
finalModel$model <- NULL
finalModel$weights <- NULL
attr(finalModel$terms$count, ".Environment") <- NULL
attr(finalModel$terms$zero, ".Environment") <- NULL
attr(finalModel$terms$full, ".Environment") <- NULL

# Save final model
saveRDS(finalModel, file = "new_models/goods/cv_goods_model.RDS")

# Write results to csv
options(width = 10000)
sink(file = "final_models/goods/goods_model_results.txt")
print(summary(goods.fit.copy))
print(summary(final_model))
sink()
options(width = 137)
