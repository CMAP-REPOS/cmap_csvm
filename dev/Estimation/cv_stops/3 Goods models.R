library(data.table)
library(pscl)
library(splines)

##########################
# Estimate Models

model_loc = 'dev/Estimation/cv_stops/'
# Load data
load(file.path(model_loc, "Stop_Counts_Goods.RData"))
source(file.path(model_loc, "0 helper functions.R"))

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
good_stop_counts[,.N, keyby = STOPS][,NSTOPS := N*STOPS][]
sum(good_stop_counts[,.N, keyby = STOPS][,NSTOPS := N*STOPS]$NSTOPS)
sum(good_stop_counts$WEIGHTED_STOPS)
length(unique(good_stop_counts$SITEID))
good_stop_counts[,.(STOPS = sum(STOPS)), by = SITEID][,.N, keyby = STOPS]

# Calculate null log-likelihood
hurdle_ll <- sum(log(0.5)*nrow(good_stop_counts))
count_ll <- sum(dpois(good_stop_counts[, STOPS], mean(good_stop_counts[, STOPS]), log = TRUE))
hurdle_ll + count_ll

# CMAP FORMULA
myFormula <- STOPS ~ 
  log1p(HH) + 
  log1p(NEmp_Ed_Health_Social_Public) + 
  log1p(NEmp_Office_Professional) + 
  log1p(NEmp_Retail) +
  log1p(NEmp_Transport_Industry) +
  log(time) + 
  log(TOTAL_EMPLOYEES) +
  log(time):(Transport_Industry) | 
  log1p(HH) + 
  log1p(NEmp_Construction) + 
  log1p(NEmp_Retail) +
  log1p(NEmp_Service_Other) +
  log1p(NEmp_Transport_Industry) +
  log(dist) + 
  Admin_Support_Waste + 
  Construction +
  Ed_Health_Social_Public +
  Office_Professional + 
  #Retail + removed to prevent errors resulting from overspecification,  decision informed by summary of employment category trip rates
  Service_FoodDrink + 
  Service_Other + 
  Transport_Industry + 
  Wholesale + 
  (Construction):log(dist) + 
  (Transport_Industry):log(dist) + 
  (Office_Professional):log(dist) + 
  log(TOTAL_EMPLOYEES)

goods.fit <- hurdle(formula = myFormula, data = good_stop_counts,
                      dist="poisson",
                      zero.dist="binomial",
                      link="logit")
summary(goods.fit)

# # Test
# good_stop_counts[, STOPSPRED := predict(object = goods.fit)]
# good_stop_counts[, STOPSPREDMC := as.integer(montecarlo.predict(object = goods.fit, at = 0:100))]
# good_stop_counts[,.N, keyby = STOPS][,NSTOPS := N*STOPS][]
# good_stop_counts[,.N, keyby = STOPSPREDMC][,NSTOPS := N*STOPSPREDMC][]
# sum(good_stop_counts[,.N, keyby = STOPS][,NSTOPS := N*STOPS]$NSTOPS)
# sum(good_stop_counts[,.N, keyby = STOPSPREDMC][,NSTOPS := N*STOPSPREDMC]$NSTOPS)
# good_stop_counts[,.(STOPS = sum(STOPS)), by = SITEID][,.N, keyby = STOPS]
# good_stop_counts[,.(STOPS = sum(STOPSPREDMC)), by = SITEID][,.N, keyby = STOPS]

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
saveRDS(finalModel, file = file.path(model_loc, "new_models/goods/cv_goods_model.RDS"))

# Write results to csv
options(width = 10000)
sink(file = file.path(model_loc, "new_models/goods/goods_model_results.txt"))
print(summary(goods.fit))
sink()
options(width = 137)
