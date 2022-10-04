library(data.table)
library(pscl)
library(splines)
library(glmmTMB)
library(foreach)
library(doParallel)
library(DHARMa)
setwd("dev/Estimation/cv_stops/")

##########################
# Estimate Models

# Load data
# load("Stop_Counts_Goods_test.RData")
# load("Counts_Meeting.RData")
load("Stop_Counts_Service_test.RData")
source("0 helper functions.R")

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




# Getting Rates -----------------------------------------------------------
#select employment category dummy, total employees, 
#pivot wider, filter to the row with their correct industry group
Data1 <- service_stop_counts %>% 
  select(SITEID, TOTAL_EMPLOYEES, STOPS, Admin_Support_Waste:Wholesale) %>% 
  pivot_longer(!c(SITEID, TOTAL_EMPLOYEES, STOPS), names_to = 'EmpCat', values_to = 'value') %>% 
  filter(value == 1)


#group by firm, get total stops, compute average stops per employee
Data2 <- Data1 %>% 
  group_by(SITEID) %>% 
  summarise(nStops = sum(STOPS), TOTAL_EMPLOYEES, EmpCat) %>% 
  distinct() %>%
  mutate(EmployeeStopRate = nStops/TOTAL_EMPLOYEES)


#group by employment category across all firms, get average employee stop rate for group
EmpCatRate <- Data2 %>% 
  group_by(EmpCat) %>% 
  summarise(EmpCatRate = mean(EmployeeStopRate))
