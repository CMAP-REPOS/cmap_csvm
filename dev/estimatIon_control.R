# CMAP CSVM
# dev script: estimation_control.R
# Script to support review of estimated models
# Copies estimated model objects into the application structure

# use init_dev.R to run here instead of sourcing from _Master_Dev.R
source("./dev/init_dev.R")

### Open and Review the Estimated Models

# Firm activities
activities <- readRDS("./dev/Estimation/cv_activities/cv_activities_model_fortesting.RDS")
# remove total column, convert to data.table, save with correct name
activities <- data.table(activities)
activities[, Total := NULL]
saveRDS(activities, "./dev/Estimation/cv_activities/cv_activities_model.RDS")

# # Scheduled Stops
# goods <- readRDS("./dev/Estimation/cv_stops/final_models/goods/cv_goods_model.RDS")
# goods$coefficients
# 
# service <- readRDS("./dev/Estimation/cv_stops/final_models/service/cv_service_model.RDS")
# service$coefficients
# 
# # Vehicle
# vehicle <- readRDS("./dev/Estimation/cv_vehicle/cv_vehicle_model.rds")
# apollo_modelOutput(vehicle)
# 
# # Stop Duration
# stopdur <- readRDS("./dev/Estimation/cv_duration/cv_duration.rds")
# apollo_modelOutput(stopdur)
# 
# # Tour typology
# tourtype <- readRDS("./dev/Estimation/cv_tours/cv_tours_model.rds")
# apollo_modelOutput(tourtype)
# 
# # Arrival 
# arrival <- readRDS("./dev/Estimation/cv_arrival/cv_arrival_model.rds")
# apollo_modelOutput(arrival)
# 
# # Intermediate
# interm <- readRDS("./dev/Estimation/cv_intermediate/cv_intermediate_model.rds")
# apollo_modelOutput(interm)
# 
# intermdev <- readRDS("./dev/Estimation/cv_intermediate/cv_intermediate_deviations.rds")
# intermattr <- readRDS("./dev/Estimation/cv_intermediate/cv_intermediate_model_attraction.rds")
# apollo_modelOutput(intermattr)

### Copy the estimated model components over to the lib/data folder for use in applcation

# # stop duration model is misnamed!
# stopdur <- readRDS("./dev/Estimation/cv_duration/cv_duration.rds")
# saveRDS(stopdur, "./dev/Estimation/cv_duration/cv_stopduration_model.rds")

paths_to_models <- file.path("./dev/Estimation", c("cv_activities/cv_activities_model.RDS")) #,
                                                   # "cv_stops/final_models/goods/cv_goods_model.RDS",
                                                   # "cv_stops/final_models/service/cv_service_model.RDS",
                                                   # "cv_vehicle/cv_vehicle_model.rds",
                                                   # "cv_duration/cv_stopduration_model.rds",
                                                   # "cv_tours/cv_tours_model.rds",
                                                   # "cv_arrival/cv_arrival_model.rds",
                                                   # "cv_intermediate/cv_intermediate_model.rds",
                                                   # "cv_intermediate/cv_intermediate_deviations.rds",
                                                   # "cv_intermediate/cv_intermediate_model_attraction.rds"))

file.copy(from = paths_to_models,
          to = "./lib/data",
          overwrite = TRUE)
