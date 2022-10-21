### Load Apollo library
library(apollo)
library(data.table)
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "Base model",
  modelDescr = "MNL model - base",
  indivID    = "ID"
)

base = 'dev/Estimation/cv_vehicle'

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = readRDS(file.path(base, "estimation_data.rds"))

database[, ID := SITEID]

# omit the transport industry records
database <- database[!industry_transport_industry == 1]

# should this be conditioned on something
database[, av_light := 1]
database[, av_medium := 1]
database[, av_heavy  := 1]

database[, activity_deliver_pickup := activity_deliver + activity_pickup]

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = 
  c(asc_light  = 0,
    asc_medium = 0,
    asc_heavy  = 0,
    
    beta_v1_industry_retail = 0,
    beta_v1_industry_wholesale = 0,
    beta_v1_industry_construction = 0,
    beta_v1_industry_transport_industry = 0,
    beta_v1_industry_admin_support_waste = 0,
    beta_v1_industry_ed_health_social_public = 0,
    beta_v1_industry_service_other = 0,
    beta_v1_industry_office_professional = 0,
    beta_v1_industry_service_fooddrink = 0,
    
    beta_v2_industry_retail = 0,
    beta_v2_industry_wholesale = 0,
    beta_v2_industry_construction = 0,
    beta_v2_industry_transport_industry = 0,
    beta_v2_industry_admin_support_waste = 0,
    beta_v2_industry_ed_health_social_public = 0,
    beta_v2_industry_service_other = 0,
    beta_v2_industry_office_professional = 0,
    beta_v2_industry_service_fooddrink = 0,
    
    beta_v1_activity_deliver_pickup = 0,
    beta_v1_activity_service = 0,

    beta_v2_activity_deliver_pickup = 0,
    beta_v2_activity_service = 0,

    beta_v1_dist_00_02 = 0,
    beta_v1_dist_02_05 = 0,
    beta_v1_dist_05_10 = 0,
    beta_v1_dist_10_20 = 0,
    beta_v1_dist_20_p = 0,

    beta_v2_dist_00_02 = 0,
    beta_v2_dist_02_05 = 0,
    beta_v2_dist_05_10 = 0,
    beta_v2_dist_10_20 = 0,
    beta_v2_dist_20_p = 0
            
    )

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = 
  c("asc_heavy", 
    "beta_v1_industry_retail", "beta_v2_industry_retail",
    "beta_v1_industry_transport_industry", "beta_v2_industry_transport_industry",
    "beta_v1_industry_service_fooddrink", "beta_v2_industry_service_fooddrink",
    "beta_v1_industry_ed_health_social_public", "beta_v2_industry_ed_health_social_public",
    "beta_v1_activity_deliver_pickup", "beta_v2_activity_deliver_pickup",
    "beta_v1_dist_00_02", "beta_v2_dist_00_02")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities =
  function(
    apollo_beta, 
    apollo_inputs, 
    functionality = "estimate"){
    
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()

  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  
  V[['light']]  = 
    asc_light + 
    beta_v1_industry_retail *	industry_retail +
    beta_v1_industry_wholesale * industry_wholesale +
    beta_v1_industry_construction *	industry_construction +
    beta_v1_industry_transport_industry *	industry_transport_industry +
    beta_v1_industry_admin_support_waste * industry_admin_support_waste +
    beta_v1_industry_ed_health_social_public *	industry_ed_health_social_public +
    beta_v1_industry_service_other * industry_service_other + 
    beta_v1_industry_office_professional *	industry_office_professional +
    beta_v1_industry_service_fooddrink * industry_service_fooddrink + 
    beta_v1_activity_deliver_pickup * activity_deliver_pickup +
    beta_v1_activity_service * activity_service +
    beta_v1_dist_00_02 * dist_00_02 + 
    beta_v1_dist_02_05 * dist_02_05 + 
    beta_v1_dist_05_10 * dist_05_10 + 
    beta_v1_dist_10_20 * dist_10_20 + 
    beta_v1_dist_20_p  * dist_20_p 
    
  V[['medium']] = 
    asc_medium +
    beta_v2_industry_retail *	industry_retail +
    beta_v2_industry_wholesale * industry_wholesale +
    beta_v2_industry_construction *	industry_construction +
    beta_v2_industry_transport_industry *	industry_transport_industry +
    beta_v2_industry_admin_support_waste * industry_admin_support_waste +
    beta_v2_industry_ed_health_social_public *	industry_ed_health_social_public +
    beta_v2_industry_service_other * industry_service_other + 
    beta_v2_industry_office_professional *	industry_office_professional +
    beta_v2_industry_service_fooddrink * industry_service_fooddrink + 
    beta_v2_activity_deliver_pickup * activity_deliver_pickup +
    beta_v2_activity_service * activity_service +
    beta_v2_dist_00_02 * dist_00_02 + 
    beta_v2_dist_02_05 * dist_02_05 + 
    beta_v2_dist_05_10 * dist_05_10 + 
    beta_v2_dist_10_20 * dist_10_20 + 
    beta_v2_dist_20_p  * dist_20_p

  V[['heavy']]  = asc_heavy

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(light = 1, medium = 2, heavy = 3),
    avail        = list(light = av_light, medium = av_medium, heavy = av_heavy),
    choiceVar    = veh_choice,
    V            = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = 
  apollo_estimate(
    apollo_beta, 
    apollo_fixed, 
    apollo_probabilities, 
    apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #
model_loc = 'dev/Estimation/cv_vehicle'
saveRDS(model, file.path(model_loc, "final_model","cv_vehicle_model.rds"))

