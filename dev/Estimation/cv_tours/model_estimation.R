### Load Apollo library
library(apollo)
library(data.table)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "Match Previous Estimation",
  modelDescr = "MNL model - previous specification",
  indivID    = "ID"
)

model_loc = 'dev/Estimation/cv_tours'

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = readRDS(file.path(model_loc, "estimation_data.rds"))
database[, ID := SITEID]

database[, is_med_veh := 1 * (veh_choice == 2)]
database[, is_hvy_veh := 1 * (veh_choice == 3)]

# availability
database[, av_bbm := 1]
database[, av_bbs := 1]
database[, av_bnm := 1]
database[, av_bns := 1]
database[, av_nbm := 1]
database[, av_nb0 := 1]
database[, av_nbs := 1]
database[, av_nnm := 1]

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #










### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = 
  c(asc_bbm = 0,
    asc_bbs = 0,
    asc_bnm = 0,
    asc_bns = 0,
    asc_nbm = 0,
    asc_nb0 = 0,
    asc_nbs = 0,
    asc_nnm = 0,
    asc_bbm_industry_retail = 0,			    
    asc_bbs_industry_retail = 0,			    
    asc_bnm_industry_retail = 0,			    
    asc_bns_industry_retail = 0,			    
    asc_nbm_industry_retail = 0,  			
    asc_nb0_industry_retail = 0,  
    asc_nbs_industry_retail = 0,  
    asc_nnm_industry_retail = 0,
    asc_bbm_industry_wholesale = 0,			    
    asc_bbs_industry_wholesale = 0,			    
    asc_bnm_industry_wholesale = 0,			    
    asc_bns_industry_wholesale = 0,			    
    asc_nbm_industry_wholesale = 0,  			
    asc_nb0_industry_wholesale = 0,  
    asc_nbs_industry_wholesale = 0,  
    asc_nnm_industry_wholesale = 0,
    asc_bbm_industry_construction = 0,
    asc_bbs_industry_construction = 0,
    asc_bnm_industry_construction = 0,
    asc_bns_industry_construction = 0,
    asc_nbm_industry_construction = 0,  
    asc_nb0_industry_construction = 0,  
    asc_nbs_industry_construction = 0,  
    asc_nnm_industry_construction = 0,
    asc_bbm_industry_transport_industry = 0,
    asc_bbs_industry_transport_industry = 0,
    asc_bnm_industry_transport_industry = 0,
    asc_bns_industry_transport_industry = 0,
    asc_nbm_industry_transport_industry = 0,  
    asc_nb0_industry_transport_industry = 0,  
    asc_nbs_industry_transport_industry = 0,  
    asc_nnm_industry_transport_industry = 0,
    asc_bbm_industry_admin_support_waste = 0,
    asc_bbs_industry_admin_support_waste = 0,
    asc_bnm_industry_admin_support_waste = 0,
    asc_bns_industry_admin_support_waste = 0,
    asc_nbm_industry_admin_support_waste = 0,  
    asc_nb0_industry_admin_support_waste = 0,  
    asc_nbs_industry_admin_support_waste = 0,  
    asc_nnm_industry_admin_support_waste = 0,
    asc_bbm_industry_ed_health_socialservices = 0,
    asc_bbs_industry_ed_health_socialservices = 0,
    asc_bnm_industry_ed_health_socialservices = 0,
    asc_bns_industry_ed_health_socialservices = 0,
    asc_nbm_industry_ed_health_socialservices = 0,  
    asc_nb0_industry_ed_health_socialservices = 0,  
    asc_nbs_industry_ed_health_socialservices = 0,  
    asc_nnm_industry_ed_health_socialservices = 0,
    asc_bbm_industry_service_other = 0,
    asc_bbs_industry_service_other = 0,
    asc_bnm_industry_service_other = 0,
    asc_bns_industry_service_other = 0,
    asc_nbm_industry_service_other = 0,  
    asc_nb0_industry_service_other = 0,  
    asc_nbs_industry_service_other = 0,  
    asc_nnm_industry_service_other = 0,
    asc_bbm_industry_office_professional = 0,			    
    asc_bbs_industry_office_professional = 0,			    
    asc_bnm_industry_office_professional = 0,			    
    asc_bns_industry_office_professional = 0,			    
    asc_nbm_industry_office_professional = 0,  			
    asc_nb0_industry_office_professional = 0,  
    asc_nbs_industry_office_professional = 0,  
    asc_nnm_industry_office_professional = 0,
    asc_bbm_industry_service_foodDrink = 0,			    
    asc_bbs_industry_service_foodDrink = 0,			    
    asc_bnm_industry_service_foodDrink = 0,			    
    asc_bns_industry_service_foodDrink = 0,			    
    asc_nbm_industry_service_foodDrink = 0,  			
    asc_nb0_industry_service_foodDrink = 0,  
    asc_nbs_industry_service_foodDrink = 0,  
    asc_nnm_industry_service_foodDrink = 0,
    asc_bbm_is_med_veh = 0,
    asc_bbs_is_med_veh = 0,
    asc_bnm_is_med_veh = 0,
    asc_bns_is_med_veh = 0,
    asc_nbm_is_med_veh = 0,  
    asc_nb0_is_med_veh = 0,  
    asc_nbs_is_med_veh = 0,  
    asc_nnm_is_med_veh = 0,  
    asc_bbm_is_hvy_veh = 0,
    asc_bbs_is_hvy_veh = 0,
    asc_bnm_is_hvy_veh = 0,
    asc_bns_is_hvy_veh = 0,
    asc_nbm_is_hvy_veh = 0,  
    asc_nb0_is_hvy_veh = 0,  
    asc_nbs_is_hvy_veh = 0,  
    asc_nnm_is_hvy_veh = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = 
  c("asc_bbm", 
    "asc_bbm_is_med_veh", 
    "asc_bbm_is_hvy_veh",
    "asc_bbm_industry_retail",			    
    "asc_bbs_industry_retail",			    
    "asc_bnm_industry_retail",			    
    "asc_bns_industry_retail",			    
    "asc_nbm_industry_retail",  			
    "asc_nb0_industry_retail",  
    "asc_nbs_industry_retail",  
    "asc_nnm_industry_retail",
    "asc_bbm_industry_wholesale",			    
    "asc_bbs_industry_wholesale",			    
    "asc_bnm_industry_wholesale",			    
    "asc_bns_industry_wholesale",			    
    "asc_nbm_industry_wholesale",  			
    "asc_nb0_industry_wholesale",  
    "asc_nbs_industry_wholesale",  
    "asc_nnm_industry_wholesale",
    "asc_bbm_industry_construction",
    "asc_bbs_industry_construction",
    "asc_bnm_industry_construction",
    "asc_bns_industry_construction",
    "asc_nbm_industry_construction",  
    "asc_nb0_industry_construction",  
    "asc_nbs_industry_construction",  
    "asc_nnm_industry_construction",
    "asc_bbm_industry_transport_industry",
    "asc_bbs_industry_transport_industry",
    "asc_bnm_industry_transport_industry",
    "asc_bns_industry_transport_industry",
    "asc_nbm_industry_transport_industry",  
    "asc_nb0_industry_transport_industry",  
    "asc_nbs_industry_transport_industry",  
    "asc_nnm_industry_transport_industry",
    "asc_bbm_industry_admin_support_waste",
    "asc_bbs_industry_admin_support_waste",
    "asc_bnm_industry_admin_support_waste",
    "asc_bns_industry_admin_support_waste",
    "asc_nbm_industry_admin_support_waste",  
    "asc_nb0_industry_admin_support_waste",  
    "asc_nbs_industry_admin_support_waste",  
    "asc_nnm_industry_admin_support_waste",
    "asc_bbm_industry_ed_health_socialservices",
    "asc_bbs_industry_ed_health_socialservices",
    "asc_bnm_industry_ed_health_socialservices",
    "asc_bns_industry_ed_health_socialservices",
    "asc_nbm_industry_ed_health_socialservices",  
    "asc_nb0_industry_ed_health_socialservices",  
    "asc_nbs_industry_ed_health_socialservices",  
    "asc_nnm_industry_ed_health_socialservices",
    "asc_bbm_industry_service_other",
    "asc_bbs_industry_service_other",
    "asc_bnm_industry_service_other",
    "asc_bns_industry_service_other",
    "asc_nbm_industry_service_other",  
    "asc_nb0_industry_service_other",  
    "asc_nbs_industry_service_other",  
    "asc_nnm_industry_service_other",
    "asc_bbm_industry_office_professional",			    
    "asc_bbs_industry_office_professional",			    
    "asc_bnm_industry_office_professional",			    
    "asc_bns_industry_office_professional",			    
    "asc_nbm_industry_office_professional",  			
    "asc_nb0_industry_office_professional",  
    "asc_nbs_industry_office_professional",  
    "asc_nnm_industry_office_professional",
    "asc_bbm_industry_service_foodDrink",			    
    "asc_bbs_industry_service_foodDrink",			    
    "asc_bnm_industry_service_foodDrink",			    
    "asc_bns_industry_service_foodDrink",			    
    "asc_nbm_industry_service_foodDrink",  			
    "asc_nb0_industry_service_foodDrink",  
    "asc_nbs_industry_service_foodDrink",  
    "asc_nnm_industry_service_foodDrink",
    "asc_bbs_is_med_veh",
    "asc_nb0_is_med_veh",
    "asc_nbs_is_med_veh",
    "asc_nnm_is_med_veh",
    "asc_bbs_is_hvy_veh",
    "asc_bnm_is_hvy_veh",
    "asc_nbm_is_hvy_veh",
    "asc_nb0_is_hvy_veh",
    "asc_nbs_is_hvy_veh",
    "asc_nnm_is_hvy_veh"
) 

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

  V[['bbm']] = asc_bbm + asc_bbm_industry_retail * industry_retail + asc_bbm_industry_wholesale * industry_wholesale + asc_bbm_industry_construction * industry_construction + asc_bbm_industry_transport_industry * industry_transport_industry + asc_bbm_industry_admin_support_waste * industry_admin_support_waste + asc_bbm_industry_ed_health_socialservices * industry_ed_health_socialservices + asc_bbm_industry_service_other * industry_service_other + asc_bbm_industry_office_professional * industry_office_professional + asc_bbm_industry_service_foodDrink * industry_service_foodDrink + asc_bbm_is_med_veh * is_med_veh + asc_bbm_is_hvy_veh * is_hvy_veh
  V[['bbs']] = asc_bbs + asc_bbs_industry_retail * industry_retail + asc_bbs_industry_wholesale * industry_wholesale + asc_bbs_industry_construction * industry_construction + asc_bbs_industry_transport_industry * industry_transport_industry + asc_bbs_industry_admin_support_waste * industry_admin_support_waste + asc_bbs_industry_ed_health_socialservices * industry_ed_health_socialservices + asc_bbs_industry_service_other * industry_service_other + asc_bbs_industry_office_professional * industry_office_professional + asc_bbs_industry_service_foodDrink * industry_service_foodDrink + asc_bbs_is_med_veh * is_med_veh + asc_bbs_is_hvy_veh * is_hvy_veh
  V[['bnm']] = asc_bnm + asc_bnm_industry_retail * industry_retail + asc_bnm_industry_wholesale * industry_wholesale + asc_bnm_industry_construction * industry_construction + asc_bnm_industry_transport_industry * industry_transport_industry + asc_bnm_industry_admin_support_waste * industry_admin_support_waste + asc_bnm_industry_ed_health_socialservices * industry_ed_health_socialservices + asc_bnm_industry_service_other * industry_service_other + asc_bnm_industry_office_professional * industry_office_professional + asc_bnm_industry_service_foodDrink * industry_service_foodDrink + asc_bnm_is_med_veh * is_med_veh + asc_bnm_is_hvy_veh * is_hvy_veh
  V[['bns']] = asc_bns + asc_bns_industry_retail * industry_retail + asc_bns_industry_wholesale * industry_wholesale + asc_bns_industry_construction * industry_construction + asc_bns_industry_transport_industry * industry_transport_industry + asc_bns_industry_admin_support_waste * industry_admin_support_waste + asc_bns_industry_ed_health_socialservices * industry_ed_health_socialservices + asc_bns_industry_service_other * industry_service_other + asc_bns_industry_office_professional * industry_office_professional + asc_bns_industry_service_foodDrink * industry_service_foodDrink + asc_bns_is_med_veh * is_med_veh + asc_bns_is_hvy_veh * is_hvy_veh
  V[['nbm']] = asc_nbm + asc_nbm_industry_retail * industry_retail + asc_nbm_industry_wholesale * industry_wholesale + asc_nbm_industry_construction * industry_construction + asc_nbm_industry_transport_industry * industry_transport_industry + asc_nbm_industry_admin_support_waste * industry_admin_support_waste + asc_nbm_industry_ed_health_socialservices * industry_ed_health_socialservices + asc_nbm_industry_service_other * industry_service_other + asc_nbm_industry_office_professional * industry_office_professional + asc_nbm_industry_service_foodDrink * industry_service_foodDrink + asc_nbm_is_med_veh * is_med_veh + asc_nbm_is_hvy_veh * is_hvy_veh
  V[['nb0']] = asc_nb0 + asc_nb0_industry_retail * industry_retail + asc_nb0_industry_wholesale * industry_wholesale + asc_nb0_industry_construction * industry_construction + asc_nb0_industry_transport_industry * industry_transport_industry + asc_nb0_industry_admin_support_waste * industry_admin_support_waste + asc_nb0_industry_ed_health_socialservices * industry_ed_health_socialservices + asc_nb0_industry_service_other * industry_service_other + asc_nb0_industry_office_professional * industry_office_professional + asc_nb0_industry_service_foodDrink * industry_service_foodDrink + asc_nb0_is_med_veh * is_med_veh + asc_nb0_is_hvy_veh * is_hvy_veh
  V[['nbs']] = asc_nbs + asc_nbs_industry_retail * industry_retail + asc_nbs_industry_wholesale * industry_wholesale + asc_nbs_industry_construction * industry_construction + asc_nbs_industry_transport_industry * industry_transport_industry + asc_nbs_industry_admin_support_waste * industry_admin_support_waste + asc_nbs_industry_ed_health_socialservices * industry_ed_health_socialservices + asc_nbs_industry_service_other * industry_service_other + asc_nbs_industry_office_professional * industry_office_professional + asc_nbs_industry_service_foodDrink * industry_service_foodDrink + asc_nbs_is_med_veh * is_med_veh + asc_nbs_is_hvy_veh * is_hvy_veh
  V[['nnm']] = asc_nnm + asc_nnm_industry_retail * industry_retail + asc_nnm_industry_wholesale * industry_wholesale + asc_nnm_industry_construction * industry_construction + asc_nnm_industry_transport_industry * industry_transport_industry + asc_nnm_industry_admin_support_waste * industry_admin_support_waste + asc_nnm_industry_ed_health_socialservices * industry_ed_health_socialservices + asc_nnm_industry_service_other * industry_service_other + asc_nnm_industry_office_professional * industry_office_professional + asc_nnm_industry_service_foodDrink * industry_service_foodDrink + asc_nnm_is_med_veh * is_med_veh + asc_nnm_is_hvy_veh * is_hvy_veh


  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(
      bbm = 1,
      bbs = 2,
      bnm = 3,
      bns = 4,
      nbm = 5,
      nb0 = 6,
      nbs = 7,
      nnm = 8),
    avail = 
      list(
        bbm = av_bbm,
        bbs = av_bbs,
        bnm = av_bnm,
        bns = av_bns,
        nbm = av_nbm,
        nb0 = av_nb0,
        nbs = av_nbs,
        nnm = av_nnm),
    choiceVar    = tour_type_choice,
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

# comparison to old model

model_old = readRDS(file.path(model_loc, "cv_tours_model_semcog.rds"))

apollo_modelOutput(model_old)

comparison = 
  data.table(
    coefficients = names(model_old$estimate), 
    old = model_old$estimate, 
    new = model$estimate)

comparison

saveRDS(model, file.path(model_loc, "cv_tours_model.rds"))


