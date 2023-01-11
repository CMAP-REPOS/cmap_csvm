### Load Apollo library
rm(list = ls())
library(apollo)
library(data.table)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "MNL - attraction",
  modelDescr = "MNL model",
  indivID    = "ID"
)

model_loc = 'dev/Estimation/cv_intermediate'
# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = readRDS(file.path(model_loc, "estimation_data_attraction.rds"))
database[, ID := SITEID]

database[, log_distance := log(1 + dist)]

database[, alt := 1:.N, .(SITEID, VEHNUM, TOUR_NUM, STOP_SEQ)]

database[, choice := alt * (choice == 1)]

database[, choice := sum(choice), .(SITEID, VEHNUM, TOUR_NUM, STOP_SEQ)]

database = 
  dcast(
    database,
    ID + VEHNUM + TOUR_NUM + STOP_SEQ + num_alts + intermediate_stop_type + choice ~ alt,
    fun.aggregate = sum,
    value.var = c("EMP", "HH", "POP", "dist", "log_distance", "e05_retail", "e16_leisure"))

database[, .N, .(choice, num_alts)][order(choice, num_alts)]

database[choice > num_alts]

max_num_alts = database[, max(num_alts)]

# availability
for (alt in 1:max_num_alts) {
  database[, (paste0("av_", alt)) := 0]
  database[alt <= num_alts, (paste0("av_", alt)) := 1]
}

database[, .N, intermediate_stop_type]

database[, is_dn := 1 * (intermediate_stop_type == 2)]
database[, is_vs := 1 * (intermediate_stop_type == 3)]
database[, is_ot := 1 * (intermediate_stop_type == 4)]

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = 
  c(b_emp = 0,
    b_pop = 0,
    b_dist = 0,
    b_retail = 0,
    b_foodDrink = 0,
    b_foodDrink_dn = 0,
    b_emp_vs = 0,
    b_pop_vs = 0,
    b_dist_vs = 0,
    b_retail_vs = 0,
    b_foodDrink_vs = 0,
    b_emp_ot = 0,
    b_pop_ot = 0,
    b_dist_ot = 0,
    b_retail_ot = 0,
    b_foodDrink_ot = 0)

alternatives = 1:max_num_alts
names(alternatives) = paste0("alt_", alternatives)

avail_list = list()

for (alt in 1:max_num_alts) {
  avail_list[[paste0("alt_", alt)]] = database[, get(paste0("av_", alt))]
}

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = 
  c("b_pop", 
    "b_emp_vs", 
    "b_pop_vs", 
    "b_retail_vs", 
    "b_foodDrink_vs", 
    "b_emp_ot", 
    "b_pop_ot", 
    "b_foodDrink_ot",
    "b_dist_ot",
    "b_retail_ot",
    "b_foodDrink")

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

  for (alt in 1:max_num_alts) {
    
    # V[[paste0("alt_", alt)]] = 
    #   (b_emp + b_emp_vs * is_vs + b_emp_ot * is_ot) * get(paste0("EMP_", alt)) / 1000 +
    #   (b_pop + b_pop_vs * is_vs + b_pop_ot * is_ot) * get(paste0("POP_", alt)) / 1000 +
    #   (b_dist + b_dist_vs * is_vs + b_dist_ot * is_ot) * get(paste0("dist_", alt)) / 0.25 +
    #   (b_retail + b_retail_vs * is_vs + b_retail_ot * is_ot) * get(paste0("e05_retail_", alt)) / 1000 +
    #   (b_foodDrink + b_foodDrink_vs * is_vs + b_foodDrink_ot * is_ot) * get(paste0("e16_leisure_", alt)) / 1000
    
    V[[paste0("alt_", alt)]] =
      (b_emp + b_emp_vs * is_vs + b_emp_ot * is_ot) * log1p(get(paste0("EMP_", alt))) +
      (b_pop + b_pop_vs * is_vs + b_pop_ot * is_ot) * log1p(get(paste0("POP_", alt))) +
      (b_dist + b_dist_vs * is_vs + b_dist_ot * is_ot) * get(paste0("dist_", alt)) / 0.25 +
      (b_retail + b_retail_vs * is_vs + b_retail_ot * is_ot) * log1p(get(paste0("e05_retail_", alt))) +
      #(b_foodDrink + b_foodDrink_vs * is_vs + b_foodDrink_ot * is_ot) * log1p(get(paste0("e16_leisure_", alt)))
      (b_foodDrink + b_foodDrink_dn * is_dn + b_foodDrink_vs * is_vs + b_foodDrink_ot * is_ot) * log1p(get(paste0("e16_leisure_", alt)))
    
  }  
    
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = alternatives,
    avail        = avail_list,
    choiceVar    = choice,
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

saveRDS(model, file.path(model_loc, "cv_intermediate_attraction_model.rds"))