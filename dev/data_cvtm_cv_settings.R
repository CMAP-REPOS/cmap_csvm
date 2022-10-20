# Script to make manual adjustments to CVTM inputs files

# load, adjust and save the input files

# cv_settings.Rdata
temp.envir <- new.env()
load("lib/data/cv_settings.RData", envir = temp.envir)
names(temp.envir)

# update names for d_bars
empcats <- fread("./lib/data/corresp_naics2_empcats.csv")
unique(empcats$EmpCatGroupedName)
temp.envir$d_bars[1:length(unique(empcats$EmpCatGroupedName))] <- 12.0
temp.envir$d_bars <- temp.envir$d_bars[1:length(unique(empcats$EmpCatGroupedName))]
names(temp.envir$d_bars) <- unique(empcats$EmpCatGroupedName)

# update industres in brand.limit
temp.envir$branch.limit <- data.table(EmpCatName = sort(unique(empcats$EmpCatName)),
                                      brlim.mu = temp.envir$branch.limit$brlim.mu, 
                                      brlim.sd = temp.envir$branch.limit$brlim.sd)

# # add cal parameters
# temp.envir$cv_calibrated_parameters <- fread("./dev/Calibration/cv_calibrated_parameters.csv")

# Remove anything that should not be in there
# Contents should be
cv_settings_objects <- c("d_bars", "hurdle_support", "branch.limit", 
                         ".Random.seed", "numZones", "deviance.threshold")
#,"cv_calibrated_parameters")

save(list = cv_settings_objects, file = "lib/data/cv_settings.RData", envir = temp.envir)
