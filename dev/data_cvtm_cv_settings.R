# Script to make manual adjustments to CVTM inputs files
source("./dev/init_dev.R")

# load, adjust and save the input files

# cv_settings.Rdata
temp.envir <- new.env()
load("lib/data/cv_settings.RData", envir = temp.envir)
names(temp.envir)

# update names for d_bars
empcats <- fread("./lib/data/corresp_naics2_empcats.csv")
unique(empcats$EmpCatGroupedName)
temp.envir$d_bars[1:length(unique(empcats$EmpCatGroupedName))] <- 40
temp.envir$d_bars <- temp.envir$d_bars[1:length(unique(empcats$EmpCatGroupedName))]
names(temp.envir$d_bars) <- unique(empcats$EmpCatGroupedName)

# update industres in branch.limit
temp.envir$branch.limit <- data.table(EmpCatName = sort(unique(empcats$EmpCatName)),
                                      brlim.mu = 1.0, 
                                      brlim.sd = 0.5)

# update number of zones samples
temp.envir$numZones <- 50

# Remove anything that should not be in there
# Contents should be
cv_settings_objects <- c("d_bars", "hurdle_support", "branch.limit", 
                         ".Random.seed", "numZones", "deviance.threshold")
#,"cv_calibrated_parameters")

save(list = cv_settings_objects, file = "lib/data/cv_settings.RData", envir = temp.envir)
