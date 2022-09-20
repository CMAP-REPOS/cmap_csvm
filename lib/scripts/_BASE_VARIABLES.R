# 1. Define TAZ ranges for different elements of the model region.
BASE_TAZ_INTERNAL <- 1L:3632L #range of TAZs that covers the CMAP model region 

# 2. Define other application parameters
BASE_SCENARIO_BASE_NAME <- "base" #base year scenario name
BASE_SCENARIO_BASE_YEAR <- 2015 #base year scenario year
BASE_SCENARIO_YEARS <- c(2015, 2020, 2025, 2030, 2035, 2040, 2045) 

BASE_SEED_VALUE  <- 5 #seed for sampling to ensure repeatable results
BASE_TIME_PERIOD_TRIP_POINT <- "START" #point in trip for time period allocation, from ("START", "MIDDLE", "END")
BASE_TOLL_SKIM_AVAILABLE <- TRUE
# 3. Define time-of-day groupings for skims and trip tables
# Units are minutes after midnight 
# (12:00 AM = 0 at the start of a range, 1440 at the end of a range)
# AM (6:30 AM - 8:59 AM) 
# MD (9:00 AM - 2:29 PM)
# PM (2:30 PM - 6:29 PM)
# EV (6:30 PM - 9:59 PM) 
# NT (10:00 PM - 6:29 AM)
BASE_TOD_RANGES <- list(NT = list(c(0, 390), c(1320, 1440)), 
                        AM = list(c(390, 540)), 
                        MD = list(c(540, 870)),
                        PM = list(c(870, 1110)),
                        EV = list(c(1110, 1320)))