# Define TAZ and Mesozone ranges for different elements of the model region
BASE_TAZ_INTERNAL <- 1L:3632L #range of TAZs that covers the CMAP model region 

# Define application time periods, run years, and other temporal inputs
BASE_SCENARIO_BASE_NAME <- "base" #base year scenario name
BASE_SCENARIO_BASE_YEAR <- 2015 #base year scenario year
BASE_SCENARIO_YEARS <- c(2015, 2020, 2025, 2030, 2035, 2040, 2045) 

# Define other application parameters
BASE_SEED_VALUE  <- 5 #seed for sampling to ensure repeatable results
BASE_TIME_PERIOD_TRIP_POINT <- "START" #point in trip for time period allocation, from ("START", "MIDDLE", "END")

