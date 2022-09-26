# 1. Define TAZ ranges for different elements of the model region.
BASE_TAZ_INTERNAL <- 1L:3632L #range of TAZs that covers the CMAP model region 

# 2. Define other application parameters
BASE_SCENARIO_BASE_NAME <- "base" #base year scenario name
BASE_SCENARIO_BASE_YEAR <- 2015 #base year scenario year
BASE_SCENARIO_YEARS <- c(2015, 2020, 2025, 2030, 2035, 2040, 2045) 

BASE_SEED_VALUE  <- 5 #seed for sampling to ensure repeatable results
BASE_TIME_PERIOD_TRIP_POINT <- "START" #point in trip for time period allocation, from ("START", "MIDDLE", "END")
BASE_SKIM_CONDITION <- "congested" #alternatives are 'congested' and 'free flow'

# 3. Define time-of-day groupings for skims and trip tables
# # Units are minutes after midnight 
# p1 - 8 pm to 6 am
# p2 - 6 am to 7 am
# p3 - 7 am to 9 am
# p4 - 9 am to 10 am
# p5 - 10 am to 2 pm
# p6 - 2 pm to 4 pm
# p7 - 4 pm to 6 pm
# p8 - 6 pm to 8 pm
BASE_TOD_RANGES <- list(P1 = list(c(0,360), c(1200,1440)),
                        P2 = list(c(360,420)),
                        P3 = list(c(420,540)),
                        P4 = list(c(540, 600)),
                        P5 = list(c(600, 840)),
                        P6 = list(c(840, 960)),
                        P7 = list(c(960, 1080)),
                        P8 = list(c(1080, 1200))
                        )
