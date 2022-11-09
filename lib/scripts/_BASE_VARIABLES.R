# 1. Define TAZ ranges for different elements of the model region.
TAZ_System <- read.csv(file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"))
BASE_TAZ_INTERNAL <- TAZ_System$TAZ #range of TAZs that covers the CMAP model region 
BASE_MZ_INTERNAL <- sort(unique(TAZ_System$Mesozone)) #range of mesozones that covers the CMAP model region 
BASE_FIPS_INTERNAL <- sort(unique(TAZ_System$CountyFIPS)) #range of county fips code that covers the CMAP model region 
BASE_TAZ_CHICAGO <- TAZ_System$TAZ[TAZ_System$chicago == 1] #range of TAZ is City of Chicago
BASE_TAZ_CMAP <- TAZ_System$TAZ[TAZ_System$cmap == 1] #range of TAZ is CMAP 7 county region
rm(TAZ_System)

# 2. Define other application parameters
BASE_SCENARIO_BASE_NAME <- "base" #base year scenario name
BASE_SCENARIO_BASE_YEAR <- 2019 #base year scenario year

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

# 4. Define settings used in Dashboard/spreadsheet report
# Column name from TAZ_System.csv that labels each TAZ with the desired group
# names for use in the dashboard. This also determines how TAZs will be grouped
# into larger regions for display in the dashboard.
BASE_DASHBOARD_GEOGRAPHY <- "county_state"

# Unit used for display in dashboard
BASE_DASHBOARD_LENGTH_UNIT <- "miles"
