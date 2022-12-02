##############################################################################################
# Title:             CMAP Commercial Services Vehicle Model
# Project:           Commercial Services Vehicle Survey and Model
# Description:       Simulation of commercial services vehicles in the CMAP region
# Date:              12-02-2022
# Author:            Resource Systems Group, Inc.
# Copyright:         Copyright 2022 RSG, Inc. - All rights reserved.
##############################################################################################

# 1. Specific scenario arguments from the command line:
#    1 - Scenario name (folder name for scenario, defaults to "base")
#    2 - Scenario year (year, defaults to 2019)
#    3 - Run Firm Synthesis component (TRUE or FALSE, defaults to TRUE)
#    4 - Run CVTM component           (TRUE or FALSE, defaults to TRUE)
#    5 - Run Trip Tables component    (TRUE or FALSE, defaults to TRUE)
#    6 - Run Dashboard component      (TRUE or FALSE, defaults to TRUE)
#    7 - Reference Scenario name for spreadsheet summary (folder name for scenario, defaults to "base")
#    8 - Reference Scenario year for spreadsheet summary (year, defaults to 2019)

SYSTEM_COMMAND_ARGS <- commandArgs(TRUE)

# 2. Run the application

source(file.path("lib", "scripts", "__Master.R"))