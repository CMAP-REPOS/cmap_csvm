##############################################################################################
# Title:             CMAP Commercial Services Vehicle Model
# Project:           Commercial Services Vehicle Survey and Model
# Description:       Simulation of commercial services vehicles in the CMAP region
# Date:              6-18-2022
# Author:            Resource Systems Group, Inc.
# Copyright:         Copyright 2022 RSG, Inc. - All rights reserved.
##############################################################################################

# 1. Specific scenario arguments from the command line:
#    - Scenario name (defaults to base)
#    - Scenario year (defaults to 2015)
#    - Run Firm Synthesis component (defaults to TRUE)
#    - Run CVTM component           (defaults to TRUE)
#    - Run Trip Tables component    (defaults to TRUE)
#    - Run Dashboard component      (defaults to TRUE)
SYSTEM_COMMAND_ARGS <- commandArgs(TRUE)

# 2. Run the application
source(file.path("lib", "scripts", "__Master.R"))