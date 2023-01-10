source("./dev/init_dev.R")
library(readxl)
library(tidyverse)
library(lubridate)

SYSTEM_DEV_DATA_PATH <- file.path(SYSTEM_DEV_PATH, 'DATA_Processed')



# Read CVGPS Data ---------------------------------------------------------
gps <- read_xlsx('dev/Data_Processed/CVGPS/TripsMediumLight_OD_ISPE83_SZPlusCounties_ValuesOnly_forRSG.xlsx')



# Count the Data ----------------------------------------------------------
gps %>% 
  select(`Tour ID`) %>% 
  distinct() 

