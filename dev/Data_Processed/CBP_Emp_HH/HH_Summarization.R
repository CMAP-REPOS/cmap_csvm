
source("./dev/init_dev.R")
library(tidyverse)
library(readxl)
HH_raw <- read_csv('dev/Data_Processed/CBP_Emp_HH/HH_IN.TXT', col_names = FALSE)
HH <- HH_raw %>% 
  select(TripGen_sz = X1,
         Num_Homes = X2)

tazs <- read_csv('dev/Data_Processed/CBP_Emp_HH/data_emp_cbp_2017.csv')

TAZ_SZ_Table <- gps_raw
