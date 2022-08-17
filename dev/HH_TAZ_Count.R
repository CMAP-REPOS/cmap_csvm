#Takes employment data, extracts subzone and Household counts, and aggregates to zone level

# Setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)

# Get Data ----------------------------------------------------------------

HH <- read_csv('dev/Data_Processed/CBP_Emp_HH/HH_IN.TXT', col_names = FALSE)


HH_1 <- HH %>% 
  select(subzone17 = X1,
         TotalHHinSZ = X2)


Zone_dict <- st_read('dev/Data_Processed/TAZ/subzones17.shp')

zonedict_1 <- Zone_dict %>% 
  select(subzone17,zone17)


# Merge -------------------------------------------------------------------

HH_2 <- HH_1 %>% 
  left_join(zonedict_1, by = 'subzone17' ) %>% 
  select(Zone17 = zone17, TotalHHinSZ, subzone17)

HH_2 %>% 
  summarise(TotalHHs = sum(TotalHHinSZ))


colSums(HH_2)



# Aggregate ---------------------------------------------------------------

HH_summary2 <- HH_2 %>% 
  group_by(Zone17) %>% 
  summarise(HH = sum(TotalHHinSZ), subzone17) %>% 
  select(-subzone17) %>% 
  as.data.frame()

# write the file ----------------------------------------------------------

write_csv(HH_summary2, 'scenarios/base/inputs/data_hh.csv')
