
library(tidyverse)
library(readxl)
library(sf)
# Process Internal (Updated File Received 11/10/22) -----
Emp <- read_xlsx('dev/Data_Processed/Future_Year_Inputs/r211_empsbySZ_fullmodelingregion_2050.xlsx')

Emp1 <- Emp %>% 
  select(subzone17, zone17, county_fips, SUM_e11:SUM_e92)



#newest file comes with Zones already attached
#Zone_dict <- st_read('dev/Data_Processed/TAZ/subzones17.shp')
# zonedict_1 <- Zone_dict %>%
#   select(subzone17,zone17, county_fip) %>% 
#   st_drop_geometry()
# 
# 
# 
# Emp_TAZ <- Emp1 %>% 
#   left_join(zonedict_1, by = c('subzone_id' = 'subzone17')) %>% 
#   select(zone17, contains('county'), SUM_e11:SUM_e92)

Emp_TAZ_grouped <- Emp1 %>% 
  group_by(zone17) %>% 
  summarise(county_fips, across(SUM_e11:SUM_e92, ~sum(.))) %>% 
  distinct()

Emp_TAZ_long <- Emp_TAZ_grouped %>% 
  pivot_longer(SUM_e11:SUM_e92, names_to = 'NAICS', values_to = 'Employment')

Emp_TAZ_long2 <- Emp_TAZ_long %>% 
  mutate(NAICS = parse_number(NAICS)) %>% 
  mutate(NAICS = as.character(NAICS)) %>% 
  distinct()

Zone_dict <- read_csv('lib/data/TAZ_System.csv') %>% 
  select(TAZ, Mesozone)


Emp_Control <- Emp_TAZ_long2 %>% 
  left_join(Zone_dict, by = c('zone17' = 'TAZ')) %>% 
  select(Zone17 = zone17, Mesozone, CountyFIPS = county_fips, NAICS, Employment) %>% 
  arrange(NAICS, Zone17) %>% 
  filter(!is.na(Zone17)) %>% 
  mutate(CountyFIPS = as.character(CountyFIPS))



#Process External
Emp_external <- read_xlsx('dev/Data_Processed/Future_Year_Inputs/xsubzonetm_211_2050.xlsx')

Emp1_external <- Emp_external %>% 
  select(subzone_id, num_jobs_sector_11:num_jobs_sector_92)

Zone_dict <- st_read('dev/Data_Processed/TAZ/subzones17.shp')

zonedict_1 <- Zone_dict %>%
  select(subzone17,zone17, county_fip) %>% 
  st_drop_geometry()



Emp_TAZ_external <- Emp1_external %>% 
  left_join(zonedict_1, by = c('subzone_id' = 'subzone17')) %>% 
  select(zone17, contains('county'), num_jobs_sector_11:num_jobs_sector_92)

Emp_TAZ_grouped_external <- Emp_TAZ_external %>% 
  group_by(zone17) %>% 
  summarise(county_fip, across(num_jobs_sector_11:num_jobs_sector_92, ~sum(.))) %>% 
  distinct()

Emp_TAZ_long_external <- Emp_TAZ_grouped_external %>% 
  pivot_longer(num_jobs_sector_11:num_jobs_sector_92, names_to = 'NAICS', values_to = 'Employment')

Emp_TAZ_long2_external <- Emp_TAZ_long_external %>% 
  mutate(NAICS = parse_number(NAICS)) %>% 
  mutate(NAICS = as.character(NAICS)) %>% 
  distinct()

Zone_dict <- read_csv('lib/data/TAZ_System.csv') %>% 
  select(TAZ, Mesozone)


Emp_Control_external <- Emp_TAZ_long2_external %>% 
  left_join(Zone_dict, by = c('zone17' = 'TAZ')) %>% 
  select(Zone17 = zone17, Mesozone, CountyFIPS = county_fip, NAICS, Employment) %>% 
  arrange(NAICS, Zone17) %>% 
  filter(!is.na(Zone17))



#Bind Interanl and Exteranl

CompleteEmp <- rbind(Emp_Control, Emp_Control_external) %>% 
  arrange(NAICS, Zone17)







write_csv(CompleteEmp, 'scenarios/future/inputs/data_emp_control_taz.csv')

