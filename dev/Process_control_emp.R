
library(tidyverse)
library(readxl)
library(sf)

Emp <- read_csv('dev/Data_Processed/Future_Year_Inputs/r211_parcelSum_empsBySZ_2050.csv')

Emp1 <- Emp %>% 
  select(subzone_id, SUM_e11:SUM_e92)

Zone_dict <- st_read('dev/Data_Processed/TAZ/subzones17.shp')

zonedict_1 <- Zone_dict %>%
  select(subzone17,zone17, county_fip) %>% 
  st_drop_geometry()



Emp_TAZ <- Emp1 %>% 
  left_join(zonedict_1, by = c('subzone_id' = 'subzone17')) %>% 
  select(zone17, contains('county'), SUM_e11:SUM_e92)

Emp_TAZ_grouped <- Emp_TAZ %>% 
  group_by(zone17) %>% 
  summarise(county_fip, across(SUM_e11:SUM_e92, ~sum(.))) %>% 
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
  select(Zone17 = zone17, Mesozone, CountyFIPS = county_fip, NAICS, Employment) %>% 
  arrange(NAICS, Zone17) %>% 
  filter(!is.na(Zone17))


write_csv(Emp_Control, 'scenarios/future/inputs/data_emp_control_taz.csv')

