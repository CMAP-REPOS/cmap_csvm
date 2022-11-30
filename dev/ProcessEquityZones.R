library(tidyverse)
equityZones <- read_csv('dev/Data_Processed/EquityZones/exc_pop_shr_mo50.txt', col_names = F)

equityZones <- equityZones %>% 
  mutate(X2 = gsub(' all:', ',', X1)) %>% 
  select(-X1) %>% 
  separate(X2,c('TAZ','ExPopProp'), sep = ',') %>% 
  mutate(ExPopProp = as.numeric(ExPopProp)) %>% 
  rename(zone17 = TAZ)



write_csv(equityZones, 'lib/data/data_equity_zones.csv')
