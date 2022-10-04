library(tidyverse)
skims_cng <- readRDS('scenarios/base/outputs/skims_tod_vehicle.rds') %>% 
  as_tibble()
skims_ff <- readRDS('scenarios/base_freeflow/outputs/skims_tod_vehicle.rds') %>% 
  as_tibble()


#want
#max toll




# Mean Time & dist & toll -------------------------------------------------------------------
skims_cng[grepl('toll', names(skims_cng))][skims_cng[grepl('toll', names(skims_cng))] == 0] <- NA
skims_ff[grepl('toll', names(skims_ff))][skims_ff[grepl('toll', names(skims_ff))] == 0] <- NA

cng_summary_1 <- skims_cng %>% 
  group_by(vehicle) %>% 
  summarise(across(3:26, ~mean(.x, na.rm = T))) %>% 
  pivot_longer(!vehicle, names_to = "skim", values_to = 'mean') %>% 
  separate(.,col = skim, into = c('skim', 'period'), sep = '\\.')

ff_summary_1 <- skims_ff %>% 
  group_by(vehicle) %>% 
  summarise(across(3:26, ~mean(.x, na.rm = T))) %>% 
  pivot_longer(!vehicle, names_to = "skim", values_to = 'mean') %>% 
  separate(.,col = skim, into = c('skim', 'period'), sep = '\\.')

ff_cng_means <- cng_summary_1 %>% 
  full_join(ff_summary_1, by = c('vehicle', 'skim', 'period')) %>% 
  rename(mean_cng = mean.x, 
         mean_ff = mean.y) %>% 
  mutate(diff = mean_cng - mean_ff)

write_csv(ff_cng_means, 'ff_cng_means.csv')

view(ff_cng_means)








# Medians time, dist, toll -----------------------------------------------------------------

cng_summary_2 <- skims_cng %>% 
  group_by(vehicle) %>% 
  summarise(across(3:26, ~median(.x, na.rm = T))) %>% 
  pivot_longer(!vehicle, names_to = "skim", values_to = 'median') %>% 
  separate(.,col = skim, into = c('skim', 'period'), sep = '\\.')

ff_summary_2 <- skims_ff %>% 
  group_by(vehicle) %>% 
  summarise(across(3:26, ~median(.x, na.rm = T))) %>% 
  pivot_longer(!vehicle, names_to = "skim", values_to = 'median') %>% 
  separate(.,col = skim, into = c('skim', 'period'), sep = '\\.')

ff_cng_medians <- cng_summary_2 %>% 
  full_join(ff_summary_2, by = c('vehicle', 'skim', 'period')) %>% 
  rename(median_cng = median.x, 
         median_ff = median.y) %>% 
  mutate(diff = median_cng - median_ff)

view(ff_cng_medians)


write_csv(ff_cng_medians, 'ff_cng_medians.csv')





# speed -------------------------------------------------------------------


cng_summary_3 <- skims_cng %>% 
  group_by(vehicle) %>% 
  summarise(across(3:26, ~sum(.x))) %>% 
  pivot_longer(!vehicle, names_to = 'skim', values_to = 'sum') %>% 
  separate(., col = skim, into = c('skim', 'period'), sep = '\\.') %>% 
  filter(!is.na(sum)) %>%
  pivot_wider(names_from = skim, values_from = sum) %>% 
  mutate(speed = dist/time*60) %>% 
  select(vehicle, period,
         speed_congested = speed)

ff_summary_3 <- skims_ff %>% 
  group_by(vehicle) %>% 
  summarise(across(3:26, ~sum(.x))) %>% 
  pivot_longer(!vehicle, names_to = 'skim', values_to = 'sum') %>% 
  separate(., col = skim, into = c('skim', 'period'), sep = '\\.') %>% 
  filter(!is.na(sum)) %>%
  pivot_wider(names_from = skim, values_from = sum) %>% 
  mutate(speed = dist/time*60) %>% 
  select(vehicle, period,
         speed_freeflow = speed)


ff_cng_speeds <- cng_summary_3 %>% 
  full_join(ff_summary_3, by = c('vehicle', 'period')) %>% 
  mutate(diff = speed_congested - speed_freeflow)

write_csv(ff_cng_speeds, 'ff_cng_meanSpeed.csv')


# max toll ----------------------------------------------------------------
cng_summary_4 <- skims_cng %>% 
  group_by(vehicle) %>% 
  summarise(across(3:26, ~max(.x, na.rm = T))) %>% 
  pivot_longer(!vehicle, names_to = 'skim', values_to = 'max') %>% 
  separate(., col = skim, into = c('skim', 'period'), sep = '\\.') %>%
  pivot_wider(names_from = skim, values_from = max) %>% 
  select(vehicle, period, 
         maxToll_congested = toll)

ff_summary_4 <- skims_ff %>% 
  group_by(vehicle) %>% 
  summarise(across(3:26, ~max(.x, na.rm = T))) %>% 
  pivot_longer(!vehicle, names_to = 'skim', values_to = 'max') %>% 
  separate(., col = skim, into = c('skim', 'period'), sep = '\\.') %>%
  pivot_wider(names_from = skim, values_from = max) %>% 
  select(vehicle, period, 
         maxToll_freeflow = toll)

ff_cng_maxToll <- cng_summary_4 %>% 
  full_join(ff_summary_4, by = c('vehicle', 'period'))

write_csv(ff_cng_maxToll, 'ff_cng_maxToll.csv')




# min/max speed -----------------------------------------------------------

all_speeds_cng <- skims_cng %>% 
  select(-contains('toll')) %>% 
  mutate(speed_p1 = dist.P1/time.P1*60,
         speed_p2 = dist.P2/time.P2*60,
         speed_p3 = dist.P3/time.P3*60,
         speed_p4 = dist.P4/time.P4*60,
         speed_p5 = dist.P5/time.P5*60,
         speed_p6 = dist.P6/time.P6*60,
         speed_p7 = dist.P7/time.P7*60,
         speed_p8 = dist.P8/time.P8*60) %>%
    select(-c(time.P1:dist.avg))


all_speeds_ff <- skims_ff %>% 
  select(-contains('toll')) %>% 
  mutate(speed_p1 = dist.P1/time.P1*60,
         speed_p2 = dist.P2/time.P2*60,
         speed_p3 = dist.P3/time.P3*60,
         speed_p4 = dist.P4/time.P4*60,
         speed_p5 = dist.P5/time.P5*60,
         speed_p6 = dist.P6/time.P6*60,
         speed_p7 = dist.P7/time.P7*60,
         speed_p8 = dist.P8/time.P8*60) %>%
  select(-c(time.P1:dist.avg))


cng_summary_5 <- all_speeds_cng %>% 
  group_by(vehicle) %>% 
  summarise(min = across(3:10, ~min(.x, na.rm = T)), max = across(3:10, ~max(.x, na.rm = T))) %>% 
  pivot_longer(!vehicle, names_to = 'skim', values_to = 'value') %>% 
  as_tibble()

write.csv(cng_summary_5, 'cng_minMax_speeds.csv')


ff_summary_5 <- all_speeds_ff %>% 
  group_by(vehicle) %>% 
  summarise(min = across(3:10, ~min(.x, na.rm = T)), max = across(3:10, ~max(.x, na.rm = T))) %>% 
  pivot_longer(!vehicle, names_to = 'skim', values_to = 'value') %>% 
  as_tibble()

write.csv(ff_summary_5, 'ff_minMax_speeds.csv')




# # no tolls --------------------------------------------------------------
cng_summary_6 <- skims_cng %>%
  select(contains('vehicle')|contains('toll')) %>% 
  group_by(vehicle) %>% 
  summarise(across(1:8, ~sum(is.na(.)))) %>% 
  pivot_longer(!vehicle, names_to = 'skim', values_to = 'na') %>% 
  mutate(p = na/13315201)
write.csv(cng_summary_6, 'cng_nTolls.csv')


ff_summary_6 <- skims_ff %>%
  select(contains('vehicle')|contains('toll')) %>% 
  group_by(vehicle) %>% 
  summarise(across(1:8, ~sum(is.na(.)))) %>% 
  pivot_longer(!vehicle, names_to = 'skim', values_to = 'na') %>% 
  mutate(p = na/13315201)

write.csv(ff_summary_6, 'ff_nTolls.csv')

