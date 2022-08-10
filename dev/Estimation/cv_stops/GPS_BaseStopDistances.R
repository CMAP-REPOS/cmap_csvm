# Takes CVGPS Data, assigns a home base TAZ, attaches sTAZ, and calculates great circle distance to each stop

# Setup -------------------------------------------------------------------
source("./dev/init_dev.R")
library(readxl)
gps <- read_xlsx('dev/Data_Processed/CVGPS/TripsMediumLight_OD_ISPE83_SZPlusCounties_ValuesOnly_forRSG.xlsx')
skim <- 
SYSTEM_DEV_DATA_PATH <- file.path(SYSTEM_DEV_PATH, 'DATA_Processed')


# Classifying Internal External --------------------------------------------



# Assigning Home sTAZ -----------------------------------------------------
unqiue_deviceIds <- unique(gps$DeviceId) #35421

#Counts origin by sTAZ where begin_tour == TRUE
OriginStop_byID <- gps %>%
  filter(`Begin Tour - After Error Checking` == TRUE) %>% 
  group_by(DeviceId, Origin_SZ17PlusCounties) %>% 
  summarise(n = n())


#for each deviceID, chooses the sTAZ with the highest number of tour_start/origin, choose either origin sTAZ
Home_sTAZ_origin <- OriginStop_byID %>% 
  group_by(DeviceId) %>% 
  slice(which.max(n))

#chooses those with n>1, a clear home sTAZ
ClearHome <- Home_sTAZ_origin %>% 
  filter(n > 1) %>% 
  select(DeviceId, Origin_SZ17PlusCounties)

rm(OriginStop_byID)




# Checking Ambiguous Homes ------------------------------------------------
#where n !> 1
noClearHome <- Home_sTAZ_origin %>% 
  filter(n == 1)

noClearHome_full <- gps %>% 
  filter(DeviceId %in% noClearHome$DeviceId) %>%
  filter()

rm(noClearHome, Home_sTAZ_origin)


#checking for devices with no clear base but just one tour and setting home TAZ to origin of tour
oneTour <- noClearHome_full %>% 
  group_by(DeviceId) %>% 
  summarise(n = n_distinct(`Tour ID`)) %>% 
  filter(n == 1)

oneTour_full <- gps %>% 
  filter(DeviceId %in% oneTour$DeviceId)



OneTour_home <- oneTour_full %>% 
  filter(`Begin Tour - After Error Checking` == TRUE) %>% 
  select(DeviceId, Origin_SZ17PlusCounties)





#for those multiple tours and no clear base
HomeIds_goodBase <- OneTour_home %>% 
  rbind(ClearHome)
nrow(HomeIds_goodBase) #31,946/35,421

MultipleTours_noClearHome <- noClearHome_full %>% 
  filter(!(DeviceId %in% HomeIds_goodBase$DeviceId))



rm(oneTour, noClearHome_full, oneTour_full, OneTour_home)








# GPS_wBaseTAZ ------------------------------------------------------------
gps_wBaseTAZ <- gps %>% 
  right_join(HomeIds_goodBase, by = 'DeviceId') %>% 
  rename(., BaseTAZ = Origin_SZ17PlusCounties.y) %>% 
  select(1:3,6, 20, 7,11:13)
rm(gps)
rm(HomeIds_goodBase)

# Read in Shapefiles ------------------------------------------------------
SYSTEM_DEV_DATA_PATH <- file.path(SYSTEM_DEV_PATH, 'DATA_Processed')
sz <- read_sf(file.path(SYSTEM_DEV_PATH,'Data_Processed','TAZ', 'subzones17.shp'))
st_crs(sz) #EPSG, 26771

#give it a centroid

sz_centroid <- sz %>% 
  mutate(geometry_centroid = st_centroid(st_geometry(.))) %>%
  rowwise() %>% 
  mutate(x = st_coordinates(geometry_centroid)[1],
         y = st_coordinates(geometry_centroid)[2])









sz_simple <- sz_centroid %>% 
  select(subzone17, zone17, x, y, geometry_centroid, geometry)

rm(sz_centroid)

# Join Shapes to GPS ------------------------------------------------------

gps_wShapes_0 <- gps_wBaseTAZ %>% 
  left_join(sz_simple, by = c('BaseTAZ' = 'subzone17'))

gps_shapes <- gps_wShapes_0 %>% 
  left_join(sz_simple, by = c('Destination_SZ17PlusCounties' = 'subzone17')) %>% 
  rename(., c(TAZ_base = zone17.x, 
              geometry_base = geometry.x,
              TAZ_dest = zone17.y,
              geometry_dest = geometry.y,
              base_x = x.x,
              base_y = y.x,
              dest_x = x.y,
              dest_y = y.y,
              geometry_centroid_base = geometry_centroid.x,
              geometry_centroid_dest = geometry_centroid.y
              ))




gps_shapes <- gps_shapes %>% 
  mutate(distance = st_distance(geometry_centroid_base, geometry_centroid_dest, by_element = T))

gps_shapes_miles <- gps_shapes %>% 
  mutate(distance_miles = distance/5280)

rm(gps_wShapes_0)


# Classifying OD (Internal/External) --------------------------------------
gps_shapes_miles_class <- gps_shapes_miles %>% 
  mutate(HOMETAZ_TYPE = case_when(as.integer(substr(TAZ_base,1,1)) == 9 & nchar(TAZ_base) == 6 ~ 'COUNTY',
                                  is.na(TAZ_base) ~ 'Unclear Base',
                                  T ~ 'TAZ')) %>%
  mutate(DTAZ_TYPE = case_when(as.integer(substr(TAZ_dest,1,1)) == 9 & nchar(TAZ_dest) == 6 ~ 'COUNTY',
                               is.na(TAZ_dest) ~ NA_character_,
                                  T ~ 'TAZ')) %>% 
  mutate(HOME_type = case_when(HOMETAZ_TYPE == 'TAZ' ~ 'I',
                               HOMETAZ_TYPE == 'Unclear Base' ~ HOMETAZ_TYPE,
                               T ~ 'X')) %>% 
  mutate(DEST_type = case_when(DTAZ_TYPE == 'TAZ' ~ 'I',
                               is.na(DTAZ_TYPE) ~ DTAZ_TYPE,
                               T ~ 'X'))
  


# Tab ---------------------------------------------------------------------


 gps_shapes_miles_class %>%
   group_by(HOME_type) %>% 
   summarise(n_trips = n(), dist_miles = mean(as.numeric(distance_miles), na.rm = T))
