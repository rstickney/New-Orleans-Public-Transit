library(data.table)
library(tidyverse)
library(tidytransit)
library(mapview)
library(sf)
library(leaflet)

# devtools::install_github('ropensci/gtfsr')
# #adapted from: https://rpubs.com/data_feelings/data607_gtfs
# #List all of the files that are contained in the RTA document downloaded
# list.files("RTA Google")
# 
# tidytransit::set_api_key()
# #API Key is 84f6f28e-daeb-486e-aeda-05391e02773f
# 
# feedlist_df <- get_feedlist() %>%
#   filter(grepl('NORTA GTFS', t, ignore.case= TRUE))

#Load in the GTFS data for NORTA (is saved in google drive)
new_orleans <- read_gtfs("data//norta.zip", 
                         local = TRUE) 

##Will manually compare the clever stops and the GTFS stops
clever_stops <- read_csv("data//clever_94_stops_clean.csv") %>%
  na.omit() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


trips <- new_orleans[["trips"]]
stop_times <- new_orleans[["stop_times"]]
routes <- new_orleans[["routes"]]
stops <- new_orleans[["stops"]]

#Join everything to isolate the broad street stops
nola =  select(routes, route_type, route_short_name,route_id) %>% 
  inner_join(select(trips, route_id, trip_id, shape_id)) %>% 
  inner_join(select(stop_times, trip_id, stop_id, stop_sequence, shape_dist_traveled)) %>% 
  select(-trip_id) %>% unique() %>% 
  inner_join(select(stops, stop_id, stop_name, stop_desc, lat=stop_lat, lon=stop_lon)) %>%
  unique() %>%
  mutate(stop.gtfs = as.numeric(stop_id)) %>%
  select(route_short_name, shape_id, stop.gtfs, stop_sequence, shape_dist_traveled,
         stop_name, stop_desc, stop_name, lat, lon)

broad_sf <- filter(nola, route_short_name == "94") 

broad_inbound <- filter(broad_sf, shape_id == 52747)

broad_outbound <- filter(broad_sf, shape_id == 52748)

glimpse(broad_inbound)

##Next need to join the AM/PM routes to the broad inbound/outbound
trial_am_test_join <- trial %>%
  filter(TIME_PERIOD == "AM Peak", DIRECTION_NAME == "INBOUND", SERVICE_PERIOD == "Weekday",
         !is.na(TIME_ACTUAL_ARRIVE)) %>% #~500 from the parameters we're looking at have blank arrival times
  left_join(broad_inbound) %>%#There are ~11 stops that can't be accounted for in terms of why 
                           #they're not in the GTFS feed
  filter(!is.na(lat)) %>%
  group_by(SERIAL_NUMBER)  %>%#Create groups within the DF--serial_number is a unique trip  
  mutate(diff = if_else(activity > 0,  #Conditional added in to adjust for when there are no passengers
                        difftime(lead(taa2, default = first(taa2)), tad2),
                        difftime(lead(taa2, default = first(taa2)), taa2)) %>% as.numeric(),
         dist = shape_dist_traveled - lag(shape_dist_traveled),
         speed = (dist*0.621371)/((diff %>% as.numeric())/3600))
        
table(trial_am_test_join$stop_name, trial_am_test_join$diff) %>%
  as.data.frame() %>%
  filter(Freq > 0) %>%
    View()

glimpse(trial_am_test_join)
  glimpse(broad_inbound)
missing <- filter(trial_am_test_join, is.na(lat)) %>%
  select(MAIN_CROSS_STREET, activity, SORT_ORDER, TRIP_START_TIME, taa2, tad2, STOP_ID )


trial_am$activity %>% 
  group_by(SERIAL_NUMBER)  %>%#Create groups within the DF--serial_number is a unique trip  
  mutate(diff = if_else(activity > 0,  #Conditional added in to adjust for when there are no passengers
                        difftime(lead(taa2, default = first(taa2)), tad2),
                        difftime(lead(taa2, default = first(taa2)), taa2)) %>% as.numeric(),
         speed = SEGMENT_MILES/((diff %>% as.numeric())/3600))

glimpse(trial_am)

