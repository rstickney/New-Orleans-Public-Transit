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
  unique() %>%
  filter(!is.na(lat)) %>%
  group_by(SERIAL_NUMBER)  %>%#Create groups within the DF--serial_number is a unique trip  
  mutate(shape_dist_traveled = if_else(is.na(shape_dist_traveled), 0, shape_dist_traveled),
    diff = if_else(activity > 0,  #Conditional added in to adjust for when there are no passengers
                        difftime(lead(taa2, default = first(taa2)), tad2),
                        difftime(lead(taa2, default = first(taa2)), taa2)) %>% as.numeric(),
         dist = (lead(shape_dist_traveled)-shape_dist_traveled),
         speed = (dist*0.621371)/((diff %>% as.numeric())/3600)) %>%
  ungroup() %>%
  hablar::rationalize()

trial_pm_test_join <- trial %>%
  filter(TIME_PERIOD == "PM Peak", DIRECTION_NAME == "OUTBOUND", SERVICE_PERIOD == "Weekday",
         !is.na(TIME_ACTUAL_ARRIVE)) %>% #~500 from the parameters we're looking at have blank arrival times
  left_join(broad_outbound) %>%#There are ~11 stops that can't be accounted for in terms of why 
  #they're not in the GTFS feed
  unique() %>%
  filter(!is.na(lat)) %>%
  group_by(SERIAL_NUMBER)  %>%#Create groups within the DF--serial_number is a unique trip  
  mutate(shape_dist_traveled = if_else(is.na(shape_dist_traveled), 0, shape_dist_traveled),
         diff = if_else(activity > 0,  #Conditional added in to adjust for when there are no passengers
                        difftime(lead(taa2, default = first(taa2)), tad2),
                        difftime(lead(taa2, default = first(taa2)), taa2)) %>% as.numeric(),
         dist = (lead(shape_dist_traveled)-shape_dist_traveled),
         speed = (dist*0.621371)/((diff %>% as.numeric())/3600)) %>%
  ungroup() %>%
  hablar::rationalize() #Replaces Inf with NAs

####Grouping the results
#Using medians and averages as there a number of outliers 
#Therefore am separating to aggregate on the activity and speeds separately, then recombining them.
am_act <- ungroup(trial_am_test_join) %>%
  select(stop_sequence, shape_dist_traveled, stop.gtfs, stop_name, PASSENGERS_ON, PASSENGERS_OFF, activity) %>%
  group_by(stop_sequence, stop.gtfs, shape_dist_traveled, stop_name) %>%
  summarise(`Total Activity` = sum(activity, na.rm = TRUE),
            `Median Activity` = median(activity, na.rm = TRUE),
            `Average Activity` = mean(activity, na.rm = TRUE) %>% round(2)) %>%
  ungroup()
  
am_agg <-  ungroup(trial_am_test_join) %>%
  select(stop.gtfs, PASSENGERS_ON, PASSENGERS_OFF, speed) %>%
  filter(speed > 0 & speed < 60) %>%
  group_by(stop.gtfs) %>%
  summarise(
            `Speed to Next Stop (Med)` = median(speed, na.rm = TRUE) %>% round(2),
            `Speed to Next Stop (Avg)` = mean(speed, na.rm = TRUE) %>% round(2)) %>%
  ungroup() %>%
  right_join(am_act) %>%
  mutate(Direction = "INBOUND", 
    shape_dist_traveled = (shape_dist_traveled *0.621371) %>% round(2),
         `Distance to Next Stop` = (lead(shape_dist_traveled) - shape_dist_traveled) %>% round(2)) %>%
  select(Direction, `Stop ID` = stop.gtfs, `Stop Name`= stop_name, `Stop Sequence` = stop_sequence, `Total Activity`,
         `Average Activity`, `Median Activity`, `Speed to Next Stop (Avg)`, `Speed to Next Stop (Med)`,
         `Distance to Next Stop`, `Distance Traveled` = shape_dist_traveled)

pm_act <- ungroup(trial_pm_test_join) %>%
  select(stop_sequence, shape_dist_traveled, stop.gtfs, stop_name, PASSENGERS_ON, PASSENGERS_OFF, activity) %>%
  group_by(stop_sequence, stop.gtfs, shape_dist_traveled, stop_name) %>%
  summarise(`Total Activity` = sum(activity, na.rm = TRUE),
            `Median Activity` = median(activity, na.rm = TRUE),
            `Average Activity` = mean(activity, na.rm = TRUE) %>% round(2)) %>%
  ungroup()

pm_agg <-  ungroup(trial_pm_test_join) %>%
  select(stop.gtfs, PASSENGERS_ON, PASSENGERS_OFF, speed) %>%
  filter(speed > 0 & speed < 60) %>%
  group_by(stop.gtfs) %>%
  summarise(
    `Speed to Next Stop (Med)` = median(speed, na.rm = TRUE) %>% round(2),
    `Speed to Next Stop (Avg)` = mean(speed, na.rm = TRUE) %>% round(2)) %>%
  ungroup() %>%
  right_join(pm_act) %>%
  mutate(Direction = "OUTBOUND", 
         shape_dist_traveled = (shape_dist_traveled *0.621371) %>% round(2),
         `Distance to Next Stop` = (lead(shape_dist_traveled) - shape_dist_traveled) %>% round(2))  %>%
  select(Direction, `Stop ID` = stop.gtfs, `Stop Name`= stop_name, `Stop Sequence` = stop_sequence, `Total Activity`,
         `Average Activity`, `Median Activity`, `Speed to Next Stop (Avg)`, `Speed to Next Stop (Med)`,
         `Distance to Next Stop`, `Distance Traveled` = shape_dist_traveled)

filt_pm <- sum(pm_agg$`Total Activity`)
filt_am <- sum(am_agg$`Total Activity`)

filt_am/6816
filt_pm/7124

write_csv(am_agg, "Weekday Peak AM Summary Stats.csv")
write_csv(pm_agg, "Weekday Peak PM Summary Stats.csv")

###Create maps for these points
stops$`Stop ID` = stops$stop_id %>% as.numeric()
am_agg_sf <- am_agg %>%
  left_join(select(stops, stop_lat, stop_lon, `Stop ID`)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

pm_agg_sf <- pm_agg %>%
  left_join(select(stops, stop_lat, stop_lon, `Stop ID`)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

bike_lanes <- st_read("data/geo_export_d398f81d-a8b6-47a3-82ea-f1b5aa1ce614.shp")
bike_reduced <- select(bike_lanes, facilityty)

mapview(bike_reduced)
#Create a polyline of where a bus lane can be added
# what_we_created <- mapview() %>%
#   mapedit::editMap()
# 
# mapview(what_we_created$finished)
# Bus_lane_poss <- what_we_created$finished %>%
#   mutate(`Possible Bus Route` = "Orleans St. to Press Drive") %>%
#   select(`Possible Bus Route`, geometry)
#write_sf(Bus_lane_poss, "94_bus_lane_option.shp")



all_points <- list(am_agg_sf, pm_agg_sf, Bus_lane_poss, bike_reduced)
mapview(all_points)

##Have all of our points...now we need to actually create the (static) map
library(ggmap)
library(ggplot2)
library(ggspatial)
library(tmap)

library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf(data = am_agg_sf, size = sqrt(tot)) 

am_pm <- rbind(am_agg_sf, pm_agg_sf) 
  mutate(`Total Activity` = sqrt(`Total Activity`))

am_agg_sf$size <- sqrt(am_agg_sf$`Total Activity`)
tm_basemap(" CartoDB.Positron") +
tm_shape() +
  tm_dots(col = "Direction", palette = "YlOrBr", size = "Total Activity") +
  tm_polygons(Bus_lane_poss) +
  tm_layout(title = "Broad St. Bus Line")
?tm_basemap

ggplot(data = am_pm) + 
  geom_sf() +
  geom_sf(data = am_pm, aes(size = am_pm$`Total Activity`, color = am_pm$Direction)) +
  geom_jitter()
  geom_polygon(data = Bus_lane_poss)
?geom_sf

write_sf(am_pm, "shape files/inbound_outbound_stats.shp")
write_sf(Bus_lane_poss, "shape files/possible_bus_lane.shp")
write_sf(bike_reduced, "shape files/bike_lanes.shp")
