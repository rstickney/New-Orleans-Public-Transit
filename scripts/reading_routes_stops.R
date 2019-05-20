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
                         local = TRUE) %>%
  gtfs_as_sf()

##Will manually compare the clever stops and the GTFS stops
clever_stops <- read_csv("data//clever_94_stops_clean.csv") %>%
  na.omit() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

st_crs(clever_stops)
stops_list <- list()

stops_list[[1]] <- clever_stops
stops_list[[2]] <- broad_sf

mapview(stops_list)

leaflet(clever_stops) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers()

  test_join <- st_join(clever_stops, broad_sf)

trips$route_id %>%
  table %>%
  View()

filter(stops, route_id == 11559) %>%
  View()
mapview(new_orleans[])
nola_sf <- new_orleans %>% 
  gtfs_as_sf()

nola_route <- new_orleans %>%
  get_route_geometry()

mapview(nola_route)

str(new_orleans)
plot(new_orleans)
new_orleans$routes$route_id[32]

stops_sf <- nola_sf[[8]][[1]]
routes_sf <- nola_sf[[8]][[2]]

test <- st_intersects(stops, broad) 

stops[test] %>% mapview()
st_crs(stops)


class(broad)
?st_intersects
nola_sf[[9]] %>% mapview()


routes_sf$route_id_check <- new_orleans$routes$route_id
routes_sf$short_name <- new_orleans$routes$route_short_name

trips <- new_orleans[["trips"]]
stop_times <- new_orleans[["stop_times"]]
routes <- new_orleans[["routes"]]
stops <- new_orleans[["stops"]]

broad_route <- filter(routes_sf, short_name == "94") 

test_stop <- get_stop_frequency(new_orleans)

index <- st_buffer(broad, 1, endCapStyle = "FLAT") %>%
  st_contains(stops)

mapview(test_stop)
stop_times$stop_id %>% unique() %>% View()

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
#When you pass a list of sf objects to mapview it makes them layers you can manipulate
broad_all <- list()
broad_all[[1]] <- broad
broad_all[[2]] <- broad_route

#Finding distance between stops is already included in GTFS data:
#https://support.trilliumtransit.com/hc/en-us/articles/214415803-Using-GTFS-to-Find-Distance-Between-Stops-For-NTD- 

glimpse(stop_times)
stop_times %>% head(1000) %>% View()
mapview(broad_all)

View(index)
routes <- new_orleans[['routes_df']] 
new_orleans %>% map_gtfs(route_ids = routes)
new_orleans %>% map_gtfs()
outes <- NYC[['routes_df']] %>%
  slice(which(grepl('a|b', route_id, ignore.case=TRUE))) %>%
  '$'('route_id')

# take the NYC `gtfs` object and map routes. includes stops by default.
NYC %>% map_gtfs(route_ids = routes)
