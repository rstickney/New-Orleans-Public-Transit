library(data.table)
library(tidyverse)
library(tidytransit)
library(mapview)
library(sf)

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
new_orleans <- read_gtfs("data\\2018 - Winter B - G.zip", 
                         local = TRUE) %>%
  gtfs_as_sf()


nola_sf <- new_orleans %>% 
  gtfs_as_sf()

nola_route <- new_orleans %>%
  get_route_geometry()

str(new_orleans)
plot(new_orleans)

new_orleans$routes$route_id[32]

stops_sf <- nola_sf[[9]][[1]]
routes_sf <- nola_sf[[9]][[2]]

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

#Join everything to isolate the broad street stops
nola =  select(routes, route_type, route_short_name,route_id) %>% 
  inner_join(select(trips, route_id, trip_id)) %>% 
  inner_join(select(stop_times, trip_id, stop_id)) %>% 
  select(-trip_id) %>% unique() %>% 
  inner_join(select(stops, stop_id, stop_name, lat=stop_lat, lon=stop_lon)) %>% 
  unique()

broad <- filter(nola, route_short_name == "94") %>%
  inner_join(stops_sf) %>%
  st_as_sf()

#When you pass a list of sf objects to mapview it makes them layers you can manipulate
broad_all <- list()
broad_all[[1]] <- broad
broad_all[[2]] <- broad_route

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
