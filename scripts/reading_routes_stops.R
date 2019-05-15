setwd("C:\\Users\\rober\\Google Drive\\Ride NOLA\\GTFS Data\\RTA_GTFSDataFeed")

library(data.table)
library(tidyverse)
library(tidytransit)
library(mapview)
library(sf)

devtools::install_github('ropensci/gtfsr')
#adapted from: https://rpubs.com/data_feelings/data607_gtfs
#List all of the files that are contained in the RTA document downloaded
list.files("RTA Google")

tidytransit::set_api_key()
#API Key is 84f6f28e-daeb-486e-aeda-05391e02773f

feedlist_df <- get_feedlist() %>%
  filter(grepl('NORTA GTFS', t, ignore.case= TRUE))

#Load in the GTFS data for NORTA (is saved in google drive)
new_orleans <- read_gtfs("C:\\Users\\rober\\Google Drive\\Ride NOLA\\GTFS Data\\RTA_GTFSDataFeed\\2018 - Winter B - G.zip", 
                         local = TRUE)

nola_sf <- new_orleans %>% 
  gtfs_as_sf()

nola_route <- new_orleans %>%
  get_route_geometry()

str(new_orleans)
plot(new_orleans)

new_orleans$routes$route_id[32]

stops <- nola_sf[[9]][[1]]
routes <- nola_sf[[9]][[2]]

test <- st_intersects(stops, broad) 

stops[test] %>% mapview()
st_crs(stops)


class(broad)
?st_intersects
nola_sf[[9]] %>% mapview()

broad <- filter(routes, short_name == "94") 
rm()
routes$route_id_check <- new_orleans$routes$route_id

routes <- new_orleans[['routes_df']] 
new_orleans %>% map_gtfs(route_ids = routes)
new_orleans %>% map_gtfs()
outes <- NYC[['routes_df']] %>%
  slice(which(grepl('a|b', route_id, ignore.case=TRUE))) %>%
  '$'('route_id')

# take the NYC `gtfs` object and map routes. includes stops by default.
NYC %>% map_gtfs(route_ids = routes)
