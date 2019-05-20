library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(chron)

#temp = list.files(pattern="*")
#myfiles = lapply(temp, read_xlsx)

#Need to read in the actual stop IDs first, as the ones from the Clever are wrong for some reason
stop_ids <- read_csv("data/clever_94_stops_tt_id.csv") %>%
  mutate(stop.clever = `Travel Time ID` %>% as.numeric, #Create a join var. for the 
         stop.gtfs = `Stop ID` %>% as.numeric) 

broad <- read_xlsx("data/RT94.xlsx") %>%
  mutate(stop.clever = STOP_ID %>% as.numeric()) %>%
  left_join(select(stop_ids, stop.clever, stop.gtfs))



##Clean up the time-stamp data (currently adds in a date due to it being done in excel); 
#convert to a time object

##Create a quick function to save space
#We need to convert it into a full date-time object to properly difference it later
tf <- function(x, y){
  time <- format(x, "%H:%M:%S") 
  date_time <- paste(y, time) %>% as.POSIXct(format="%Y-%m-%d %H:%M:%S")
}

#Remove excess variables to start:

trial <- filter(broad, TIMEPOINT == 0) %>% #Filter out timepoints that are -1, as they are false
  #(there is some type of error in clever...)
  select(stop.gtfs, SERIAL_NUMBER, SORT_ORDER, SURVEY_DATE, SERVICE_PERIOD, TIME_PERIOD, TRIP_START_TIME,
         DIRECTION_NAME, ROUTE_NUMBER, STOP_ID, MAIN_CROSS_STREET, SEGMENT_MILES,
         TIME_SCHEDULED, TIME_ACTUAL_ARRIVE, TIME_ACTUAL_DEPART,
         PASSENGERS_ON, PASSENGERS_OFF, PASSENGERS_SPOT) %>%
  mutate(tst2 = tf(TRIP_START_TIME, SURVEY_DATE),
         taa2 = tf(TIME_ACTUAL_ARRIVE, SURVEY_DATE),
         tad2 = tf(TIME_ACTUAL_DEPART, SURVEY_DATE),
         activity = PASSENGERS_OFF + PASSENGERS_ON,
         stop_id = as.character(STOP_ID)) 


broad_trips$stop_sequence %>%
  table() %>%
  View()

broa

filter(broad_outbound, stop_sequence > 83) %>%
  select(stop_sequence, shape_dist_traveled, stop_name) %>%
  unique() %>%
  View()

glimpse(broad_trips)

trial_am$STOP_ID %>%
  table() %>%
  View()

table(broad_trips$shape_id)

broad_inbound <- filter(broad_trips, shape_id == 52747) %>%
  select(-arrival_time)
broad_outbound <- filter(broad_trips, shape_id == 52748) %>%
  select(-arrival_time)

broad_outbound %>%
  select(stop_id, stop_sequence, shape_dist_traveled, stop_name) %>%
  unique() %>%
  View()

glimpse(broad_inbound)
table(broad_inbound$stop_sequence) %>%
  View()


##This will join the stops to their proper points, but more importantly has the distance they are along
#the route. With this will need to eliminate any points that have "shape_dist_traveled" as NA,
#then get the diff time between the remaining stops, and figure out the speed
trial_am_jn <- broad_inbound %>%
  select(stop_id, stop_sequence, shape_dist_traveled, stop_lat, stop_lon) %>%
  unique() %>%
  mutate(stop.gtfs = as.numeric(stop_id)) %>%
  right_join(trial_am, by = "stop.gtfs")

glimpse(broad_inbound)

select(trial_am_jn, SEGMENT_MILES, shape_dist_traveled) %>%
  unique() %>%
  View()

filter(broad_sh1, stop_sequence < 4) %>%
  select(stop_sequence, shape_dist_traveled, stop_name) %>%
  unique() %>%
  View()
