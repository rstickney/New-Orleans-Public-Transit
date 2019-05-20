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






