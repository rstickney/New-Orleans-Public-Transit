library(tidyverse)
library(data.table)
library(readxl)

setwd("H:\\RTA\\New-Orleans-Public-Transit\\data\\clever data all\\Bus Routes_Jan01.15-01.29\\Bus Routes_Jan01.15-01.29")
temp = list.files(pattern = "*.XLSX") 
myfiles = lapply(temp, read_xlsx, sheet = 1)


transit <- do.call(rbind.data.frame, myfiles)

test_files <- myfiles[1:3]

test_run <- lapply(myfiles, function(x){select(x, SERIAL_NUMBER,
                   SCHEDULE_ID,
                   SCHEDULE_NAME,
                   SURVEY_DATE,
                   PATTERN_ID,
                   ROUTE_NAME,
                   DIRECTION_NAME,
                   TRIP_START_TIME,
                   TIME_PERIOD,
                   SERVICE_PERIOD,
                   SORT_ORDER,
                   STOP_ID,
                   MAIN_CROSS_STREET,
                   TIMEPOINT,
                   SEGMENT_MILES,
                   TIME_ACTUAL_ARRIVE,
                   TIME_ACTUAL_DEPART,
                   PASSENGERS_ON,
                   PASSENGERS_OFF,
                   PASSENGERS_IN,
                   WHEELCHAIRS,
                   BICYCLES)})

test_bind <- do.call(rbind.data.frame, test_run) %>%
  filter(TIMEPOINT != -1) %>%
  mutate(TRIP_START_TIME = tf(TRIP_START_TIME),
         TIME_ACTUAL_ARRIVE = tf(TIME_ACTUAL_ARRIVE),
         TIME_ACTUAL_DEPART = tf(TIME_ACTUAL_DEPART))

         
setwd("H:\\RTA\\New-Orleans-Public-Transit\\data\\clever data all")
write_csv(test_bind, "rta_sample_data.csv")

#Function to create the date-time objects
tf <- function(x){
  time <- format(x, "%H:%M:%S") 
  }

##Creating a stream graph
devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)
library(lubridate)


test_bind %>%
  mutate(time = hms(TIME_ACTUAL_ARRIVE)) %>%
  group_by(ROUTE_NAME, DIRECTION_NAME) %>%
  tally(wt=PASSENGERS_IN) %>%
  streamgraph(key = "ROUTE_NAME", value = "PASSENGERS_IN", date = "time")

stops <- test_bind %>%
  select(ROUTE_NAME, DIRECTION_NAME, SORT_ORDER, MAIN_CROSS_STREET, STOP_ID) %>%
  unique()

filter(test_bind, is.na(MAIN_CROSS_STREET)) %>%
  View()

filter(stops, is.na(MAIN_CROSS_STREET)) %>%
  View()
