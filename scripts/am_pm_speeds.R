setwd("C:\\Users\\rober\\Dropbox\\Transit Data\\Bus Routes_Jan01.15-01.29")

library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(chron)

#temp = list.files(pattern="*")
#myfiles = lapply(temp, read_xlsx)
broad <- read_xlsx("RT94.xlsx")


#Remove excess variables to start:
#Keep:
  # -SURVEY_DATE
  # -SERVICE_PERIOD
  # -TIME_PERIOD
  # -TRIP_START_TIME
  # -DIRECTION_NAME
  # -ROUTE_NUMBER
  # -STOP_ID
  # -MAIN_CROSS_STREET
  # -SEGMENT_MILES
  # -TIME_SCHEDULED
  # -TIME_ACTUAL_ARRIVE
  # -TIME_ACTUAL_DEPART
  # -PASSENGERS_ON
  # -PASSENGERS_OFF
  # -PASSENGERS_SPOT
  
trial <- filter(broad, TIMEPOINT == 0) %>% #Filter out timepoints that are -1, as they are false
                                          #(there is some type of error in clever...)
  select(SERIAL_NUMBER, SORT_ORDER, SURVEY_DATE, SERVICE_PERIOD, TIME_PERIOD, TRIP_START_TIME,
                DIRECTION_NAME, ROUTE_NUMBER, STOP_ID, MAIN_CROSS_STREET, SEGMENT_MILES,
                TIME_SCHEDULED, TIME_ACTUAL_ARRIVE, TIME_ACTUAL_DEPART,
                PASSENGERS_ON, PASSENGERS_OFF, PASSENGERS_SPOT) %>%
  mutate(tst2 = tf(TRIP_START_TIME, SURVEY_DATE),
         taa2 = tf(TIME_ACTUAL_ARRIVE, SURVEY_DATE),
         tad2 = tf(TIME_ACTUAL_DEPART, SURVEY_DATE),
         activity = PASSENGERS_OFF + PASSENGERS_ON)

##Clean up the time-stamp data (currently adds in a date due to it being done in excel); 
#convert to a time object

##Create a quick function to save space
#We need to convert it into a full date-time object to properly difference it later
tf <- function(x, y){
  time <- format(x, "%H:%M:%S") 
  date_time <- paste(y, time) %>% as.POSIXct(format="%Y-%m-%d %H:%M:%S")
  }


##Use mutate to create multiple variables at once
#When bus arrives to an actual scheduled stop, there time 
trial_am <- trial %>%
  filter(TIME_PERIOD == "AM Peak", DIRECTION_NAME == "INBOUND", SERVICE_PERIOD == "Weekday",
         !is.na(TIME_ACTUAL_ARRIVE)) %>% #~500 from the parameters we're looking at have blank arrival times
  group_by(SERIAL_NUMBER)  %>%#Create groups within the DF--serial_number is a unique trip  
  mutate(diff = if_else(activity > 0,  #Conditional added in to adjust for when there are no passengers
                        difftime(lead(taa2, default = first(taa2)), tad2),
                        difftime(lead(taa2, default = first(taa2)), taa2)) %>% as.numeric(),
         speed = SEGMENT_MILES/((diff %>% as.numeric())/3600))

trial_pm <- trial %>%
  filter(TIME_PERIOD == "PM Peak", DIRECTION_NAME == "OUTBOUND", SERVICE_PERIOD == "Weekday",
         !is.na(TIME_ACTUAL_ARRIVE)) %>% #~500 from the parameters we're looking at have blank arrival times
  group_by(SERIAL_NUMBER)  %>%
  mutate(diff = if_else(activity > 0,  #Conditional added in to adjust for when there are no passengers
                        difftime(lead(taa2, default = first(taa2)), tad2),
                        difftime(lead(taa2, default = first(taa2)), taa2)) %>% as.numeric(),
         speed = SEGMENT_MILES/((diff %>% as.numeric())/3600))
         
examine <- filter(trial_am, SERIAL_NUMBER == 831629 | 
         SERIAL_NUMBER == 834981 | 
         SERIAL_NUMBER == 836333)  
range(trial_am$diff)
range(trial_pm$diff)
