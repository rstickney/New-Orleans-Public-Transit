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
  left_join(select(stop_ids, stop.id, stop.gtfs))

select(broad, STOP_ID, MAIN_CROSS_STREET) %>%
  unique() %>%
  View()

broad %>%
  select(SORT_ORDER, STOP_ID, MAIN_CROSS_STREET, DIRECTION_NAME) %>%
  unique() %>%
  View()
  

filter(broad, DIRECTION_NAME == "INBOUND") %>%
  select(SORT_ORDER, STOP_ID, MAIN_CROSS_STREET) %>%
  unique() %>%
  write_csv("clever_stops_inbound.csv")

filter(broad, DIRECTION_NAME == "OUTBOUND") %>%
  select(SORT_ORDER, STOP_ID, MAIN_CROSS_STREET) %>%
  unique() %>%
  write_csv("clever_stops_outbound.csv")
##Clean up the time-stamp data (currently adds in a date due to it being done in excel); 
#convert to a time object

##Create a quick function to save space
#We need to convert it into a full date-time object to properly difference it later
tf <- function(x, y){
  time <- format(x, "%H:%M:%S") 
  date_time <- paste(y, time) %>% as.POSIXct(format="%Y-%m-%d %H:%M:%S")
}

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
summary(broad)
trial <- filter(broad, TIMEPOINT == 0) %>% #Filter out timepoints that are -1, as they are false
                                          #(there is some type of error in clever...)
  select(SERIAL_NUMBER, SORT_ORDER, SURVEY_DATE, SERVICE_PERIOD, TIME_PERIOD, TRIP_START_TIME,
                DIRECTION_NAME, ROUTE_NUMBER, STOP_ID, MAIN_CROSS_STREET, SEGMENT_MILES,
                TIME_SCHEDULED, TIME_ACTUAL_ARRIVE, TIME_ACTUAL_DEPART,
                PASSENGERS_ON, PASSENGERS_OFF, PASSENGERS_SPOT) %>%
  mutate(tst2 = tf(TRIP_START_TIME, SURVEY_DATE),
         taa2 = tf(TIME_ACTUAL_ARRIVE, SURVEY_DATE),
         tad2 = tf(TIME_ACTUAL_DEPART, SURVEY_DATE),
         activity = PASSENGERS_OFF + PASSENGERS_ON,
         stop_id = as.character(STOP_ID))

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


###Aggregating the boarding and travel time info--method works very quickly and well
#need to investigate the negative travel tiems on it and get a better sense of the data
#Until then convert it into NAs where appropriate
trial_am <- trial_am %>%
  mutate(diff = if_else(diff <0 , 0, diff)) %>%
  naniar::replace_with_na(replace = list(diff = 0))


test_agg <- ungroup(trial_am) %>%
  select(SORT_ORDER, stop_id, MAIN_CROSS_STREET, PASSENGERS_ON, PASSENGERS_OFF, activity, diff) %>%
  group_by(SORT_ORDER, stop_id, MAIN_CROSS_STREET) %>%
  summarise(mean_activity = sum(activity, na.rm = TRUE),
            mean_diff = mean(diff, na.rm = TRUE))


##Can clearly see that the stop IDs don't match up with the ones from Arrival time data
test_merge <- stops_sf %>%
  merge(test_agg) %>%
  select(stop_id, stop_name, MAIN_CROSS_STREET)


glimpse(test_merge)


sum(test_agg$mean_diff, na.rm = TRUE) #Seems to be a reasonable total travel time
trial_am <- trial_am %>%
  mutate(diff = if_else(diff <0 , 0, diff)) %>%
  naniar::replace_with_na(replace = list(diff = 0))



?naniar::replace_with_na_if
trial_agg <- trial_am %>%
  group_by(SERIAL_NUMBER) %>%
  summarise((diff2 = if_else(activity > 0,  #Conditional added in to adjust for when there are no passengers
            difftime(lead(taa2, default = first(taa2)), tad2),
            difftime(lead(taa2, default = first(taa2)), taa2)) %>% as.numeric()))
  
glimpse(trial_am)
filter(trial_am, STOP_ID == 835)$SERIAL_NUMBER %>% table() %>% View()
glimpse(broad)
broad$SCHEDULE_ID %>% table()
