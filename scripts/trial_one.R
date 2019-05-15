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
                PASSENGERS_ON, PASSENGERS_OFF, PASSENGERS_SPOT)
##Clean up the time-stamp data (currently adds in a date due to it being done in excel); 
#convert to a time object

##Create a quick function to save space
#We need to convert it into a full date-time object to properly difference it later
tf <- function(x, y){
  time <- format(x, "%H:%M:%S") 
  date_time <- paste(y, time) %>% as.POSIXct(format="%Y-%m-%d %H:%M:%S")
  }

result <- map(trial$TIME_ACTUAL_ARRIVE, tf, y = test_map$SURVEY_DATE)

result <- test_map$TIME_ACTUAL_ARRIVE %>%
  tf(test_map$SURVEY_DATE)

library(purrr)
test_map <- head(trial_gr, 10) %>%
  nest()
test_map[[1]]
##Create unique identifier that groups by:
  # -ROUTE_NUMBER
  # -SERVICE_PERIOD
  # -TIME_PERIOD
  # -DIRECTION_NAME
  # -SURVEY_DATE

##Use mutate to create multiple variables at once
#When bus arrives to an actual scheduled stop, there time 
trial_am <- trial %>%
  filter(TIME_PERIOD == "AM Peak", DIRECTION_NAME == "INBOUND", SERVICE_PERIOD == "Weekday",
         !is.na(TIME_ACTUAL_ARRIVE)) %>% #~500 from the parameters we're looking at have blank arrival times
  mutate(tst2 = tf(TRIP_START_TIME, SURVEY_DATE),
         taa2 = tf(TIME_ACTUAL_ARRIVE, SURVEY_DATE),
         tad2 = tf(TIME_ACTUAL_DEPART, SURVEY_DATE),
         id = paste0(ROUTE_NUMBER,"_", SERVICE_PERIOD, "_", TIME_PERIOD, "_", 
                     DIRECTION_NAME,"_", SURVEY_DATE)) %>% #Create a unique ID for reference)
  group_by(SERIAL_NUMBER) #Create groups within the DF--serial_number is a unique trip  
  nest()


travel_func <- function(x, y, z){
  lead(x[,y], default = first(x[,y])) - x[,z]
}

travel_func(trial_am, "taa2", "tad2")

test_map <- modify(trial_am, ~travel_func, y = "taa2", z = "tad2")
View(table(trial_am$STOP_ID))

#Am attempting to get the actual time difference on here, but have two problems:
#1) Group by on the serial number and date isn't working...will need to turn it into a nested DF
#2) The speeds that are emerging are by no means realist. Will need to re-measure the distance?
ott <- trial_am %>%
  group_by(SERIAL_NUMBER, SURVEY_DATE, TRIP_START_TIME) %>%
  mutate(diff = difftime(lead(taa2, default = first(taa2)), tad2),
         speed = SEGMENT_MILES/(as.numeric(diff)/3600))
  select(STOP_ID, MAIN_CROSS_STREET, SEGMENT_MILES, taa2, tad2, diff, speed, PASSENGERS_ON, PASSENGERS_OFF)


summary(ott$diff)
summary(trial_am)
range(ott$diff)
unique(trial_am$tst2)


trial_gr <- trial %>%
  mutate(tst2 = tf(TRIP_START_TIME, SURVEY_DATE),
         taa2 = tf(TIME_ACTUAL_ARRIVE, SURVEY_DATE),
         tad2 = tf(TIME_ACTUAL_DEPART, SURVEY_DATE),
         id = paste0(ROUTE_NUMBER,"_", SERVICE_PERIOD, "_", TIME_PERIOD, "_", 
                     DIRECTION_NAME,"_", SURVEY_DATE)) %>% #Create a unique ID for reference)
  group_by(ROUTE_NUMBER, TIME_PERIOD, 
           DIRECTION_NAME, SURVEY_DATE) #Create groups within the DF


trial$DIRECTION_NAME %>% as.factor() %>% summary()
glimpse(trial_gr)

###Calculate the 3 base time amounts we are looking for:
  # 1) Overall travel time (arrival time next stop - arrival time current stop)
  # 2) Dwell time (departure time current stop - arrival time current stop)
  # 3) Travel time (arrival time next stop - departure time current stop)

#1) Overall travel time
overall <- filter(trial, id == "94_Weekday_AM Peak_INBOUND_2019-01-22") %>%
  mutate(diff = lead(taa2) - taa2)

odd <- filter(overall, diff < 0)
glimpse(overall)
min(odd$diff)
lead(overall$taa2) - overall$taa2  

ott <- overall %>%
  group_by(id) %>%
  arrange(SURVEY_DATE)
  mutate(diff = lead(taa2, default = first(taa2)) - taa2)
lead(trial_am$taa2)
  glimpse(ott)

diff_df <- trial_am %>%
  filter(SURVEY_DATE == ymd("2019-01-15"), TRIP_START_TIME == ymd_hms("1899-12-31 07:52:00")) %>%
  group_by(SERIAL_NUMBER, tst2) %>%
  summarize(difference = difftime(last(taa2), first(taa2), units = "hour"))



