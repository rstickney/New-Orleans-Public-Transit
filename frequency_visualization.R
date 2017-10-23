rm(list = ls())
setwd("C:\\Users\\rober\\Google Drive\\NOLA Transport\\Transit data\\RTA_GTFSDataFeed")

library(data.table)
library(plyr)
library(dplyr)
library(readr)

#adapted from: https://rpubs.com/data_feelings/data607_gtfs
#List all of the files that are contained in the RTA document downloaded
list.files("RTA Google")

#Import the individual text files (they are linked to one another)
routes = read_csv("RTA Google/routes.txt",trim_ws = T)
stops = read_csv("RTA Google/stops.txt",trim_ws = T)
trips = read_csv("RTA Google/trips.txt",trim_ws = T)
#transfers = read_csv("RTA Google/transfers.txt",trim_ws = T)
agency = read_csv("RTA Google/agency.txt",trim_ws = T)
stoptime = read_csv("RTA Google/stop_times.txt",trim_ws = T)

#install.packages('knitr', dependencies = TRUE)
#(dynamic reporting package)
library(knitr)
kable(head(stops))

library(leaflet)

# Get coordinates for each stop
nola =  select(routes, route_type, route_short_name,route_id) %>% 
  inner_join(select(trips, route_id, trip_id)) %>% 
  inner_join(select(stoptime, trip_id, stop_id)) %>% 
  select(-trip_id) %>% unique() %>% 
  inner_join(select(stops, stop_id, stop_name, lat=stop_lat, lon=stop_lon)) %>% 
  unique()

nola_modes = data.frame(route_type = c(0, 3), 
                        mot = c("Streetcar", "Bus"))

nola$route_type = factor(nola$route_type)
nola_modes$route_type = factor(nola_modes$route_type)
nola = left_join(nola, nola_modes)

factpal=colorFactor(palette = c("#91148d", "#006400"), 
                    domain = nola$mot, levels = levels(nola$mot), ordered = FALSE, na.color = "#808080", alpha = FALSE)


####Split the stoptimes into three unique time-frames--
##1) 6 am - 7 pm
##2) 7 pm - 12 am
##3) 12 am - 6 am

####Turn all of this into a function

period_counts <- function(x, y, z){
  
  library(lubridate)
  daytime_cnt <- stoptime[!is.na(stoptime$departure_time),] #for some reason there are a lot of NAs on the departure time...need to investigate
  daytime_cnt$departure_time <- hms(daytime_cnt$departure_time)
  
  #Create the parameters for start and end of a day
  day_start <- hms(x) 
  day_end <- hms(y)
  
  daytime_cnt <- daytime_cnt[daytime_cnt$departure_time < day_end,]
  daytime_cnt <- daytime_cnt[daytime_cnt$departure_time > day_start,]
  
  # Get coordinates for each stop and counts of trips passing through each stop
  nola_day_cnt = select(routes, route_type, route_short_name,route_id) %>% 
    inner_join(select(trips, route_id, trip_id)) %>% 
    inner_join(select(daytime_cnt, trip_id, stop_id)) %>% 
    group_by(stop_id) %>% 
    summarise(cnt=n()) %>% 
    inner_join(select(stops, stop_id, stop_name, lat=stop_lat, lon=stop_lon)) %>% 
    left_join(select(nola, stop_id, route_type, route_id, route_short_name)) %>%
    unique()
  
  nola_modes = data.frame(route_type = c(0, 3), 
                          mot = c("Streetcar", "Bus"))
  
  nola$route_type = factor(nola$route_type)
  nola_modes$route_type = factor(nola_modes$route_type)
  nola = left_join(nola, nola_modes)
  
  #Adding the "Streetcar" & "Bus" designation to each route type
  nola_day_cnt$route_type = factor(nola_day_cnt$route_type)
  nola_day_cnt = left_join(nola_day_cnt, nola_modes)
  
  ##Get the average per hour for each stop--note that this isn't it's ACTUAL frequency every hour, but presumably the 
  #collective average from the year
  nola_day_cnt$cnt_average <- (nola_day_cnt$cnt)/z
  
  return(nola_day_cnt) 
}

#1) Daytime: 6 am- 7 pm----
period_start <- "06:00:00"
period_end   <- "18:59:00"
total_hrs    <- 13

day_cnt <-period_counts(period_start, period_end, total_hrs)

#write.csv(test2, "test.csv")

#2) 

period_start <- "19:00:00"
period_end   <- "23:59:00"
total_hrs    <- 5

evening_cnt <- period_counts(period_start, period_end, total_hrs)


#3) 

late_start <- "00:00:00"
late_end   <- "05:59:59"
total_late <- 6

late_cnt <- period_counts(late_start, late_end, total_late)

#write.csv(late_night, "late_norta.csv")

####combining day_cnt, evening_cnt, and late_cnt into a single data frame

##First create a new variable for them that identifies their time of day (will make things easier later)
day_cnt$period <- "Daytime (6 am - 7 pm)"
evening_cnt$period <- "Evening (7 pm - 12 am)"
late_cnt$period <- "Late Night (12 am - 6 am)"

#Combine the 3 data frames to one
all_stop_cnt <- rbind(day_cnt, evening_cnt, late_cnt)

#Create percentile distributions for the relative avg frequency per hour
all_stop_cnt$avg_pct <- all_stop_cnt$cnt_average/max(all_stop_cnt$cnt_average)

# #Create a new Variable Defining frequency according to percentile
# all_stop_cnt$frequency_label <- cut(all_stop_cnt$avg_pct, breaks = 5, 
#                       labels = c("Lowest", "Lower", "Medium", "Higher", "Highest"))

all_stop_cnt$frequency_number <- cut(all_stop_cnt$avg_pct, breaks = 10, 
                                    labels = 1:10)

write.csv(all_stop_cnt, "norta_full_cnt.csv")


library(leaflet)
library(knitr)

#Creating bins for the cnts in each time
test <- all_stop_cnt
test$frequency_number <- as.numeric(test$frequency_number)

##Current map uses frequency number, done by simple breaks

#Map without Kenner
test1 <- filter(test, route_short_name != "201" & route_short_name != "202")

##Creating it as a function in case wanted to do different time splits.
mapmaker <- function(x){
  #Split the input data frame into a list containing the dataframes the each contain a unique period value
  x.df <- split(x, x$period)
  
  #Create the basemap layer, using the "CartoDB" map
  l <- leaflet() %>% addProviderTiles("CartoDB")
  
  #Adding the circle markers from each data frame in the list
  names(x.df) %>%
    purrr::walk( function(df){
      l <<- l %>%
        addCircleMarkers(data = x.df[[df]],
                         lng = ~lon, lat = ~lat,
                         stroke = F,
                         fillOpacity = 0.5,
                         radius = ~(frequency_number),
                         color = ~factpal(mot),
                         group = df,
                         label = ~route_short_name,
                         labelOptions = labelOptions(noHide = F,
                                                     direction = 'auto')) #%>%
#        addMarkers(~lon, ~lat, label = ~route_short_name)
      
    })
  
  l <- l %>% #Adds layer controls for the map, allowing you to select between the three time-periods
    addLayersControl(
      baseGroups = names(x.df), #baseGroups is to select one at a time. To layer them, use overlayGroups 
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend(colors = c("#91148d", "#006400"),
              labels = levels(test$mot), title = "Mode of Transport")
  return(l)
}

new_test <- mapmaker(test1)
