

#Install and load all the required packages

install.packages("tidyverse")
install.packages("lubridate")
library("lubridate")
library("tidyverse")
library(data.table)
library(dplyr)
library(readr)




#Import and combine data
june2021 <- read_csv("2021-2022-Divvy-tripdata/202106-divvy-tripdata.csv")
july2021 <- read_csv("2021-2022-Divvy-tripdata/202107-divvy-tripdata.csv")
aug2021 <- read_csv("2021-2022-Divvy-tripdata/202108-divvy-tripdata.csv")
sep2021 <- read_csv("2021-2022-Divvy-tripdata/202109-divvy-tripdata.csv")
oct2021 <- read_csv("2021-2022-Divvy-tripdata/202110-divvy-tripdata.csv")
nov2021 <- read_csv("2021-2022-Divvy-tripdata/202111-divvy-tripdata.csv")
dec2021 <- read_csv("2021-2022-Divvy-tripdata/202112-divvy-tripdata.csv")
jan2022 <- read_csv("2021-2022-Divvy-tripdata/202201-divvy-tripdata.csv")
feb2022 <- read_csv("2021-2022-Divvy-tripdata/202202-divvy-tripdata.csv")
mar2022 <- read_csv("2021-2022-Divvy-tripdata/202203-divvy-tripdata.csv")
april2022 <- read_csv("2021-2022-Divvy-tripdata/202204-divvy-tripdata.csv")
may2022 <- read_csv("2021-2022-Divvy-tripdata/202205-divvy-tripdata.csv")

View(may2022)

#check the data structure
str(june2021)
str(july2021)
str(aug2021)
str(sep2021)
str(oct2021)
str(nov2021)
str(dec2021)
str(jan2022)
str(feb2022)
str(mar2022)
str(april2022)
str(may2022)

# change started_at and ended_at column data from chr datatype to dttm

feb2022 <- mutate(
  feb2022, 
  started_at = as.POSIXct(started_at),
  ended_at = as.POSIXct(ended_at)
)

#merge the data frames into one

whole_yr_trips <-bind_rows(june2021,july2021,aug2021,sep2021,oct2021,nov2021,dec2021,
                           jan2022,feb2022,mar2022,april2022,may2022)


View(whole_yr_trips)

#calculate the ride length

whole_yr_trips$ride_length <- difftime(
  whole_yr_trips$ended_at, 
  whole_yr_trips$started_at,
  units = "secs"
)

#convert to numerical. by default the ride_length would be chr data type

whole_yr_trips$ride_length <- as.numeric(
  as.character(whole_yr_trips$ride_length)
)

# remove entries with invalid ride_length

whole_yr_trips_cleaned <- whole_yr_trips %>%
  filter(!(ride_length < 0))

#remove entries with null start and end station name

whole_yr_trips_cleaned <- whole_yr_trips_cleaned %>%
  filter(
    !(is.na(start_station_name) |
        start_station_name == "")
  ) %>% 
  
  filter(
    !(is.na(end_station_name) |
        end_station_name == "")
  )
#remove duplicate entries
whole_yr_trips_cleaned[!duplicated(whole_yr_trips_cleaned$ride_id), ]
View(whole_yr_trips_cleaned) 
#check ride types
ride_type <-whole_yr_trips_cleaned %>%
  
  mutate(year = year(started_at), ) %>%
  
  group_by(rideable_type) %>%
  
  select(rideable_type, year) %>%
  
  count(rideable_type)

View(ride_type)

#add day of the week
whole_yr_trips_cleaned$weekday <- wday(whole_yr_trips_cleaned$started_at, label=TRUE)
View(whole_yr_trips_cleaned)

#aggregate data into days of the week
days_of_week <- whole_yr_trips_cleaned %>% 
  group_by(weekday) %>% 
  count(weekday) 
View(days_of_week)

#check unique station names

unique_stations <- whole_yr_trips_cleaned %>% 
  group_by(start_station_name) %>% 
  count(start_station_name) 
View(unique_stations)


#count of monthly rides
montly_rides <- whole_yr_trips_cleaned %>% 
  group_by(month(started_at)) %>% 
  count(ride_id) %>% 
  arrange(month(started_at))
View(montly_rides)
#members or casual count
member_or_casual_count <- whole_yr_trips_cleaned %>% 
  group_by(member_casual) %>% 
  count(member_casual)
View(member_or_casual_count)


#save all new data frames
write.csv(whole_yr_trips_cleaned,'whole_yr_trips_cleaned.csv')
write.csv(days_of_week,'day_of_week_data.csv')
write.csv(ride_type,'ride_type_count.csv')
write.csv(member_or_casual_count,'member_or_casual_count.csv')









