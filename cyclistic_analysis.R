setwd("~/Desktop/Data Portfolio/Cyclistic Case Study")

install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

#Uploading my data sets
jun_2020 <- read_csv("202006-divvy-tripdata.csv")
jul_2020 <- read_csv("202007-divvy-tripdata.csv")
aug_2020 <- read_csv("202008-divvy-tripdata.csv")
sep_2020 <- read_csv("202009-divvy-tripdata.csv")
oct_2020 <- read_csv("202010-divvy-tripdata.csv")
nov_2020 <- read_csv("202011-divvy-tripdata.csv")
dec_2020 <- read_csv("202012-divvy-tripdata.csv")
jan_2021 <- read_csv("202101-divvy-tripdata.csv")
feb_2021 <- read_csv("202102-divvy-tripdata.csv")
mar_2021 <- read_csv("202103-divvy-tripdata.csv")
apr_2021 <- read_csv("202104-divvy-tripdata.csv")
may_2021 <- read_csv("202105-divvy-tripdata.csv")

#Making sure we have consistent column names
colnames(jun_2020)
colnames(jul_2020)
colnames(aug_2020)
colnames(sep_2020)
colnames(oct_2020)
colnames(nov_2020)
colnames(dec_2020)
colnames(jan_2021)
colnames(feb_2021)
colnames(mar_2021)
colnames(apr_2021)
colnames(may_2021)
#Looks like we do

#Now checking to make sure we have consistent data types for each column
str(jun_2020)
str(jul_2020)
str(aug_2020)
str(sep_2020)
str(oct_2020)
str(nov_2020)
str(dec_2020)
str(jan_2021)
str(feb_2021)
str(mar_2021)
str(apr_2021)
str(may_2021)

#converting start_station_id, end_station_id to chr
jun_2020 <- mutate(jun_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
jul_2020 <- mutate(jul_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
aug_2020 <- mutate(aug_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
sep_2020 <- mutate(sep_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
oct_2020 <- mutate(oct_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
nov_2020 <- mutate(nov_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))

#double checking I changed these before I stack them
glimpse(jun_2020)
glimpse(jul_2020)
glimpse(aug_2020)
glimpse(sep_2020)
glimpse(oct_2020)
glimpse(nov_2020)

#Stacking all 12 months into one data frame
all_trips12 <- bind_rows(jun_2020,jul_2020,aug_2020,sep_2020,oct_2020,nov_2020,
                         dec_2020,jan_2021,feb_2021,mar_2021,apr_2021,may_2021)

#Inspecting our new data
glimpse(all_trips12)
colnames(all_trips12)
nrow(all_trips12)
dim(all_trips12)
head(all_trips12)
str(all_trips12)
summary(all_trips12)

#Observations for each
table(all_trips12$member_casual)

#Adding columns for date, month, day, and year
all_trips12$date <- as.Date(all_trips12$started_at) #The default format is yyyy-mm-dd
all_trips12$month <- format(as.Date(all_trips12$date), "%m")
all_trips12$day <- format(as.Date(all_trips12$date), "%d")
all_trips12$year <- format(as.Date(all_trips12$date), "%Y")
all_trips12$day_of_week <- format(as.Date(all_trips12$date), "%A")

#Adding a ride length column
all_trips12$ride_length <- difftime(all_trips12$ended_at,all_trips12$started_at)

#Double checking columns
str(all_trips12)

#check
is.factor(all_trips12$ride_length)
#Changing ride_length to numeric so we can better use it
all_trips12$ride_length <- as.numeric(as.character(all_trips12$ride_length))
#check
is.numeric(all_trips12$ride_length)

#checking for wonky data
min(all_trips12$ride_length)

#Removing data that doesn't make sense (such as the negative data that we have 
# as a min)
#new version of data with underscore g for 'good'
all_trips12_g <- all_trips12[!(all_trips12$ride_length<0),]

#Some descriptive analysis (all in seconds)
#Gives shortest ride, longest ride, mean, median, and 1st & 3rd Quartiles
summary(all_trips12_g$ride_length) 

#Comparing members and casual users
aggregate(all_trips12_g$ride_length ~ all_trips12_g$member_casual, FUN = mean)
aggregate(all_trips12_g$ride_length ~ all_trips12_g$member_casual, FUN = median)
aggregate(all_trips12_g$ride_length ~ all_trips12_g$member_casual, FUN = max)
aggregate(all_trips12_g$ride_length ~ all_trips12_g$member_casual, FUN = min)

#Average ride time each day for members vs casual users
aggregate(all_trips12_g$ride_length ~ all_trips12_g$member_casual + 
            all_trips12_g$day_of_week, FUN = mean)

#Putting days of the week in order
all_trips12_g$day_of_week <- ordered(all_trips12_g$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", 
                                             "Wednesday", "Thursday", "Friday", 
                                             "Saturday"))

#Average ride time each day for members vs casual users (now in order)
aggregate(all_trips12_g$ride_length ~ all_trips12_g$member_casual + 
            all_trips12_g$day_of_week, FUN = mean)

#Analyzing ridership data by type and weekday
all_trips12_g %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%                  #groups by user type and weekday
  summarise(number_of_rides = n()                       #calculates the number of rides 
            ,average_duration = mean(ride_length)) %>%  #calculates the average duration
  arrange(member_casual, weekday)                       #sorts

#Creating a visual for number of rides by rider type
all_trips12_g %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Number of Rides by Rider Type per Day", 
                                      caption = "Data Source: Divvy (Chicago's Bike Share Program)",
                                      x = "Day of the Week",
                                      y = "Number of Rides")

#Visual for average duration
all_trips12_g %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + scale_y_continuous("Average Duration") + 
  labs(title = "Average Ride Duration by Rider Type per Day", caption = "Data Source: Divvy (Chicago's Bike Share Program)",
       x = "Day of The Week")

#Creating a csv file I can use for further visualization
ride_counts <- aggregate(all_trips12_g$ride_length ~ all_trips12_g$member_casual +
                           all_trips12_g$day_of_week, FUN = mean)
write.csv(ride_counts, file = '~/Desktop/Data Portfolio/Cyclistic Case Study/cyclistic_avg_ride_length.csv')




