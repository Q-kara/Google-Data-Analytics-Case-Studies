library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

getwd()
setwd("/Users/Quewin/Documents/case study/Cyclistic Bike share/orignal")

Jan2021<-read.csv("202101-divvy-tripdata.csv")
Feb2021<-read.csv("202102-divvy-tripdata.csv")
Mar2021<-read.csv("202103-divvy-tripdata.csv")
Apr2021<-read.csv("202104-divvy-tripdata.csv")
May2021<-read.csv("202105-divvy-tripdata.csv")
Jun2021<-read.csv("202106-divvy-tripdata.csv")
Jul2021<-read.csv("202107-divvy-tripdata.csv")
Aug2021<-read.csv("202108-divvy-tripdata.csv")
Sep2021<-read.csv("202109-divvy-tripdata.csv")
Oct2021<-read.csv("202110-divvy-tripdata.csv")
Nov2021<-read.csv("202111-divvy-tripdata.csv")
Dec2021<-read.csv("202112-divvy-tripdata.csv")


all_trips<-bind_rows(Jan2021,Feb2021,Mar2021
                    ,Apr2021,May2021,Jun2021
                    ,Jul2021,Aug2021,Sep2021
                    ,Oct2021,Nov2021,Dec2021)

rm(Jan2021,Feb2021,Mar2021,Apr2021,May2021,Jun2021,Jul2021,Aug2021,Sep2021,Oct2021,Nov2021,Dec2021)

bike_trips_2021<-unique(all_trips)


bike_trips_2021$date <- as.Date(bike_trips_2021$started_at)
bike_trips_2021$month <- format(as.Date(bike_trips_2021$date), "%m")
bike_trips_2021$day <- format(as.Date(bike_trips_2021$date), "%d")
bike_trips_2021$year <- format(as.Date(bike_trips_2021$date), "%Y")
bike_trips_2021$day_of_week <- format(as.Date(bike_trips_2021$date), "%A")
bike_trips_2021$start_hour <- bike_trips_2021$started_at %>% ymd_hms() %>% hour()

bike_trips_2021$ride_length_in_seconds <- difftime(bike_trips_2021$ended_at,bike_trips_2021$started_at)
bike_trips_2021$ride_length_in_seconds <-as.numeric(as.character(bike_trips_2021$ride_length_in_seconds)) 

bike_trips_v2 <- bike_trips_2021 %>% filter(ride_length_in_seconds > 0)
print(paste("Number of rides less than  or equal to 0 are", nrow(bike_trips_2021)-nrow(bike_trips_v2)))

bike_trips_v2$month<-month.abb[as.numeric(bike_trips_v2$month)]
bike_trips_v2$month<-ordered(bike_trips_v2$month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct" ,"Nov", "Dec"))

bike_trips_v2$day_of_week<- ordered(bike_trips_v2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

summary(bike_trips_v2$ride_length_in_seconds)

aggregate(bike_trips_v2$ride_length_in_seconds ~ bike_trips_v2$member_casual,  FUN = mean)
aggregate(bike_trips_v2$ride_length_in_seconds ~ bike_trips_v2$member_casual,  FUN = median)
aggregate(bike_trips_v2$ride_length_in_seconds ~ bike_trips_v2$member_casual,  FUN = max)
aggregate(bike_trips_v2$ride_length_in_seconds ~ bike_trips_v2$member_casual,  FUN = min)

aggregate(bike_trips_v2$ride_length_in_seconds ~ bike_trips_v2$member_casual + bike_trips_v2$day_of_week, FUN = mean)

ggplot(bike_trips_v2, aes(member_casual, fill = member_casual))+
  geom_bar()+
  labs(title = "1) Customer Distribution: Casual X Members" , x = "Customer Type" ,y = "Number Of customers" , fill = "Type of customer")+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels = c("Casual Members", "Annual Members"))+
  scale_fill_discrete(labels = c("Casual Members", "Annual Members"))

ggplot(bike_trips_v2, aes( rideable_type, fill = rideable_type ))+
  geom_bar()+
  labs(title = "2) Bike Type Distribution", y = "Number of bikes in circulation", x = "Types of bikes", fill = "Type of Bike")+
  scale_y_continuous(labels = comma)+
  facet_wrap(~member_casual)+
  scale_fill_discrete(labels = c("Classic Bike", "Docked Bike", "Electric Bike"))+
  scale_x_discrete(labels = c("Classic Bike", "Docked Bike", "Electric Bike"))


bike_trips_v2 %>% group_by(month) %>% 
  ggplot(aes(month, fill = member_casual))+
  geom_bar(position = "dodge")+
  labs(title = "3) Most popular months", x = "Months", y =" Number of rides", fill = "Type of customer")+
  scale_y_continuous(labels = comma)+
  scale_fill_discrete(labels = c("Casual Member", "Annual Member"))

bike_trips_v2 %>% 
  mutate(weekday = wday(started_at,label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length_in_seconds)) %>%
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge")+
  labs(title = "4) Comparison of Customers during the week and the amount of rides",  x = 'Weekday', y ='Number of rides', fill = 'Type of customer')+
  scale_y_continuous(labels = comma)+
  scale_fill_discrete(labels = c("Casual Member", "Annual Member"))

hour_dis <-bike_trips_v2 %>% group_by(start_hour) %>%mutate(start_hour = as.factor(start_hour)) %>% 
  ggplot(aes(start_hour, fill = member_casual))+
  geom_bar(position = "dodge")+
  labs(title = "5) Most popular hours", x = "Ride starting hour", y = "Amount of rides", fill = "Type of customer")+
  scale_y_continuous(labels = comma)+
  scale_fill_discrete(labels = c("Casual Members", "Annual Members"))
hour_dis

hour_dis+facet_wrap(~day_of_week)+theme(axis.text.x = element_text(size = 8))
