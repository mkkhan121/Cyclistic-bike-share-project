## install packages and load
install.packages("tidyverse")
install.packages("janitor")
install.packages("dplyr")
install.packages("plyr")
install.packages("ggmap")
install.packages("skimr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("readr")
install.packages("googlesheets4")
install.packages("imager")
install.packages("rmarkdown")

## Loading the packages
library(tidyverse)
library(janitor)
library(dplyr)
library(plyr)
library(ggmap)
library(skimr)
library(lubridate)
library(ggplot2)
library(readr)
library(googlesheets4)
library(imager)
library(rmarkdown)
getwd()

## Importing data in Rstudio
Jan22 <- read_csv("202201-divvy-tripdata.csv")
Feb22 <- read_csv("202202-divvy-tripdata.csv")
Mar22 <- read_csv("202203-divvy-tripdata.csv")
Apr22 <- read_csv("202204-divvy-tripdata.csv")
May22 <- read_csv("202205-divvy-tripdata.csv")
Jun22 <- read_csv("202206-divvy-tripdata.csv")
Jul22 <- read_csv("202207-divvy-tripdata.csv")
Aug22 <- read_csv("202208-divvy-tripdata.csv")
Sep22 <- read_csv("202209-divvy-publictripdata.csv")
Oct22 <- read_csv("202210-divvy-tripdata.csv")
Nov22 <- read_csv("202211-divvy-tripdata.csv")
Dec22 <- read_csv("202212-divvy-tripdata.csv")

## Checking columns consistency in all datasets
colnames(Jan22)
colnames(Feb22)
colnames(Mar22)
colnames(Apr22)
colnames(May22)
colnames(Jun22)
colnames(Jul22)
colnames(Aug22)
colnames(Sep22)
colnames(Oct22)
colnames(Sep22)
colnames(Oct22)
colnames(Nov22)
colnames(Dec22)

##Checking data structure
str(Jan22)
str(Feb22)
str(Mar22)
str(Apr22)
str(May22)
str(Jun22)  
str(Jul22)
str(Aug22)
str(Sep22)
str(Oct22)
str(Nov22)
str(Dec22)

## Combining all data sets into one large data frame using rbind function
bike_rides <- rbind(Jan22, Feb22, Mar22, Apr22, May22, Jun22, Jul22, Aug22, Sep22, Oct22, Nov22, Dec22)
## Removing individual data sets to free up memory
rm(Jan22, Feb22, Mar22, Apr22, May22, Jun22, Jul22, Aug22, Sep22, Oct22, Nov22, Dec22)
## Checking the new data frame
head(bike_rides)
class(bike_rides)
dim(bike_rides)
colSums(is.na(bike_rides))
summary(is.na(bike_rides))
colnames(bike_rides)
summary(bike_rides)
## Processing our data frame
dim(bike_rides)
# Removing the duplicates
bike_rides %>% distinct()
view(bike_rides)
dim(bike_rides)
str(bike_rides)
## Create date variables
bike_rides <- bike_rides %>% mutate(start_date = as.Date(bike_rides$started_at))
bike_rides <- bike_rides %>% mutate(end_date = as.Date(bike_rides$ended_at))
bike_rides <- bike_rides %>%
  mutate(
    Start_Yr = year(started_at),
    Start_Mth = month(started_at),
    Start_Day = wday(started_at),
    Start_Hr = hour(started_at)
  )

bike_rides <- bike_rides %>%
  mutate(
    End_Yr = year(ended_at),
    End_Mth = month(ended_at),
    End_Day = wday(ended_at),
    End_Hr = hour(ended_at))
  
##Adding Seasons column
bike_rides <- bike_rides %>% mutate(season = recode(Start_Mth,
                                                    `12` = "Winter",
                                                    `1` = "Winter",
                                                    `2` = "Winter",
                                                    `3` = "Spring",
                                                    `4` = "Spring",
                                                    `5` = "Spring",
                                                    `6` = "Summer",
                                                    `7` = "Summer",
                                                    `8` = "Summer",
                                                    `9` = "Fall",
                                                    `10` = "Fall",
                                                    `11` = "Fall"))
view(bike_rides)
colSums(is.na(bike_rides))
str(bike_rides)
# Lets explore where ride_length_min is greater than 24 hours(1440 mins)
sum(bike_rides$ride_length_min > 1440)
# Less than 1 minute
sum(bike_rides$ride_length_min < 1)
# To confirm any negative figure, use less than 0
sum(bike_rides$ride_length_min < 0)
# Greater than 6 hours(360 mins)
sum(bike_rides$ride_length_min > 360)
# removing ride_length_min > 24 hours and < 0 mins
bike_rides <- bike_rides %>% 
  filter(ride_length_min >= 0 & ride_length_min <= 1440)
## Addressing missing values
# checking if Na values in start and end_station_id pertains to any user and bike type
bike_rides %>% is.na(start_station_id) %>%
  count(start_station_id, start_station_name, bike_type, user_type)
bike_rides <- bike_rides %>%
  mutate(ride_length_min = difftime(ended_at,started_at,units='mins',2))
view(bike_rides)
dim(bike_rides)
str(bike_rides)
# renaming the columns
bike_rides <- rename(bike_rides, "bike_type" = "rideable_type", "user_type" = "member_casual")

## Addressing missing values
# checking if Na values in start and end_station_id pertains to any user and bike type
bike_rides %>% filter(is.na(start_station_id)) %>%
  count(start_station_id, start_station_name, bike_type, user_type)
# we can see here that electric bikes with mixed user types are linked to na values in start station id
# we can see here that electric bikes are associated with na values in start station id codes
# check for end station id codes
bike_rides %>% filter(is.na(end_station_id)) %>%
  count(end_station_id, end_station_name, bike_type, user_type)
# here three different bike types with both users pertains to na values

## Creating a column ride_length_sec
bike_rides %>% filter(ended_at < started_at) %>% count()
bike_rides %>% filter(ended_at > started_at) %>% count()
dim(bike_rides)
bike_rides$bike_held_min = NULL
bike_rides$units = NULL
# creating column ride_length_sec (in seconds)
bike_rides <- bike_rides %>%
  mutate(ride_length_sec = ended_at - started_at)
bike_rides$bike_held = NULL
str(bike_rides)

## Checking the missing date(Na) and removing it
summary(is.na(bike_rides))
colSums(is.na(bike_rides))
# since start/end station name and id and ending lat/lng data is immaterial, i would proceed removing them
bike_rides %>% drop_na()
colSums(is.na(bike_rides))
bike_rides <- bike_rides %>% filter(is.na(start_station_name)==F)
colSums(is.na(bike_rides))
# since start/end station name and id and ending lat/lng data is immaterial, i would proceed removing them
bike_rides <- bike_rides %>% filter(is.na(end_station_name)==F)
bike_rides <- bike_rides %>% filter(is.na(end_lat)==F)
colSums(is.na(bike_rides))
bike_rides$ride_length_sec = NULL
# creating column ride_length_hour (in hours)
bike_rides$ride_length_hour <- round(as.numeric(difftime(bike_rides$ended_at, bike_rides$started_at, units = "hours")), 2)
# converting bike_type and user_type as factors
bike_rides$bike_type <- as.factor(bike_rides$bike_type)
bike_rides$user_type <- as.factor(bike_rides$user_type)
str(bike_rides)

## Analyse our data frame
# At this phase, we will try differentiating member vs. casual riders to look for trends and patterns
## lets start with conducting descriptive analysis
# Removing rides greater than 24 hours as it is unlikely and less than 1 mins as they could be false start
# removing ride_length_min > 24 hours and < 1 mins
bike_rides <- bike_rides %>% filter(ride_length_min >= 1, ride_length_min < 1440)

# we will look for mean, median, max and min of our ride_length_min (minutes)
bike_rides %>%
  summarise(avg_ride_length = mean(ride_length_min), median_length = median(ride_length_min),
            max_length= max(ride_length_min), min_length = min(ride_length_min))
# lets summarise the descriptive stats by user_type
bike_rides %>% group_by(user_type) %>% 
  summarise(avg_ride_length = mean(ride_length_min), median_length = median(ride_length_min),
                                                 max_length= max(ride_length_min), min_length = min(ride_length_min))
# it can be seen that casual riders have almost double the average ride length compared to members and took longer rides than members

# let's see Total number of casual and member users
bike_rides %>%
  select(user_type) %>%
  group_by(user_type) %>%
  count() %>%
  arrange()
# there are 830,371 more member riders compared to casual riders
# let's visualize this data using a plot
ggplot(data = bike_rides) + geom_bar(mapping = aes(x = user_type, fill = user_type)) +
  labs(title = "No of users", x = "user type", y = "count")
# as can be seen there are more members compared to casual bike riders

## let's see count of each bike type
bike_rides %>%
  group_by(bike_type) %>% 
  summarise(count = length(ride_id))
# let's also visualize usage of different bike types by the users
ggplot(data = bike_rides) + geom_bar(mapping = aes(x = bike_type, fill = bike_type)) + facet_wrap(~user_type) +
  labs(title = "Bike type usage", x = "Bike_Type", y = "Count")
# members never used docked bikes while used mostly classic bikes
# casual riders used docked bikes alongside classic and electric bikes

## Analysing ride_length_min thoroughly
# let's see average ride length by user type
aggregate(bike_rides$ride_length_min ~ bike_rides$user_type, FUN = mean)
# shows that casual riders had taken twice more rides compared to members

# let's summarise ride_length_min by looking at ranges of minutes to see if anything stands out
bike_rides$user_type <- as.factor(bike_rides$user_type)
str(bike_rides$user_type)
bike_rides %>%
  group_by(user_type) %>%
  summarize("<=5min" = sum(ride_length_min <=5),
            "<=15min" = sum(ride_length_min <=15),
            "<=30min" = sum(ride_length_min <=30),
            "<=45min" = sum(ride_length_min <=45),
            "<=60min" = sum(ride_length_min <=60),
            ">2hrs" = sum(ride_length_min >120),
            ">4hrs" = sum(ride_length_min >240),
            ">6hrs" = sum(ride_length_min >360),) 

# i have done analysis in Google sheets to see how much % of ride length lies in each ride interval
kable(df[1:5, ], caption = "Rides interval analysis.")


# Key takeaways
# 100% of all the rides taken are 60 mins or less
# 73% of all the rides taken are 45 mins or less
# 23% of all the rides taken are 15 mins or less
# this data will greatly assist cyclistic in assessing the strategic direction

## let's see the user type by count and percentage
bike_rides %>%
  group_by(user_type) %>%
  summarise(count = n(), Percentage = n()/nrow(bike_rides)*100)

## let's visualize user type by total rides and by bike type in a plot
bike_rides %>%
  group_by(user_type, bike_type) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=user_type, y=count, fill=bike_type)) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x="Bike Type", y="Number of Rides", title = "Total Rides by user type and bike type")
# only casual riders used docked bikes whereas members used only classic and electric bikes

## I will analyse rides taken by users based on sequence starting with minutes, hours, week day, months and finally seasons
# let's create different ride length intervals alongside ride length interval column and then visualize
bike_rides <- bike_rides %>% mutate(ride_length_inter = case_when(
  ride_length_min <9.99 ~ "< 10 min",
  ride_length_min <14.99 ~ "< 15 min",
  ride_length_min >=15 & ride_length_min <=20.99 ~ "15-20 min",
  ride_length_min >=21 & ride_length_min <=30.99  ~ "21-30 min",
  ride_length_min >=31 & ride_length_min <=60.99  ~ "31-60 min",
  ride_length_min >=60 & ride_length_min <=120.99  ~ "61-120 min",
  ride_length_min >=121 & ride_length_min <=240.99  ~ "121-240 min",
  ride_length_min >=241  ~ "241+ min"))

# let's visualize the total rides taken by user type and ride length intervals
bike_rides %>%
  group_by(user_type, ride_length_inter) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=factor(ride_length_inter, level = c("< 10 min", "< 15 min", "15-20 min" ,"21-30 min", "31-60 min",
                                                   "61-120 min", "121-240 min", "241+ min")), y=count, fill=user_type)) +
  geom_col(position = "dodge") +
  labs(x="Ride Length intervals", y="Number of Rides", title = "Total Rides by user type and ride length intervals")
# key takeaways
# member riders took mostly shorter rides i.e. between 1-20 min rides
# casual riders took longer rides compared to members

## let's see average or mean of ride length for "< 10 min" interval
bike_rides %>% filter(ride_length_inter == "< 10 min") %>%
  group_by(user_type) %>%
  summarize(avg_ride_length=mean(ride_length_min))

# now average or mean of "< 20min" intervals
bike_rides %>% filter(ride_length_inter == "< 15 min" | ride_length_inter== "15-20 min") %>%
  group_by(user_type) %>%
  summarize(avg_ride_length=mean(ride_length_min))
# casual riders on average took longer rides under the 10 min and 15-20 min intervals

## let's visualize the demand for bikes based on hour of day
bike_rides %>%
  ggplot(aes(Start_Hr, fill= user_type)) +
  labs(x="Day Hour", title="Bike demand by day hour") +
  geom_bar()
# Key takeaways
# most demand for bikes is between 7am-8am and in the evening between 4pm-6pm for both user types

# let's visualize total number of rides by users based on day hour
bike_rides %>% 
  group_by(user_type, Start_Hr) %>% 
  summarise(count = n()) %>%  
  arrange(user_type, Start_Hr) %>% 
  ggplot(aes(x=factor(Start_Hr, level= c(6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,0,1,2,3,4,5)),
             y=count, fill=user_type)) + 
  geom_col(position = "dodge") +
  labs(x="Day hour", y="No of Rides", title = "Total rides by users vs. day hour")
# Key takeaways
# for both casual and member riders most number of rides are between 16pm-18pm

# let's also visualize average ride length in mins by users based on day hour
bike_rides %>% 
  group_by(user_type, Start_Hr) %>% 
  summarise(count = n(), average_ride_length=mean(ride_length_min)) %>% 
  arrange(user_type, Start_Hr) %>% 
  ggplot(aes(x=factor(Start_Hr, level= c(6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,0,1,2,3,4,5)),
             y=average_ride_length, fill=user_type)) + 
  geom_col(position = "dodge") +
  labs(x="Day hour", y="Avg ride length (in mins)", title = "Average rides by users and day hour")
# Key takeaways
# casual riders took longer rides on average in mins peaking from 10am-3pm
# member riders have consistent average rides in mins throughout the day

## let's add day time column
bike_rides <- bike_rides %>% mutate(day_time = case_when(
  Start_Hr >= 6 & Start_Hr < 9 ~ "Early Morning",
  Start_Hr >= 9 & Start_Hr < 12 ~ "Mid Morning",
  Start_Hr >= 12 & Start_Hr < 18  ~ "Afternoon",
  Start_Hr >= 18 & Start_Hr <= 23  ~ "Evening",
  Start_Hr >= 0 & Start_Hr < 3  ~ "Early Night",
  Start_Hr >= 3 & Start_Hr < 6  ~ "Late Night"))
bike_rides <- bike_rides %>% relocate(day_time, .before = season)

## let's visualize total number of rides by users based on day time
axis_labels <- c("Early Morning \n6am-9am", "Mid Morning \n9am-12pm", "Afternoon \n12pm-6pm", "Evening \n6pm-11pm", "Early Night \n11pm-3am", "late Night \n3am-6am")
bike_rides %>% 
  group_by(user_type, day_time) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x=factor(day_time, level= c("Early Morning", "Mid Morning", "Afternoon", "Evening", "Early Night", "Late Night")),
             y=count, fill=user_type)) + 
  geom_col(position = "dodge", width = 0.4) + 
  labs(x="Day time", y="Total Rides", title = "Total Rides by user type vs. Day time") +
  scale_x_discrete(labels = axis_labels)
# Key takeaways
# members took most number of rides early morning compared to casual riders
# both users took most rides in the afternoon and evening time

## let's now visualize average ride length in mins by users based on day time
axis_labels <- c("Early Morning \n6am-9am", "Mid Morning \n9am-12pm", "Afternoon \n12pm-6pm", "Evening \n6pm-11pm", "Early Night \n11pm-3am", "late Night \n3am-6am")
bike_rides %>% 
  group_by(user_type, day_time) %>% 
  summarise(count = n(), average_ride_length=mean(ride_length_min)) %>% 
  ggplot(aes(x=factor(day_time, level= c("Early Morning", "Mid Morning", "Afternoon", "Evening", "Early Night", "Late Night")),
             y=average_ride_length, fill=user_type)) + 
  geom_col(position = "dodge", width = 0.4) + 
  labs(x="Day time", y="Avg ride length in mins", title = "Avg Ride length in mins by users vs. Day time") +
  scale_x_discrete(labels = axis_labels)
# Key takeaways
# casual riders on average took longer rides in minutes than members peaking in mid-morning and afternoon

## let's convert month and day data from numerical to categorical for analysis based on week day
bike_rides <- bike_rides %>%
  mutate(Start_Mth = format(as.Date(started_at), "%B")) %>%
  mutate(Start_Day = format(as.Date(started_at), "%A")) %>%
  mutate(End_Mth = format(as.Date(ended_at), "%B")) %>%
  mutate(End_Day = format(as.Date(ended_at), "%A"))

str(bike_rides)

# let's order start_day column before proceeding to further analysis
bike_rides$Start_Day <- ordered(bike_rides$Start_Day, 
                                      levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


## let's see the average ride length in mins during only weekdays for both users
bike_rides  %>% filter(Start_Day != "Saturday" &
                         Start_Day != "Sunday") %>% group_by(user_type, Start_Hr) %>% 
  summarise(avg_ride_length_min = mean(ride_length_min),.groups="drop") %>% 
  ggplot(aes(x = Start_Hr, y = avg_ride_length_min)) + geom_point() + 
  geom_line(aes(group = user_type, colour = user_type)) + ylim(0, NA) + 
  ggtitle("Average ride length by hour on weekdays") + 
  labs(x = "Hour", y = "Avg ride length in mins")
# Key takeaways
# casual riders against members took longer rides on average during weekdays, peaking between 10am-12am

## let's now see average ride length by users during weekends only
bike_rides  %>% filter(Start_Day == "Saturday" |
                         Start_Day == "Sunday") %>% group_by(user_type, Start_Hr) %>% 
  summarise(avg_ride_length_min = mean(ride_length_min),.groups="drop") %>% 
  ggplot(aes(x = Start_Hr, y = avg_ride_length_min)) + geom_point() + 
  geom_line(aes(group = user_type, colour = user_type)) + ylim(0, NA) + 
  ggtitle("Average ride length by hour on weekends") + 
  labs(x = "Hour", y = "Avg ride length in mins")
# Key takeaways
# on weekends casual riders took longer rides on average in mins peaking between 10am-15pm

## let's analyse total rides and average rides taken by users based on the week day
bike_rides %>%
  group_by(user_type, Start_Day) %>%
  summarise(number_of_rides = n()
            ,avg_ride_length_min = mean(ride_length_min),.groups="drop") %>%
  arrange(user_type, Start_Day)
# let's see average rides by users based on week day side by side
aggregate(bike_rides$ride_length_min ~ bike_rides$user_type + bike_rides$Start_Day, FUN = mean)
# Key takeaways
# casual riders took more rides on the weekends where average ride length is also much higher than members
# member riders have more number of rides on the weekdays which could mean commuting to work etc.

# let's visualize number of rides and average rides taken by users based on the day of week
bike_rides %>%
  group_by(user_type, Start_Day) %>%
  summarise(number_of_rides = n(), .groups="drop") %>%
  arrange(user_type, Start_Day)  %>%
  ggplot(aes(x = Start_Day, y = number_of_rides, fill = user_type)) +
  labs(x="Week_day", y="number_of_rides", title ="Rides by users vs. Day of the week") +
  geom_col(width=0.4, position = position_dodge(width=0.4)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
# let's visualize the same by incorporating geom_point and line
bike_rides %>%
  group_by(user_type, Start_Day) %>%
  summarise(number_of_rides = n(), .groups="drop") %>%
  arrange(user_type, Start_Day)  %>%
  ggplot(aes(x=factor(Start_Day, level= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
             y=number_of_rides, color = user_type)) +
  geom_point() + geom_line(aes(group = user_type)) +
  labs(x="Week_day", y="number_of_rides", title ="Rides by users vs. Day of the week") +
  ylim(0, NA)

# let's now visualize average rides taken by users based on week day
bike_rides %>%  
  group_by(user_type, Start_Day) %>% 
  summarise(average_ride_length = mean(ride_length_min), .groups="drop") %>%
  ggplot(aes(x = Start_Day, y = average_ride_length, fill = user_type)) +
  geom_col(width=0.4, position = position_dodge(width=0.4)) + 
  labs(x="Week_day", y="Avg_ride_length_mins", title ="Average ride length by users Vs. Week day")
# Key takeaways
# casual riders on average took longer rides compared to members fluctuating between 20-25 mins
# members have consistent average rides throughout week i.e. between 10-15 min rides
# casual riders average ride length on weekends is much higher i.e. greater than 25 mins

## Let's visualize total rides by users in each month
# first let's order the months data
bike_rides$Start_Mth <- ordered(bike_rides$Start_Mth, 
                                levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
bike_rides %>%  
  group_by(user_type, Start_Mth) %>% 
  summarise(number_of_rides = n(),.groups="drop") %>% 
  arrange(user_type, Start_Mth)  %>% 
  ggplot(aes(x = Start_Mth, y = number_of_rides, fill = user_type)) +
  labs(title ="Total number of rides by users Vs. Month", x = "Month", y= "Total Rides") +
  theme(axis.text.x = element_text(angle = 50)) +
  geom_col(width=0.4, position = position_dodge(width=0.4)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
# Key takeaways
# members compared to casual riders have higher number of rides throughout the year
# casual riders took most number of rides in the summer season i.e. June and July
# members took most number of rides from May till September 

## Let's visualize average ride length in mins for both users
bike_rides %>%  
  group_by(user_type, Start_Mth) %>% 
  summarise(average_ride_length = mean(ride_length_min),.groups="drop") %>%
  arrange(user_type, Start_Mth)  %>% 
  ggplot(aes(x = Start_Mth, y = average_ride_length, fill = user_type)) +
  labs(title ="Average ride length by users Vs. Month", x = "Month", y= "Avg ride length in mins") +
  theme(axis.text.x = element_text(angle = 50)) +
  geom_col(width=0.4, position = position_dodge(width=0.4)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
# Key takeaways
# casual riders have more number of rides on average compared to members
# casual riders took most rides on average in minutes in the months March, April and May
# members took most number of rides on average in the months from May till September

## Let's visualize Total rides by each user type in each month side by side
bike_rides %>%
  group_by(user_type, Start_Mth) %>%
  summarise(number_of_rides = n(), .groups="drop") %>%
  arrange(user_type, Start_Mth)  %>%
  ggplot(aes(x = Start_Mth, y = number_of_rides,  fill = Start_Mth)) + 
  geom_col() + facet_wrap(~user_type) + 
  labs(title = "Total Rides per Month", x = "Month", y = "Total rides") +
  theme(axis.text.x = element_text(size = 4))
# Key takeaways
# casual riders had most number of rides in the summer period from June-August
# members have most number of rides from May-September as our previous analysis suggested

## Finally let's now visualize rides by users based on seasons of year
bike_rides %>% 
  group_by(user_type, season) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x=factor(season, level= c("Spring", "Summer", "Fall", "Winter")), y=count, fill=user_type)) + 
  geom_col(position = "dodge", width = 0.5) + 
  labs(x="Season", y="Total Rides", title = "Total Rides by user type vs. season")
# Key takeaways
# members have more bike rides in total compares to casual riders in every season of the year
# casual and member riders have most rides in the summer season
# member riders used most bikes in the summer and fall season 

## let's now focus our analysis to bike starting stations
# counting top 10 popular starting stations for casual riders
bike_rides %>% 
  filter(!(is.na(start_station_name))) %>% 
  filter(user_type == "casual") %>% 
  group_by(start_station_name) %>% 
  summarize(count=n()) %>% 
  arrange(-count) %>% top_n(10)
# counting top 10 popular starting stations for members
bike_rides %>% 
  filter(!(is.na(start_station_name))) %>% 
  filter(user_type == "member") %>% 
  group_by(start_station_name) %>% 
  summarize(count=n()) %>% 
  arrange(-count) %>% top_n(10)
# let's visualize top 10 popular starting stations for casual riders
bike_rides %>% 
  filter(!(is.na(start_station_name))) %>% 
  filter(user_type == "casual") %>% 
  group_by(start_station_name) %>% 
  summarize(count=n()) %>% 
  arrange(-count) %>% top_n(10) %>%
  mutate(start_station_name= fct_reorder(start_station_name, count)) %>% 
  ggplot(aes(x=start_station_name, y=count, fill=count)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_gradient(low="blue", high="red") +
  labs(x="Start Station Name", y="No of rides", title="Top 10 starting stations for casual riders")

# let's visualize top 10 popular starting stations for members
bike_rides %>% 
  filter(!(is.na(start_station_name))) %>% 
  filter(user_type == "member") %>% 
  group_by(start_station_name) %>% 
  summarize(count=n()) %>% 
  arrange(-count) %>% top_n(10) %>%
  mutate(start_station_name= fct_reorder(start_station_name, count)) %>% 
  ggplot(aes(x=start_station_name, y=count, fill=count)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_gradient(low="blue", high="red") +
  labs(x="Start Station Name", y="No of rides", title="Top 10 starting stations for member riders")
# Key takeaways
# casual riders, most popular starting station is Streeter Dr & Grand Ave 
# member riders, most popular starting station is Kingsbury St & Kinzie St 

# let's confirm if the ending stations for users are also the same as starting stations
bike_rides %>% 
  filter(!(is.na(end_station_name))) %>% 
  filter(user_type == "casual") %>% 
  group_by(end_station_name) %>% 
  summarize(count=n()) %>% 
  arrange(-count) %>% top_n(10) %>%
  mutate(end_station_name= fct_reorder(end_station_name, count)) %>% 
  ggplot(aes(x=end_station_name, y=count, fill=count)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_gradient(low="blue", high="red") +
  labs(x="Ending Station Name", y="No of rides", title="Top 10 ending stations for casual riders")

bike_rides %>% 
  filter(!(is.na(end_station_name))) %>% 
  filter(user_type == "member") %>% 
  group_by(end_station_name) %>% 
  summarize(count=n()) %>% 
  arrange(-count) %>% top_n(10) %>%
  mutate(end_station_name= fct_reorder(end_station_name, count)) %>% 
  ggplot(aes(x=end_station_name, y=count, fill=count)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_gradient(low="blue", high="red") +
  labs(x="Ending Station Name", y="No of rides", title="Top 10 ending stations for member riders")
# key takeaways
# most popular starting and ending stations for casual and member riders are the same

# Let's now see bike starting and ending stations based on most round trips taken
bike_rides %>%
  group_by(start_station_name, end_station_name) %>%
  filter(start_station_name!="NULL") %>%
  summarize(count=n()) %>% ungroup %>%
  arrange(-count) %>% top_n(10)
# Key takeaways
# Streeter Dr & Grand Ave has the most round trips taken by users with more than 10,000 rides
# Ellis Ave & 60th St to University Ave & 57th St has the second most round trips, could entail that students taking these rides

## Let's do analysis based on map and coordinates in order to see location based rides
chicago.lines <- bike_rides %>% filter(start_lng != end_lng & start_lat != end_lat) %>%
  group_by(user_type,
           bike_type,
           start_station_id,
           start_lng,
           start_lat,
           end_lng,
           end_lat,
           start_station_name,
           end_station_name) %>%
  summarize(rides = n(),.groups="drop") %>%
  filter(rides > 100)
# creating 2 data frames based on usertype only
casuals <- chicago.lines %>% filter(user_type == "casual")
members <- chicago.lines %>% filter(user_type == "member")

# setting coordinates data and fetching stamen map for visualization
chicago <- c(left = -87.8, 
                             bottom = 41.8, 
                             right = -87.6, 
                             top = 42.1)
chicago_map <- get_stamenmap(bbox = chicago, maptype = "terrain", zoom = 12)

# visualising ride activity in chicago map for casual riders
ggmap(chicago_map) + 
  geom_point(casuals, mapping=aes(x=start_lng,y=start_lat,color=bike_type),size=2)+
  coord_fixed(.7) + 
  xlab('')+ylab('') +
  ggtitle("Ride activity by casual riders")
# casual riders had most rides near the shore or bay area, implying that tourists are using them

# visualising ride activity for member riders in chicago map
ggmap(chicago_map,darken = c(0.1)) + 
  geom_point(members, mapping=aes(x=start_lng,y=start_lat,color=bike_type),size=2)+
  coord_fixed(.7) + 
  xlab('')+ylab('') +
  ggtitle("Ride activity by member riders")
# member riders are more spread out in the city, could imply that they take rides for work travel purposes

## Exporting
write.csv(bike_rides, "bike_rides.csv")
# Further visualization with dashboard will be done in Tableau public
# i will drop few columns that are not needed because of tableau public file size limit
drop_1 <- c("end_lat", "end_lng", "End_Month","End_Yr", "End_Day", "started_at", "ended_at", "ride_length_hour")
bike_rides_1 <- bike_rides[ , !(names(bike_rides) %in% drop_1)]
write.csv(bike_rides_1, "bike_rides_drop.csv")


