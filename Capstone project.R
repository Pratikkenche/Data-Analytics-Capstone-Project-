# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("D:/Course with Projects/Google Data analytics/Google Data Analytics Course 8/Case Study/Capstone project rstudio/csv") 
#sets your working directory to simplify calls to data

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload cyclistics datasets (csv files) here
tripdata_01 <- read_csv('Cyclistic_tripdata_2021_01.csv');tripdata_02 <- read_csv('Cyclistic_tripdata_2021_02.csv')
tripdata_03 <- read_csv('Cyclistic_tripdata_2021_03.csv');tripdata_04 <- read_csv('Cyclistic_tripdata_2021_04.csv')
tripdata_05 <- read_csv('Cyclistic_tripdata_2021_05.csv');tripdata_06 <- read_csv('Cyclistic_tripdata_2021_06.csv')
tripdata_07 <- read_csv('Cyclistic_tripdata_2021_07.csv');tripdata_08 <- read_csv('Cyclistic_tripdata_2021_08.csv')
tripdata_09 <- read_csv('Cyclistic_tripdata_2021_09.csv');tripdata_10 <- read_csv('Cyclistic_tripdata_2021_10.csv')
tripdata_11 <- read_csv('Cyclistic_tripdata_2021_11.csv');tripdata_12 <- read_csv('Cyclistic_tripdata_2021_12.csv')

colnames(tripdata_01);colnames(tripdata_02)
colnames(tripdata_03);colnames(tripdata_04)
colnames(tripdata_05);colnames(tripdata_06)
colnames(tripdata_07);colnames(tripdata_08)
colnames(tripdata_09);colnames(tripdata_10)
colnames(tripdata_11);colnames(tripdata_12)

str(tripdata_01)

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(tripdata_02, tripdata_03, tripdata_04, tripdata_05, tripdata_06, 
                       tripdata_07, tripdata_08, tripdata_09, tripdata_10, tripdata_11, tripdata_12)
View(all_trips)
# Rename columns

all_trips <- rename(all_trips, user_type=member_casual)

# Remove lat, long cols
all_trips <- all_trips %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created

colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

View(all_trips %>% filter(user_type=='Customer'))
View(all_trips %>% filter(user_type!='casual' & user_type != 'member'))

# Begin by seeing how many observations fall under each cols
table(all_trips$user_type)
table(all_trips$rideable_type)


# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date),'%m')
all_trips$day <- format(as.Date(all_trips$date),'%d')
all_trips$year <- format(as.Date(all_trips$date),'%Y')
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
View(all_trips %>% filter(ride_length < 0))
# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and 
# checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name=='HQ QR' | all_trips$ride_length < 0),]

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
a<-mean(all_trips_v2$ride_length) #straight average (total ride length / rides)

summary(all_trips_v2$ride_length)
# to remove na values from a column
all_trips_v2 <- all_trips_v2[!is.na(all_trips_v2$ride_length),]
table(is.na(all_trips_v2$ride_length))
mean(all_trips_v2$ride_length)
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$user_type, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$user_type, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$user_type, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$user_type, FUN = min)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$user_type + all_trips_v2$day_of_week, FUN = mean)

View(all_trips_v2 %>% 
       +          mutate(weekday = wday(started_at, label = TRUE)))

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(user_type, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()						    	#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(user_type, weekday)	

# we can also do above code without mutating the weekday
all_trips_v2 %>% 
       group_by(user_type, day_of_week) %>%  #groups by usertype and day_of_week
       summarise(number_of_rides = n()						    	#calculates the number of rides and average duration 
                , average_duration = mean(ride_length)) %>% # calculates the average duration
        arrange(user_type, day_of_week)

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  group_by(user_type,day_of_week) %>% 
  summarise(num_of_rides=n(), avg_duration = mean(ride_length)) %>%  #n() is function for count
  arrange(user_type,day_of_week) %>% 
  ggplot(aes(x=day_of_week,y=num_of_rides,fill=user_type))+ geom_col()

# Let's create a visualization for average duration
all_trips_v2 %>% 
  group_by(user_type, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(user_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = user_type)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$user_type + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'D:/Course with Projects/Google Data analytics/Google Data Analytics Course 8/Case Study/Capstone project rstudio/avg_ride_length.csv')

all_trips_v2 %>% 
  group_by(user_type, day_of_week,month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length))  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = user_type,)) +
  geom_col(position = "dodge")+facet_wrap(~month)+theme(axis.text.x = element_text(angle=45))+
  labs(title = 'Relation between Day_of_week VS Avg_duration' , subtitle = 'Plots over a period of 11 Months', caption = 'from 02=Feburary to 12=December')

all_trips_v2 %>% 
  group_by(user_type,day_of_week,month) %>% 
  summarise(num_of_rides=n(), avg_duration = mean(ride_length)) %>%  #n() is function for count
  ggplot(aes(x=day_of_week,y=num_of_rides,fill=user_type))+ geom_col(position = 'dodge')+theme(axis.text.x = element_text(angle=45))+
  labs(title = 'Relation between Day_of_week VS Num_of_rides' , subtitle = 'Plots over a period of 11 Months', caption = 'from 02=Feburary to 12=December')+
  facet_wrap(~month)


