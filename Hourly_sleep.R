#Load up the libraries----
library(tidyverse)
library(lubridate)
library(skimr)

#Set up a working directory
setwd("C:/vladperesad/GDACP")

#import dataset----
minute_sleep_merged <- read.csv("datasets/minuteSleep_merged.csv")


#exploring the dataset-----

skim(minute_sleep_merged)
#use skim function to see if data makes sense and take a look at its structure

minute_sleep_merged <- distinct(minute_sleep_merged)
#remove the duplicates if theyre present (543 rows)

minute_sleep_merged <- minute_sleep_merged %>% 
  mutate(DateNew = strptime(date, "%m/%d/%Y %I:%M:%S %p")) %>% 
  tidyr::separate(DateNew, c("Date", "Time"), sep = " ", remove = FALSE) %>%  
  filter(Time >= "20:00:00" | Time <= "08:00:00") %>% 
  select(Id, LogId = logId, DateTime = DateNew, Value = value)
#shape date column into appropriate format YYYY-MM-DD HH:MM:SS
#separate date and time columns
#filter records with times between 8:00 and 20:00 as we're not interested in naps
#reshape the dataframe a little bit

minute_sleep_merged$DateTime <- as.POSIXct(minute_sleep_merged$DateTime)
#turn date column into proper datetime type

skim(minute_sleep_merged)
#run the skim function again to see how the data changed (188521 vs 172148)
#value mean (from 1.10 to 1.09)
#value std (from 0,329 to 0.320)
#data makes sense in terms of values and dates

summary(minute_sleep_merged)
#dataset contains records of sleep dates and times( from 2016-04-11 to 2016-05-12) 
#and quality of sleep for every minute graded from 1 (sleeping), 2(restless), 3(awake)


MSM_subset <- minute_sleep_merged %>%
  mutate(ShiftedTime = DateTime+hours(4)) %>% 
  tidyr::separate(ShiftedTime, c("Date", "SftTime"), sep = " " ) %>% 
  select(Id, LogId, SftTime)
#create a 4 hour offset (so that is is plotted from 20 to 23 and then from 0 to 12)
#separate an offset date_time into date and shifted time
#and select columns we need for further calculations

MSM_subset <- MSM_subset  %>% 
  group_by(LogId) %>% 
  slice(1)
#create a subset that contains times at which each sleep began

MSM_new <- right_join(minute_sleep_merged, MSM_subset, by = c("LogId", "Id"))
#performing a right join to join dataset containing start sleep times to a main one 

MSM_new <- MSM_new %>%
  select(Id, LogId, DateTime, SftTime, Value) %>% 
  group_by(Id, LogId, SftTime) %>% 
  summarise(MeanValue = mean(Value)) %>% 
  mutate(Date = today()) %>%
  unite("DateTime", 5,3, sep = " ")
#calculate mean sleep quality value for each sleep

MSM_new$DateTime <- as_datetime(MSM_new$DateTime)

(sleep_quality_vs_time <- ggplot(MSM_new, aes(x =DateTime, y = MeanValue))+
    geom_point()+
    geom_smooth(color = "#fe7f2d")+
    scale_x_datetime(date_breaks = "2 hours",
                     date_labels = c("21:00","21:00","23:00","01:00","03:00","05:00","07:00"))+
    scale_y_continuous(limits = c(1, 1.6))+
    theme_minimal()+
    labs(title = "Bedtime vs. quality of sleep",
         subtitle = "How the time we go to bed affects our sleep",
         x = "",
         y = "Sleep quality index"))

ggsave(sleep_quality_vs_time, filename = "bedtime_vs_sleep.png", height = 9, width = 16)  
