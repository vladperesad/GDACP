#Load up the libraries----
library(tidyverse)
library(skimr)
library(ggcorrplot)
library(lubridate)

#Set up a working directory
setwd("C:/vladperesad/GDACP")



#DataFrames----
daily_activity_merged <- read.csv("datasets/dailyActivity_merged.csv")

#Exploring the structure of the dataset and shaping it appropriately----

str(daily_activity_merged)


daily_activity_merged$Id <- as.factor(daily_activity_merged$Id)
#turn Id to a factor

daily_activity_merged <- daily_activity_merged %>% 
  mutate(Date = strptime(ActivityDate, "%m/%d/%Y")) %>% 
  select(!(ActivityDate))
#turn date into a proper format YYYY/MM/DD

daily_activity_merged$Date <- as.Date(daily_activity_merged$Date)
#and turn it into a date type

daily_activity_merged$DayOfWeek = ifelse(wday(daily_activity_merged$Date)== 1,7, wday(daily_activity_merged$Date)-1)
#pull number of the day of the week from the date 1 for Mon 7 for Sun

#Exploring and cleaning the dataset----

length(unique(daily_activity_merged$Id))
#count distinct Ids (should be 30, but 33 instead)

daily_activity_merged <- daily_activity_merged %>%
  group_by(Id) %>% 
  mutate(Count = length(Id)) %>%
  filter(Count > 7) %>% 
  select(Id, Count, Date, everything()) %>% 
  ungroup()
#add another column that has a count of days each Id submitted data
#get rid of records that count less than 7 days (1 full week)

summary(daily_activity_merged)
#all the values seem to make sense (no negative values, or any numbers out of normal range)

skimr::skim(daily_activity_merged)
# taking a look at the dataset using skimr, paying attention to n_missing and complete_rate

anyDuplicated(daily_activity_merged)
#checking for duplicates

#create a barchart that represents time people spend in each kind of activity----

DAM_mins_long <- daily_activity_merged %>% 
  select(Date, DayOfWeek, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes ) %>% 
  gather(key = IntensityLevel, value = Minutes, c(3:6))
#select data that represents time spent doing each activity and turn it to a long format

#and plot it
(barplot_act <-ggplot(DAM_mins_long, aes(x = reorder(IntensityLevel, desc(Minutes)), y = Minutes, fill = IntensityLevel))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_x_discrete(labels = c("SedentaryMinutes"="Sedentary",
                                "LightlyActiveMinutes" = "Lightly active",
                                "VeryActiveMinutes" = "Very Active",
                                  "FairlyActiveMinutes" = "Fairly Active"))+
    theme(legend.position = "none")+
    labs(title = "Most and least popular activities",
         subtitle = "How much time users spend performing each activity",
         x = ""))
#very obvious that people spend more time sitting than all other activities together


#create a correlation matrix to see if any values correlate with others----

str(sub_corr_mat_2)

corr_mat <- daily_activity_merged %>% 
  select(!c(Id, Date, TotalDistance, TrackerDistance, LoggedActivitiesDistance))
#get rid of columns that don't make any difference as they are calculated based on others
corr_mat <- round(cor(corr_mat),1)
#create a correlation matrix and round values for easier reading
(corr_plot <- ggcorrplot(cor(corr_mat), hc.order = TRUE, type = "lower", lab = TRUE)+
    theme_minimal()+
    labs(x = "",
         y = "")+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)))
#plotting the corr matrix
#such strong correlation between LAD and LAM is explained by the fact that 
#values in the first are calculated based on the second 
#same for MAD and FAM as well as VAD and VAM


#getting deeper investigating correlations----

#plot scatterplot showing correlation between number of steps and calories burnt
(steps_cal <- ggplot(daily_activity_merged, aes(x = TotalSteps, y = Calories))+
    geom_point()+
    stat_smooth(method = "lm", color = "#fe7f2d")+
    theme_minimal()+
    labs(title = "Steps vs. calories",
        subtitle = "Correlation between number of steps taken and amount of calories burned",
        x = "Steps"))
#more steps made = more calories burnt !genius!
#the point is it seems like a lot of people consider taking steps 
#as calories burning activity

(steps_dist <-ggplot(daily_activity_merged, aes(x = TotalSteps, y =TrackerDistance))+
    geom_point()+
    stat_smooth(method = "lm", color = "#fe7f2d")+
    theme_minimal()+
    labs(title = "Distance (km) vs. steps",
         subtitle = "Correlation between number of steps counted by a pedometer and distance covered according to device's GPS tracker ",
         x = "Steps",
         y = "Distance (km)"))
#tracker distance seems to directly correlate with number of steps taken 
#which means that distance measured by GPS correlates with number of steps counted by a pedometer in a device
#which means not thread mill
#hence device used outside, needs to be waterproof for rain and legible under bright sun 


#count how many times each day is represented in the dataset
day_of_week_cnt <- daily_activity_merged %>% 
  group_by(DayOfWeek) %>% 
  summarize(count = n())
#turnes out Tuesday is represented more than any other day, so simple sum won't suffice
#theres a need to calculate the mean for each day of the week


#find out which day of the week people burn more calories = being more active

cal_mean <- mean(daily_activity_merged$Calories)
cal_by_day <- daily_activity_merged %>% 
  group_by(DayOfWeek) %>% 
  summarize(CaloriesMean = mean(Calories),
            RelativeVal = 100*(mean(Calories)-cal_mean)/cal_mean)

(cal_by_day_plot <- ggplot(cal_by_day, aes(x = as.factor(DayOfWeek), y = RelativeVal, fill = RelativeVal))+
    geom_bar(stat = "identity")+
    scale_x_discrete(labels = c("Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
    theme_minimal()+
    labs(title = "Amount of calories burnt by day of week",
         x = "",
         y = "Relative calories (%)")+
  theme(legend.position = "none"))
#there is a clear bend over the middle of the week where people feel the least motivated
#need to prompt em
#they feel the most motivated at the beginning and on the weekend
#makes sense to offer new plans and subscriptions those days - more likely to get a sign up
