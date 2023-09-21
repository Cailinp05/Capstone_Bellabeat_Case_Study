# Fitness_Data_Script
# Capestone_Project
# Cailin_Pillay

# Import libraries I might need

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(here)
library(skimr)
library(janitor)
library(ggpubr)
library(ggrepel)
library(shiny)
library(plotly)
library(psych)
library(corrplot)
library(gganimate)
library(gifski)
library(av)
library(transformr)
library(reshape2)
library(plotly)
library(gridExtra)

# Load datasets -----------------------------------------------------------
daily_activity <- read.csv("fitness/dailyActivity_merged.csv")
daily_calories <- read.csv("fitness/dailyCalories_merged.csv")
daily_intensities <- read.csv("fitness/dailyIntensities_merged.csv")
daily_steps <- read.csv("fitness/dailySteps_merged.csv")
heartrate <- read.csv("fitness/heartrate_seconds_merged.csv")
hourly_calories <- read.csv("fitness/hourlyCalories_merged.csv")
hourly_intensities <- read.csv("fitness/hourlyIntensities_merged.csv")
hourly_steps <- read.csv("fitness/hourlySteps_merged.csv")
minute_calories_narrow <- read.csv("fitness/minuteCaloriesNarrow_merged.csv")
minute_calories_wide <- read.csv("fitness/minuteCaloriesWide_merged.csv")
minute_intensities_narrow <- read.csv("fitness/minuteIntensitiesNarrow_merged.csv")
minute_intensities_wide <- read.csv("fitness/minuteIntensitiesWide_merged.csv")
minute_steps_narrow <- read.csv("fitness/minuteStepsNarrow_merged.csv")
minute_steps_wide <- read.csv("fitness/minuteStepsWide_merged.csv")
minute_sleep <- read.csv("fitness/minuteSleep_merged.csv")
sleep <- read.csv("fitness/sleepDay_merged.csv")
weight <- read.csv("fitness/weightLogInfo_merged.csv")
minute_met_narrow <- read.csv("fitness/minuteMETsNarrow_merged.csv")

# Summary of datasets -----------------------------------------------------
summary(daily_activity)
summary(daily_calories)
summary(daily_intensities)
summary(daily_steps)
summary(heartrate)
summary(hourly_calories)
summary(hourly_intensities)
summary(hourly_steps)
summary(minute_calories_narrow)
summary(minute_calories_wide)
summary(minute_intensities_narrow)
summary(minute_intensities_wide)
summary(minute_steps_narrow)
summary(minute_calories_wide)
summary(minute_intensities_narrow)
summary(minute_intensities_wide)
summary(minute_steps_narrow)
summary(minute_steps_wide)
summary(minute_sleep)
summary(sleep)
summary(weight)
summary(minute_met_narrow)

# Head of datasets --------------------------------------------------------
head(daily_activity)
head(daily_calories)
head(daily_intensities)
head(daily_steps)
head(heartrate)
head(hourly_calories)
head(hourly_intensities)
head(hourly_steps)
head(minute_calories_narrow)
head(minute_calories_wide)
head(minute_intensities_narrow)
head(minute_intensities_wide)
head(minute_steps_narrow)
head(minute_calories_wide)
head(minute_intensities_narrow)
head(minute_intensities_wide)
head(minute_steps_narrow)
head(minute_steps_wide)
head(minute_sleep)
head(sleep)
head(weight)
head(minute_met_narrow)

# Change date formats
daily_activity$Ymd <- mdy(daily_activity$ActivityDate)
daily_steps$Ymd <- mdy(daily_steps$ActivityDay)
daily_calories$Ymd <- mdy(daily_calories$ActivityDay)

# Strings of all datasets -------------------------------------------------
str(daily_activity)
str(daily_calories)
str(daily_intensities)
str(daily_steps)
str(heartrate)
str(hourly_calories)
str(hourly_intensities)
str(hourly_steps)
str(minute_calories_narrow)
str(minute_calories_wide)
str(minute_intensities_narrow)
str(minute_intensities_wide)
str(minute_steps_narrow)
str(minute_calories_wide)
str(minute_intensities_narrow)
str(minute_intensities_wide)
str(minute_steps_narrow)
str(minute_steps_wide)
str(minute_sleep)
str(sleep)
str(weight)
str(minute_met_narrow)

# Change date formats
daily_activity %>% count(Ymd, sort = TRUE) %>% 
  ggplot() + geom_col(aes(x=Ymd, y=n)) +
  labs(title= "Count of Activities by Day")

# total_steps and view
total_steps <- daily_activity %>% group_by(Ymd) %>% 
  summarise( Steps = sum(TotalSteps,na.rm=TRUE), 
             Mean = mean(TotalSteps, na.rm=TRUE))
ggplot(total_steps) + geom_line(aes(x=Ymd, y=Steps)) +
  labs(title="Total Steps By Day for 2016") + 
  scale_y_continuous(labels= comma)

ggplot(total_steps) + geom_line(aes(x=Ymd, y=Mean)) +
  labs(title="Average Steps By Day for 2016") + 
  scale_y_continuous(labels= comma)

# total_distance and view
total_distance <- daily_activity %>% group_by(Ymd) %>% 
  summarise( Distance = sum(TotalDistance,na.rm=TRUE), 
             Mean = mean(TotalDistance, na.rm=TRUE))

ggplot(total_distance) + geom_line(aes(x=Ymd, y=Distance)) +
  labs(title="Total Distance By Date for 2016", y= "Total Distance") + 
  scale_y_continuous(labels= comma)

ggplot(total_distance) + geom_line(aes(x=Ymd, y=Mean)) +
  labs(title="Average Distance By Date for 2016", y= "Total Distance") + 
  scale_y_continuous(labels= comma)

# Okay so what all can I focus on and plot to answer the questions?
# How are people using smart products? ------------------------------------
# Some plots I could make:
# Plot sleep vs active vs steps
# Total steps by day and average
# total distance by day and average 
# steps vs minutes asleep 
# Hourly steps throughout the day average
# Correlations daily steps vs daily sleep 
# Sleep vs activity 
# Daily steps vs calories 
# Time worn per day 
# Calories burnt vs activity 
# Sleep vs time in bed
# sleep vs steps per hour 
# Average Total Intensity vs Time/hours
# Minutes asleep vs Sedentary Minutes 
# Sleep monitoring 
# Heart rate 
# Weight measurements 
# Average Time distribution per day 
# what is used the most? steps vs sleep vs weight 
# average steps per day for the month
# average hourly steps 
# When do people work out and which group is most active 
# count of minutes of exercise per hour 
# Average steps, heart-rate, distance covered
# Which months in each year have been really strenuous in terms of step counts and flights climbed?
# Comparative study of the step-counts, calories burned, distance covered from the previous months
# A weekly breakdown of the step-counts to check for dominant days in a month
# Are there any outliers?
# How does the activity compare on weekdays and weekends?
# Does the subject transition to a healthy lifestyle after a certain period?

# Merge daily_steps and daily_calories datasets
merged_steps_calories <- merge(daily_steps, daily_calories, by = "Ymd")

# Create a line plot for average daily steps and calories
ggplot(merged_steps_calories, aes(x = Ymd)) +
  geom_line(aes(y = StepTotal, color = "Total Steps")) +
  geom_line(aes(y = Calories, color = "Calories"), linetype = "dashed") +
  labs(title = "Average Daily Steps and Calories",
       x = "Date",
       y = "Count") +
  scale_color_manual(values = c("Total Steps" = "blue", "Calories" = "red"))

# Example: Sleep duration over time

library(lubridate)

# Split the character column into separate date columns
sleep$Date <- as.Date(sleep$SleepDay, format = "%m/%d/%Y")

# Print the updated data frame
print(sleep)
sleep$Month <- month(sleep$Date)

ggplot(sleep, aes(x = Date, y = TotalMinutesAsleep, fill = factor(Month))) +
  geom_bar(stat = "identity") +
  labs(title = "Sleep Duration Over Time by Month",
       x = "Date",
       y = "Sleep Duration (minutes)") +
  scale_fill_discrete(name = "Month")

# Example: Line plot of heart rate over time

# Split the character column into separate date columns
heartrate$Date <- as.Date(heartrate$Time, format = "%m/%d/%Y")

ggplot(heartrate, aes(x = Date, y = Value)) +
  geom_line() +
  labs(title = "Heart Rate Trends Over Time",
       x = "Date",
       y = "Heart Rate")

# Example: Line plot of weight changes over time
weight$Date <- as.Date(weight$Date, format = "%m/%d/%Y")

ggplot(weight, aes(x = Date, y = WeightKg)) +
  geom_line() +
  labs(title = "Weight Changes Over Time",
       x = "Date",
       y = "Weight (kg)")

# Daily steps vs minutes sleep --------------------------------------------

sleep_tidy <- sleep %>% 
  mutate(Date = as.Date(sleep$SleepDay, format = "%m/%d/%Y"))

steps_tidy <- daily_steps %>% 
  mutate(Date = as.Date(daily_steps$ActivityDay, format = "%m/%d/%Y"))


merged_steps_sleep <- merge(sleep_tidy, steps_tidy, by = "Date")

ggplot(merged_steps_sleep, aes(x = TotalMinutesAsleep, y = StepTotal, color = Date)) +
  geom_point() +
  labs(title = "Sleep Duration vs. Steps",
       x = "Total Minutes Asleep",
       y = "Total Steps") +
  scale_color_viridis_c()

merged_steps_sleep$Month <- month(merged_steps_sleep$Date)

ggplot(merged_steps_sleep, aes(x = TotalMinutesAsleep, y = StepTotal, color = Month)) +
  geom_point() +
  labs(title = "Sleep Duration vs. Steps",
       x = "Total Minutes Asleep",
       y = "Total Steps") +
  scale_color_viridis_c()

# Line plot of TotalMinutesAsleep and StepTotal over time
ggplot(merged_steps_sleep, aes(x = Date)) +
  geom_line(aes(y = TotalMinutesAsleep, color = "Sleep Duration")) +
  geom_line(aes(y = StepTotal, color = "Total Steps")) +
  labs(title = "Sleep Duration and Steps Over Time",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Sleep Duration" = "blue", "Total Steps" = "red"))


# Hourly steps per day average --------------------------------------------

str(daily_steps)

# Convert the Ymd column to a Date object
daily_steps$Ymd <- as.Date(daily_steps$Ymd)

# Create separate columns for Day, Month, and Year
daily_steps$Day <- day(daily_steps$Ymd)
daily_steps$Month <- month(daily_steps$Ymd)
daily_steps$Year <- year(daily_steps$Ymd)

# Print the updated dataset
print(daily_steps)

# Hourly steps vs hourly calories -----------------------------------------

str(hourly_calories)

# Convert ActivityHour to a POSIXct object
hourly_calories$ActivityHour <- as.POSIXct(hourly_calories$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")

# Create separate columns for Hour, Day, Month, and Year
hourly_calories <- hourly_calories %>%
  mutate(Hour = hour(ActivityHour),
         Day = day(ActivityHour),
         Month = month(ActivityHour),
         Year = year(ActivityHour))

# Convert ActivityHour to a POSIXct object
hourly_steps$ActivityHour <- as.POSIXct(hourly_steps$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")

# Create separate columns for Hour, Day, Month, and Year
hourly_steps <- hourly_steps %>%
  mutate(Hour = hour(ActivityHour),
         Day = day(ActivityHour),
         Month = month(ActivityHour),
         Year = year(ActivityHour))

# Combine the two datasets based on common columns (Hour, Day, Month, and Year)
hourly_steps_calories <- merge(hourly_calories, hourly_steps, by = c("Hour", "Day", "Month", "Year"))

# Scatter plot of Hourly Steps vs Hourly Calories with color gradient based on Day
ggplot(hourly_steps_calories, aes(x = StepTotal, y = Calories, color = Day)) +
  geom_point() +
  labs(title = "Hourly Steps vs Hourly Calories",
       x = "Hourly Steps",
       y = "Hourly Calories") +
  scale_color_viridis_c() +  # Apply a color scale
  theme_minimal()  # Use a minimalistic theme for better readability

# Correlations daily steps vs daily sleep ---------------------------------
daily_steps <- read.csv("fitness/dailySteps_merged.csv")
str(daily_steps)

# Convert ActivityDay to a Date object
daily_steps$ActivityDay <- as.Date(daily_steps$ActivityDay, format = "%m/%d/%Y")

# Create separate columns for Day, Month, and Year
daily_steps <- daily_steps %>%
  mutate(Hour = hour(ActivityDay),
         Day = day(ActivityDay),
         Month = month(ActivityDay),
         Year = year(ActivityDay))

str(sleep)
daily_sleep <- sleep 
str(daily_sleep)

# Convert SleepDay to a POSIXct object
daily_sleep$SleepDay <- as.POSIXct(daily_sleep$SleepDay, format = "%m/%d/%Y %I:%M:%S %p")

# Create separate columns for Day, Month, Year, and Hour
daily_sleep <- daily_sleep %>%
  mutate(Day = day(SleepDay),
         Month = month(SleepDay),
         Year = year(SleepDay),
         Hour = hour(SleepDay))

# Rename the 'ActivityDay' column to 'Date'
daily_steps <- daily_steps %>%
  rename(Date = ActivityDay)

# Merge the two datasets based on common dates
daily_steps_sleep <- merge(daily_sleep, daily_steps, by = "Date")

# Scatter plot of TotalMinutesAsleep vs StepTotal
ggplot(daily_steps_sleep, aes(x = TotalMinutesAsleep, y = StepTotal)) +
  geom_point() +
  labs(title = "Correlation Between Daily Sleep and Steps",
       x = "Total Minutes Asleep",
       y = "Total Steps") +
  theme_minimal()

# Line plot of TotalMinutesAsleep and StepTotal over time
ggplot(daily_steps_sleep, aes(x = Date)) +
  geom_line(aes(y = TotalMinutesAsleep, color = "Sleep")) +
  geom_line(aes(y = StepTotal, color = "Steps")) +
  labs(title = "Trends in Daily Sleep and Steps",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Sleep" = "blue", "Steps" = "red")) +
  theme_minimal()

library(corrplot)

# Merge the two datasets based on common dates
daily_steps_sleep <- merge(daily_sleep, daily_steps, by = "Date")

# Calculate the correlation matrix
correlation_matrix <- cor(daily_steps_sleep$TotalMinutesAsleep, daily_steps_sleep$StepTotal)
correlation_matrix

# Calculate the total minutes of sleep and total steps

total_sleep_minutes <- sum(daily_steps_sleep$TotalMinutesAsleep)
total_steps <- sum(daily_steps_sleep$StepTotal)

# Create a data frame for the pie chart
pie_data <- data.frame(Category = c("Sleep", "Steps"),
                       Value = c(total_sleep_minutes, total_steps))

# Create a pie chart using ggplot2
ggplot(pie_data, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Proportion of Time Spent Sleeping vs Taking Steps",
       fill = "Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Sleep vs activity -------------------------------------------------------
str(daily_sleep)
str(daily_activity)

# Convert ActivityDate to a Date object
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format = "%m/%d/%Y")

# Create separate columns for Day, Month, Year, Hour, and Date
daily_activity <- daily_activity %>%
  mutate(Day = day(ActivityDate),
         Month = month(ActivityDate),
         Year = year(ActivityDate),
         Hour = hour(ActivityDate),
         Date = date(ActivityDate))

# Merge the two datasets based on common dates
daily_activity_sleep <- merge(daily_activity, daily_sleep, by = "Date")

# Time series plot of activity variables and sleep over time
ggplot(daily_activity_sleep, aes(x = ActivityDate)) +
  geom_line(aes(y = TotalSteps, color = "Total Steps")) +
  geom_line(aes(y = VeryActiveMinutes, color = "Very Active Minutes")) +
  geom_line(aes(y = TotalMinutesAsleep, color = "Total Minutes Asleep")) +
  labs(title = "Trends in Daily Activity and Sleep",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Total Steps" = "blue", "Very Active Minutes" = "green", "Total Minutes Asleep" = "purple")) +
  theme_minimal()

# Stacked area plot of activity levels over time
ggplot(daily_activity, aes(x = Date, y = LightlyActiveMinutes, fill = SedentaryMinutes)) +
  geom_area(alpha = 0.7) +
  labs(title = "Composition of Activity Levels Over Time",
       x = "Date",
       y = "Minutes") +
  scale_fill_gradient(low = "red", high = "blue") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Scatter plot with bubble size and color representing activity and sleep
ggplot(daily_activity_sleep, aes(x = TotalSteps, y = VeryActiveMinutes, size = TotalSteps, color = TotalMinutesAsleep)) +
  geom_point(alpha = 0.7) +
  labs(title = "Relationship Between Steps, Activity, and Sleep",
       x = "Total Steps",
       y = "Very Active Minutes") +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(3, 10)) +
  theme_minimal()

# pie chart of activity ---------------------------------------------------
str(daily_activity)
library(scales)
# Proportion of Different Activity Levels:
# Create a pie chart to show the proportion of time spent in different activity levels (Very Active, Moderately Active, Lightly Active, Sedentary).

# Create a data frame for the pie chart
pie_data <- data.frame(
  Category = c("Very Active", "Moderately Active", "Lightly Active", "Sedentary"),
  Value = c(sum(daily_activity$VeryActiveMinutes),
            sum(daily_activity$ModeratelyActiveMinutes),
            sum(daily_activity$LightlyActiveMinutes),
            sum(daily_activity$SedentaryMinutes))
)

# Calculate percentage values
total_value <- sum(pie_data$Value)
pie_data$Percentage <- pie_data$Value / total_value * 100

# Create a pie chart with percentage labels
ggplot(pie_data, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Proportion of Time Spent in Different Activity Levels",
       fill = "Activity Level") +
  geom_text(aes(label = paste0(sprintf("%.2f", Percentage), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Activity Level"))

# Proportion of Calories Burned by Activity Level with Percentage Labels:
# Create a data frame for the pie chart
pie_data_calories <- data.frame(
  Category = c("Very Active", "Moderately Active", "Lightly Active", "Sedentary"),
  Value = c(sum(daily_activity$Calories * daily_activity$VeryActiveMinutes / 60),
            sum(daily_activity$Calories * daily_activity$ModeratelyActiveMinutes / 60),
            sum(daily_activity$Calories * daily_activity$LightlyActiveMinutes / 60),
            sum(daily_activity$Calories * daily_activity$SedentaryMinutes / 60))
)

# Calculate percentage values
total_value_calories <- sum(pie_data_calories$Value)
pie_data_calories$Percentage <- pie_data_calories$Value / total_value_calories * 100

# Create a pie chart with percentage labels
ggplot(pie_data_calories, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Proportion of Calories Burned by Activity Level",
       fill = "Activity Level") +
  geom_text(aes(label = paste0(sprintf("%.2f", Percentage), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Activity Level"))

# Proportion of Total Distance Covered by Activity Level with Percentage Labels:
# Create a data frame for the pie chart
pie_data_distance <- data.frame(
  Category = c("Very Active", "Moderately Active", "Lightly Active", "Sedentary"),
  Value = c(sum(daily_activity$VeryActiveDistance),
            sum(daily_activity$ModeratelyActiveDistance),
            sum(daily_activity$LightActiveDistance),
            sum(daily_activity$SedentaryActiveDistance))
)

# Calculate percentage values
total_value_distance <- sum(pie_data_distance$Value)
pie_data_distance$Percentage <- pie_data_distance$Value / total_value_distance * 100

# Create a pie chart with percentage labels
ggplot(pie_data_distance, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Proportion of Total Distance Covered by Activity Level",
       fill = "Activity Level") +
  geom_text(aes(label = paste0(sprintf("%.2f", Percentage), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Activity Level"))

# heartrate ---------------------------------------------------------------
heartrate <- read.csv("fitness/heartrate_seconds_merged.csv")
str(heartrate)

# Extract Hour and Minutes using lubridate
heartrate$Hour <- hour(mdy_hms(heartrate$Time))
heartrate$Minutes <- minute(mdy_hms(heartrate$Time))
# Extract Month, Year, and Date using lubridate
heartrate$Month <- month(mdy_hms(heartrate$Time))
heartrate$Year <- year(mdy_hms(heartrate$Time))
heartrate$Day <- date(mdy_hms(heartrate$Time))

# Convert the "Day" column to a Date object
heartrate$Day <- as.Date(heartrate$Day)

# Create a new column "dayoftheweek" with the day of the week
heartrate$dayoftheweek <- weekdays(heartrate$Day)

# If you want to convert the day names to Monday-Sunday format, you can use this:
heartrate$dayoftheweek <- factor(heartrate$dayoftheweek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

heart_rate <- heartrate %>%
  group_by(Day, Month, Year) %>%
  summarise(mean_heart_rate = mean(Value, na.rm = TRUE))

heart_rate

#heartrate$MonthYear <- floor_date(heartrate$Day, unit = "month") 
#heartrate$MonthYear <- format(heartrate$Day, "%Y-%m")

heart_rate$Month <- factor(heart_rate$Month, levels = c(4, 5), labels = c("April", "May"))

# Create the plot
#ggplot(heart_rate, aes(x = MonthYear, y = mean_heart_rate, color = Month)) +
#  geom_line() +
#  labs(x = "Month/Year", y = "Heart Rate", title = "Daily Mean Heart Rate") +
# scale_color_manual(values = c("4" = "pink", "5" = "blue"))
# Extract the month and year from the 'Day' column
# heartrate$MonthYear <- format(heartrate$Day, "%Y-%m")
#heartrate <- heartrate %>%
#  mutate(MonthYear = format(Day, "%Y-%m"))

# Create the plot
p1 <- ggplot(heart_rate, aes(x = Day, y = mean_heart_rate, color = Month)) +
  geom_line() +
  labs(x = "Month/Year", y = "heart_rate", title = "Daily Mean Heartrate") +
  scale_color_manual(values = c("April" = "red", "May" = "blue"))

p1

install.packages("gganimate")
install.packages("gifski")
install.packages("av")
install.packages("transformr")
library(gganimate)
library(gifski)
library(av)
library(transformr)

#animating the above plot

plot1 <- p1+transition_reveal(as.numeric(Day))

animate(plot1)

# Convert the 'Day' column to a factor to ensure proper ordering
#heart_rate$Day <- as.factor(heart_rate$Day)
# Create the ggplot object
#plot2 <- ggplot(heart_rate, aes(x = Day, y = mean_heart_rate, color = Month, group = 1)) +
#  geom_line() +
#  labs(x = "Month/Year", y = "Heart Rate", title = "Daily Mean Heart Rate") +
#  scale_color_manual(values = c("April" = "red", "May" = "blue")) +
#  transition_states(Month, transition_length = 2, state_length = 1) +
#  enter_fade() +
#  exit_fade()
# Animate the plot and specify the animation format
#anim <- animate(plot2, nframes = 500, duration = 10, end_pause = 10)
# Save the animation as a GIF
#anim_save("heart_rate_animation.gif", anim)
# Save the animation as a GIF
#gganimate::anim_save("heart_rate_animation.gif", anim)

# Dailysteps --------------------------------------------------------------
daily_steps <- read.csv("fitness/dailySteps_merged.csv")

str(daily_steps)

# Extract Month, Year, and Date using lubridate
# Assuming daily_steps is your data frame
daily_steps$ActivityDay <- as.Date(daily_steps$ActivityDay, format = "%m/%d/%Y")

# Extracting Month, Year, and Date
daily_steps$Month <- format(daily_steps$ActivityDay, "%m")
daily_steps$Year <- format(daily_steps$ActivityDay, "%Y")
daily_steps$Date <- format(daily_steps$ActivityDay, "%d")

# Display the updated data frame
head(daily_steps)

# Assuming daily_steps is your data frame
daily_steps$dayoftheweek <- weekdays(daily_steps$ActivityDay)

# Convert day names to Monday-Sunday format
daily_steps$dayoftheweek <- factor(daily_steps$dayoftheweek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Convert ActivityDay to Date if not already done
daily_steps$ActivityDay <- as.Date(daily_steps$ActivityDay, format = "%m/%d/%Y")

# Create a new data frame to aggregate steps by day and year
daily_steps_aggregated <- daily_steps %>%
  group_by(Year, ActivityDay) %>%
  summarise(TotalSteps = sum(StepTotal))

# Create the plot
ggplot(daily_steps_aggregated, aes(x = ActivityDay, y = TotalSteps, group = Year, color = Year)) +
  geom_line() +
  labs(x = "Date", y = "Total Steps", title = "Total Steps Taken Per Day by Year") +
  scale_x_date(date_labels = "%b %d, %Y", date_breaks = "1 month") +
  theme_minimal()

# Convert ActivityDay to Date if not already done
daily_steps$ActivityDay <- as.Date(daily_steps$ActivityDay, format = "%m/%d/%Y")

# Create a new data frame to aggregate steps by day and month
daily_steps_aggregated <- daily_steps %>%
  group_by(Year, Month, ActivityDay) %>%
  summarise(TotalSteps = sum(StepTotal))

# Create the plot
ggplot(daily_steps_aggregated, aes(x = ActivityDay, y = TotalSteps, group = Month, color = Month)) +
  geom_line() +
  labs(x = "Date", y = "Total Steps", title = "Total Steps Taken Per Day by Month") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal()

# Create a new data frame to aggregate steps by weekday
daily_steps_aggregated <- daily_steps %>%
  mutate(Weekday = weekdays(ActivityDay)) %>%
  group_by(Weekday) %>%
  summarise(TotalSteps = sum(StepTotal))

# Define a custom color palette
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Order the weekdays in the desired order (Monday to Sunday)
daily_steps_aggregated$Weekday <- factor(daily_steps_aggregated$Weekday,
                                         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Create the plot with custom colors
ggplot(daily_steps_aggregated, aes(x = Weekday, y = TotalSteps, fill = Weekday)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  labs(x = "Days of the week", y = "Total Steps", title = "Total Steps Taken Per Weekday with Custom Colors") +
  theme_minimal()

# Calories ----------------------------------------------------------------
daily_calories <- read.csv("fitness/dailyCalories_merged.csv")

str(daily_calories)

# Extract Month, Year, and Date using lubridate
daily_calories$ActivityDay <- as.Date(daily_calories$ActivityDay, format = "%m/%d/%Y")

# Extracting Month, Year, and Date
daily_calories$Month <- format(daily_calories$ActivityDay, "%m")
daily_calories$Year <- format(daily_calories$ActivityDay, "%Y")
daily_calories$Date <- format(daily_calories$ActivityDay, "%d")

# Display the updated data frame
head(daily_calories)

daily_calories$dayoftheweek <- weekdays(daily_calories$ActivityDay)

# Convert day names to Monday-Sunday format
daily_calories$dayoftheweek <- factor(daily_calories$dayoftheweek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Convert ActivityDay to Date if not already done
daily_calories$ActivityDay <- as.Date(daily_calories$ActivityDay, format = "%m/%d/%Y")

daily_calories_total <- daily_calories %>%
  group_by(ActivityDay) %>%
  summarize(TotalCalories = sum(Calories))

ggplot(daily_calories_total, aes(x = ActivityDay, y = TotalCalories)) +
  geom_line() +
  labs(title = "Total Calories Burned Daily",
       x = "Date",
       y = "Total Calories") +
  theme_minimal()

daily_calories_total_month <- daily_calories %>%
  mutate(ActivityDay = as.Date(ActivityDay)) %>%
  group_by(Year, Month, ActivityDay) %>%
  summarize(TotalCalories = sum(Calories))

# Aggregate by month to get the total calories burned per month
monthly_total_calories <- daily_calories_total_month %>%
  group_by(Year, Month) %>%
  summarize(TotalCalories = sum(TotalCalories))

ggplot(monthly_total_calories, aes(x = Month, y = TotalCalories, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Calories Burned Per Month",
       x = "Month",
       y = "Total Calories",
       fill = "Year") +
  theme_minimal()

daily_calories_total_weekday <- daily_calories %>%
  mutate(ActivityDay = as.Date(ActivityDay),
         weekday = weekdays(ActivityDay)) %>%
  group_by(weekday) %>%
  summarize(TotalCalories = sum(Calories))

# Ensure that the weekdays are in the desired order
daily_calories_total_weekday$weekday <- factor(
  daily_calories_total_weekday$weekday,
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
)

# Create the bar plot with custom colors
ggplot(daily_calories_total_weekday, aes(x = weekday, y = TotalCalories, fill = weekday)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Calories Burned Per Weekday",
       x = "Days of the week",
       y = "Total Calories") +
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  theme_minimal()

# Create a heatmap of total calories burned per month and year
heatmap_plot <- ggplot(monthly_total_calories, aes(x = Month, y = Year, fill = TotalCalories)) +
  geom_tile() +
  labs(title = "Total Calories Burned Per Month and Year",
       x = "Month",
       y = "Year",
       fill = "Total Calories") +
  scale_fill_gradient(low = "blue", high = "red") +  # Choose color scale
  theme_minimal()

# Display the heatmap
print(heatmap_plot)

# Intensity ---------------------------------------------------------------
daily_intensities <- read.csv("fitness/dailyIntensities_merged.csv")

str(daily_intensities)

# Convert ActivityDay to Date
daily_intensities$ActivityDay <- as.Date(daily_intensities$ActivityDay, format = "%m/%d/%Y")

# Extract year, month, and weekday
daily_intensities$Year <- year(daily_intensities$ActivityDay)
daily_intensities$Month <- month(daily_intensities$ActivityDay, label = TRUE)
daily_intensities$Weekday <- weekdays(daily_intensities$ActivityDay)

# Create a custom color palette
custom_colors <- c("#E89594", "#D69AB8", "#A3A9D2", "#57B8CD", "#26C0A5", "#65BE68", "#ACB230", "#EE982D")

# Create a data frame with the total minutes for each activity level
activity_totals <- data.frame(
  Activity = c("Sedentary", "Lightly Active", "Fairly Active", "Very Active"),
  TotalMinutes = c(
    sum(daily_intensities$SedentaryMinutes),
    sum(daily_intensities$LightlyActiveMinutes),
    sum(daily_intensities$FairlyActiveMinutes),
    sum(daily_intensities$VeryActiveMinutes)
  )
)

# Calculate the percentages
activity_totals$Percentage <- (activity_totals$TotalMinutes / sum(activity_totals$TotalMinutes)) * 100

# Create a pie chart with labels
ggplot(activity_totals, aes(x = "", y = TotalMinutes, fill = Activity)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Distribution of Activity Levels") +
  theme_void() +
  theme(legend.position = "bottom") +
  geom_text(aes(label = paste0(Activity, "\n", round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(axis.text = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

# Create a pie chart with percentages and a legend
ggplot(activity_totals, aes(x = "", y = TotalMinutes, fill = Activity)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Distribution of Activity Levels") +
  theme_void() +
  theme(legend.position = "bottom") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Activity")) +
  theme(axis.text = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

# Create a pie chart with percentages and a legend
ggplot(activity_totals, aes(x = "", y = TotalMinutes, fill = Activity)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Distribution of Activity Levels") +
  theme_void() +
  theme(legend.position = "bottom") +
  geom_text(aes(label = ifelse(Activity %in% c("Fairly Active", "Very Active"), "", paste0(round(Percentage, 1), "%"))),
            position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Activity")) +
  theme(axis.text = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

activity_weekday <- daily_intensities %>%
  group_by(Weekday) %>%
  summarise(
    Sedentary = sum(SedentaryMinutes),
    LightlyActive = sum(LightlyActiveMinutes),
    FairlyActive = sum(FairlyActiveMinutes),
    VeryActive = sum(VeryActiveMinutes)
  ) %>%
  gather(Activity, Minutes, -Weekday)

# Define custom colors for the activity levels
custom_colors <- c(
  "Sedentary" = "#F09099",
  "LightlyActive" = "#81B0D8",
  "FairlyActive" = "#55BF7A",
  "VeryActive" = "#ED9837"
)

# Create the bar plot
ggplot(activity_weekday, aes(x = Weekday, y = Minutes, fill = Activity)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Total Activities per Weekday",
       x = "Weekday",
       y = "Total Minutes") +
  theme_minimal() +
  theme(legend.title = element_blank())

activity_month <- daily_intensities %>%
  group_by(Month) %>%
  summarise(
    Sedentary = sum(SedentaryMinutes),
    LightlyActive = sum(LightlyActiveMinutes),
    FairlyActive = sum(FairlyActiveMinutes),
    VeryActive = sum(VeryActiveMinutes)
  ) %>%
  gather(Activity, Minutes, -Month)

custom_colors <- c(
  "Sedentary" = "#F09099",
  "LightlyActive" = "#81B0D8",
  "FairlyActive" = "#55BF7A",
  "VeryActive" = "#ED9837"
)

# Create the stacked bar plot
ggplot(activity_month, aes(x = Month, y = Minutes, fill = Activity)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Total Activities per Month",
       x = "Month",
       y = "Total Minutes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title = element_blank())

activity_year <- daily_intensities %>%
  group_by(Year) %>%
  summarise(
    Sedentary = sum(SedentaryMinutes),
    LightlyActive = sum(LightlyActiveMinutes),
    FairlyActive = sum(FairlyActiveMinutes),
    VeryActive = sum(VeryActiveMinutes)
  ) %>%
  gather(Activity, Minutes, -Year)

# Define custom colors for the activity levels
custom_colors <- c(
  "Sedentary" = "#F09099",
  "LightlyActive" = "#81B0D8",
  "FairlyActive" = "#55BF7A",
  "VeryActive" = "#ED9837"
)

# Create the stacked bar plot
ggplot(activity_year, aes(x = Year, y = Minutes, fill = Activity)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Total Activities for the Year",
       x = "Year",
       y = "Total Minutes") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Create a histogram to visualize the distribution of activity minutes
ggplot(daily_intensities, aes(x = SedentaryMinutes)) +
  geom_histogram(binwidth = 30, fill = "#F09099", color = "black") +
  labs(title = "Distribution of Sedentary Minutes",
       x = "Sedentary Minutes",
       y = "Frequency") +
  theme_minimal()

# Create histograms for other activity levels (LightlyActive, FairlyActive, VeryActive)
ggplot(daily_intensities, aes(x = LightlyActiveMinutes)) +
  geom_histogram(binwidth = 30, fill = "#81B0D8", color = "black") +
  labs(title = "Distribution of Lightly Active Minutes",
       x = "Lightly Active Minutes",
       y = "Frequency") +
  theme_minimal()

ggplot(daily_intensities, aes(x = FairlyActiveMinutes)) +
  geom_histogram(binwidth = 30, fill = "#55BF7A", color = "black") +
  labs(title = "Distribution of Fairly Active Minutes",
       x = "Fairly Active Minutes",
       y = "Frequency") +
  theme_minimal()

ggplot(daily_intensities, aes(x = VeryActiveMinutes)) +
  geom_histogram(binwidth = 30, fill = "#ED9837", color = "black") +
  labs(title = "Distribution of Very Active Minutes",
       x = "Very Active Minutes",
       y = "Frequency") +
  theme_minimal()

# Reshape the data from wide to long format using the tidyr package
library(tidyr)
daily_intensities_long <- daily_intensities %>%
  pivot_longer(cols = c(SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes),
               names_to = "Activity",
               values_to = "Minutes")

# Create a time series plot for the four activity measures
ggplot(daily_intensities_long, aes(x = ActivityDay, y = Minutes, color = Activity)) +
  geom_line() +
  labs(title = "Time Series of Activity Minutes",
       x = "Date",
       y = "Minutes") +
  scale_color_manual(values = c("#F09099", "#81B0D8", "#55BF7A", "#ED9837")) +
  theme_minimal()

# Reshape the data from wide to long format using the tidyr package
library(tidyr)
daily_intensities_long <- daily_intensities %>%
  pivot_longer(cols = c(SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes),
               names_to = "Activity",
               values_to = "Minutes")

# Create a box plot for activity minutes by Weekday
ggplot(daily_intensities_long, aes(x = Weekday, y = Minutes, fill = Activity)) +
  geom_boxplot() +
  labs(title = "Distribution of Activity Minutes by Weekday",
       x = "Weekday",
       y = "Minutes") +
  scale_fill_manual(values = c("#F09099", "#81B0D8", "#55BF7A", "#ED9837")) +  # Custom colors
  theme_minimal()

# Create scatter plots for pairs of activity measures
scatter_plot_sedentary_lightly <- ggplot(daily_intensities, aes(x = SedentaryMinutes, y = LightlyActiveMinutes)) +
  geom_point(color = "#F09099") +
  labs(title = "Scatter Plot: Sedentary vs. Lightly Active Minutes",
       x = "Sedentary Minutes",
       y = "Lightly Active Minutes") +
  theme_minimal()

scatter_plot_fairly_very <- ggplot(daily_intensities, aes(x = FairlyActiveMinutes, y = VeryActiveMinutes)) +
  geom_point(color = "#55BF7A") +
  labs(title = "Scatter Plot: Fairly Active vs. Very Active Minutes",
       x = "Fairly Active Minutes",
       y = "Very Active Minutes") +
  theme_minimal()

library(gridExtra)
grid.arrange(scatter_plot_sedentary_lightly, scatter_plot_fairly_very, ncol = 2)

# Reshape the data from wide to long format using the tidyr package
library(tidyr)
daily_intensities_long <- daily_intensities %>%
  pivot_longer(cols = c(SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes),
               names_to = "Activity",
               values_to = "Minutes")

# Create a density plot for activity distribution throughout the day
ggplot(daily_intensities_long, aes(x = Minutes, fill = Activity)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Activity Distribution Throughout the Day",
       x = "Minutes",
       y = "Density") +
  scale_fill_manual(values = c("#F09099", "#81B0D8", "#55BF7A", "#ED9837")) +  # Custom colors
  theme_minimal()

# Reshape the data from wide to long format using the tidyr package
library(tidyr)
daily_intensities_long <- daily_intensities %>%
  pivot_longer(cols = c(SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes),
               names_to = "Activity",
               values_to = "Minutes")

# Create a density plot for activity distribution throughout the weekdays
ggplot(daily_intensities_long, aes(x = Minutes, fill = Activity)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Weekday, ncol = 2) +  # Create separate plots for each weekday
  labs(title = "Density Plot of Activity Distribution Throughout the Weekdays",
       x = "Minutes",
       y = "Density") +
  scale_fill_manual(values = c("#F09099", "#81B0D8", "#55BF7A", "#ED9837")) +  # Custom colors
  theme_minimal()

# Reshape the data from wide to long format using the tidyr package
library(tidyr)
daily_intensities_long <- daily_intensities %>%
  pivot_longer(cols = c(SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes),
               names_to = "Activity",
               values_to = "Minutes")

# Create a density plot for activity distribution throughout the months
ggplot(daily_intensities_long, aes(x = Minutes, fill = Activity)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Month, ncol = 3) +  # Create separate plots for each month
  labs(title = "Density Plot of Activity Distribution Throughout the Months",
       x = "Minutes",
       y = "Density") +
  scale_fill_manual(values = c("#F09099", "#81B0D8", "#55BF7A", "#ED9837")) +  # Custom colors
  theme_minimal()

# Reshape the data from wide to long format using the tidyr package
library(tidyr)
daily_intensities_long <- daily_intensities %>%
  pivot_longer(cols = c(SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes),
               names_to = "Activity",
               values_to = "Minutes")

# Create a density plot for activity distribution throughout the year
ggplot(daily_intensities_long, aes(x = Minutes, fill = Activity)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Year, ncol = 1) +  # Create a single plot for the entire year
  labs(title = "Density Plot of Activity Distribution Throughout the Year",
       x = "Minutes",
       y = "Density") +
  scale_fill_manual(values = c("#F09099", "#81B0D8", "#55BF7A", "#ED9837")) +  # Custom colors
  theme_minimal()

install.packages("shiny")
library(shiny)

ui <- fluidPage(
  titlePanel("Daily Activity Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:", choices = colnames(daily_intensities)[3:13]),
      dateRangeInput("date_range", "Select Date Range:")
    ),
    mainPanel(
      plotOutput("activity_plot")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    # Filter data based on date range
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    filtered <- daily_intensities %>%
      filter(ActivityDay >= start_date, ActivityDay <= end_date)
    return(filtered)
  })
  
  output$activity_plot <- renderPlot({
    variable_name <- input$variable
    ggplot(filtered_data(), aes(x = ActivityDay, y = !!sym(variable_name))) +
      geom_line() +
      labs(y = variable_name) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

# sleep -------------------------------------------------------------------
sleep <- read.csv("fitness/sleepDay_merged.csv")
str(sleep)

# Check data and remove NA's 
sum(duplicated(sleep))
sleep <- sleep %>%
  distinct() %>%
  drop_na()
sum(duplicated(sleep))

# Extract Hour and Minutes using lubridate
sleep$Hour <- hour(mdy_hms(sleep$SleepDay))
sleep$Minutes <- minute(mdy_hms(sleep$SleepDay))
# Extract Month, Year, and Date using lubridate
sleep$Month <- month(mdy_hms(sleep$SleepDay))
sleep$Year <- year(mdy_hms(sleep$SleepDay))
sleep$Day <- date(mdy_hms(sleep$SleepDay))

# Convert the "Day" column to a Date object
sleep$Day <- as.Date(sleep$Day)

# Create a new column "dayoftheweek" with the day of the week
sleep$dayoftheweek <- weekdays(sleep$Day)

# If you want to convert the day names to Monday-Sunday format, you can use this:
sleep$dayoftheweek <- factor(sleep$dayoftheweek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Define custom colors
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a time series plot for TotalMinutesAsleep
ggplot(data = sleep, aes(x = Day, y = TotalMinutesAsleep)) +
  geom_line(color = custom_colors[1]) +  # Use the first custom color
  labs(title = "Time Series Plot of TotalMinutesAsleep",
       x = "Date",
       y = "Total Minutes Asleep") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  scale_color_manual(values = custom_colors[1])  # Match the color with the line

# Create a time series plot for TotalTimeInBed
ggplot(data = sleep, aes(x = Day, y = TotalTimeInBed)) +
  geom_line(color = custom_colors[2]) +  # Use the second custom color
  labs(title = "Time Series Plot of TotalTimeInBed",
       x = "Date",
       y = "Total Time In Bed") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  scale_color_manual(values = custom_colors[2])  # Match the color with the line

# Define custom colors
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a box plot for TotalMinutesAsleep by dayoftheweek
ggplot(data = sleep, aes(x = dayoftheweek, y = TotalMinutesAsleep, fill = dayoftheweek)) +
  geom_boxplot() +
  labs(title = "Box Plot of TotalMinutesAsleep by Day of the Week",
       x = "Day of the Week",
       y = "Total Minutes Asleep") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors)

# Create a box plot for TotalTimeInBed by dayoftheweek
ggplot(data = sleep, aes(x = dayoftheweek, y = TotalTimeInBed, fill = dayoftheweek)) +
  geom_boxplot() +
  labs(title = "Box Plot of TotalTimeInBed by Day of the Week",
       x = "Day of the Week",
       y = "Total Time In Bed") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors)

# Define custom colors
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a histogram for TotalMinutesAsleep
ggplot(data = sleep, aes(x = TotalMinutesAsleep, fill = custom_colors[1])) +
  geom_histogram(binwidth = 30, color = "black", position = "identity") +
  labs(title = "Histogram of TotalMinutesAsleep",
       x = "Total Minutes Asleep",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors[1])

# Create a histogram for TotalTimeInBed
ggplot(data = sleep, aes(x = TotalTimeInBed, fill = custom_colors[2])) +
  geom_histogram(binwidth = 30, color = "black", position = "identity") +
  labs(title = "Histogram of TotalTimeInBed",
       x = "Total Time In Bed",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors[2])

# Define custom colors
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a data frame with average values by month
average_sleep_by_month <- aggregate(cbind(TotalMinutesAsleep, TotalTimeInBed) ~ Month, data = sleep, FUN = mean)
average_sleep_by_month
# Create a bar plot for TotalMinutesAsleep by month
ggplot(data = average_sleep_by_month, aes(x = factor(Month), y = TotalMinutesAsleep, fill = factor(Month))) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Average TotalMinutesAsleep by Month",
       x = "Month",
       y = "Average Total Minutes Asleep") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors)

# Create a bar plot for TotalTimeInBed by month
ggplot(data = average_sleep_by_month, aes(x = factor(Month), y = TotalTimeInBed, fill = factor(Month))) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Average TotalTimeInBed by Month",
       x = "Month",
       y = "Average Total Time In Bed") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors)
average_sleep_by_month

# Define custom colors
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a data frame with average values by weekday
average_sleep_by_weekday <- aggregate(cbind(TotalMinutesAsleep, TotalTimeInBed) ~ dayoftheweek, data = sleep, FUN = mean)

# Reorder weekdays to ensure they are in the correct order (Sunday to Saturday)
average_sleep_by_weekday$dayoftheweek <- factor(average_sleep_by_weekday$dayoftheweek, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Create a bar plot for TotalMinutesAsleep by weekday
ggplot(data = average_sleep_by_weekday, aes(x = dayoftheweek, y = TotalMinutesAsleep, fill = dayoftheweek)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Average TotalMinutesAsleep by Weekday",
       x = "Weekday",
       y = "Average Total Minutes Asleep") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors)

# Create a bar plot for TotalTimeInBed by weekday
ggplot(data = average_sleep_by_weekday, aes(x = dayoftheweek, y = TotalTimeInBed, fill = dayoftheweek)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Average TotalTimeInBed by Weekday",
       x = "Weekday",
       y = "Average Total Time In Bed") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors)

# Define custom colors
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a data frame with average values by month and dayoftheweek
average_sleep_by_month_and_day <- aggregate(cbind(TotalMinutesAsleep, TotalTimeInBed) ~ Month + dayoftheweek, data = sleep, FUN = mean)

# Reorder the levels of dayoftheweek to ensure they are displayed in the correct order
average_sleep_by_month_and_day$dayoftheweek <- factor(average_sleep_by_month_and_day$dayoftheweek, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Create a heatmap for TotalMinutesAsleep
ggplot(data = average_sleep_by_month_and_day, aes(x = Month, y = dayoftheweek, fill = TotalMinutesAsleep)) +
  geom_tile() +
  labs(title = "Heatmap of Sleep Duration (TotalMinutesAsleep)",
       x = "Month",
       y = "Day of the Week",
       fill = "Average Total Minutes Asleep") +
  scale_fill_gradientn(colors = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

# Create a heatmap for TotalTimeInBed
ggplot(data = average_sleep_by_month_and_day, aes(x = Month, y = dayoftheweek, fill = TotalTimeInBed)) +
  geom_tile() +
  labs(title = "Heatmap of Sleep Duration (TotalTimeInBed)",
       x = "Month",
       y = "Day of the Week",
       fill = "Average Total Time In Bed") +
  scale_fill_gradientn(colors = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

# Define custom colors
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a scatter plot
ggplot(data = sleep, aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) +
  geom_point(color = custom_colors[1], size = 3) +
  labs(title = "Scatter Plot of TotalMinutesAsleep vs. TotalTimeInBed",
       x = "Total Minutes Asleep",
       y = "Total Time In Bed") +
  theme_minimal() +
  scale_color_manual(values = custom_colors[1])

# Define custom colors
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a data frame with the count of sleep records by month
sleep_records_by_month <- data.frame(table(sleep$Month))

# Rename the columns for clarity
colnames(sleep_records_by_month) <- c("Month", "Count")

str(sleep_records_by_month)

# Create a pie chart
ggplot(data = sleep_records_by_month, aes(x = "", y = Count, fill = factor(Month))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Sleep Records by Month",
       fill = "Month") +
  theme_void() +
  scale_fill_manual(values = custom_colors)

# weight ------------------------------------------------------------------
weight <- read.csv("fitness/weightLogInfo_merged.csv")
view(weight)
str(weight)
sum(duplicated(weight))

# Calculate the average BMI
average_bmi <- mean(weight$BMI, na.rm = TRUE)
average_bmi

# Print the average BMI
cat("Average BMI:", round(average_bmi, 2), "\n")

# Create a plot of the average BMI
ggplot(data = weight, aes(x = 1, y = BMI)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue") +
  geom_text(aes(label = round(average_bmi, 2)), vjust = -0.5) +
  labs(x = NULL, y = "Average BMI") +
  theme_minimal()

weight <- read.csv("fitness/weightLogInfo_merged.csv")

# Load the lubridate package
library(lubridate)

# Convert the Date column to a proper date-time format
weight$Date <- as.POSIXct(weight$Date, format = "%m/%d/%Y %I:%M:%S %p")

# Create separate columns for Day, Month, Year, and daysoftheweek
weight$Day <- day(weight$Date)
weight$Month <- month(weight$Date)
weight$Year <- year(weight$Date)
weight$DayOfWeek <- weekdays(weight$Date)

# Create separate columns for hours, minutes, and seconds
weight$Hours <- hour(weight$Date)
weight$Minutes <- minute(weight$Date)
weight$Seconds <- second(weight$Date)

view(weight)
str(weight)

# Create a custom color palette
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a time series plot for WeightKg
ggplot(data = weight, aes(x = Date)) +
  geom_line(aes(y = WeightKg, color = DayOfWeek, group = DayOfWeek)) +
  scale_color_manual(values = custom_colors) +
  labs(x = "Date", y = "Weight", title = "Weight Over Time by Day of the Week (Kg)") +
  theme_minimal()

# Create a time series plot for WeightPounds
ggplot(data = weight, aes(x = Date)) +
  geom_line(aes(y = WeightPounds, color = DayOfWeek, group = DayOfWeek)) +
  scale_color_manual(values = custom_colors) +
  labs(x = "Date", y = "Weight", title = "Weight Over Time by Day of the Week (Pounds)") +
  theme_minimal()

# Create a custom color palette
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a bar plot for the distribution of measurements across days of the week
ggplot(data = weight, aes(x = factor(DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), fill = DayOfWeek)) +
  geom_bar() +
  scale_fill_manual(values = custom_colors) +
  labs(x = "Day of the Week", y = "Count", title = "Distribution of Measurements by Day of the Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a custom color palette
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a box plot for WeightKg
ggplot(data = weight, aes(x = "WeightKg", y = WeightKg)) +
  geom_boxplot(fill = custom_colors[1]) +
  labs(x = "", y = "WeightKg", title = "Box Plot of WeightKg") +
  theme_minimal()

# Create a box plot for WeightPounds
ggplot(data = weight, aes(x = "WeightPounds", y = WeightPounds)) +
  geom_boxplot(fill = custom_colors[2]) +
  labs(x = "", y = "WeightPounds", title = "Box Plot of WeightPounds") +
  theme_minimal()

# Create a custom color palette
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a histogram for WeightKg
ggplot(data = weight, aes(x = WeightKg, fill = "WeightKg")) +
  geom_histogram(binwidth = 2, color = "black") +
  scale_fill_manual(values = custom_colors[1]) +
  labs(x = "Weight (Kg)", y = "Frequency", title = "Histogram of WeightKg") +
  theme_minimal()

# Create a histogram for WeightPounds
ggplot(data = weight, aes(x = WeightPounds, fill = "WeightPounds")) +
  geom_histogram(binwidth = 5, color = "black") +
  scale_fill_manual(values = custom_colors[2]) +
  labs(x = "Weight (Pounds)", y = "Frequency", title = "Histogram of WeightPounds") +
  theme_minimal()

# Create a histogram for BMI
ggplot(data = weight, aes(x = BMI, fill = "BMI")) +
  geom_histogram(binwidth = 2, color = "black") +
  scale_fill_manual(values = custom_colors[4]) +
  labs(x = "BMI", y = "Frequency", title = "Histogram of BMI") +
  theme_minimal()

# Load the ggplot2 package
library(ggplot2)

# Create a custom color palette
custom_colors <- c("#D19CBA", "#9FAAD2")

# Calculate the proportions of manual and automatic reports
manual_count <- sum(weight$IsManualReport == "True")
automatic_count <- nrow(weight) - manual_count
report_counts <- data.frame(Category = c("Manual", "Automatic"), Count = c(manual_count, automatic_count))

# Calculate the percentage of each category rounded to 2 decimal places
report_counts$Percentage <- round((report_counts$Count / sum(report_counts$Count)) * 100, 2)

# Create the pie chart with percentages
ggplot(data = report_counts, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors) +
  labs(fill = "Report Type") +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Proportion of Manual vs. Automatic Reports") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# Load the ggplot2 and reshape2 packages
library(ggplot2)
library(reshape2)

# Create a custom color palette
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Select numeric variables for correlation analysis
numeric_vars <- c("WeightKg", "WeightPounds", "Fat", "BMI")  # Add other variables as needed

# Subset the data to include only the selected numeric variables
numeric_data <- weight[, numeric_vars]

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data)

# Create a heatmap of the correlation matrix
ggplot(data = melt(correlation_matrix), aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = custom_colors, limits = c(-1, 1)) +
  labs(title = "Correlation Heatmap", x = "Variable", y = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load the ggplot2 package
library(ggplot2)

# Create a custom color palette
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a line chart for WeightKg variation by Hours of the day
ggplot(data = weight, aes(x = Hours, y = WeightKg, group = 1)) +
  geom_line(color = custom_colors[1]) +
  labs(x = "Hour of the Day", y = "WeightKg", title = "WeightKg Variation by Hour of the Day") +
  theme_minimal()

# Create a line chart for WeightPounds variation by Hours of the day
ggplot(data = weight, aes(x = Hours, y = WeightPounds, group = 1)) +
  geom_line(color = custom_colors[2]) +
  labs(x = "Hour of the Day", y = "WeightPounds", title = "WeightPounds Variation by Hour of the Day") +
  theme_minimal()

# MET ---------------------------------------------------------------------
minute_met_narrow <- read.csv("fitness/minuteMETsNarrow_merged.csv")
str(minute_met_narrow)
view(minute_met_narrow)

minute_met_narrow <- minute_met_narrow %>%
  mutate(
    ActivityMinute = as.POSIXct(ActivityMinute, format = "%m/%d/%Y %I:%M:%S %p"),
    Day = as.integer(format(ActivityMinute, "%d")),
    Month = as.integer(format(ActivityMinute, "%m")),
    Year = as.integer(format(ActivityMinute, "%Y")),
    Hours = as.integer(format(ActivityMinute, "%H")),
    Minutes = as.integer(format(ActivityMinute, "%M")),
    Seconds = as.integer(format(ActivityMinute, "%S"))
  )

view(minute_met_narrow)

# Extract Hour and Minutes using lubridate
# minute_met_narrow$Hour <- hour(mdy_hms(minute_met_narrow$ActivityMinute))
# minute_met_narrow$Minutes <- minute(mdy_hms(minute_met_narrow$ActivityMinute))
# Extract Month, Year, and Date using lubridate
# minute_met_narrow$Month <- month(mdy_hms(minute_met_narrow$ActivityMinute))
# minute_met_narrow$Year <- year(mdy_hms(minute_met_narrow$ActivityMinute))
# minute_met_narrow$Day <- date(mdy_hms(minute_met_narrow$ActivityMinute))

# Convert the "Day" column to a Date object
minute_met_narrow$Day <- as.Date(minute_met_narrow$Day)

# Create a new column "dayoftheweek" with the day of the week
minute_met_narrow$dayoftheweek <- weekdays(minute_met_narrow$Day)

# If you want to convert the day names to Monday-Sunday format, you can use this:
minute_met_narrow$dayoftheweek <- factor(minute_met_narrow$dayoftheweek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

view(minute_met_narrow)

# Create a custom color palette
custom_colors <- c("#D19CBA", "#9FAAD2", "#55B8CD", "#1AC0A7", "#5CBF6B", "#A2B52F", "#E59C1D")

# Create a ggplot object
ggplot(minute_met_narrow, aes(x = ActivityMinute, y = METs, color = dayoftheweek)) +
  geom_line() +
  scale_color_manual(values = custom_colors) +
  labs(
    x = "Time",
    y = "METs",
    title = "METs Over Time by Day of the Week"
  ) +
  theme_minimal()

# Group by hour and day of the week, calculate the average METs
heatmap_data <- minute_met_narrow %>%
  group_by(Hours, dayoftheweek) %>%
  summarise(Avg_METs = mean(METs))

# Create a ggplot object for the heatmap
ggplot(heatmap_data, aes(x = Hours, y = dayoftheweek, fill = Avg_METs)) +
  geom_tile() +
  scale_fill_gradient(low = "#D19CBA", high = "#E59C1D") +  # Use custom colors
  labs(
    x = "Hour of the Day",
    y = "Day of the Week",
    fill = "Average METs",
    title = "Heatmap of METs by Hour and Day of the Week"
  ) +
  theme_minimal()

ggplot(minute_met_narrow, aes(x = Hours, y = METs)) +
  geom_point() +
  facet_wrap(~ dayoftheweek, scales = "free_y", ncol = 3) +
  labs(
    x = "Hour of the Day",
    y = "METs",
    title = "Scatter Plots of METs by Hour (Faceted by Day of the Week)"
  ) +
  theme_minimal()

# Calculate the average METs value for each day
avg_METs_by_day <- minute_met_narrow %>%
  mutate(Date = as.Date(ActivityMinute)) %>%
  group_by(Date) %>%
  summarise(Avg_METs = mean(METs))

# Create an interactive calendar heatmap
avg_METs_by_day %>%
  plot_ly(x = ~Date, y = ~Avg_METs, z = ~Avg_METs, type = "heatmap", colors = custom_colors) %>%
  colorbar(title = "Average METs") %>%
  layout(
    title = "Interactive Calendar Heatmap of Average METs",
    xaxis = list(type = "category", title = "Day"),
    yaxis = list(title = "Average METs"),
    margin = list(l = 50, r = 50, b = 100)
  )
