#quantified self: 
# http://www.fitbit.com/uk
# http://www.nike.com/us/en_us/c/nikeplus-fuel
# https://jawbone.com/up
library(dplyr)
library(lubridate)

# 1
if(!file.exists("activity.zip")) {
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  dest <- "activity.zip"
  download.file(url = url, destfile = dest)
  unzip(zipfile = "activity.zip")
}
colClasses <- c("numeric", "Date", "numeric")
df <- read.table(file = "activity.csv", header = TRUE, 
                 colClasses = colClasses, sep="," )
df <- tbl_df(df)

# check enumeration of interval
max_interval_value <- max(df$interval) # 2355
# check number of interval enumerations
unique_intervals <- length(unique(df$interval)) # 288
expected_unique_intervals <- 24 * 12 # 288
expected_max_interval <- 24 * 12 * 5 # 1440

# We have correct(expected) number of intervals but wrong values ??
# Let's see the diff between intervals
diff(df$interval)[1:30]
df$interval[1:30]
# it seems that we have last 2 digits are minutes 
# and any number before stands for hour
# When this is used for plotting we don't want gaps
# some options: 1/ divide by 100 and take the quotient * 60 + reminder
# 2/ make a factor (thus we won't get gaps in plot)
# 3/ convert to time value

# The easiest way is to keep  interval as is but use as.factor in the plots
# 

convert_interval_values <- function(interval){
  a <- interval %/% 100
  b <- interval %% 100 
  res <- a * 60 + b
  return(res)
}

convert_interval_back2original <- function(interval) {
  a <- interval %/% 60
  b <- interval %% 60 
  res <- a * 100 + b
  return(res)
  
}

imputedata <- function(lut , df) {
  names(lut) <- c("key", "value")
  na_ind <- is.na(df$steps)
  intervals2impute <- df[na_ind, "interval"]
  to_impute <- lut[intervals2impute %in% lut$key ,"value"]
  df$steps[na_ind] <- to_impute
  df
  to_impute
}

df$interval2 <- convert_interval_values(df$interval)
df[10:20,]

# Let's remove the original interval
df <- df %>%
  select(steps, date, interval = interval2)

df %>%
  group_by(interval) %>%
  summarize(avg_steps_per_interval = length(steps)) 



# 2
# total numberof steps/day
df_tot <- df %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps, na.rm = TRUE))

hist(df_tot$total_steps, breaks = 20)

summarize(total_steps = sum(steps, na.rm = TRUE)
          
steps_per_day <- df_tot %>%
  summarize(
    average = mean(df_tot$total_steps),
    median = median(df_tot$total_steps)
  )

# mean and median steps/day
# OBS! dates with only NA in steps will have result 0 in the summarize
# another option would be to remove steps with NA as value --> less dates
  
# 3/ What is the average daily activity pattern?
df_interval <- df %>%
  group_by(interval) %>%
  summarize(avg_steps_per_interval = mean(steps, na.rm = TRUE))

# with(df_interval,plot(x= factor(interval, ordered = TRUE), 
#                       y = avg_steps_per_interval, type="l"))


with(df_interval,plot(x= interval, 
                      y = avg_steps_per_interval, type="l"))


# 4, there is a package "impute" that could assist
# however, imputation should take account in what type of data
# and what source of data we have. 
# Without any domain knowledge it's a bit daring task to impute any data

df_interval_median <- df %>%
  group_by(interval) %>%
  summarize(median_steps_per_interval = median(steps, na.rm = TRUE))

df_interval_max <- df %>%
  group_by(interval) %>%
  summarize(max_steps_per_interval = max(steps, na.rm = TRUE))

df_imputed <- imputedata(lut = df_interval_median, df)

# Variables
# steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
# date: The date on which the measurement was taken in YYYY-MM-DD format
# interval: Identifier for the 5-minute interval in which measurement was taken

# assignment:
# Ultimately, you will need to complete the entire assignment in a single R 
# markdown document that can be processed by knitr and 
# be transformed into an HTML file.
# echo = TRUE

# OBS!!!! Change to this library instead!!!
# https://github.com/guaponi/RepData_PeerAssessment1

# 1

# 2


# 3/ What is the average daily activity pattern?
# 
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
# 
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

# 4/ Imputing missing values
# 
# Note that there are a number of days/intervals where there are missing values 
# (coded as NA). The presence of missing days may introduce bias into 
# some calculations or summaries of the data.
# 
# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
# 
# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, 
# you could use the mean/median for that day, 
# or the mean for that 5-minute interval, etc.
# 
# Create a new dataset that is equal to the original dataset 
# but with the missing data filled in.
# 
# Make a histogram of the total number of steps taken each day 
# and Calculate and report the mean and median total number of 
# steps taken per day. Do these values differ from the estimates 
# from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates 
# of the total daily number of steps?
# 
# 


# 5 Are there differences in activity patterns between weekdays and weekends?
# 
# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.
# 
# Create a new factor variable in the dataset with two levels – 
# “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
# 
# Make a panel plot containing a time series plot (i.e. type = "l") 
# of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). 
# See the README file in the GitHub repository to see an example 
# of what this plot should look like using simulated data.

