# Loading and preprocessing the data
# Process/transform the data (if necessary) into a format suitable for your analysis

fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filename <- "getdata_dataset.zip"
download.file(fileURL, filename, mode = "wb")
list.files()
unzip("getdata_dataset.zip", files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)
data <- read.csv("activity.csv", header=TRUE)


# What is mean total number of steps taken per day?

## Make a histogram of the total number of steps taken each day
hist(tapply(data$steps, data$date, sum,na.rm=TRUE), breaks=20, 
     xlab= "sum of steps taken per day", main= "histogram")

## Calculate and report the mean and median total number of steps taken per day
mean(tapply(data$steps, data$date, sum,na.rm=TRUE))
median(tapply(data$steps, data$date, sum,na.rm=TRUE))


# What is the average daily activity pattern?


## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
## averaged across all days (y-axis)

plot(tapply(data$steps, data$interval, mean, na.rm=TRUE),type = "l", xlab = "interval", ylab="mean of steps")

## Which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?

which.max(tapply(data$steps, data$interval, mean, na.rm=TRUE))


# Imputing missing values
##Calculate and report the total number of missing values in the dataset

nrow(data[is.na(data$steps),])

##Devise a strategy for filling in all of the missing values in the dataset.
##The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
##or the mean for that 5-minute interval, etc.

complete <- data
average <- aggregate(x = list(steps = data$steps), by = list(interval = data$interval),FUN = mean, na.rm = TRUE)


for( i in 1:nrow(complete)){
  ifelse(is.na(complete[i,1]), 
         complete[i,1] <- average[average$interval==complete[i,3],2],
         complete[i,1] <- complete[i,1])}


## Make a histogram of the total number of steps taken each day 

hist(tapply(complete$steps, complete$date, sum, na.rm=TRUE), breaks = 20,
     xlab= "sum of steps taken per day", main= "histogram")

## Calculate and report the mean and median total number of steps taken per day
mean(tapply(complete$steps, complete$date, sum, na.rm=TRUE))
median(tapply(complete$steps, complete$date, sum, na.rm=TRUE))

##  Do these values differ from the estimates from the first part of the assignment?
## what is the impact of imputing missing data on the estimates of the total daily number of steps?
Mean and Median is higher in data with imputation. 
Because there are some day which have only missig data, these days`s frequency are 0.

# Are there differences in activity patterns between weekdays and weekends?

library(chron)
Sys.setlocale("LC_TIME","C")
complete$date <- as.Date(as.character(complete$date))
complete$week <- ifelse(weekdays(complete$date)=="Sunday"|weekdays(complete$date)=="Saturday", complete$week <- "weekend", complete$week <- "weekday")

library(lattice)
average <- aggregate(steps~interval+week,data=complete,mean,na.rm=TRUE)
xyplot (steps ~ interval |week, data = average, type="l", groups = week)

