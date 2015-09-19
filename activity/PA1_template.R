# Read the data file
activity=read.csv("activity.csv")
str(activity)

#What is mean total number of steps taken per day?
##Calculate the total number of steps taken per day

library(dplyr)
activity<-tbl_df(activity)
str(activity)
dailyactivity<-activity %>%
  select(date,steps) %>%
    group_by(date) %>%
  summarize(na=sum(is.na(steps)),steps=sum(steps)) %>%
  ungroup()
View(dailyactivity)
##Make a histogram of the total number of steps taken each day
png("histogram.png")
hist(dailyactivity$steps,col="red",xlab="Number of steps per day",ylab="",main="Histogram showing the distribution of daily steps")
dev.off()
##Calculate and report the mean and median of the total number of steps taken per day
library(xtable)
xtable(summary(dailyactivity$steps))

summary(dailyactivity$steps)

##What is the average daily activity pattern?
patternactivity<-activity %>%
  select(interval,steps) %>%
  group_by(interval) %>%
  summarize(steps=mean(steps,na.rm=T)) %>%
  ungroup()
View(patternactivity)
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
##the average number of steps taken, averaged across all days (y-axis)
png("Time_series.png")
plot(patternactivity$interval,
     patternactivity$steps,type="l",lwd=2,col="red",xaxp  = c(0, 2400, 24),
     ylab="Mean Number of steps",xlab="Time")
dev.off()
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max(patternactivity$steps)
patternactivity$interval[which.max(patternactivity$steps)]
#Imputing missing values
##Calculate and report the total number of missing values in the dataset 
summary(activity)
missing<-activity %>%
  select(date,steps) %>%
  group_by(date) %>%
  summarize(na=sum(is.na(steps))) %>%
  ungroup()
##Multiple imputation
library(mice)
summary(activity)
set.seed(144)
imputed = complete(mice(activity))
summary(imputed)

##
dailyimputed=imputed %>%
  select(date,steps) %>%
  group_by(date) %>%
  summarize(steps=sum(steps)) %>%
  ungroup
##comparing before and after imputation
###Making histogram
png("histogram2.png")
par(mfcol=c(2,1))
hist(dailyactivity$steps,col="red",xlab="Number of steps per day",ylab="",main="Histogram showing the distribution of daily steps before imputation of missing values")
hist(dailyimputed$steps,col="blue",xlab="Number of steps per day",ylab="",main="Histogram showing the distribution of daily steps after imputation of missing values")

dev.off()

###Calculating mean and median steps
summary(dailyimputed$steps)
summary(dailyactivity$steps)

#Are there differences in activity patterns between weekdays and weekends?
##Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
# Deal with Date, conveting it from factor into date
activity$date=strptime(activity$date,"%Y-%m-%d")
activity$weekday=weekdays(activity$date)
activity$day= ifelse(activity$weekday=="Saturday"|activity$weekday=="Sunday", 1, 0)
str(activity)
View(activity)

patternactivityday<-activity %>%
  select(interval,steps,day) %>%
  group_by(day,interval) %>%
  summarize(steps=mean(steps,na.rm=T)) %>%
  ungroup()
View(patternactivityday)
patternactivityweekday=patternactivityday[patternactivityday$day==0,]
View(patternactivityweekday)
patternactivityweekend=patternactivityday[patternactivityday$day==1,]
View(patternactivityweekend)
windows()
par(mfrow=c(2,1),mar=c(4,4,2,1),oma=c(0,0,2,0))
plot(patternactivityweekday$interval,
     patternactivityweekday$steps,type="l",lwd=2,col="red",xaxp  = c(0, 2400, 24),
     ylab="Mean Number of steps",xlab="Interval",main="Weekday")
plot(patternactivityweekend$interval,
     patternactivityweekend$steps,type="l",lwd=2,col="red",xaxp  = c(0, 2400, 24),
     ylab="Mean Number of steps",xlab="Interval",main="Weekend")
mtext("Pattern of activity on weekdays and weekends",outer=TRUE)
dev.off()

