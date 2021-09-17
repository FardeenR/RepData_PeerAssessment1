---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---






## 1. Loading and Processing the Data


```r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, "%Y-%m-%d")
activity <- data[ with (data, { !(is.na(steps)) } ), ]
head(activity)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```



## 2. What is mean total number of steps taken per day?

Calculate the total number of steps taken per day


```r
daily_steps <- activity %>% group_by(date) %>% summarise(steps=sum(steps))
hist(daily_steps$steps, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->



Report the mean and median of the total number of steps taken per day


```r
summary(daily_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```



## 3. What is the average daily activity pattern?

Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg_step_daily <- aggregate(steps ~ interval, activity, mean)
plot(avg_step_daily$interval, avg_step_daily$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avg_step_daily[which.max(avg_step_daily$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

The interval 835 has the maximum number of steps



## 4. Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
sum(is.na(data))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


Replacing NAs with mean for that 5-min interval


```r
activity_impute <- data
for (i in 1:nrow(activity_impute)) {
  if (is.na(activity_impute$steps[i])) {
    interval_value <- activity_impute$interval[i]
    steps_value <- avg_step_daily[
      avg_step_daily$interval == interval_value,]
    activity_impute$steps[i] <- steps_value$steps
  }
}
```



Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
dataset_imputed <- aggregate(steps ~ date, activity_impute, sum)
head(dataset_imputed)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```



Make a histogram of the total number of steps taken each day


```r
hist(dataset_imputed$steps, main="Histogram of total number of steps per day (imputed)", 
     xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



Calculate and report the mean and median total number of steps taken per day.


```r
mean(dataset_imputed$steps)
```

```
## [1] 10766.19
```

```r
median(dataset_imputed$steps)
```

```
## [1] 10766.19
```



Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
## mean and median of data without NA's
mean(daily_steps$steps)
```

```
## [1] 10766.19
```

```r
median(daily_steps$steps)
```

```
## [1] 10765
```

Mean values is almost the same but there is slight difference in median value.



## 5. Are there differences in activity patterns between weekdays and weekends?


Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity_impute['type_of_day'] <- weekdays(as.Date(activity_impute$date))
activity_impute$type_of_day[activity_impute$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
activity_impute$type_of_day[activity_impute$type_of_day != "weekend"] <- "weekday"

activity_impute$type_of_day <- as.factor(activity_impute$type_of_day)

activity_imputed_by_interval <- aggregate(steps ~ interval + type_of_day, activity_impute, mean)
```



Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
qplot(interval, 
      steps, 
      data = activity_imputed_by_interval, 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
  facet_wrap(~ type_of_day, ncol = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
