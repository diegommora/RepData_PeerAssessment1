---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
activity <- read.csv(unzip("activity.zip"),stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date,"%Y-%m-%d")
```


## What is mean total number of steps taken per day?
* Total number of steps per day

```r
require(dplyr)
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
stepsday <- activity %>% group_by(date) %>% summarize(Total_Steps=sum(steps,na.rm = TRUE))
stepsday
```

```
## # A tibble: 61 x 2
##    date       Total_Steps
##    <date>           <int>
##  1 2012-10-01           0
##  2 2012-10-02         126
##  3 2012-10-03       11352
##  4 2012-10-04       12116
##  5 2012-10-05       13294
##  6 2012-10-06       15420
##  7 2012-10-07       11015
##  8 2012-10-08           0
##  9 2012-10-09       12811
## 10 2012-10-10        9900
## # … with 51 more rows
```

* Make a histogram of the total number of steps taken each day

```r
hist(stepsday$Total_Steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->
* Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps <- mean(stepsday$Total_Steps)
median_steps <- median(stepsday$Total_Steps)
mean_steps
```

```
## [1] 9354.23
```

```r
median_steps
```

```
## [1] 10395
```

## What is the average daily activity pattern?
* Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intervals <- activity %>% group_by(interval) %>% summarize(Avg_Steps=mean(steps,na.rm = TRUE))
with(intervals,plot(interval,Avg_Steps,type="l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervals[intervals$Avg_Steps==max(intervals$Avg_Steps),]
```

```
## # A tibble: 1 x 2
##   interval Avg_Steps
##      <int>     <dbl>
## 1      835      206.
```

## Imputing missing values
* Calculate and report the total number of missing values in the dataset 

Total NA's in the data set:

```r
sum(is.na(activity))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity2 <- activity
#find all the na values and then fill the avg steps value of that date (this values are in the intervals data frame)
for (i in 1:17568){
        if(is.na(activity2$steps[i])){
                activity2$steps[i] <- intervals[intervals$interval==activity2$interval[i],2]
        }
}
names(activity2)
```

```
## [1] "steps"    "date"     "interval"
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
activity2$steps <- as.numeric(activity2$steps)
stepsday2 <- activity2 %>% group_by(date) %>% summarise(Total_Steps=sum(steps))
hist(stepsday2$Total_Steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
mean_steps2 <- mean(stepsday2$Total_Steps)
median_steps2 <- median(stepsday2$Total_Steps)
mean_steps2
```

```
## [1] 10766.19
```

```r
median_steps2
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
weekdays <- weekdays(activity2$date)
for (i in 1:17568){
        if(weekdays[i] %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")){
                activity2$weekday[i] <- "Weekday"
        }
        else
                activity2$weekday[i] <- "Weekend"
}
activity2$weekday <- as.factor(activity2$weekday)
summary(activity2$weekday)
```

```
## Weekday Weekend 
##   12960    4608
```
## Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
intervals2 <- activity2 %>% group_by(interval,weekday) %>% summarize(Avg_Steps=mean(steps,na.rm = TRUE))
qplot(interval,Avg_Steps,data=intervals2, facets = weekday~.)+geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
