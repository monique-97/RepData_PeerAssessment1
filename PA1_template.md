---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Change language:


```r
Sys.setlocale(category = "LC_ALL", locale = "english")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

Read data:

```r
if (!file.exists('activity.csv')) {
  unzip(zipfile = "activity.zip")
}
all_data <- read.csv(file="activity.csv", header=TRUE)
```


## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day:


```r
steps_aggregated <- aggregate(steps ~ date, all_data, FUN=sum)
```

Make a histogram of the total number of steps taken each day

```r
hist(steps_aggregated$steps, main = "Total number of steps taken each day", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day

```r
steps_mean <- mean(steps_aggregated$steps, na.rm = TRUE)
steps_mean
```

```
## [1] 10766.19
```

```r
steps_median <- median(steps_aggregated$steps, na.rm = TRUE)
steps_median
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
steps_interval_aggregated <- aggregate(steps~interval, all_data, FUN=mean, na.rm=TRUE)
```

Making a plot:

```r
plot(x = steps_interval_aggregated$interval, y = steps_interval_aggregated$steps, type = "l") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval <- steps_interval_aggregated[which.max(steps_interval_aggregated$steps),]
max_interval
```

```
##     interval    steps
## 104      835 206.1698
```




## Imputing missing values

Calculate and report the total number of missing values in the dataset


```r
missing_values <- is.na(all_data$steps)

number_of_miss <- length(which(missing_values==TRUE))
```

Devise a strategy for filling in all of the missing values in the dataset.
Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
new_all_data <- transform(all_data, steps = ifelse(is.na(all_data$steps),
                                                   steps_interval_aggregated$steps[match(all_data$interval,steps_interval_aggregated$interval)],
                                                   all_data$steps))
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
new_steps_aggregated <- aggregate(steps ~ date, new_all_data, FUN=sum)
hist(new_steps_aggregated$steps, main = "Total number of steps taken each day", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
new_steps_mean <- mean(new_steps_aggregated$steps, na.rm = TRUE)
new_steps_median <- median(new_steps_aggregated$steps, na.rm = TRUE)

difference_mean <- new_steps_mean - steps_mean
difference_median <- new_steps_median - steps_mean
```

What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
total_steps <- sum(steps_aggregated$steps)
new_total_steps <- sum(new_steps_aggregated$steps)

difference <- new_total_steps - total_steps
```




## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – 
“weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
new_all_data_weekday <- cbind(new_all_data, datetype= ifelse(weekdays(as.Date(new_all_data$date)) == "Saturday" | 
                                                                     weekdays(as.Date(new_all_data$date)) == "Sunday", "weekend", 
                                                                     "weekday"))

new_all_data_weekday_agg <- aggregate(steps ~ interval + datetype, new_all_data_weekday, mean)
```

Make a panel plot containing a time series plot of the 
5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(ggplot2)
ggplot(data = new_all_data_weekday_agg, aes(x = interval, y = steps)) + 
    geom_line() +
    facet_grid(datetype ~ .) +
    ggtitle("Average number of steps taken") +
    xlab("5-minute interval") +
    ylab("Average number of steps") +
    theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->



