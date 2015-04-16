# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv", header=TRUE, sep=",")
activity$steps <- ifelse(is.na(activity$steps), 0, activity$steps) #replace NA with 0 in "steps" column
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 32.48   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.:  0.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```


## What is mean total number of steps taken per day?\  
1. Make a histogram of the total number of steps taken each day

```r
steps.perdate <- aggregate(steps ~ date, data=activity, FUN=sum)
hist(steps.perdate$steps, xlab="Steps per day", ylim=c(0,30),
     main="Histogram of total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 
  
2. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps.perdate$steps)
```

```
## [1] 9354.23
```

```r
median(steps.perdate$steps)
```

```
## [1] 10395
```


## What is the average daily activity pattern?  
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps.interval <- aggregate(steps ~ interval, data = activity, FUN=mean)
plot(steps ~ interval, data=steps.interval, type="l", 
     xlab="Interval in 5 mins", ylab="Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
