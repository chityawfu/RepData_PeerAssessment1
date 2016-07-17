Reproducible Research: Peer Assessment 1
==========================================
Created by Chit Yaw FU on July 17, 2016

### Basic settings

```r
echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers
```



### Loading and processing the data

```r
setwd("c:/MYDS")
dat<-read.csv('./data/activity.csv',stringsAsFactors = FALSE) #date in 2nd col read in as chr
str(dat)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(dat)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```



### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

* Make a histogram of the total number of steps taken each day

```r
dailydat <- aggregate(steps ~ date, data=dat, sum, na.rm = TRUE)
hist(dailydat$steps, breaks=20, main="Total Steps per Day", xlab="Steps", ylab="Frequency", col="lightgreen")
```

![plot of chunk Plot histogram1](figure/Plot histogram1-1.png)

* Calculate and report the mean and median total number of steps taken per day

Mean & Median of total number of steps taken per day:

```r
step_mean<-mean(dailydat$steps,na.rm=TRUE)
cat("The mean of steps taken each day is ",step_mean,"\n")
```

```
## The mean of steps taken each day is  10766.19
```

```r
step_median<-median(dailydat$steps,na.rm=TRUE)
cat("The median of steps taken each day is ",step_median,"\n")
```

```
## The median of steps taken each day is  10765
```



### What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg_interval<-aggregate(steps~interval, data=dat,mean,na.rm=TRUE)
with(avg_interval,
     plot(interval,steps,
          type='l',
          col='steelblue',
          lwd=2,
          main='Average steps within 5-minute interval',
          xlab='Interval',
          ylab='Average Steps'
          ))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
idx<-which.max(avg_interval$steps)
cat("The highest average number of steps of ",avg_interval[idx,2]," is found at ",avg_interval[idx,1],"th interval.\n",sep="")
```

```
## The highest average number of steps of 206.1698 is found at 835th interval.
```
The result suggests physical activity peaks at 8:35am. 


### Imputing missing values
* The total number of rows with NAs:


```r
num_NA<-sum(!complete.cases(dat))
cat("The number of missing observations is",num_NA)
```

```
## The number of missing observations is 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy is to use the mean for that 5-minute interval to fill each NA value in the steps column.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newdat<-dat
idx_na<-!complete.cases(dat) 
int_miss<-dat$interval[idx_na] 
newdat$steps[idx_na]<-with(avg_interval,steps[interval==int_miss]) 
head(newdat)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
newdailydat <- aggregate(steps ~ date, data=newdat, sum, na.rm = TRUE)
hist(newdailydat$steps, breaks=20, main="Total Steps per Day (after imputing missign values)", xlab="Steps", ylab="Frequency", col="darkblue")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)


* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean & Median of total number of steps taken per day:

```r
newstep_mean<-mean(newdailydat$steps,na.rm=TRUE)
newstep_median<-median(newdailydat$steps,na.rm=TRUE)
cat("After imputing missing values, the mean of steps taken each day is ",newstep_mean,"\n")
```

```
## After imputing missing values, the mean of steps taken each day is  10766.19
```

```r
cat("After imputing missing values, the median of steps taken each day is ",newstep_median,"\n")
```

```
## After imputing missing values, the median of steps taken each day is  10765.59
```
Compare them with the two before imputing missing data:

```r
step_mean
```

```
## [1] 10766.19
```

```r
step_median
```

```
## [1] 10765
```
So, after imputing the missing data, the new mean and median are close to the old values. 

### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
dat$date <- as.Date(dat$date)
dat$dayType<- ifelse(weekdays(dat$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
dat$dayType <-as.factor(dat$dayType)
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
newavg_interval<-aggregate(steps~interval+dayType, data=dat,mean,na.rm=TRUE)


library(lattice)
     xyplot(steps ~ interval | dayType, 
            data=newavg_interval,
            type = "l",  
            lwd=2,
            main = "Average Number of Steps within Intervals",
            xlab = "Intervals",
            ylab = "Average Number of Steps",
            layout = c(1, 2))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)


* The result suggests that people are physically active in the morning on weekday, but sit idle for the rest of the day. By contrast, people are quite active at all times during weekend.  
