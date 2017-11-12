# Reproducible Research: Peer Assessment 1
Benjamin Smith  

###1. Load data
- Set working directory


- Load required R packages.

```r
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(lattice)
library(knitr)
```
- read CSV into memory

```r
activity <- read.csv("1Data/activity.csv", header=TRUE, na.strings = "NA") ##read data
```

###2. Process and Transform data
- Create date time from date and interval

```r
time0 <- c(activity$interval) ##set interval as a vector
time1 <- str_pad(time0, 4, pad="0") ##add leading 0's to values with stringr
activity$datetime <- ymd_hm(paste(activity$date, time1)) ##parse date-time with lubridate
```

- Display data structure and summary

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ datetime: POSIXct, format: "2012-10-01 00:00:00" "2012-10-01 00:05:00" ...
```

```r
summary(activity, na.rm=TRUE)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840                   
##     datetime                  
##  Min.   :2012-10-01 00:00:00  
##  1st Qu.:2012-10-16 05:58:45  
##  Median :2012-10-31 11:57:30  
##  Mean   :2012-10-31 11:57:30  
##  3rd Qu.:2012-11-15 17:56:15  
##  Max.   :2012-11-30 23:55:00  
## 
```

## What is mean total number of steps taken per day?  

###1. Calculate steps per day


```r
#isolate date from datetime
activity$date <- as.Date(activity$datetime)  

#dplyr group/sum steps
total.steps <- activity %>%
        group_by(date) %>%
        summarise(sum.steps = sum(steps,na.rm=TRUE), na=mean(is.na(steps)))  
```

###2. Make a histogram of the total number of steps taken each day

- **Note:** Histogram ignores days where 0 steps are recorded

```r
a <- ggplot(activity, aes(date, steps)) 
b <- a + geom_bar(stat = "identity", colour = "blue", fill = "blue", width = 0.7)
c <- b + labs(title = "Histogram of Total Number of Steps Taken Each Day", 
        x = "Date", y = "Total number of steps")
print(c)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

###3. Calculate and report the mean and median of the total number of steps taken per day


```r
#calculate mean and median steps per day
Mean.Steps <- as.integer(mean(total.steps$sum.steps))
Median.Steps <- as.integer(median(total.steps$sum.steps))  
```
 
- Mean total number of steps per day is **9354**.  
- Median total number of steps per day is **10395**.
  
## What is the average daily activity pattern?

###1. Make a time series line graph of the average number of steps taken, averaged across all days (y) to interval (x)
  

```r
int.mean <- activity %>%
        group_by(interval) %>%
        summarise(StepCount=mean(steps, na.rm=TRUE))
plot(x = 1:nrow(int.mean), y=int.mean$StepCount, type="l",
     col = "blue", xaxt = "n", xlab="Interval (24h Time of Day)", 
     ylab="Mean Steps across All Days", main="Mean Steps per Time Interval")
axis(1, labels=int.mean$interval[seq(1,288,12)],
     at = seq_along(int.mean$interval)[seq(1,288,12)])
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max.steps <- filter(int.mean, StepCount==max(StepCount))
```
  
- Interval **835** contains the highest average number of steps at **206**.  
  
## Imputing missing values
###1. Calculate and report the total number of missing values in the dataset.

```r
na.count<- sum(is.na(activity$steps))
na.pct<- mean(is.na(activity$steps))
```
  
- The total count of missing values in the data set is **2304**.

###2. Devise a strategy for filling in all the missing values in the dataset.

- **13.1%** of all day-intervals are missing step counts.  
- Imputing missing values can be accomplished effectively by filling NA values with the mean of the corresponding 5-minute interval across all days.

###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  
- Count of Na's at start

```r
sum(is.na(activity))
```

```
## [1] 2304
```
- Create new data set equal to original but with Na's replaced with mean steps per interval

```r
naActivity <- is.na(activity$steps) #isolate NA's
actsplt <- split(activity, as.factor(activity$interval)) #isolate intervals
mean_int <- sapply(actsplt, function(x) mean(x$steps, na.rm=TRUE)) #mean steps by intervals

nday <- length(levels(as.factor(activity$date))) #count of days in dataset

act_fill <- activity
act_fill[naActivity,]$steps <- rep(mean_int, nday)[naActivity]
cbind(head(activity[naActivity,]), head(act_fill[naActivity,]))
```

```
##   steps       date interval            datetime     steps       date
## 1    NA 2012-10-01        0 2012-10-01 00:00:00 1.7169811 2012-10-01
## 2    NA 2012-10-01        5 2012-10-01 00:05:00 0.3396226 2012-10-01
## 3    NA 2012-10-01       10 2012-10-01 00:10:00 0.1320755 2012-10-01
## 4    NA 2012-10-01       15 2012-10-01 00:15:00 0.1509434 2012-10-01
## 5    NA 2012-10-01       20 2012-10-01 00:20:00 0.0754717 2012-10-01
## 6    NA 2012-10-01       25 2012-10-01 00:25:00 2.0943396 2012-10-01
##   interval            datetime
## 1        0 2012-10-01 00:00:00
## 2        5 2012-10-01 00:05:00
## 3       10 2012-10-01 00:10:00
## 4       15 2012-10-01 00:15:00
## 5       20 2012-10-01 00:20:00
## 6       25 2012-10-01 00:25:00
```

- Count of Na's in new set to validate

```r
sum(is.na(act_fill))
```

```
## [1] 0
```

###4. Make a histogram of the total number of steps each day and calculate and report the mean and median total number of steps taken per day.  Do these values differ from the estimates from the first part of the assignment?  What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
a <- ggplot(act_fill, aes(date, steps)) 
b <- a + geom_bar(stat = "identity", colour = "blue", fill = "blue", width = 0.7)
c <- b + labs(title = "Histogram of Total Number of Steps Taken Each Day (with NA's replaced by mean per 5-minute interval)", 
        x = "Date", y = "Total number of steps")
print(c)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

#### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
Mean before and after imputation  

```r
c(before=mean(tapply(activity$steps, as.factor(activity$date), function(x) sum(x, na.rm=T))),
  after=mean(tapply(act_fill$steps, as.factor(act_fill$date), sum)))
```

```
##   before    after 
##  9354.23 10766.19
```
  
Median before and after imputation  

```r
c(before=median(tapply(activity$steps, as.factor(activity$date), function(x) sum(x, na.rm=T))),
  after=median(tapply(act_fill$steps, as.factor(act_fill$date), sum)))
```

```
##   before    after 
## 10395.00 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

###1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or a weekend day.

```r
activity$date <- as.Date(activity$date)
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") #Create weekday vector
activity$wday <- factor((weekdays(activity$date) %in% weekday),
                        levels=c(FALSE, TRUE), labels=c("weekend","weekday")) #create two-level factor variable for weekend/weekday
str(activity$wday) #display structure of wday
```

```
##  Factor w/ 2 levels "weekend","weekday": 2 2 2 2 2 2 2 2 2 2 ...
```

```r
summary(activity$wday) #display summary of wday
```

```
## weekend weekday 
##    4608   12960
```

###2. Make a panel plot containing a time series plot (type="l") of the 5-minute interval (x) and the average number of steps averaged across all weekday days or weekend days (y).

```r
act_fill <- aggregate(steps ~ interval + wday, data = activity, mean)
names(act_fill) <- c("interval", "wday", "steps")
xyplot(steps ~ interval | factor(wday),
        data=act_fill,
       type = "l",
       layout = c(2,1),
       xlab="5-minute Interval",
       ylab="Mean Steps",
       main="Mean Steps per Interval - Weekend vs. Weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

###Submit Assignment      

```r
#knit2html()
```
