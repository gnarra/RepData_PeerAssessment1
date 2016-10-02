# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data



```r
unzip("activity.zip")
dataSet <- read.csv(file="activity.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
summary(dataSet)
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
```

## What is mean total number of steps taken per day?
1) Calculate the total number of steps taken per day
2) Make a histogram of the total number of steps taken each day
3) Calculate and report the mean and median of the total number of steps taken per day


```r
dataSums <- aggregate(dataSet$steps, by = list(dataSet$date), FUN="sum", na.rm=TRUE)

# Make a Histogram
library(ggplot2)
qplot(dataSums$x, binwidth = 500, main = "Total number of steps taken each day", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Calculate Mean
mean(dataSums$x)
```

```
## [1] 9354.23
```

```r
# Calculate Median
median(dataSums$x)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
1) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
dataIntervals <- aggregate(dataSet$steps, by = list(dataSet$interval), FUN="mean", na.rm=TRUE)
qplot(dataIntervals$Group.1, dataIntervals$x, geom="line", xlab = "5 Min Intervals", ylab = "Steps", main = "Time Series plot over a average day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
dataIntervals[which.max(dataIntervals$x), ]
```

```
##     Group.1        x
## 104     835 206.1698
```

## Imputing missing values
1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3) Create a new dataset that is equal to the original dataset but with the missing data filled in.
4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Get the NA Rows count
naDataSet <- subset(dataSet, is.na(dataSet$steps))
nrow(naDataSet)
```

```
## [1] 2304
```

```r
# Replace each missing value with the mean value of its 5-minute interval
fillValues <- function(steps, interval) {
    value <- NA
    if (!is.na(steps))
        value <- c(steps)
    else
        value <- (dataIntervals[dataIntervals[, 1]==interval, "x"])
    return(value)
}
newDataSet <- dataSet
newDataSet$steps <- mapply(fillValues, newDataSet$steps, newDataSet$interval)

newDataSums <- aggregate(newDataSet$steps, by = list(newDataSet$date), FUN="sum", na.rm=TRUE)
qplot(newDataSums$x, binwidth = 500, main = "Total number of steps taken each day", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# Calculate Mean
mean(newDataSums$x)
```

```
## [1] 10766.19
```

```r
# Calculate Median
median(newDataSums$x)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1) Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
newDataSet$date = as.Date(newDataSet$date, "%Y-%m-%d")
newDataSet$weekdays <- factor(format(newDataSet$date, "%A"))
levels(newDataSet$weekdays) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))

newDataIntervals <- aggregate(newDataSet$steps, by = list(newDataSet$interval, newDataSet$weekdays), FUN="mean", na.rm=TRUE)

# Plot 
library(gridExtra)

plot1 <- ggplot(data = newDataIntervals[which(newDataIntervals$Group.2 == "weekday"), ], aes(x = Group.1, y=x)) + geom_line() + xlab("Interval") + ylab("Steps") + ggtitle("Weekday Steps")

plot2 <- ggplot(data = newDataIntervals[which(newDataIntervals$Group.2 == "weekend"), ], aes(x = Group.1, y=x)) + geom_line() + xlab("Interval") + ylab("Steps") + ggtitle("Weekend Steps")

grid.arrange(plot1, plot2, ncol=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
