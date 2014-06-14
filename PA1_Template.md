Reproducable Research Peer Assessment 1: Activity Monitoring
========================================================
This assignment is to analyze the Activity Monitoring dataset
-------------------------------------------------------------

**First, I need to read in the data, and format the Date column as a date:**

```r
data <- read.csv("activity.csv")
data$Date <- as.POSIXlt(data$date)
```


**Create a Histogram of the total number of steps taken each day (excluding NA's), and caclulate Mean & Median:**

```r
## Make a histogram of steps, and calculate mean, median:
totalSteps <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)
hist(totalSteps$steps, col = "blue", xlab = "Steps Taken", main = "Histogram of Steps Taken")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean <- round(mean(totalSteps$steps), 1)
median <- median(totalSteps$steps)
```

**The Mean number of steps taken each day is 1.0766 &times; 10<sup>4</sup>, and the Median is 10765.**

Plot the mean daily steps taken per interval:
---------------------------------------------

```r
## Calculate steps per Interval:
intervalSteps <- aggregate(steps ~ interval, data = data, FUN = "mean", na.rm = TRUE)
## Make Plot:
plot(steps ~ interval, data = intervalSteps, type = "l", xlab = "Interval", 
    ylab = "Steps", main = "Avg Steps Taken by Interval:")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r

## Find the interval with the highest mean steps:
max <- intervalSteps[which.max(intervalSteps$steps), ]$interval
```


**The interval of time with the most Avegerage number of steps is interval 835.**

Count the number of Missing Values and fill them in:
----------------------------------------------------


```r
## Find the number of NA's in the dataset:
missing <- sum(is.na(data$steps))
```

**The number of missing values in the dataset is: 2304**


```r
## Fill in NA's with mean of that interval (in new dataset called 'Fdata'):
Fdata <- data
nas <- 0
for (i in 1:length(data$steps)) {
    if (is.na(Fdata[i, 1]) == TRUE) {
        int <- Fdata[i, 3]
        avg <- intervalSteps[intervalSteps$interval == int, 2]
        Fdata[i, 1] <- avg
        nas <- nas + 1
    }
}
```

**There were 2304 missing values**

New Histogram:
--------------
**Here is a new histogram of number of steps using the imputed data:**

```r
## Make a new histogram of steps, and calculate new mean, median (use Fdata):
totalSteps2 <- aggregate(steps ~ date, data = Fdata, sum)
hist(totalSteps2$steps, col = "red", xlab = "Steps Taken", main = "New Histogram of Steps Taken (with Imputed Data)")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
mean2 <- round(mean(totalSteps2$steps), 1)
median2 <- median(totalSteps2$steps)
```

**Using the imputed data, the new mean is 1.0766 &times; 10<sup>4</sup>, and the new median is 1.0766 &times; 10<sup>4</sup>.**  
**That is compared to the original mean of 1.0766 &times; 10<sup>4</sup> and original median of 10765.  **

**Those values are the same, even with the imputed data.**

Plot by Weekend/Weekday:
------------------------


```r
## Create new columns in Fdata for day of week, and the weekend Factor
## variable:
Fdata$dow <- weekdays(Fdata$Date, abbreviate = TRUE)
Fdata$dayType <- factor(c("weekday", "weekend"))

## Go throuh the file and indicate whether each date is weekend or weekday:
for (i in 1:length(Fdata$dow)) {
    if (Fdata$dow[i] %in% c("Sat", "Sun")) {
        Fdata[i, 6] <- "weekend"
    } else {
        Fdata[i, 6] <- "weekday"
    }
}
Fdata$dayType <- as.factor(Fdata$dayType)
# Fdata$interval <- as.factor(Fdata$interval)
library("plyr")
library("lattice")
## Make new Intervals data:
intervalSteps2 <- aggregate(steps ~ interval + dayType, Fdata, "mean")
xyplot(steps ~ interval | factor(dayType), data = intervalSteps2, aspect = 1/2, 
    type = "l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


