Reproducable Research Peer Assessment 1: Activity Monitoring
========================================================
**This assignment is to analyze the Activity Monitoring dataset.**


**First, I need to read in the data, and format the Date column as a date:**

```r
data <- read.csv("activity.csv")
data$Date <- as.POSIXlt(data$date)
```


**Create a Histogram of the number of steps taken each day (excluding NA's), and caclulate Mean & Median:**

```r
## Make a histogram of steps, and calculate mean, median:
hist(na.omit(data$steps), col = "blue", xlab = "Steps Taken", main = "Histogram of Steps Taken")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean <- round(mean(na.omit(data$steps)), 2)
median <- median(na.omit(data$steps))
```

**The Mean number of steps taken each day is 37.38, and the Median is 0.**

**Plot the mean steps taken per interval:**

```r
# df <- data.frame(interval)
Cdata <- na.omit(data)
## Take the mean of each interval and save to dataframe:
intAvg <- tapply(Cdata$steps, Cdata$interval, mean, na.rm = TRUE, simplify = FALSE)
intAvg <- as.data.frame(intAvg, col.names = c("Interval", "Steps"))
intAvg <- cbind(row.names(intAvg), intAvg)
names(intAvg) <- c("interval", "Steps")

## Change Datatypes:
intAvg$Steps <- as.numeric(intAvg$Steps)
intAvg$interval <- as.numeric(levels(intAvg$interval))[intAvg$interval]

## Make Plot:
plot(intAvg$interval, intAvg$Steps, type = "l", xlab = "Interval", ylab = "Steps", 
    main = "Avg Steps Taken by Interval:")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
# qplot(intAvg$interval, intAvg$Steps, geom='line', xlab = 'Interval', ylab
# = 'Steps') qplot(interval, Steps, data=intAvg, geom='line', xlab =
# 'Interval', ylab = 'Steps')


## Find the interval with the highest mean steps:
max <- intAvg[intAvg$Steps == max(intAvg$Steps), ]
maxInterval <- max[1, 1]
```

**The interval of time with the most Avegerage number of steps is interval 835 with an average of 206.1698 steps.**


```r
## Find the number of NA's in the dataset:
missing <- length(which(is.na(data$steps)))
```

**The number of missing values in the dataset is: 2304**


```r
## Fill in NA's with mean of that interval (in new dataset called 'Fdata'):
Fdata <- data
for (i in 1:length(data$steps)) {
    if (is.na(Fdata[i, 1]) == TRUE) {
        int <- Fdata[i, 3]
        avg <- intAvg[(intAvg$interval == int), 2]
        Fdata[i, 1] <- avg
    }
}
```

**Here is a new histogram of steps using the imputed data:**

```r
## Make a new histogram of steps, and calculate new mean, median (use Fdata):
hist(Fdata$steps, col = "red", xlab = "Steps Taken", main = "New Histogram of Steps Taken (with Imputed Data)")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
Fmean <- round(mean(Fdata$steps), 2)
Fmedian <- median(Fdata$steps)
```

**Using the imputed data, the new mean is 37.38, and the new median is 0.**  
**That is compared to the original mean of 37.38 and original median of 0.  **

**Those values are the same, even with the imputed data.**


```r
## Create new columns in Fdata for day of week, and the weekend Factor
## variable:
Fdata$dow <- weekdays(Fdata$Date, abbreviate = TRUE)
Fdata$weekend <- factor(c("weekday", "weekend"))

## Go throuh the file and indicate whether each date is weekend or weekday:
for (i in 1:length(Fdata$dow)) {
    if (Fdata$dow[i] %in% c("Sat", "Sun")) {
        Fdata$weekend <- "weekend"
    } else {
        Fdata$weekend <- "weekday"
    }
}
Fdata$weekend <- as.factor(Fdata$weekend)
Fdata$interval <- as.factor(Fdata$interval)
library("plyr")
## Make new Intervals data: FintAvg <- tapply(Fdata$steps,
## list(Fdata$interval,Fdata$weekend), mean, simplify=FALSE)
FintAvg <- ddply(Fdata, c("interval", "weekend"), summarize, mean = mean(steps))
FintAvg <- aggregate(x = Fdata$steps, by = list(Fdata$weekend, Fdata$interval), 
    FUN = mean)

FintAvg <- as.data.frame(FintAvg, col.names = c("Interval", "Steps", "weekend"))
FintAvg <- cbind(row.names(FintAvg), FintAvg)
names(FintAvg) <- c("interval", "Steps", "weekend")


## Change Datatypes:
FintAvg$Steps <- as.numeric(FintAvg$Steps)
FintAvg$interval <- as.numeric(levels(FintAvg$interval))[FintAvg$interval]

## Make Plot:
library("lattice")
# plot(Steps ~ interval | weekend, data=FintAvg, layout=c(2,1))
# plot(FintAvg$interval, intAvg$Steps, type='l', xlab = 'Interval', ylab =
# 'Steps', main='Imputed Data')
```
