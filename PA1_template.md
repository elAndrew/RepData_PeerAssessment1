---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
  
First let's create a sub-directory of our working director to store our data, 
./data/

```r
if(!file.exists("./data")){
        dir.create("./data")
}
```
  
  
Now we will unzip the activity.zip file (in the working directory) to our *./data/* 
sub-directory, and look at the contents.

```r
if(!file.exists("./data/activity.csv")){
        unzip("activity.zip", exdir = "./data/", overwrite = FALSE)
}
```
  
Read the data into R

```r
activity <- read.csv("./data/activity.csv")
```
  
## What is mean total number of steps taken per day?
  
  
First let's look at the total number of steps taken, per day.

```r
stepsperday <- 
        with(activity,
             aggregate(steps~date, FUN = sum))

library(knitr)
kable(head(stepsperday))
```



date          steps
-----------  ------
2012-10-02      126
2012-10-03    11352
2012-10-04    12116
2012-10-05    13294
2012-10-06    15420
2012-10-07    11015
  
  
Now let's view the total steps taken per day as a histogram

```r
hist(stepsperday$steps, main = "", xlab = "Number of steps per day", breaks = 10)
```

<img src="PA1_template_files/figure-html/histogram of steps-1.png" style="display: block; margin: auto;" />
Most days have between 7,000 and 15,000 total steps logged.  
  
  
Now we'll calculate and report the mean and median of the total number of steps per day

```r
meansteps <- mean(stepsperday$steps)
mediansteps <- median(stepsperday$steps)
```
* The **mean** is 10,766 per day
* The **median** is 10,765 per day  
*(rounded values)*


## What is the average daily activity pattern?

First, we'll calculate the average number of steps for each interval, accross all days

```r
avgStepsPerInterval <- 
        with(activity,
             aggregate(steps~interval, FUN = mean))

kable(head(avgStepsPerInterval))
```



 interval       steps
---------  ----------
        0   1.7169811
        5   0.3396226
       10   0.1320755
       15   0.1509434
       20   0.0754717
       25   2.0943396
  
  
Now we will plot that average number of steps per interval.

```r
plot(avgStepsPerInterval$interval, avgStepsPerInterval$steps, type = "l",
     ylab = "Average number of steps", xlab = "Five minute intervals, numbered")
```

<img src="PA1_template_files/figure-html/plot inverval steps-1.png" style="display: block; margin: auto;" />
  
  
Now we will calculate which five minute interval, on average, contains the maximum number of steps.

```r
loc <- which.max(avgStepsPerInterval$steps)
maxsteps <-  max(avgStepsPerInterval$steps)
maxinterval <- avgStepsPerInterval$interval[loc]
```
At interval **835**, the average number of steps is highest at, 206
*(rounded)*.
  
  
## Imputing missing values
  
Let's calculate the number of rows with missing values.

```r
nMissing <- sum(!complete.cases(activity))
```
There are **2304** rows containing missing values.  
  
  
Now we will add a column to our dataframe, with the average number of steps for each interval.  These values will be used to impute the missing values

```r
names(avgStepsPerInterval)[2] <- "avgStepsInt"
activity <- merge(activity, avgStepsPerInterval, by="interval")
```
  
  
Now let's create a new dataset, called *activity1* that is equal to the orginal dataset, but with the missing values filled in.

```r
activity$steps[is.na(activity$steps) == TRUE] <-
        activity$avgStepsInt[is.na(activity$steps)]

library(dplyr)
activity1 <- activity %>%
        select(-avgStepsInt)
```
  
  
Now we will redo some of the above calculations on the new dataset.
  
First let's look at the total number of steps taken, per day, with the new dataset.

```r
stepsperday1 <- 
        with(activity1,
             aggregate(steps~date, FUN = sum))

kable(head(stepsperday1))
```



date             steps
-----------  ---------
2012-10-01    10766.19
2012-10-02      126.00
2012-10-03    11352.00
2012-10-04    12116.00
2012-10-05    13294.00
2012-10-06    15420.00
  
  
Now let's view the total steps taken per day as a histogram, with the new dataset.

```r
hist(stepsperday1$steps, main = "", xlab = "Number of steps per day", breaks = 10)
```

<img src="PA1_template_files/figure-html/histogram of steps 1-1.png" style="display: block; margin: auto;" />
There is a narrower shape, but it is not notably different.
  
  
Now we'll calculate and report the mean and median of the total number of steps per day

```r
meansteps1 <- mean(stepsperday1$steps)
mediansteps1 <- median(stepsperday1$steps)
```
* The **mean** is 10,766 per day
* The **median** is 10,766 per day  
*(rounded values)*  
  
Just a very slight difference in these figures with the imputed missing values.
  
  
## Are there differences in activity patterns between weekdays and weekends?

First we will add a factor variable *daytype*, which indicates whether each observation occurs on a weekend.

```r
activity1 <- activity1 %>%
        mutate(daytype = as.Date(date)) %>%
        mutate(daytype = weekdays(daytype))

weekendfilter <- activity1$daytype == "Saturday" | activity1$daytype == "Sunday"

activity1$daytype[weekendfilter] <- "weekend"
activity1$daytype[!weekendfilter] <- "weekday"

activity1$daytype <- factor(activity1$daytype)
```
  
  
Now, we'll calculate the average number of steps for each interval, for each weekend days and weekday days.

```r
avgStepsPerInterval1 <- 
        with(activity1,
             aggregate(steps~interval+daytype, FUN = mean))

kable(head(avgStepsPerInterval1))
```



 interval  daytype        steps
---------  --------  ----------
        0  weekday    2.2511530
        5  weekday    0.4452830
       10  weekday    0.1731656
       15  weekday    0.1979036
       20  weekday    0.0989518
       25  weekday    1.5903564
  
  
And finally, we will plot the average number of steps per five minute interval, averaged across weekday days, and weekend days.

```r
library(ggplot2)

p1 <- ggplot(avgStepsPerInterval1, aes(interval, steps)) +
        geom_line() +
        facet_wrap(~daytype, ncol = 1) +
        theme_bw() +
        ylab("Average number of steps") +
        xlab("Five minute intervals, numbered")
p1
```

<img src="PA1_template_files/figure-html/plot inverval steps 1-1.png" style="display: block; margin: auto;" />
