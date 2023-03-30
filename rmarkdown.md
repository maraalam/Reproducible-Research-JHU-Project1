---
title: "Reproducible Research: Activity Monitoring Data Analysis"
author: "Maria Cristina Alameda Salas"
date: "29/3/2023"
output: 
  html_document:
    keep_md: true
---





## Loading and preprocessing the data
write me a text about loading and preprocessing data in R
Loading and preprocessing data in R is an essential step in the data analysis pipeline. Before embarking on any data analysis task, it is important to ensure that the data is in the right format and is clean. In R, there are various packages that can be used to load and preprocess data.

To load data, we can use the read.table() function for reading in datasets with delimited columns, read.csv() for reading in comma-separated files, and readLines() for reading in raw text files. Once the data is loaded into R, we can start the preprocessing step.

The preprocessing step involves several sub-steps, including data cleaning, removal of missing data, and feature selection. The dplyr and tidyr packages provide a number of functions that facilitate these sub-tasks, including filter(), select(), mutate(), group_by(), summarise(), gather(), spread(), and separate().

The tm package is a popular tool for text mining and is particularly useful for loading and preprocessing large amounts of text data. It provides functions for building a corpus of text documents, removing stop words, stemming words, and converting the corpus to a term-document matrix.

Overall, loading and preprocessing data in R is an important step in any data analysis project. By using the right tools and techniques, we can ensure that our data is of high quality and is ready for analysis.


```r
activity_data <- read.csv('activity.csv')
```

Let's see some rows of data

```r
head(activity_data)
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

And statistics too...!

```r
summary(activity_data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?

```r
#activity_data[is.na(activity_data)] <- 0 # na -> 0 (for mean calculation purpose)
steps_per_day <- aggregate(steps ~ date, activity_data, FUN = sum, na.rm = TRUE)
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

Let's plot this data

```r
library(ggplot2)
ggplot(data=steps_per_day, aes(steps, colour = steps)) +
    geom_histogram(binwidth = 1000) +
    ylab("Frequency") +
    xlab("Total number of steps")
```

![](rmarkdown_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
mean_steps <- mean(steps_per_day$steps, na.rm=TRUE)

sprintf("Mean total number of steps taken per day: %f", mean_steps)
```

```
## [1] "Mean total number of steps taken per day: 10766.188679"
```

## What is the average daily activity pattern?

```r
library(ggplot2)
averages <- aggregate(steps ~ interval, activity_data, FUN = sum)

ggplot(data=averages, aes(x=interval, y=steps, colour = steps)) +
    geom_line() +
    xlab("by 5-minute interval") +
    ylab("Average number of steps from all days")
```

![](rmarkdown_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
averages[which.max(averages$steps),]
```

```
##     interval steps
## 104      835 10927
```

## Imputing missing values

Loading data again

```r
activity_data <- read.csv('activity.csv')
```

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


1. Calculating and reporting the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing <- is.na(activity_data$steps)
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```
There are `missing$TRUE` rows with steps equal to a NA value.


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

STRATEGY -> filling with mean of that interval

```r
fill.value <- function(steps, interval) {
    filled_activity_dataset <- NA
    if (!is.na(steps))
        filled_activity_dataset <- c(steps)
    else
        filled_activity_dataset <- (averages[averages$interval==interval, "steps"])
    return(filled_activity_dataset)
}

filled_activity_dataset <- activity_data
filled_activity_dataset$steps <- mapply(fill.value, filled_activity_dataset$steps, filled_activity_dataset$interval)

summary(filled_activity_dataset)
```

```
##      steps             date              interval     
##  Min.   :    0.0   Length:17568       Min.   :   0.0  
##  1st Qu.:    0.0   Class :character   1st Qu.: 588.8  
##  Median :    0.0   Mode  :character   Median :1177.5  
##  Mean   :  292.3                      Mean   :1177.5  
##  3rd Qu.:   43.0                      3rd Qu.:1766.2  
##  Max.   :10927.0                      Max.   :2355.0
```
No more NA values.


3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_per_day <- aggregate(steps ~ date, activity_data, FUN = sum)

library(ggplot2)

ggplot(data=steps_per_day, aes(steps, colour = steps)) +
    geom_histogram(binwidth = 1000) +
    ylab("Frequency") +
    xlab("Total number of steps")
```

![](rmarkdown_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
mean_steps <- mean(steps_per_day$steps, na.rm=TRUE)

sprintf("Mean total number of steps taken per day: %f", mean_steps)
```

```
## [1] "Mean total number of steps taken per day: 10766.188679"
```
Of course these values differ. The mean of zero-filled dataset is slightly lower than this mean-interval-filled dataset.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
library(timeDate)
weekends_distintion <- function(date) {
    if (isWeekday(date))
      return ('Weekday')
    else
      return ('Weekend')
}

filled_activity_dataset$date <- as.Date(filled_activity_dataset$date)
filled_activity_dataset$day <- sapply(filled_activity_dataset$date, FUN=weekends_distintion)
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
averages <- aggregate(steps ~ interval + day, data = filled_activity_dataset, FUN = mean)

ggplot(averages, aes(interval, steps, colour = steps)) + geom_line() + facet_wrap(~day, nrow=2) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![](rmarkdown_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
