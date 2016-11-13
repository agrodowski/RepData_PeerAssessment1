# Reproducible Research: Peer Assessment 1
Adam Grodowski (adam.grodowski@gmail.com)  
November 13th, 2016  

## Prerequisites

This assignment has been made using embedded knit HTML function of R Studio version 1.0.44, Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/602.2.14 (KHTML, like Gecko), with the following session information:

- R version 3.3.2 (2016-10-31)
- Platform: x86_64-apple-darwin16.1.0 (64-bit)
- Running under: macOS Sierra 10.12.1
- locale: en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
- attached base packages: stats, graphics, grDevices, utils, datasets, methods, base
- other attached packages: knitr_1.15, ggplot2_2.2.0, data.table_1.9.6


## Loading and preprocessing the data

Since raw data bundle is already provided, we proceed directly to decompression step followed by reading data from CSV file and type conversions. 

There are three variables provided: ***steps*** (integer), ***date*** (Date as character) and ***interval*** (integer). 


```r
require(data.table)
```

```
## Loading required package: data.table
```

```r
unzip("activity.zip")
data <- fread("activity.csv",
            colClasses=c("numeric","character","integer"),
            header=T)
#data$time <- as.POSIXct(paste(data$date, sprintf("%04d", data$interval)), format="%Y-%m-%d %H%M")
#data[ ,c("date","interval") := NULL]
```

There are 17568 observations loaded, with the following summary:

```r
summary(data)
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
We start with computing the total number of steps taken per day. For this part of the assignment, all missing values are ignored.

```r
dataOk <- na.omit(data, col="steps")
dataOk[, stepsperday := sum(steps), by=date]
```

```
##        steps       date interval stepsperday
##     1:     0 2012-10-02        0         126
##     2:     0 2012-10-02        5         126
##     3:     0 2012-10-02       10         126
##     4:     0 2012-10-02       15         126
##     5:     0 2012-10-02       20         126
##    ---                                      
## 15260:     0 2012-11-29     2335        7047
## 15261:     0 2012-11-29     2340        7047
## 15262:     0 2012-11-29     2345        7047
## 15263:     0 2012-11-29     2350        7047
## 15264:     0 2012-11-29     2355        7047
```

The following histogram illustrates the total number of steps taken per day.

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
g <- ggplot(dataOk, aes(stepsperday)) +
        geom_histogram(binwidth = 5000) +
        xlab("steps per day") +
        ggtitle("Total number of steps per day")
g
```

![](PA1_template_files/figure-html/histogram steps per day-1.png)<!-- -->

In addition, the mean and the median of the total number of steps per day are
computed.

```r
mean1 <- mean(dataOk$stepsperday)
mean1
```

```
## [1] 10766.19
```

```r
median1 <- median(dataOk$stepsperday)
median1
```

```
## [1] 10765
```

## What is the average daily activity pattern?
The following time series plot illustrates the average number of steps across all days per interval.

```r
meanPerInterval <- dataOk[, mean(steps), by = interval]
g <- ggplot(meanPerInterval, aes(x=interval,y=V1)) +
        geom_line() +
        ylab("steps (mean)") +
        ggtitle("Average number of steps across all days per interval")
g
```

![](PA1_template_files/figure-html/daily activity-1.png)<!-- -->

Next, the interval with the maximum number of steps () can be computed in 
the following way:

```r
meanPerInterval[V1 == max(V1)]
```

```
##    interval       V1
## 1:      835 206.1698
```

## Imputing missing values
There are 2304 observations with missing steps, as computed below:

```r
nrow(data[is.na(steps)])
```

```
## [1] 2304
```

We are going to approximate missing number of steps using average number of steps across all days per interval, computed in the previous paragraph. We will use function 'set' in a loop per each unique value of interval, and we will refer to the columns by their index.

```r
for(i in unique(data$interval)) {
    # set all NA steps which belong to a certain interval 'i'
    set(data,which(is.na(data[[1]]) & i == data[[3]]),1L, 
        meanPerInterval[interval == i]$V1)
        #0)
}
```

Once all missing values are substituted, we will plot two histograms to quickly determine the change. We will combine two data-sets (with and without missing values) into one and create a type variable to designate the origin of a data-set. 

```r
# create the new origin variable
data[, stepsperday := sum(steps), by=date]
```

```
##            steps       date interval stepsperday
##     1: 1.7169811 2012-10-01        0    10766.19
##     2: 0.3396226 2012-10-01        5    10766.19
##     3: 0.1320755 2012-10-01       10    10766.19
##     4: 0.1509434 2012-10-01       15    10766.19
##     5: 0.0754717 2012-10-01       20    10766.19
##    ---                                          
## 17564: 4.6981132 2012-11-30     2335    10766.19
## 17565: 3.3018868 2012-11-30     2340    10766.19
## 17566: 0.6415094 2012-11-30     2345    10766.19
## 17567: 0.2264151 2012-11-30     2350    10766.19
## 17568: 1.0754717 2012-11-30     2355    10766.19
```

```r
data$origin <- 'with missing values imputed'
dataOk$origin <- 'with missing values ignored'
```


```r
# combine into a new data-set
dataCombined <- rbind(dataOk, data)
g <- ggplot(dataCombined, aes(stepsperday,fill = origin)) +
        geom_histogram(binwidth = 5000, alpha = 0.5, position="identity") +
        xlab("steps per day") +
        ggtitle("Total number of steps per day")
g
```

![](PA1_template_files/figure-html/histogram steps comparison between datasets-1.png)<!-- -->

We observe that the data-set with missing step values substituted with the mean value of steps of a corresponding interval has the increased number of steps around the mean value.

Next, we are going to compare the quantiles between the two data-sets using boxplot.

```r
g <- ggplot(dataCombined, aes(y = stepsperday, x = origin)) + geom_boxplot()
g
```

![](PA1_template_files/figure-html/boxplot steps comparison between datasets-1.png)<!-- -->


We observe that the mean value is identical between these two data-sets, but the median is not.

```r
mean2 <- mean(data$stepsperday)
mean2
```

```
## [1] 10766.19
```

```r
mean2 - mean1
```

```
## [1] 0
```

```r
median2 <- median(data$stepsperday)
median2
```

```
## [1] 10766.19
```

```r
median2 - median1
```

```
## [1] 1.188679
```

Adding some non-zero values of steps in place of NA values has altered the distribution but not the mean value. To prove that, lets divide our initial set (with ignored NA steps) into a mutually exclusive interval sub-sets, and lets compute the mean value of steps for every interval *i*, where the *steps~i~* is the sum of all steps of the interval *i* and *n* is the number of non-NA steps within the interval *i*. 
$$
\mu_{i} = \dfrac {steps_i}{n}
$$
Next, lets divide our set of the steps with the imputed NA steps. As assumed before - we are going to replace every NA value with the mean value of all steps of the corresponding interval. As shown below, the mean value of the interval sub-set does not change, and therefore, the mean value of all steps neither does. The number of NA steps within the interval *i* is denoted as *na*, and the number of non-NA steps within the interval *i* is denoted as *n*.
$$
\mu'_{i} 
    = \dfrac {steps_i + na*\mu_{int}} {n + na} 
    = \dfrac {steps_i + na*(\dfrac {steps_i}{n})} {n + na} 
    = \dfrac {n*\dfrac {steps_i}{n} + na*\dfrac {steps_i}{n}} {n + na}
    = \dfrac {n+na}{n+na}*\dfrac {steps_i}{n}
    = 1*\mu_{int} = \mu_{int}
$$

## Are there differences in activity patterns between weekdays and weekends?

The following code creates a factor variable 'datetype' with two values: 
weekday and weekend.

```r
data[, datetype := 'weekday']
```

```
##            steps       date interval stepsperday
##     1: 1.7169811 2012-10-01        0    10766.19
##     2: 0.3396226 2012-10-01        5    10766.19
##     3: 0.1320755 2012-10-01       10    10766.19
##     4: 0.1509434 2012-10-01       15    10766.19
##     5: 0.0754717 2012-10-01       20    10766.19
##    ---                                          
## 17564: 4.6981132 2012-11-30     2335    10766.19
## 17565: 3.3018868 2012-11-30     2340    10766.19
## 17566: 0.6415094 2012-11-30     2345    10766.19
## 17567: 0.2264151 2012-11-30     2350    10766.19
## 17568: 1.0754717 2012-11-30     2355    10766.19
##                             origin datetype
##     1: with missing values imputed  weekday
##     2: with missing values imputed  weekday
##     3: with missing values imputed  weekday
##     4: with missing values imputed  weekday
##     5: with missing values imputed  weekday
##    ---                                     
## 17564: with missing values imputed  weekday
## 17565: with missing values imputed  weekday
## 17566: with missing values imputed  weekday
## 17567: with missing values imputed  weekday
## 17568: with missing values imputed  weekday
```

```r
data[weekdays(as.Date(date)) %in% c("Saturday","Sunday"), datetype := 'weekend']
```

```
##            steps       date interval stepsperday
##     1: 1.7169811 2012-10-01        0    10766.19
##     2: 0.3396226 2012-10-01        5    10766.19
##     3: 0.1320755 2012-10-01       10    10766.19
##     4: 0.1509434 2012-10-01       15    10766.19
##     5: 0.0754717 2012-10-01       20    10766.19
##    ---                                          
## 17564: 4.6981132 2012-11-30     2335    10766.19
## 17565: 3.3018868 2012-11-30     2340    10766.19
## 17566: 0.6415094 2012-11-30     2345    10766.19
## 17567: 0.2264151 2012-11-30     2350    10766.19
## 17568: 1.0754717 2012-11-30     2355    10766.19
##                             origin datetype
##     1: with missing values imputed  weekday
##     2: with missing values imputed  weekday
##     3: with missing values imputed  weekday
##     4: with missing values imputed  weekday
##     5: with missing values imputed  weekday
##    ---                                     
## 17564: with missing values imputed  weekday
## 17565: with missing values imputed  weekday
## 17566: with missing values imputed  weekday
## 17567: with missing values imputed  weekday
## 17568: with missing values imputed  weekday
```

```r
data[, dateype := as.factor(datetype)]
```

```
##            steps       date interval stepsperday
##     1: 1.7169811 2012-10-01        0    10766.19
##     2: 0.3396226 2012-10-01        5    10766.19
##     3: 0.1320755 2012-10-01       10    10766.19
##     4: 0.1509434 2012-10-01       15    10766.19
##     5: 0.0754717 2012-10-01       20    10766.19
##    ---                                          
## 17564: 4.6981132 2012-11-30     2335    10766.19
## 17565: 3.3018868 2012-11-30     2340    10766.19
## 17566: 0.6415094 2012-11-30     2345    10766.19
## 17567: 0.2264151 2012-11-30     2350    10766.19
## 17568: 1.0754717 2012-11-30     2355    10766.19
##                             origin datetype dateype
##     1: with missing values imputed  weekday weekday
##     2: with missing values imputed  weekday weekday
##     3: with missing values imputed  weekday weekday
##     4: with missing values imputed  weekday weekday
##     5: with missing values imputed  weekday weekday
##    ---                                             
## 17564: with missing values imputed  weekday weekday
## 17565: with missing values imputed  weekday weekday
## 17566: with missing values imputed  weekday weekday
## 17567: with missing values imputed  weekday weekday
## 17568: with missing values imputed  weekday weekday
```

In order to compare weekends with workdays, we will plot again the average
number of step across all weekend and all workdays per interval in two plots

```r
meanPerInterval2 <- data[, mean(steps), by = list(interval,datetype)]
g <- ggplot(meanPerInterval2, aes(x=interval,y=V1), group = datetype) +
        geom_line() +
        ylab("steps (mean)") +
        ggtitle("Average number of steps across weekdays and weekends per interval") +
        facet_grid(datetype ~ .)
g
```

![](PA1_template_files/figure-html/daily activities per weekday or weekend-1.png)<!-- -->

The average number of steps is higher afternoon during the weekend than it is
during weekdays. At the same time, morning average (intervals between 
5:00 - 10:00) is lower.
