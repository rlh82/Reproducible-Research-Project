---
output: html_document
---
Loading and preprocessing the data

What is mean total number of steps taken per day?

``` r
library(ggplot2)

total_steps<-tapply(activity$steps,activity$date,sum,na.rm=TRUE)
qplot(total_steps,xlab = "Total number of steps taken per day for binwidth 1000",binwidth=1000)
```

![](PA1_template_files/figure-markdown_github/plot,mean,median-1.png?raw=true)

``` r
mean_steps<-mean(total_steps,na.rm = TRUE)
median_steps<-median(total_steps,na.rm = TRUE)

mean_steps
```

    ## [1] 9354.23

``` r
median_steps
```

    ## [1] 10395

What is the average daily activity pattern?

``` r
library(ggplot2)
steps_interval<-aggregate(x=list(steps=activity$steps),by=list(interval=activity$interval),mean,na.rm=TRUE)
ggplot(steps_interval,aes(x=interval,y=steps))+geom_line()+xlab("5 minutes interval")+ylab("Average number of steps taken")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-1-1.png?raw=true)

``` r
steps_interval[max(which.max(steps_interval$steps)),]
```

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values

``` r
total_na<-sum(is.na(activity$steps))
total_na
```

    ## [1] 2304

``` r
library(Hmisc)
```

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, round.POSIXt, trunc.POSIXt, units

``` r
activity_data_imputed<-activity
activity_data_imputed$steps<-impute(activity$steps,fun=mean)

step_day_imputed<-tapply(activity_data_imputed$steps,activity_data_imputed$date,sum)
qplot(step_day_imputed, xlab = "Total number of steps per day imputed", ylab = "Frequency(binwidth=500)", binwidth=500)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png?raw=true)

``` r
steps_imputed_mean<- mean(step_day_imputed)
steps_imputed_median<- median(step_day_imputed)

steps_imputed_mean
```

    ## [1] 10766.19

``` r
steps_imputed_median
```

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?

``` r
activity_data_imputed$dayType<-ifelse(as.POSIXlt(activity_data_imputed$date)$wday%in%c(0,6),'weekends','weekdays')

average_activity_data_imputed<-aggregate(steps~interval+dayType,data=activity_data_imputed ,sum,na.rm=TRUE)
ggplot(average_activity_data_imputed,aes(interval,steps) )+facet_grid(.~dayType)+geom_line()+xlab ( "5 minute interval") +ylab ( "Average number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png?raw=true)
