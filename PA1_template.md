---
title: "Reproducible Research:Peer Assessment 1"
author: "Shivam Giri"
date: "July 9, 2017"
output: html_document
---


Loading and preprocessing the data
```{r setup, include=FALSE,echo=TRUE}
knitr::opts_chunk$set(echo = TRUE)
if(!file.exists('activity.csv')){
  unzip('activity.zip')
}
activity<-read.csv("activity.csv")

```



What is mean total number of steps taken per day?

```{r plot,mean,median,echo=TRUE}

library(ggplot2)

total_steps<-tapply(activity$steps,activity$date,sum,na.rm=TRUE)
qplot(total_steps,xlab = "Total number of steps taken per day for binwidth 1000",binwidth=1000)

mean_steps<-mean(total_steps,na.rm = TRUE)
median_steps<-median(total_steps,na.rm = TRUE)

mean_steps
median_steps

```



What is the average daily activity pattern?

```{r, echo=TRUE}
library(ggplot2)
steps_interval<-aggregate(x=list(steps=activity$steps),by=list(interval=activity$interval),mean,na.rm=TRUE)
ggplot(steps_interval,aes(x=interval,y=steps))+geom_line()+xlab("5 minutes interval")+ylab("Average number of steps taken")
steps_interval[max(which.max(steps_interval$steps)),]


```



Imputing missing values

```{r, echo=TRUE}
total_na<-sum(is.na(activity$steps))
total_na

library(Hmisc)
activity_data_imputed<-activity
activity_data_imputed$steps<-impute(activity$steps,fun=mean)

step_day_imputed<-tapply(activity_data_imputed$steps,activity_data_imputed$date,sum)
qplot(step_day_imputed, xlab = "Total number of steps per day imputed", ylab = "Frequency(binwidth=500)", binwidth=500)

steps_imputed_mean<- mean(step_day_imputed)
steps_imputed_median<- median(step_day_imputed)

steps_imputed_mean
steps_imputed_median

```


Are there differences in activity patterns between weekdays and weekends?

```{r,echo=TRUE}

activity_data_imputed$dayType<-ifelse(as.POSIXlt(activity_data_imputed$date)$wday%in%c(0,6),'weekends','weekdays')

average_activity_data_imputed<-aggregate(steps~interval+dayType,data=activity_data_imputed ,sum,na.rm=TRUE)
ggplot(average_activity_data_imputed,aes(interval,steps) )+facet_grid(.~dayType)+geom_line()+xlab ( "5 minute interval") +ylab ( "Average number of steps")


```




