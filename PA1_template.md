---
title: "Peer Assessment 1"
output: html_document
---



This a project for reproducible research.
===============================================


####**Loading and processing the data**

*Load the data using read.csv, and checking the data structure
```{r, echo = TRUE}
activity <- read.csv("./activity.csv", stringsAsFactors=FALSE)
str(activity)
head(activity)
```


*transform the variable format in the data
```{r, echo = TRUE}
activity$date <- as.Date(activity$date) 
activity$steps <- as.numeric(activity$steps)
activity$interval <- as.numeric(activity$interval)
```




####**The mean total number of steps taken per day**

*Calculating the steps taken per day (ignoring the missing values)

1.Obtain the number of total steps for each day by aggregate the data by date
```{r, echo = TRUE}
df <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)
```
2.Plot histogram
```{r, echo = TRUE}
head(df)
colnames(df) <- c("date", "steps")
hist(df$steps,col="gray", breaks=30,
     main="Distribution of number of steps per day", xlab="Number of steps per day")
```

3. calculate the mean and median
```{r, echo = TRUE}
mean(df$steps)
median(df$steps)
```




####**The average daily activity pattern**


*Time serial plot of the 5-minute interval and the average number of steps taken

1.Obtain the averaged number of steps for each interval
```{r, echo = TRUE}
aveint <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
```

2.Graph time serial plot of the 5-minutes interval
```{r, echo = TRUE}
colnames(aveint) <- c("interval", "steps")
plot(steps~interval, data=aveint, type="l",
     main="Distribution of number of steps for each interval",
     xlab="5-minute interval") 
```



*Calculate the 5-minute interval that contains the maximum number of steps
```{r, echo = TRUE}
subset(aveint,steps==max(steps))
```




####**Imputing missing values**


*Calculate the total number of missing values in the dataset
```{r, echo = TRUE}
sum(is.na(activity))
```



*Fill the missing values with the average steps per 5-minute interval and create a new dataset. And then check the missing values again.
```{r, echo = TRUE}
repldata <- activity
repldata[is.na(repldata)] <- mean(aveint$steps)
sum(is.na(repldata))
```



*Make a histogram of the total number of steps taken each day for the imputing data
```{r, echo = TRUE}
df1 <- aggregate(steps~date, repldata, FUN=sum, na.rm=TRUE)
hist(df1$steps,col="light gray", breaks=30,
     main="Distribution of number of steps per day (Imputing)", xlab="Number of steps per day")
```



*Calculate the mean and median total number of steps taken per day
```{r, echo = TRUE}
mean(df1$steps)
median(df1$steps)
```

*After imputing the missing values, the values of the mean and median increased from 9354.23 and 10395, respectively. Therefore, the imputing result shits the distribution of the number of steps per day, which makes the values of mean and median at the same place.




####**Differences in activity patterns between weekdays and weekends?**


*Create a new factor variable in the imputed dataset with two levels - weekday and weekend


1.Change the class of date variable
```{r, echo = TRUE}
repldata$date <- as.Date(repldata$date)
```

2.Use weekdays to retrieve "day" and create a weekday data frame
```{r, echo = TRUE}
weekday<- as.data.frame(weekdays(repldata$date))

colnames(weekday) <- "day"
```

3.The following section needs the car package: install the car package and load it. Recode the weekday variable to "Weekday" and "Weekend"  
```{r, echo = TRUE}
library(car)
weekday$day <- recode(weekday$day,"c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')='Weekday'") 
weekday$day <- recode(weekday$day,"c('Sunday', 'Saturday')='Weekend'")
```

4.COmbine the weekday data frame to the previously imputed dataset
```{r, echo = TRUE}
combweek <- cbind(weekday, repldata)

head(combweek) 
str(combweek)
```


*obtain the averaged number of steps for each interval
```{r, echo = TRUE}
aveintw <- aggregate(combweek$steps, by=list(combweek$interval, combweek$day), FUN=mean, na.rm=TRUE)
colnames(aveintw) <- c("interval", "day", "steps")
```


*Graph the new time serial plot of the 5-minutes interval
```{r, echo = TRUE}
library(lattice)
xyplot(steps ~ interval | factor(day), layout = c(1, 2), data=aveintw, type='l')
```
