activity<-read.csv('activity.csv')
completeac<-activity[complete.cases(activity), ]
head(completeac)
summary(completeac)
aggbydate<-aggregate(completeac$steps, by = list(completeac$date), FUN = sum)
colnames(aggbydate)<-c('date','totalsteps')
hist(aggbydate$totalsteps, main = 'the total number of steps taken each day',
     xlab = 'step')
mean(aggbydate$totalsteps)
median(aggbydate$totalsteps)
avg_by_interval<-aggregate(completeac$steps, by=list(completeac$interval), FUN=mean)
colnames(avg_by_interval)<-c('interval','step')
plot(avg_by_interval$interval,avg_by_interval$step, type = 'l',col=1, main = 'average number of steps by interval',
     xlab = 'intervals',ylab = 'average number of steps')
max_interval<- subset(avg_by_interval,step==max(avg_by_interval$step),select = c(interval))
max_interval

sum(is.na(activity$steps))

for (i in 1:nrow(activity)) {
  if(is.na(activity$steps[i])) {
    avg <- avg_by_interval$step[which(avg_by_interval$interval == activity$interval[i])]
    activity$steps[i] <- avg 
  }
}

newaggregate<-aggregate(activity$steps, by = list(activity$date), FUN = sum)
colnames(newaggregate)<-c('date','step')
hist(newaggregate$step, main = 'the total number of steps taken each day',
     xlab = 'step')
mean(newaggregate$step)
median(newaggregate$step)
###not changed because we used the average to fill out the missng values

activity$day <- weekdays(as.Date(activity$date))

weekday <- function(date_day) {
  if  (!(date_day == 'Saturday' || date_day == 'Sunday')) {
    x <- 'Weekday'
  } else {
    x <- 'Weekend'
  }
  x
}

activity$weekday_type <- as.factor(sapply(activity$day, weekday))

library(ggplot2)
steps_per_day_weekday <- aggregate(steps ~ interval+weekday_type, activity, mean)
plot <- ggplot(steps_per_day_weekday, aes(interval, steps)) +
  geom_line(stat = "identity", aes(colour = weekday_type)) +
  theme_gray() +
  facet_grid(weekday_type ~ ., scales="fixed", space="fixed") +
  labs(x="Interval", y=expression("Steps")) +
  ggtitle("Steps Per Interval by day type")
plot

### We could see some differences between these two plots. Poeple usually
### start walking later in the weekend than in the weekdays.
