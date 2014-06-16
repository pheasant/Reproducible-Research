#Reproducible Research - Assignment 1 
#=====================================

setwd("/Users/apo/Desktop/Coursera/R/Reproducible-Research")

#Loading and processing Data
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

#create a dataframe with no NA values called activity1

activity1 <- activity[complete.cases(activity),]
row.names(activity1) <- NULL 


#What is mean total number of steps taken per day?-----------------------------------------


#Aggregate the number of steps in each day
#Make a histogram of the total number of steps taken each day

agg <- aggregate(activity1$steps, by = list(activity1$date), sum)
colnames(agg) <- c("date","steps")
#or
total_steps_per_day <- tapply(activity$steps, activity$date, sum)


hist(agg$steps, xlab = "Sum of Steps per Day", main = "Total Number of Steps taken each Day", col = "red")

#Calculate and report the mean and median total number of steps taken per day
mean(agg$steps, na.rm = T) #mean
median(agg$steps, na.rm = T) #median

#What is the average daily activity pattern?-------------------------------------------------

#sort by interval
activity1$interval <- sort(activity1$interval)

aggStep = aggregate(steps ~ interval, data=activity, FUN=mean)
View(aggStep)
colnames(aggStep)
plot(aggStep, type = "l", main = "Mean number of steps by interval")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maxstep<- max(aggStep$steps,na.rm=T) #calculate the maximum value of steps
maxstep #the maximum number of steps
aggStep$interval[which(aggStep$step == maxstep)] #find the interval to which the maximum number of steps corresponds

#Imputting the Missing Values-------------------------------------------------------------------

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity))

#Devise a strategy for filling in all of the missing values in the dataset.
#The strategy does not need to be sophisticated. For example, you could use 
#the mean/median for that day, or the mean for that 5-minute interval, etc.
noNactivity <- activity
noNactivity$completeSteps <- noNactivity$steps # Creating column for the imputed values

aggStep1 <- aggregate(activity1$steps, by = list(activity1$date), sum)
aggStep1 = aggregate(activity1$steps, by = list(activity1$interval),FUN=mean)
aggStep1
#Imputing the missing values by using the mean of the 5-minute interval

for (i in 1 : length(noNactivity[,4])){   
                if (is.na(noNactivity$completeSteps[i])){
                                noNactivity$completeSteps[i] <- aggStep$steps[which(aggStep$interval == noNactivity$interval[i])]                                               
        }
}

View(aggStep)
View(noNactivity)
View(activity)
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
completeActivity <- activity
completeActivity$steps <- noNactivity$completeSteps
View(completeActivity)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total 
#number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?


completeAgg<- aggregate(completeActivity$steps, by = list(completeActivity$date), sum)
colnames(completeAgg) <- c("date","steps")


#histogram
hist(completeAgg$steps, xlab = "Sum of Steps per Day", main = "Total Number of Steps taken each Day", col = "green")

##Calculate and report the mean and median total number of steps taken per day for completeAgg
mean(completeAgg$steps, na.rm = T) #mean
median(completeAgg$steps, na.rm = T) #median

#The impact is that the mean approaches the median


#Are there differences in activity patterns between weekdays and weekends?---------------------------

completeActivity$day <- weekdays(completeActivity$date) #create a column indicating the day of the week

#create a a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).



#create two subsets form the complete data, one for weekdays and one for weekends

completeActivity$day[which(completeActivity$day != "Saturday" &  completeActivity$day != "Sunday")] <- "Weekday" 
completeActivity$day[which(completeActivity$day == "Saturday" |  completeActivity$day == "Sunday")] <- "Weekend" 
completeActivity$day <- as.factor(completeActivity$day)


aggdat = aggregate(completeActivity$steps ~ completeActivity$interval+completeActivity$day, data=completeActivity, FUN=mean)
colnames(aggdat) <- c("interval","day","steps")
View(aggdat)



library(lattice)
xyplot(steps ~ interval| day, 
       data = aggdat,
       type = "l",
       main="Number of steps per period",
       xlab="Period", 
       layout=c(1,2)
)
library(knitr)
library(markdown)

setwd("/Users/apo/Desktop/Coursera/R/Reproducible-Research")
knit2html("PA1_template.Rmd")
