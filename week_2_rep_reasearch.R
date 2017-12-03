library(dplyr)
library(knitr)
Sys.setlocale("LC_TIME", "C")

# Reading data

current_directory = getwd()

if(!file.exists("./activity.csv")) {

    file_url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

    download.file(file_url,destfile = "./activity.zip")

    unzip("./activity.zip",exdir=getwd())

}

data_ex= read.csv("./activity.csv")

# Exploring variables

summary_data_ex = summary(data_ex)

# Creating dataset for number of steps per day and calculating mean and median
# ignoring NAs and ploting histogram

data_grouped_by_day = group_by(data_ex,date)
data_summarized = summarise(data_grouped_by_day,total_steps = sum(steps))
hist(data_summarized$total_steps,breaks = 10)
mean_daily_steps = mean(data_summarized$total_steps,na.rm = TRUE)
median_daily_steps = median(data_summarized$total_steps,na.rm = TRUE)


# Creating dataset for displaying average daily steps per 5 min interval
# plotting it and finding out the interval with maximum activity (number of steps)

data_grouped_by_interval = group_by(data_ex,interval)
data_daily_average = summarise(data_grouped_by_interval,mean_daily = mean(steps,na.rm = TRUE))
plot(data_daily_average$interval,data_daily_average$mean_daily,type = "l")

max_average_daily = data_daily_average$interval[which.max(data_daily_average$mean_daily)]

#Finding out total NAs in the dataset, after noticing that just the
# steps variable contains NAs

total_nas = sum(is.na(data_ex$steps))

## Filling NAs using the rounded Daily (to transform it in an integer) average 
#at that 5 min interval as a filling criteria

data_ex_na_filled = data_ex

for(i in 1:length(data_ex_na_filled$steps)) {
    
    if(is.na(data_ex_na_filled$steps[i])) {
        
        data_ex_na_filled$steps[i] = round(data_daily_average$mean_daily[grep(data_ex_na_filled$interval[i],data_daily_average$interval)[1]])
        
    }
    
}

# Calculating mean, median and histogram of total daily steps for
# NA filled dataset

data_grouped_by_day_nafilled = group_by(data_ex_na_filled,date)
data_summarized_nafilled = summarise(data_grouped_by_day_nafilled,total_steps = sum(steps))
hist(data_summarized_nafilled$total_steps,breaks = 10)
mean_daily_steps_nafilled = mean(data_summarized_nafilled$total_steps)
median_daily_steps_nafilled = median(data_summarized_nafilled$total_steps)

# Classifying days in weekdays and weekends

data_ex_na_filled = mutate(data_ex_na_filled,weekday = weekdays(as.Date(date)))




data_factor = mutate(data_ex_na_filled,
                    classification=ifelse(weekday == "Saturday"| weekday == "Sunday","weekend","weekday"))

data_interval = group_by(data_factor,interval,classification)

data_interval_summarized = summarise(data_interval,mean_steps = mean(steps))

# Making a panel plot of the average daily activity pattern on weedays and
# weekends

library(ggplot2)

plot = ggplot(data = data_interval_summarized,
             aes(interval,mean_steps))

plot = plot + geom_line() + facet_grid(classification ~ .)

plot








