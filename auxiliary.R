setwd("C:\\Users\\Jesus\\Google Drive\\Learning\\Coursera - Data Science\\Course 5 - Reproducible Research\\Course Project 1\\RepData_PeerAssessment1")

dat <- read.csv("activity.csv")

library(ggplot2)
library(dplyr)

data_df <- tbl_df(dat)
data_df <- data_df %>% 
    transmute(steps, date = as.POSIXct(strptime(date, "%Y-%m-%d")), interval) 
#convert date from factor to date

totalStepsDay <- data_df %>% group_by(date) %>% summarize(Total = sum(steps, na.rm = TRUE))

hist(totalStepsDay$Total, xlab = "Total Number Steps Taken On A Day", 
     main = "Histogram concerning total number of steps taken on a day")

mn <- mean(totalStepsDay$Total)
md <- median(totalStepsDay$Total)

avgStepsInt <- data_df %>% group_by(interval) %>% summarize(AverageNo = mean(steps, na.rm = TRUE))
plot(avgStepsInt$interval, avgStepsInt$AverageNo, type = "l")

max <- avgStepsInt %>% filter(interval, AverageNo == max(AverageNo))
maxEl <- max[[1,1]]
maxVal <- max[[1,2]]

numberNA <- sum(is.na(data_df$steps))

newData <- data_df %>% group_by(interval) %>% 
    mutate(newStepValue = ifelse(is.na(steps), mean(steps, na.rm = TRUE),steps)) %>%
    select(steps = newStepValue, date, interval) %>% 
    ungroup()

newTotalStepsDay <- newData %>% group_by(date) %>% summarize(Total = sum(steps, na.rm = FALSE))

hist(newTotalStepsDay$Total, xlab = "Total Number Steps Taken On A Day", 
     main = "Histogram concerning total number of steps taken on a day")

mnNew <- mean(newTotalStepsDay$Total)
mdNew <- median(newTotalStepsDay$Total)

data_df %>% filter(., is.na(steps)) %>% group_by(date) %>% summarise(n())
dataWithNA <- data_df %>% filter(., is.na(steps)) %>% group_by(date) %>% summarise(count = n()) %>% select(year = format(as.POSIXct(date, origin="1970-01-01"), format="%Y"), count)

newDataWithWD <- newData %>% 
    mutate(weekday = factor(weekdays(date) %in% c("Saturday","Sunday"), 
                                    labels = c("weekday","weekend")))

avgWD <- newDataWithWD %>% group_by(weekday, interval) %>% summarize(average = sum(steps))

ggplot(avgWD, aes(interval, average)) + geom_line() + facet_grid(. ~ weekday)