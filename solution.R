################################################################################################################
################################################################################################################
####################################### REPRODUCIBLE RESEARCH PROJECT 1 ########################################


#### LOAD ggplot2 AND chron ####
library(ggplot2)
library(chron)


#### LOAD DATA ####
data <- read.csv("activity.csv")


#### TOTAL NUMBER OF STEPS TAKEN EACH DAY ####
steps <- tapply(data$steps, data$date, sum, na.rm = TRUE)


#### HISTOGRAM OF THE TOTAL NUMBER OF STEPS TAKEN EACH DAY ####
png(filename = "plot1.png", width = 1800, height = 800)
hist(steps, xlab = "Steps")
dev.off()


#### MEAN AND MEDIAN NUMBER OF STEPS TAKEN EACH DAY ####
mean.steps <- mean(steps)
median.steps <- median(steps)


#### TIME SERIES PLOT OF THE AVERAGE NUMBER OF STEPS TAKEN ####
steps <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
x <- as.numeric(names(steps))
png(filename = "plot2.png", width = 1800, height = 800)
g <- qplot(x, steps, geom = "line", xlab = "5-minute interval", 
        ylab = "Number of steps taken", main = "Number of steps taken, averaged across all days")
print(g)
dev.off()

#### THE 5-MINUTE INTERVAL THAT, ON AVERAGE, CONTAINS THE MAXIMUM NUMBER OF STEPS ####
max.steps <- as.numeric(names(steps)[which.max(steps)])


#### TOTAL NUMBER OF MISSING VALUES IN THE DATASET ####
total.miss <- sum(is.na(data$steps))

#### FILLING MISSING VALUES IN THE DATASET ####
for (k in seq_along(x)){
  data$steps[is.na(data$steps)][data$interval[is.na(data$steps)] == x[k]] <- as.numeric(steps[k])
}

#### TOTAL NUMBER OF STEPS TAKEN EACH DAY ####
steps <- tapply(data$steps, data$date, sum, na.rm = TRUE)

#### HISTOGRAM OF THE TOTAL NUMBER OF STEPS TAKEN EACH DAY ####
png(filename = "plot3.png", width = 1800, height = 800)
hist(steps, xlab = "Steps")
dev.off()

#### MEAN AND MEDIAN NUMBER OF STEPS TAKEN EACH DAY ####
mean.steps <- mean(steps)
median.steps <- median(steps)


#### AVERAGE NUMBER OF STEPS TAKEN PER 5-MINUTE INTERVAL ACROSS WEEKDAYS AND WEEKENDS ####
type.day.bool <- is.weekend(as.Date(data$date))
type.day <- character()
type.day[type.day.bool] <- "weekend"
type.day[!type.day.bool] <- "weekday"
data$type_day <- type.day

steps <- data.frame(tapply(data$steps, list(data$interval, data$type_day), mean))
steps_min <- c(steps$weekday, steps$weekend)
type_day <- rep(c("weekday", "weekend"), each = nrow(steps))
x <- rep(as.numeric(rownames(steps)), times = 2)
steps <- data.frame(x, steps_min, type_day)

png(filename = "plot4.png", width = 1800, height = 800)
p <- qplot(x, steps_min, data = steps, geom = "line", facets = .~type_day, xlab = "5-min interval", 
          ylab = "Averaged steps taken per 5-minute interval", 
          main = "Averaged steps taken per 5-minute interval across weekdays and weekends")
print(p)
dev.off()