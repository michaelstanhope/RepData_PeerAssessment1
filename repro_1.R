setwd("C:/Users/502179021/Documents/RStuff/")

data <- read.csv("activity.csv", stringsAsFactors = FALSE)

data$steps <- as.numeric(data$steps)
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- as.numeric(data$interval)

dataWithSteps <- subset(data, !is.na(data$steps))

dailySteps <- tapply(dataWithSteps$steps, dataWithSteps$date, FUN = sum)

hist(dailySteps, 
     breaks = 20, 
     main = "Steps per day", 
     xlab = "Number of steps (per day)" )

mean(dailySteps)
median(dailySteps)

meanIntervalSteps <- tapply(dataWithSteps$steps, dataWithSteps$interval, FUN = mean)
intervalNames <- as.POSIXlt(gsub(" ", "0", sprintf("%04s", names(meanIntervalSteps))), format = "%H%M")
plot(intervalNames, meanIntervalSteps, type = "l")
plot(names(meanIntervalSteps), meanIntervalSteps, type = "l")
which(max(as.numeric(meanIntervalSteps)) == as.numeric(meanIntervalSteps))
names(meanIntervalSteps[which(max(as.numeric(meanIntervalSteps)) == as.numeric(meanIntervalSteps))])

nrow(data[is.na(data$steps),])

intMean <- data.frame(cbind(names(meanIntervalSteps), as.vector(meanIntervalSteps)))
names(intMean) <- c("interval", "steps")

merged <- merge(data, intMean, by.x = "interval", by.y = "interval", all.x = TRUE)
merged[is.na(merged$steps.x),"steps.x"] <- as.vector(merged[is.na(merged$steps.x),"steps.y"]) 

merged$steps.x <- as.numeric(merged$steps.x)
merged$date <- as.Date(merged$date, format = "%Y-%m-%d")
merged$interval <- as.numeric(merged$interval)

mergedDailySteps <- tapply(merged$steps.x, merged$date, FUN = sum)

hist(mergedDailySteps, 
     breaks = 20, 
     main = "Steps per day", 
     xlab = "Number of steps (per day)" )

mean(mergedDailySteps)
median(mergedDailySteps)
