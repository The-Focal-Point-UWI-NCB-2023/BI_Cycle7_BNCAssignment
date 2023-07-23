#view the structure of the duration column
str(data$duration)

#view the summary of the data
summary(data$duration)

#find the sum of missing values 
dur_na_sum <- sum(is.na(data$duration))

#find the sum of missing values 
dur_null_sum <- sum(is.null(data$duration))

#convert from seconds to minutes
data$duration_temp <- round(as.duration(data$duration) / dminutes(1))
data$duration_before <- data$duration
data$duration <- data$duration_temp

data$duration <- ifelse(data$duration>12, mean(data$duration), data$duration)


#shows the distribution
##chose histogram because of numeric values 
duration.hist_before <- hist(data$duration_before, main = 'Histogram of Duration (before cleaning)')
duration.hist_after <- hist(data$duration,main = 'Histogram of Duration (after cleaning and conversion)')


#density plot for alternate visualization
d_before <- density(data$duration_before,na.rm = T)
plot(d_before,frame=FALSE, col = "Blue",main = 'Density plot of Duration (before cleaning)')

#density plot for alternate visualization
d_after <- density(data$duration,na.rm = T)
plot(d_after,frame=FALSE, col = "Blue",main = 'Histogram of Duration (after cleaning and conversion)')

avg_duration <- mean(data$duration)

#Remove temp variables
data$duration_temp <- NULL
data$duration_before <- NULL
duration.hist_before <- NULL
duration.hist_after <- NULL
d_before <- NULL
d_after <- NULL




