#("admin", "unemployed", "management", "housemaid", 
#"entrepreneur", "student", "blue-collar", "self-employed", 
#"retired", "technician", "services")

data$job <- as.character(data$job)
data$job[data$job=='admin.'] <- "admin"
data$job <- as.factor(data$job)

summary(data$job)

jobPreClean <- data$job

#plot(density(as.numeric(jobPreClean)))
#plot(hist(as.numeric(jobPreClean)))

levels(jobPreClean)

# Replace blank values with average per job
# For each blank job get the balance

#Select all the rows which are empty
empty_rows <- which(data$job == "")
#Find the average balance of a person based on their job
avgBalancePerJob <- aggregate(data$balance, list(data$job), FUN = mean, na.rm = TRUE)
#View(avgBalancePerJob)

#print(empty_rows)
#Loop over empty rows, selecting the closest Job match based on their balance and the average Job Balance
for (row in empty_rows) {
  closestMatch <- avgBalancePerJob$Group.1[which.min(abs(data$balance[row] - avgBalancePerJob$x))]
  print(paste("Row:", row))
  print(paste("Current Balance:",data$balance[row]))
  print(paste("Current Value:", data$job[row]))
  print(paste("Closest Match:", closestMatch))
  data$job[row] <- closestMatch
}
empty_rows <- NULL
avgBalancePerJob <- NULL

# Remove empty factor levels
data$job <- droplevels(data$job)
levels(data$job)
