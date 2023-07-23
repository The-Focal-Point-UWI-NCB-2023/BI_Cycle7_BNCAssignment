#Viewing the number of unique values within the column (Should be 3 (Primary, Secondary, Tertiary))
length(unique(data$education)) #= 4

#Counting Empty Spaces 
sum(data$education == "") # 47 missing values

#Counting Na values
sum(is.na(data$education)) # No NA values

avg.balance <- data %>%
  filter(education != "") %>%
  group_by(education) %>%
  summarize(avg.balance = mean(balance, na.rm = TRUE))

#attach(avg_balance)

tempdf <- data %>%
  mutate(
    tertiary.diff = abs(balance - avg.balance$avg.balance[3]),
    secondary.diff = abs(balance - avg.balance$avg.balance[2]),
    primary.diff = abs(balance - avg.balance$avg.balance[1])
  )

tempdf <- tempdf %>%
  rowwise() %>%
  mutate(
    education = if_else(
      education == "",
      if_else(
        tertiary.diff <= secondary.diff & tertiary.diff <= primary.diff,
        "tertiary",
        if_else(
          secondary.diff <= primary.diff,
          "secondary",
          "primary"
        )
      ),
      education
    )
    )

# Removing temporary fields
data$education <- as.factor(tempdf$education)
tempdf <- NULL
avg_balance <- NULL

# Checking sum of each category
sum(data$education == "primary")
sum(data$education == "secondary")
sum(data$education == "tertiary")

# Remove empty factor levels
data$education <- droplevels(data$education)
levels(data$job)

#str(data$education)
summary(data$education)  






