#Viewing the number of unique values within the column (Should be 3 (Primary, Secondary, Tertiary))
length(unique(data$education)) #= 4

#Counting Empty Spaces 
sum(data$education == "") # 47 missing values

#Counting Na values
sum(is.na(data$education)) # No NA values

avg_balance <- data %>%
  filter(education != "") %>%
  group_by(education) %>%
  summarize(avg_balance = mean(balance, na.rm = TRUE))

#attach(avg_balance)

tempdf <- data %>%
  mutate(
    tertiary_diff = abs(balance - avg_balance$avg_balance[3]),
    secondary_diff = abs(balance - avg_balance$avg_balance[2]),
    primary_diff = abs(balance - avg_balance$avg_balance[1])
  )

tempdf <- tempdf %>%
  rowwise() %>%
  mutate(
    education = if_else(
      education == "",
      if_else(
        tertiary_diff <= secondary_diff & tertiary_diff <= primary_diff,
        "tertiary",
        if_else(
          secondary_diff <= primary_diff,
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
  






