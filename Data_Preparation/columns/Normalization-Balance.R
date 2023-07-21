
min.balance <- min(data$balance, na.rm = T) # Setting Min Value
max.balance <- max(data$balance, na.rm = T) # Setting Max value

# Original 252
new.min <- -112 # Setting new min value for normalized Data to 1
new.max <- 669 # Setting new max value for normalized Data to 100


#Normalization Based on Formula
data<- data %>% 
  # Mutate function essentially  alters the content of a column.
  # In this case we Altering the deposit column to updated with min max normalized values 
  mutate(balance = as.integer(
    ((balance - min.balance)/(max.balance - min.balance ) * (new.max - new.min) + new.min)
    #Formula for Min Max = normalized_value = (value - minimum_value) / (maximum_value - minimum_value) * (new_max - new_min) + new_min
    )
  )

boxplot(data$balance)


