
min.deposit <- min(data$deposit, na.rm = T) # Setting Min Value
max.deposit <- max(data$deposit, na.rm = T) # Setting Max value

new.min <- 0 # Setting new min value for normalized Data to 0
# Orginal Max - 479
new.max <- 199 # Setting new max value for normalized Data to 199



#data$deposit <- as.integer(df$deposit)

data<- data %>% 
  # Mutate function essentially  alters the content of a column.
  # In this case we Altering the deposit column to updated with min max normalized values 
  mutate(deposit = as.integer(
    (deposit - min.deposit)/(max.deposit - min.deposit ) * (new.max - new.min) + new.min
    #Formula for Min Max = normalized_value = (value - minimum_value) / (maximum_value - minimum_value) * (new_max - new_min) + new_min
  )
  )


