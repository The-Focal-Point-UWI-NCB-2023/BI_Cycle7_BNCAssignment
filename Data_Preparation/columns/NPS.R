# 624 NAs, No value to analysis remove
data$NPS <- ifelse(is.na(data$NPS), -1, data$NPS)

#data$NPS <- NULL
