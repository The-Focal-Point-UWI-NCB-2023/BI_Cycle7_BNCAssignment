# 624 NAs
data$NPS <- ifelse(is.na(data$NPS), -1, data$NPS)
