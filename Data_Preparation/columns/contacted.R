nrow(data[is.na(data$contacted),])

data$contacted <- ifelse(is.na(data$contacted), 0, data$contacted)

nrow(data[is.na(data$contacted),])
