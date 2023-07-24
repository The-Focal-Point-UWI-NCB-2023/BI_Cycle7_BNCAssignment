nrow(data[is.na(data$contacted),])

data[is.na(data$contacted),] = 0

nrow(data[is.na(data$contacted),])
