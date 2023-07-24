boxplot(data$loanvalue)
na.loanvalue <- rownames(data[is.na(data$loanvalue),])
length(na.loanvalue)
d <- density(data$loanvalue,na.rm = TRUE)
plot(d,frame=FALSE,col="Blue",main="skew of data before cleaning")


for (i in na.loanvalue){
    print(i)
    temp.row <- data[i,]
    similar.rows <- data[data$agerange == temp.row$agerange & data$job == temp.row$job & data$loan == temp.row$loan & data$housing == temp.row$housing & !is.na(data$loanvalue) ,]
    if (nrow(similar.rows)<1){
      similar.rows <- data[data$agerange == temp.row$agerange & data$job == temp.row$job & !is.na(data$loanvalue), ]
    }
    round(mean(similar.rows$loanvalue),0)
    data[i,]$loanvalue = round(mean(similar.rows$loanvalue),0)
}

summary(data)
boxplot(data$loanvalue)
d <- density(data$loanvalue,na.rm = TRUE)
plot(d,frame=FALSE,col="Blue",main="skew of data after cleaning")
