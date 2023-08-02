#Run Combine.R and finalModel.R

predTest <- predict(DTmodel, combinations, type="class")
probTest <- predict(DTmodel, combinations, type="prob")

# Adding variables
combinations$pred <- predTest
combinations$prob1 <- probTest[,2]
combinations$prob0 <- probTest[,1]
combinations$prob <- ifelse(combinations$prob0 > combinations$prob1, combinations$prob0, combinations$prob1)

write.csv(combinations,"dataCombinations.csv", row.names = FALSE)
