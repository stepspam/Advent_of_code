
data<-scan("day1.txt")

combinations <- combn(data,3,simplify = TRUE)

combinationssum <- colSums(combinations)

locationvalue <- match(2020, combinationssum)

return <- combinations[c(1,2,3),locationvalue]

solutiondayab<- return[1]*return[2]*return[3]

solutiondayab

