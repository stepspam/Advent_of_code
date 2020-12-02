
data<-scan("day1.txt")

combinations <- combn(data,2,simplify = TRUE)

combinationssum <- colSums(combinations)

locationvalue <- match(2020, combinationssum)

return <- combinations[c(1,2),locationvalue]

solutionday1 <- return[1]*return[2]

solutionday1

