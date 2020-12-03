
#prepare matrix for calculation
##read text file and write to vector
map_table <- read_lines("day3.txt")
##transform to matrix class
map_table <- as.matrix(map_table)
##split the 1 column into 31 columns
map_table <-  strsplit(map_table,"")
##transform list to matrix
map_table <- matrix(unlist(map_table), ncol =323)
##transpose to meet question 
map_table <- t(map_table)
##replace trees with numeric value 1 to use in sum later
map_table <- replace(map_table,map_table==".","0")
map_table <- replace(map_table,map_table=="#","1")



#build row vector
x <- rep(NA,323)
x[1] <- 1
k<-1

for (i in 1:323) {
  print(k)
  k<- ifelse(k>31,k-31,k)
  x[i]<-k
  k<-k+3
}

#build column vector
y <- rep(NA,323)
y[1] <- 1

k<-1
for (i in 1:323) {
  y[i]<-k
  k=k+1
}

#build tree vector
t<- rep(NA,323)

for (i in 1:323){
  t[i] <- map_table[y[i],x[i]]
}
t <- as.numeric(t)

solution3a <- sum(t)
