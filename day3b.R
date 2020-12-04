library(readr)
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

#prepare the 5 runs
right <- c(1,3,5,7,1)
down <-c(1,1,1,1,2)

#prepare number of steps per run
nsteps <- round(323/down)

#build solution vector
solution3b <- rep(NA,5)
j<-1

for (l in 1:5) {
  
      #build row vector
      x <- rep(NA,nsteps[j])
      x[1] <- 1
      k<-1
      
      for (i in 1:nsteps[j]) {
        k<- ifelse(k>31,k-31,k)
        x[i]<-k
        k<-k+right[j]
      }
      
      #build column vector
      y <- rep(NA,nsteps[j])
      y[1] <- 1
      
      k<-1
      for (i in 1:nsteps[j]) {
        y[i]<-k
        k=k+down[j]
      }
      
      #build tree vector
      t<- rep(NA,nsteps[j])
      
      for (i in 1:nsteps[j]){
        t[i] <- map_table[y[i],x[i]]
      }
      t <- as.numeric(t)
      
      #fill solution vector
      solution3b[l] <- sum(t)
      j<-j+1
}

#calculate solution
solution3btotal <- prod(solution3b)
solution3btotal
