library(dplyr)
library(tidyr)
library(stringr)
library(readr)

q_data<-read_lines("day6.txt")



#define number of rows
k<-1
for (i in 1:length(q_data)){
  k <- ifelse(q_data[i]=="",k+1,k)
}

#build vector with every row one pasport
y <- rep(NA,k)

k    <- 1

for (i in 1:length(q_data)){
  
  k <- ifelse(q_data[i]=="",k+1,k)
  
  y[k] <- ifelse(is.na(y[k]),q_data[i],str_c(y[k]," ",q_data[i]))
  
  print(k)
  
}

#solution 1:
x <- gsub(" ", "", y, fixed = TRUE)
x <- vapply(strsplit(x, NULL), function(x) paste(sort(x), collapse = ''), '')
x<- gsub('([[:alpha:]])\\1+', '\\1', x)

q_data_tidy <- as_tibble(x)
q_data_tidy <- q_data_tidy %>%
  mutate(yes_count = str_length(value))
  
solutionday6a <- q_data_tidy %>%
  summarise(sum(yes_count))
solutionday6a

#solution 2: 
y<-trimws(y)

z=matrix(NA,length(y),26)
a=rep(NA,length(y))
k <- 0
a<-0


#count # of people per group
for (i in 1:length(y)){
  a = str_count(y[i], '\\w+')
 
  for(j in 1:26){
    k= str_count(y[i],letters[j])
    z[i,j]=ifelse(k==a,1,0)
  }
}

solutionday6b=sum(z)


