library(readr)

library(tidyr)

library(dplyr)
library(stringr)

boarding_plan <- read_lines("day5.txt")

row_plan <- str_sub(boarding_plan,1,7)
col_plan <- str_sub(boarding_plan,8,10)


#build row vector
row_result <- rep(NA,length(row_plan))
k<-1
p<-128

for (i in 1:length(row_plan)) {
  
  #loop_start <- ifelse(str_sub(row_plan[i],k,k)=="F",1,p/2+1)
  loop_end   <- ifelse(str_sub(row_plan[i],k,k)=="F",p/2,p)
  k<-k+1
  #loop_start <- ifelse(str_sub(row_plan[i],k,k)=="F",loop_start,(loop_end-p/4)+1)
  loop_end   <- ifelse(str_sub(row_plan[i],k,k)=="F",loop_end-p/4,loop_end)
  k<-k+1
  #loop_start <- ifelse(str_sub(row_plan[i],k,k)=="F",loop_start,(loop_end-p/8)+1)
  loop_end   <- ifelse(str_sub(row_plan[i],k,k)=="F",loop_end-p/8,loop_end)
  k<-k+1
  #loop_start <- ifelse(str_sub(row_plan[i],k,k)=="F",loop_start,(loop_end-p/16)+1)
  loop_end   <- ifelse(str_sub(row_plan[i],k,k)=="F",loop_end-p/16,loop_end)
  k<-k+1
  #loop_start <- ifelse(str_sub(row_plan[i],k,k)=="F",loop_start,(loop_end-p/32)+1)
  loop_end   <- ifelse(str_sub(row_plan[i],k,k)=="F",loop_end-p/32,loop_end)
  k<-k+1
  #loop_start <- ifelse(str_sub(row_plan[i],k,k)=="F",loop_start,(loop_end-p/64)+1)
  loop_end   <- ifelse(str_sub(row_plan[i],k,k)=="F",loop_end-p/64,loop_end)
  k<-k+1
  #loop_start <- ifelse(str_sub(row_plan[i],k,k)=="F",loop_start,(loop_end-p/128)+1)
  loop_end   <- ifelse(str_sub(row_plan[i],k,k)=="F",loop_end-p/128,loop_end)
  
  row_result [i]<-loop_end-1
  
  k<-1
}

#build col vector
col_result <- rep(NA,length(col_plan))
k<-1
p<-8

for (i in 1:length(col_plan)) {
  
  
  loop_end   <- ifelse(str_sub(col_plan[i],k,k)=="L",p/2,p)
  k<-k+1
  
  loop_end   <- ifelse(str_sub(col_plan[i],k,k)=="L",loop_end-p/4,loop_end)
  k<-k+1
  
  loop_end   <- ifelse(str_sub(col_plan[i],k,k)=="L",loop_end-p/8,loop_end)
  k<-k+1
  
  col_result [i]<-loop_end-1
  
  k<-1
}



boarding_table <- as_tibble(cbind(row_result,col_result))

boarding_table <- boarding_table %>%
  mutate(ID = row_result * 8 + col_result)

boarding_map <- boarding_table %>%
  spread(col_result,ID)

empty_seats <- as_tibble(is.na.data.frame(boarding_map))



