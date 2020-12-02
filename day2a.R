library(tidyr)
library(dplyr)
library(stringr)

pasw_data <- read.table("day2.txt",sep=" ",col.names = c("range","letter","password"))

pasw_data <- separate(pasw_data,range,sep = "-",into=c("min_count","max_count"))
pasw_data <- separate(pasw_data,letter,sep=":", into= "letter_cond")



pasw_data$cond_count <- str_count(pasw_data[,4],pattern=pasw_data[,3])

pasw_data$pass_cond <- if_else(pasw_data$min_count<pasw_data$cond_count )
                                  
