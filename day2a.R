library(tidyr)
library(dplyr)
library(stringr)

pasw_data <- read.table("day2.txt",sep=" ",col.names = c("range","letter","password"))

pasw_data <- separate(pasw_data,range,sep = "-",into=c("min_count","max_count"))
pasw_data <- separate(pasw_data,letter,sep=":", into= "letter_cond")

pasw_data$min_count <- as.numeric(pasw_data$min_count)
pasw_data$max_count <- as.numeric(pasw_data$max_count)

pasw_data$cond_count <- str_count(pasw_data[,4],pattern=pasw_data[,3])

pasw_data$pass_cond_1 <- ifelse(pasw_data$min_count<=pasw_data$cond_count & pasw_data$max_count>=pasw_data$cond_count,TRUE,FALSE)



solutionday2a <- pasw_data %>% count(pass_cond_1)                                 
View(solutionday2a)
