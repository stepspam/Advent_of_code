library(tidyr)
library(dplyr)
library(stringr)

pasw_data <- read.table("day2.txt",sep=" ",col.names = c("range","letter","password"))

pasw_data <- separate(pasw_data,range,sep = "-",into=c("min_count","max_count"))
pasw_data <- separate(pasw_data,letter,sep=":", into= "letter_cond")

pasw_data$min_count <- as.numeric(pasw_data$min_count)
pasw_data$max_count <- as.numeric(pasw_data$max_count)

pasw_data$cond_min <- str_sub(pasw_data$password,pasw_data$min_count,pasw_data$min_count)
pasw_data$cond_max <- str_sub(pasw_data$password,pasw_data$max_count,pasw_data$max_count)



#pasw_data$cond_count <- str_count(pasw_data[,4],pattern=pasw_data[,3])

pasw_data$pass_cond_min <- ifelse(pasw_data$letter_cond==pasw_data$cond_min,TRUE,FALSE)
pasw_data$pass_cond_max <- ifelse(pasw_data$letter_cond==pasw_data$cond_max,TRUE,FALSE)

pasw_data$cond_overal <- ifelse(pasw_data$pass_cond_min | pasw_data$pass_cond_max,TRUE,FALSE)
pasw_data$cond_overal <- ifelse(pasw_data$pass_cond_min == pasw_data$pass_cond_max,FALSE,pasw_data$cond_overal)

solutionday2b <- pasw_data %>% count(cond_overal)                                 
View(solutionday2b)
