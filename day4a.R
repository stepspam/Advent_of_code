library(dplyr)
library(tidyr)
library(stringr)


psp_data<-read_lines("day4.txt")



#define number of rows
k<-1
for (i in 1:length(psp_data)){
   k <- ifelse(psp_data[i]=="",k+1,k)
}

#build vector with every row one pasport
x <- rep(NA,k)

k    <- 1

for (i in 1:length(psp_data)){
  
  k <- ifelse(psp_data[i]=="",k+1,k)
  
  x[k] <- ifelse(is.na(x[k]),psp_data[i],str_c(x[k]," ",psp_data[i]))
  
  print(k)
  
}
x <- trimws(x)

#prepare dataframe with tidy pasport data
psp_table <- as_tibble(x,colnames=c("source"))
psp_table <- psp_table %>% 
  rename(Source=value) %>%
  mutate(id = row_number()) %>%
  relocate(id) %>%
  separate_rows(Source,sep=" ") %>%
  separate(Source,sep=":",into = c("data_type","data_value")) %>%
  spread(data_type,data_value) %>%
  relocate(id,byr,iyr,eyr,hgt,hcl,ecl,pid,cid) %>%
  #rename("Birth_year"=byr,"Issue_year"=iyr,"Expiration_year"=eyr, "Height"=hgt, "Hair_color"=hcl, "Eye_color"=ecl, "Passport_id"=pid, "Country_id"=cid) %>% 
  mutate(valid_base = if_else(is.na(byr) |is.na(iyr)|is.na(eyr)|is.na(hgt)|is.na(hcl)|is.na(ecl)|is.na(pid),FALSE,TRUE)) %>%
  mutate(valid_cid = if_else(is.na(cid),FALSE,TRUE))

#calculate number of valid & invalid passports
solutionday4a <- psp_table %>%
  count(valid_base)

