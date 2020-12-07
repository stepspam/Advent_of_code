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
  mutate(valid_cid = if_else(is.na(cid),FALSE,TRUE)) %>%
  filter(valid_base==TRUE)

psp_table_valid <- psp_table %>%
  mutate(valid_byr = if_else(byr>=1920 & byr<=2002,TRUE,FALSE)) %>%
  mutate(valid_iyr = if_else(iyr>=2010 & iyr<=2020,TRUE,FALSE)) %>%
  mutate(valid_eyr = if_else(eyr>=2020 & eyr<=2030,TRUE,FALSE)) %>%
  mutate(hgt_value = as.numeric(str_extract(hgt, "[0-9]+"))) %>%
  mutate(hgt_unit = str_extract(hgt, "[aA-zZ]+")) %>%
  drop_na(hgt_unit) %>%
  mutate(valid_hgt = if_else(hgt_unit=="cm", 
                                    if_else(hgt_value>=150 & hgt_value<=193,TRUE,FALSE),
                                    if_else(hgt_value>=59 & hgt_value<=76,TRUE,FALSE)
                                  )
         ) %>%
  mutate(valid_hcl = if_else(str_detect(hcl,"#[:alnum:]{6}"),TRUE,FALSE)) %>%
  mutate(valid_ecl = if_else(str_detect(ecl,"amb")|str_detect(ecl,"blu")|str_detect(ecl,"brn")|str_detect(ecl,"gry")|str_detect(ecl,"grn")|str_detect(ecl,"hzl")|str_detect(ecl,"oth"),TRUE,FALSE)) %>%
  mutate(valid_pid = if_else(str_detect(pid,"[:digit:]{9}") & str_count(pid)<10,TRUE,FALSE)) %>%
  mutate(valid_overall = valid_byr & valid_iyr & valid_eyr & valid_hgt & valid_hcl & valid_ecl & valid_pid) %>%
  select(-c(valid_base,valid_cid)) %>%
  relocate(hgt, .after=valid_eyr)

solutionday4b<- psp_table_valid %>%
  count(valid_overall)



