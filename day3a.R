library(tidyr)
library(dplyr)
library(stringr)
library(readr)


map_table <- read_lines("day3.txt")

map_table <- as_tibble(map_table)

map_table %>% separate(map_table)

