library(tidyverse)
library(rvest)
library(httr)

sopa_list <- read.csv('data/original/SOPA Report Links.txt', header = FALSE, sep="\n")

sub_list <- sopa_list[1:10,]

for (url in sub_list){
  
}

sopa_test <- read_html(sopa_list[1,])

sopa_table <- sopa_test %>% 
  html_element("table") %>%
  html_table()

sopa_test %>% html_nodes("table") %>% html_table()
