library("tidyverse")

table <- read.csv('OurData.csv')

table[,1] <- NULL
table$country <- NULL

table <- table%>%
  filter(sub.region != "")

final_table <- group_by(table, sub.region)%>%
  summarise_if(is.numeric, sum)
