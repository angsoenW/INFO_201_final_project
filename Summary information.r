library(dplyr)
library(tidyverse)
library(ggplot2)

US_data <- read.csv("OurData.csv", stringsAsFactors = FALSE)
US_data <- US_data[,-1]

rows_count <- nrow(US_data)
#we have 3102 rows in the dataset
col_count <- ncol(US_data)
#we have 658 columns in the dataset
uniq_city <- unique(US_data[,2])
num_city <- length(uniq_city)
#we have 1536 unique cities

# A function that takes in a dataset and returns a list of info about it:
summary_info <- list(US_data)
transportation_type <- US_data[,3]
Driving_info <- US_data %>%
  filter(grepl('driving', transportation_type))%>%
  filter(X1.14.20 == max(X1.14.20, na.rm = T)) %>%
  pull(region)
#On the first day, the most changed city in driving is Butler county
Transit_info <- US_data %>%
  filter(grepl('transit', transportation_type))%>%
  filter(X1.14.20 == max(X1.14.20, na.rm = T)) %>%
  pull(region)
#On the first day, the most changed city in transit is Annapolis
Walking_info <- US_data %>%
  filter(grepl('walking', transportation_type))%>%
  filter(X1.14.20 == max(X1.14.20, na.rm = T)) %>%
  pull(region)
#On the first day, the most changed city in walking is Milwaukee county
