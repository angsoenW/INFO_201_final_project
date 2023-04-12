library(dplyr)
library(tidyverse)
library(ggplot2)

US_data <-read.csv("OurData.csv")

driving <- US_data %>%
  filter(transportation_type == "driving") %>%
nrow()

walking <- US_data %>%
  filter(transportation_type == "walking") %>%
nrow()

transit <- US_data %>%
  filter(transportation_type == "transit") %>%
nrow()

df <- data.frame(
  groups <- c("driving", "walking", "transit"), 
  values <- c(driving, walking, transit)
)

labels_df <- df %>%
  mutate(perc = values....c.driving..walking..transit. / sum(values....c.driving..walking..transit.), 
         cumulative = cumsum(values....c.driving..walking..transit.),
         midpoint = cumulative - values....c.driving..walking..transit. / 2,
         labels = paste0(round((values....c.driving..walking..transit./ sum(values....c.driving..walking..transit.)) * 100, 1), "%"))

pie_chart <- ggplot(labels_df, aes(x="", y=values, fill=groups)) + geom_bar(stat="identity", width=1, color="white") + 
  coord_polar("y", start=0) + theme_void() + geom_text(aes(label = labels), position = position_stack(vjust=0.5)) +
  ggtitle("Methods of Transportation in United States")




                                                                                                                                         