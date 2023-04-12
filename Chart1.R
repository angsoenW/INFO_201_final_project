library(dplyr)
library(tidyverse)
library(ggplot2)

charD <- read.csv("OurData.csv")
charD[,1] <- NULL
charD$country <- NULL
charD$geo_type <- NULL
charD$region <- NULL

charD <- charD %>%
  filter(!sub.region == "")%>%
  filter(transportation_type == "transit")%>%
  group_by(sub.region)%>%
  summarise_if(is.numeric, sum)

for (i in 1:35){
  cons <- charD[i, 2]
  for (h in 2:653){
    charD[i,h] <- charD[i,h]/cons
  }
}

test <- gather(charD, "Date", "Traffic_Change_Rate", -1)

test$Date <- gsub("X", "", test$Date)
test$Date <- as.Date(test$Date, "%m.%d.%y")

linear_regression <- ggplot(test, aes(Date, Traffic_Change_Rate)) + 
       geom_point(size = 1, shape = 2) +
       geom_smooth(aes(colour="Trend(Linear Reg.)")) +
  scale_colour_manual(name="legend", values=c("blue")) +
  ggtitle("Change of Public Transit Usage(by time)")

