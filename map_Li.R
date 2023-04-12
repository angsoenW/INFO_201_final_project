
library(tidyverse)
library(ggplot2)
library(patchwork)
library(matrixStats)
library(gridExtra)

library(lintr)
lint("map_Li.R")

# remove old data
rm(list= ls())

# read in the Jasper's table(subject to change)
df <- read.csv("OurData.csv", stringsAsFactors = FALSE)

# only use the county data, ignore city data(they are not in right form)
df <- df[df$geo_type == "county", ]
# replace NA with standard value = 100
df[is.na(df)] <- 100


# generate a df contain map data and std for state level
# (mean of the different type of transportation)
by_state_df <- df %>%
  mutate(std = rowSds(as.matrix(df[, 7:659]))) %>%
  group_by(sub.region) %>%
  summarise(state_std = mean(std)) 

by_state_df$sub.region <- tolower(by_state_df$sub.region)

state_data <- map_data("state") %>%
  right_join(by_state_df, by=c("region" = "sub.region"))

state_data <- subset(state_data, select = -subregion)

# use graphic theme provided by TA
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines0
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank(), # remove border around plot
    plot.title = element_text(hjust = 0.5)
  )

# Create the map for state, based on state std
# (mean of all available transportation type)
std_map_state <- ggplot(state_data) + # graphing map_data table
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = state_std),
    color="gray", size = 0.3
  ) + 
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(limits = c(min(state_data$state_std), max(state_data$state_std)), 
                        na.value = "blue", low="yellow", high="red", 
                        name = "rate of fluctuation") + 
  blank_theme + # minimalist theme
  ggtitle("Mobility Fluctuation by State") # title

# generate a df contain map data and std for county level
# (highest of the different type of transportation for each state)
by_county_df <- df %>%
  mutate(std = rowSds(as.matrix(df[, 7:659]))) %>%
  unite("polyname", c("region", "sub.region"), sep = ",") %>%
  group_by(polyname) %>%
  slice_max(std)

by_county_df$polyname <- tolower(by_county_df$polyname)
by_county_df$polyname<-gsub(" county","",as.character(by_county_df$polyname))

county_data <- map_data("county") %>%
  unite("polyname", c("subregion", "region"), sep = ",") %>%
  right_join(by_county_df, by = "polyname") %>%
  select(c(long:transportation_type, std))


# Create the map for county, based on county std(highest transportation type)
std_map_county <- ggplot() + # graphing map_data table
  geom_polygon(
    data = county_data,
    mapping = aes(x = long, y = lat, group = group, fill = std),
    color="gray", size = 0.3
  ) + 
  scale_fill_continuous(limits = c(min(county_data$std), max(county_data$std)), 
                        na.value = "blue", low="yellow", high="red",
                        name = "rate of fluctuation") + 
  coord_map() + # use a map-based coordinate system
  blank_theme + # minimalist theme
  ggtitle("Mobility Fluctuation by County") # title


# Create the map for county, based on biggest change of transportation map
# (highest transportation type)
type_map_county <- ggplot() + # graphing map_data table
  geom_polygon(
    data = county_data,
    mapping = aes(x = long, y = lat, group = group, fill = transportation_type),
    color="gray", size = 0.3
  ) + 
  coord_map() + # use a map-based coordinate system
  scale_fill_discrete(name = "Transportation Type") +
  blank_theme + # minimalist theme
  ggtitle("Most Impacted Transportation Type") # title

# combine all maps with assigned style
lay <- rbind(c(1),
             c(2),
             c(3))
final_graph <- grid.arrange(std_map_state, std_map_county, type_map_county, 
                            layout_matrix = lay)











