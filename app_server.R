data <- read.csv("OurData.csv")

driving <- data %>%
  filter(sub.region == "Washington") %>%
  filter(transportation_type == "driving") %>%
  nrow()

walking <- data %>%
  filter(sub.region == "Washington") %>%
  filter(transportation_type == "walking") %>%
  nrow()

transit <- data %>%
  filter(sub.region == "Washington") %>%
  filter(transportation_type == "transit") %>%
  nrow()

df_sea<- data.frame(
  groups <- c("driving", "walking", "transit"),
  values <- c(driving, walking, transit)
)

driving_nyc <- data %>%
  filter(sub.region == "New York") %>%
  filter(transportation_type == "driving") %>%
  nrow()

walking_nyc <- data %>%
  filter(sub.region == "New York") %>%
  filter(transportation_type == "walking") %>%
  nrow()

transit_nyc <- data %>%
  filter(sub.region == "New York") %>%
  filter(transportation_type == "transit") %>%
  nrow()

df_nyc <- data.frame(
  groups_nyc <- c("driving", "walking", "transit"),
  values_nyc <- c(driving_nyc, walking_nyc, transit_nyc)
)

driving_cal <- data %>%
  filter(sub.region == "California") %>%
  filter(transportation_type == "driving") %>%
  nrow()

walking_cal <- data %>%
  filter(sub.region == "California") %>%
  filter(transportation_type == "walking") %>%
  nrow()

transit_cal <- data %>%
  filter(sub.region == "California") %>%
  filter(transportation_type == "transit") %>%
  nrow()

df_cal <- data.frame(
  groups_cal <- c("driving", "walking", "transit"),
  values_cal <- c(driving_cal, walking_cal, transit_cal)
)

p1 <- plot_ly(df_sea, labels = ~groups, values = ~values, type = 'pie') %>%
  layout(title = "Methods of Transportation in Washington")
p2 <- plot_ly(df_nyc, labels = ~groups_nyc, values = ~values_nyc, type = 'pie') %>%
  layout(title = "Methods of Transportation in New York")
p3 <- plot_ly(df_cal, labels = ~groups_cal, values = ~values_cal, type = 'pie') %>%
  layout(title = "Methods of Transportation in California")

#Wang's data:
dataW <- read.csv("OurData.csv")

dataW <- dataW%>%
  filter(sub.region == "")
dataW$country = NULL
dataW$X = NULL
dataW$geo_type = NULL
dataW$sub.region = NULL
dataW$region = NULL

driveW <- dataW %>%
  filter(transportation_type == "driving")%>%
  gather("Date", "Traffic_Change_Rate", -1)
driveW$transportation_type = NULL
driveW$Date <- gsub("X", "", driveW$Date)
driveW$Date <- as.Date(driveW$Date, "%m.%d.%y")

walkW <- dataW %>%
  filter(transportation_type == "walking")%>%
  gather("Date", "Traffic_Change_Rate", -1)
walkW$transportation_type = NULL
walkW$Date <- gsub("X", "", walkW$Date)
walkW$Date <- as.Date(walkW$Date, "%m.%d.%y")

publicW <- dataW %>%
  filter(transportation_type == "transit")%>%
  gather("Date", "Traffic_Change_Rate", -1)
publicW$transportation_type = NULL
publicW$Date <- gsub("X", "", publicW$Date)
publicW$Date <- as.Date(publicW$Date, "%m.%d.%y")
#Wang out



# Li- following code for map tab
# ---------------------------------------------------------------------------
# read in the Jasper's table(subject to change)
map_df <- read.csv("OurData.csv", stringsAsFactors = FALSE)
# only use the county data, ignore city data(they are not in right form)
map_df <- map_df[map_df$geo_type == "county", ]
# replace NA with standard value = 100
map_df[is.na(map_df)] <- 100
# clean col names
colnames(map_df)[7:ncol(map_df)] <- as.character(as.Date(colnames(map_df)[7:ncol(map_df)], "X%m.%d.%y"))

# covid map data prepare
covid_df <- read.csv("data/state_cum_data_nytimes.csv", stringsAsFactors = FALSE)
covid_df <- covid_df %>%
  select(-deaths, -fips) %>%
  pivot_wider(names_from = date, values_from = cases)
covid_df[is.na(covid_df)] <- 0
# ---------------------------------------------------------------------------
# Li- raw vars for map code end here




server <- function(input, output) {
  output$piechart <- renderPlotly({
    if(input$sType == "1") {p1}
    else if(input$sType == "2") {p2}
    else {p3}
  })


  # Li-server code for map
  # -------------------------------------------------------------------------
  # get user input update
  given_range <- reactive({
    start <- grep(as.character(input$timeRange[1]), colnames(map_df))
    end <- grep(as.character(input$timeRange[2]), colnames(map_df))
    return(c(start,end))
  })

  # generate a df contain map data and std for state level
  # (mean of the different type of transportation)
  get_map_df <- reactive({
    start <- given_range()[1]
    end <- given_range()[2]
    # deal with same day selection edge case
    if(end == start) {
      if(end != ncol(map_df)){
        end <- end + 1
      } else {
        start <- start - 1
      }
    }
    by_state_df <- map_df %>%
      mutate(std = rowSds(as.matrix(map_df[, start:end]))) %>%
      group_by(sub.region) %>%
      summarise(state_std = mean(std))
    by_state_df <- by_state_df %>%
      mutate(code = state.abb[match(by_state_df$sub.region,state.name)])
    by_state_df <- na.omit(by_state_df)
    #prepare cases map data
    cases_map_df <- covid_df %>%
      mutate("new_cases" = .[[end]] - .[[start]]) %>%
      select(state, new_cases)
    cases_map_df <- cases_map_df %>%
      mutate(code = state.abb[match(cases_map_df$state,state.name)])
    # aggregate 2 df
    by_state_df <- by_state_df %>%
      left_join(cases_map_df, by = "code")

    return(by_state_df)
  })

  # generate table tab
  output$check <- renderTable({
    get_map_df()[,c(1,2,5)]
  })

  # range text and notification
  output$range <- renderText({
    text <- paste("Current selected date range:\n",
                  as.Date(input$timeRange[1]), "to", as.Date(input$timeRange[2]))
    if(input$timeRange[2] - input$timeRange[1] > 60){
      text <- paste(text,"\n\nNote: For animation, \nwe recommend select a range less than two months")
    } else {
      text <- paste(text, "\n\nGood pick")
    }
    return(text)
  })

  # generate mobility fluctuation map
  output$mapPlot <- renderPlotly({
    by_state_df <- get_map_df()
    by_state_df$hover <- with(by_state_df, sub.region)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    fig <- plot_geo(by_state_df, locationmode = 'USA-states')
    fig <- fig %>% add_trace(
      z = ~state_std, locations = ~code, text = ~hover,
      colors = 'Purples'
    )
    fig <- fig %>% layout(
      title = "Mobility Fluctuation Map",
      legend=list(title=list(text='<b> Mobility(std) </b>')),
      geo = g
    )
  })

  # generate new cases graph
  output$casesPlot <- renderPlotly({
    by_state_df <- get_map_df()
    by_state_df$hover <- with(by_state_df, sub.region)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    fig2 <- plot_geo(by_state_df, locationmode = 'USA-states')
    fig2 <- fig2 %>% add_trace(
      z = ~new_cases, locations = ~code, text = ~hover,
      colors = "Reds"
    )
    fig2 <- fig2 %>% layout(
      title = "New Cases Map",
      geo = g
    )
  })
  # ---------------------------------------------------------------------------
  # Li - server code for map end here


#Wang in
  dataInputW <- reactive({
    startDateW <- input$DatesW[1]
    endDateW <- input$DatesW[2]
    if(input$TypeW == 1){
      fileW <- walkW
    } else if (input$TypeW == 3) {
      fileW <- driveW
    } else{
      fileW <- publicW
    }
    fileW %>%
      filter(Date >= startDateW)%>%
      filter(Date <= endDateW)
  })
  
  output$scatter <- renderPlot({
    ggplot(dataInputW(), aes(Date, Traffic_Change_Rate)) + 
      geom_point(size = 1, shape = 2) +
      geom_smooth(aes(colour="Trend(Linear Reg.)")) +
      scale_colour_manual(name="legend", values=c("blue")) +
      ggtitle("Change of transportation by Usage(by time)")
    
  })
}
#Wang out