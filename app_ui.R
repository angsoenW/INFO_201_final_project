# source("app_server.R")

# Li-followings are variables needed for maps tab:
# ----------------------------------------------------------------------------
# read in the Jasper's table(subject to change)
map_df <- read.csv("OurData.csv", stringsAsFactors = FALSE)
# only use the county data, ignore city data(they are not in right form)
map_df <- map_df[map_df$geo_type == "county", ]
# replace NA with standard value = 100
map_df[is.na(map_df)] <- 100
# clean col names
colnames(map_df)[7:ncol(map_df)] <- as.character(as.Date(colnames(map_df)[7:ncol(map_df)], "X%m.%d.%y"))
# ----------------------------------------------------------------------------
# global var prepare code end here

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Readex+Pro:wght@300&display=swap');
      body {
        background-color: #ffffe6;
        color: black;
      }"))
  ),
  navbarPage("Transportation in Covid Times",
             tabPanel("Introduction",
                      h1("Introduction"),
                      p("This project mainly explores how the pandemic influenced people's choice of transportation.
                          This dataset was collected from Apple’s Maps app which was collected through
                          users' devices. The dataset provides visualization of changes in transportation
                          usage patterns of people during the COVID-19 pandemic. Such visualizations help
                          us understand the behavioral changes of people in the United States maneuvered
                          by the devastating global pandemic. Our analysis filtered down all data which
                          only happened in the United States city and county, which was how the transportation
                          trends had changed because of the COVID-19. It also revealed the relationship between
                          region, subregion, and change of the transportation type.In addition, Our dataset provides
                          information on transportation types, occurring dates, and mobility changes."),
                      br(),
                      img(src = "Tendencias_Movilidad_ENG.png", height = 600, width = 450, style="display: block; margin-left: auto; margin-right: auto;")
                      ),

             
             tabPanel("Chart 1",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "TypeW",
                              "Transportation type:",
                              choices = list("By walking" = 1,
                                             "By public transit" = 2,
                                             "By driving" = 3),
                              selected = 1
                              
                 ),
                 dateRangeInput("DatesW", "Date range:",
                                start = "2020-01-13",
                                end = "2021-10-26",
                                min = "2020-01-13",
                                max = "2021-10-26",
                                format = "yyyy-mm-dd",
                                separator = " - ",
                 )
               ),
               
               mainPanel(h1("Change of transportation Usage in the USA by time"),
                         p("This chart demonstrates the relationship among different transportation 
                          choice in certain period of time chosen by user. By using scatterplot, user
                            can obeserve how transportation usage has shifted througout this pendemic."),
                 plotOutput(outputId = "scatter")
               )
             )),
             
             tabPanel("Chart 2",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(
                            inputId = "sType",
                            label = "Select a Region:",
                            choices = list("Washington"= '1', "New York"= '2', "California"= '3'),
                          )
                        ),
                        mainPanel(
                          h1("Methods of Transportation"),
                          p("This is a pie chart that compares the methods of transportation used
                            in Washington, New York and California. As these three regions have",
                            strong("very different methods of transportation.")),
                          br(),
                          plotlyOutput(outputId = "piechart")
                        )
                      )),
             # Li- UI part for map tab
             # ---------------------------------------------------------
             tabPanel("Chart 3",
                      h1("Mobility Fluctuation during Covid-19"),
                      p("These maps reflects mobility and covid cases respectively: "),
                      p(strong("Mobility Fluctuation Map:"), "Represent the fluctuation of people's mobility pattern with a baseline set to pre-Covid period
                        (darker means larger fluctuation)"),
                      p(strong("New Cases Map:"), " Represent the amount of new cases in each state during the same period
                        (darker means more cases)"),
                      p(strong("Quick insights of this part:"), "In general, The comparison of two maps shows that
                        states where people have changed there mobility pattern the most (compared to pre-Covid period) tend to be states with less cases.
                        On the other hand, states where people remains doing what they did before Covid has larger number of new cases"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "timeRange", label = "Choose a Date Interval\n (click play button to get animated map)",
                                      min = as.Date("2020-01-30"), max = as.Date(colnames(map_df)[651]),
                                      value = c(as.Date("2020-01-30"),as.Date(colnames(map_df)[651])),
                                      animate = animationOptions(200) #local PC performance limit
                          ),
                          verbatimTextOutput("range")
                        ),
                        mainPanel(
                          br(),
                          tabsetPanel(
                            id = "graphs",
                            tabPanel("Maps", plotlyOutput(outputId = "mapPlot"), plotlyOutput(outputId = "casesPlot")),
                            tabPanel("Data Table", tableOutput(outputId = "check"))
                          )
                        )
                      )),
             # -----------------------------------------------------------
             # Li- map UI ends here


             tabPanel("Conclusion",
                      h1("Conclusion"),
                      p("First, through the analysis of the dataset,
                        we found that mobility trends has a strong positive relationship
                        with new cases of COVID-19. If people changed their transportation
                        , the states? new cases would be less than unchanged states.
                        In addition, some states constantly increasing new cases but will
                        suddenly change when the mobility trends are changed.
                        Therefore, we found that transportation mobility trends are related
                        to the pandemic new cases."),
                      p("Second, from the analysis we learned that the COVID-19 cases is not
                        only related to population, city area, and weekend effects, but after
                        taking those elements out of analysis, we still could pursue that trend
                        of transportation way has direct relationship with pandemic cases. "),
                      p("Third, we found that even though the COVID-19 triggered the change of
                        the transportation choosing, driving is the most popular way for people
                        to choose for their transportation. Moreover, we found that there are
                        still some people need transit for their transportation, even it might
                        cause increase of probability of getting Covid-19. "),
                      p("Overall, transit has the largest change during pandemic mobility trends,
                        but there are still people need to choose different transportation,
                        and the pandemic total cases has a strong relationship with mobility trends."),
                      img(src = "map.png", height = 320, width = 500, style="display: block; margin-left: auto; margin-right: auto;"),
                      img(src = "pie chart.png", height = 320, width = 510, style="display: block; margin-left: auto; margin-right: auto;"),
                      img(src = "Scatterplot.png", height = 320, width = 500, style="display: block; margin-left: auto; margin-right: auto;"),
                      ),
))
