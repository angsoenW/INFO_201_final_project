install.packages('rsconnect')
library("shiny")
library("tidyverse")
library("plotly")
# additional library for map:
library(matrixStats)
# publish
library(rsconnect)


source("app_ui.R")
source("app_server.R")

shinyApp(ui = ui, server = server)





