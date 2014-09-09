library(shiny)
require(rCharts)
source('workout.R')
workout <- get_workout_data()
options(RCHART_LIB = 'dimple')

## Define the UI for the application
shinyUI(pageWithSidebar(
    headerPanel("Workout plots!"),
    sidebarPanel(
        #selectInput('select', label = h4("select a sport"), choices = list(levels(workout$sport)))
    ),
    mainPanel(
        h4("test"),
        showOutput("mychart", "dimple")
    )
))