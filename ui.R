library(shiny)
require(rCharts)
source('workout.R')
workout <- get_workout_data()
options(RCHART_LIB = 'dimple')

## Define the UI for the application
shinyUI(pageWithSidebar(h2("Workout plots!"),
            sidebarPanel(
                selectInput('sport', label = h4("select a sport"), choices = as.list(c("All", as.character(levels(workout$sport))))),
                selectInput('year', label = h4("select a year"), choices = as.list(c(2014, 2015)))
            ),
            mainPanel(
                     showOutput("mychart", "dimple")
            )
    )
    
)