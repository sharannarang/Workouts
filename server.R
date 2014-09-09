library(shiny)
require(rCharts)
library(reshape2)
source('workout.R')

options(RCHART_WIDTH = 900)

shinyServer(function(input, output) {
    output$mychart <- renderChart({
        p <- generate_dPlot_month()
        p$set(dom="mychart")
        return(p)
    })
})
