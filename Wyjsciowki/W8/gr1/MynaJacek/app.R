library(shiny)
library(reshape2)
library(dplyr)
library(ggplot2)
library(r2d3)

ui <- fluidPage(
    sidebarPanel(
      sliderInput("bars", "Number of bars:",
                  min = 0, max = 10, value = 5
      )
    ),
    mainPanel (
      d3Output(outputId = "distPlot")
    )
)

server <- function(input, output) {
  
  output$distPlot <- renderD3({
    r2d3(data = runif(input[["bars"]], 0, 1), script = "./r2d3-example.js")
  })
  
}

shinyApp(ui = ui, server = server)