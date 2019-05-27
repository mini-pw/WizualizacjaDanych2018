library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simpler Shiny App"),
  
  sliderInput(inputId = "Num", label = "Number", min=1, max=30, value=6),
  
  h2("Scatterplot"),
  d3Output("Plot", height = 600)
)

server <- function(input, output) {
  
  n <- reactive(input[["Num"]])
  
  numbers <- reactive({
    round(runif(input$Num),3)
  })

  output[["plot"]] <- renderD3({
    r2d3(data = numbers(),
         script = "r2d3-example.js")
  })
}

shinyApp(ui = ui, server = server)
