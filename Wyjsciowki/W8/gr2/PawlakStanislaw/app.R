library(shiny)
library(SmarterPoland)
library(dplyr)
library(r2d3)

ui <- fluidPage(
  
  titlePanel("Simpler Shiny App"),
  numericInput("num", label = h3("Numeric input"), value = 6),
  h2("Bar plot"),
  d3Output("plot", height = 600),
  plotOutput("countries_plot", height = 600, click = "country_click"),
  verbatimTextOutput("plot_value")
)

server <- function(input, output) {
  
  output[["plot"]] <- renderD3(r2d3(data = round(runif( input$num), 2),
                                    script = "r2d3-example.js"))
}

shinyApp(ui = ui, server = server)

