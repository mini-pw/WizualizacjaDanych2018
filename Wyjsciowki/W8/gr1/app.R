library(shiny)
library(dplyr)
library(r2d3)

ui <- fluidPage(
  
  titlePanel("wyjsciowka 8"),
  sliderInput(inputId = "n", label = "select n", min = 1, max = 100, value = 10, step = 1),
  d3Output("js_plot", height = 600)
)

server <- function(input, output) {
  
  
  
  output[["js_plot"]] <- renderD3({
    n <- input[['n']]
    r2d3(
      data <- round(runif(n),2),
      script = "D:\\mgr\\wizualizacja_danych\\WizualizacjaDanych2018\\Wyjsciowki\\W8\\gr1\\r2d3-example.js"
    )
  })
  
}

shinyApp(ui = ui, server = server)
