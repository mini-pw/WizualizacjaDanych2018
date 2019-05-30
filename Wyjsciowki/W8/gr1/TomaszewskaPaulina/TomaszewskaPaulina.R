library(shiny)
#install.packages('r2d3')
library(r2d3)
r2d3(
  data = c (0.3, 0.6, 0.8, 0.95, 0.40, 0.20),
  script = system.file("examples/barchart.js", package = "r2d3")
)



ui <- fluidPage(
  
  titlePanel("Losowowanie"),
  numericInput("in","Liczba losowań:", 10, min = 1, max = 100),
  h2("barplot"),
  d3Output("random_plot", height = 600)

)

server <- function(input, output) {
  output[["random_plot"]] <- renderD3({
    r2d3(
      data = runif(input[['in']]),
      script = "./r2d3-example.js"
    )
  })
  
}

shinyApp(ui = ui, server = server)