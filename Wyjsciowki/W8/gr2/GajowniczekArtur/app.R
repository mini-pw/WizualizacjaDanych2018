library(shiny)
library(r2d3)

ui <- fluidPage(
  inputPanel(
    sliderInput("n_bars", label = "Number of bars:",
                min = 2, max = 50, value = 1, step = 1)
  ),
  d3Output("d3")
)

server <- function(input, output) {
  output$d3 <- renderD3({
    r2d3(
      round(runif(input$n_bars), 2),
      script = "./r2d3-example.js")
  })
}

shinyApp(ui = ui, server = server)