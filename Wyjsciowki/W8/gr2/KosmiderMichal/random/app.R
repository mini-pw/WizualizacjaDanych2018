library(shiny)
library(r2d3)

ui <- fluidPage(
  inputPanel(
    sliderInput("bar_max", label = "Max:",
                min = 0, max = 10, value = 3, step = 1)
  ),
  d3Output("d3")
)

server <- function(input, output) {
  output$d3 <- renderD3({
    r2d3(
      round(runif(input$bar_max), digits = 2),
      script = "./r2d3-example.js")
  })
}

shinyApp(ui = ui, server = server)
