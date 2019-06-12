library(shiny)
library(r2d3)

ui <- fluidPage(
  inputPanel(
    numericInput("num", 
                 h3("Numeric input"), 
                 value = 4) 
  ),
  d3Output("d3")
)

server <- function(input, output) {
  output$d3 <- renderD3({
    r2d3(
      data = round(runif(input$num, 0, 1), digits = 2),
      #script = system.file("examples/baranims.js", package = "r2d3")
      script = "./r2d3-example.js"
    )
  })
}

shinyApp(ui = ui, server = server)
