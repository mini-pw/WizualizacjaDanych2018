library(shiny)
library(r2d3)
ui <- fluidPage(
  
  titlePanel("Ostatnia wyjściówka!"),
  mainPanel(
      sliderInput("n_values", "Ile liczb",
                  1,100, 10),
      d3Output("d3")
      
    )
  )


server <- function(input, output) {
  
  data <- reactive(runif(input$n_values,0,1))
  output$d3 <- renderD3({
    r2d3(
      data = data(),
      script = ("r2d3-example.js")
    )
  })
  }

shinyApp(ui = ui, server = server)