library(shiny)
library(r2d3)

ui <- fluidPage(
  
  titlePanel("D3 App"),
    sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "slider",
                  label = "Give numer",
                  min = 1,
                  max = 50,
                  value = 5)
      
    ),
    
    mainPanel(
      d3Output(outputId = "d3")
    )
  )
)

server <- function(input, output) {
  
    output$d3 <- renderD3({
      r2d3(
        data = round(runif(input$slider), 2),
        script = "r2d3-example.js"
      )
    })
    
}

shinyApp(ui = ui, server = server)