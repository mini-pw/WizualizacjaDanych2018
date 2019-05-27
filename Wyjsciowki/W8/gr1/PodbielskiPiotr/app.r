library(shiny)
library(r2d3)



ui <- fluidPage(
  titlePanel("Piotr Podbielski's Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider", label = h3("Choose n"), min = 5, 
                  max = 30, value = 20)
    ),
    mainPanel(
      h2("Plot"),
      d3Output("plot", height = 800)
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderD3({
    r2d3(
      data = round(runif(input$slider), 4),
      script = "./r2d3-example.js"
    )
  })
}

shinyApp(ui = ui, server = server)
