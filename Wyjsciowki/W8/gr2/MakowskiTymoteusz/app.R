library(shiny)
library(r2d3)

ui <- fluidPage(
  
  titlePanel("Simpler Shiny App"),
  
  h2("Scatterplot"),
  # plotOutput("countries_plot", height = 600, click = "country_click"),
  # verbatimTextOutput("plot_value")
  sliderInput(
      "n_bar", "Bars:",
      min = 1,
      max = 50,
      value = 6
  ),
  d3Output("d3")
)

server <- function(input, output) {

    dane <- reactive({
        runif(input[["n_bar"]], 0, 1)
    })
    
    output[["d3"]] <- renderD3({
        r2d3(data = round(dane(), 2), script = "./r2d3-example.js")
    })
    
}

shinyApp(ui = ui, server = server)
