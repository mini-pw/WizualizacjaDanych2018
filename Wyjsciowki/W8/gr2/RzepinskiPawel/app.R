library(shiny)
library(dplyr)
library(r2d3)

ui <- fluidPage(
  
  titlePanel("Random vectors - Paweł Rzepiński"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("bars_count", "Number of bars:",
                  min = 1, max = 10, value = 5
      )
    ),
    mainPanel(
      h2("Barplot"),
      d3Output("bar_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  output[["bar_plot"]] <- renderD3({
    data_vec <- round(runif(input[["bars_count"]]), 2)
    r2d3(data = data_vec,
         script = "./r2d3-example.js")
  })
  
  
}

shinyApp(ui = ui, server = server)