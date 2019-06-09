library(shiny)
library(SmarterPoland)
library(dplyr)
library(r2d3)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("obs", "Select numbers count:", 10, min = 1, max = 100),
      verbatimTextOutput("value")
    ),
    
    mainPanel(
      h2("Histogram"),
      d3Output("hist")
    )
  )
)

server <- function(input, output) {
  
  number_r <- reactive({
     round(runif(input[["obs"]], 0, 1), 2)
  })
  
  
  output[["hist"]] <- renderD3({
    r2d3(
      data = number_r(),
      script = "~/Desktop/WizualizacjaDanych2018/Materiały/S13/r2d3-example.js"
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
