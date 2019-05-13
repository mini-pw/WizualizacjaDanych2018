library(shiny)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen",
                 label = "Select Data:",
                 choices = names(airquality)[1:4],
                 selected = names(airquality)[1])
    ),
    
    mainPanel(
      h2("Selected feature"),
      plotOutput("plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  airquality_r <- reactive({
    
    filter(airquality, airquality %in% input[["selected"]]) 
  })
  
  output[["plot"]] <- renderPlot({
    p <- ggplot(airquality, aes_string(x = "Month", y = input[["chosen"]])) +
      geom_count() +
      theme_bw()
    p
  })
  
}

shinyApp(ui = ui, server = server)


