library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_diet",
                         label = "Select diets:",
                         choices = unique(ChickWeight[["Diet"]]),
                         selected = unique(ChickWeight[["Diet"]])),
      
      sliderInput(inputId = "time_frame",
                  label = "Select time frame:",
                  min = min(ChickWeight[['Time']]),
                  max = max(ChickWeight[['Time']]),
                  value = c(min(ChickWeight[['Time']]), max(ChickWeight[['Time']]))
                  )
    ),
    
    mainPanel(
      h2("Scatterplot"),
      plotOutput("chickweight_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  chick_r <- reactive({
    
    filter(ChickWeight, Diet %in% input[["chosen_diet"]] & Time %in% min(input[["time_frame"]]):max(input[["time_frame"]]))
  })
  
  output[["chickweight_plot"]] <- renderPlot({
    p <- ggplot(chick_r(), aes(x = Time, y = weight, color = Diet)) +
      geom_point() +
      theme_bw()
    
    p
  })
}

shinyApp(ui = ui, server = server)
