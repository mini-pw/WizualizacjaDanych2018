
library(shiny)
library(dplyr)
library(ggplot2)

head(ChickWeight)
minTime <- max(ChickWeight['Time'])
maxTime <- min(ChickWeight['Time'])

ui <- fluidPage(
  
  titlePanel("Simple Chicken App"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "chosen_time_range",
                  label = "Select time range",
                  min=min(ChickWeight['Time']),
                  max=max(ChickWeight['Time']),
                  value=c(min(ChickWeight['Time']), max(ChickWeight['Time']))),
      checkboxGroupInput(inputId = "chosen_diet",
                         label = "Select diets:",
                         choices = unique(ChickWeight[["Diet"]]),
                         selected = unique(ChickWeight[["Diet"]]))
      
    ),
    
    mainPanel(
      h2("Scatterplot"),
      plotOutput("chick_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  continent_colors <- c(Asia = "red", Europe = "green", Africa = "orange", Americas = "black", 
                        Oceania = "blue")
  
  chicks_r <- reactive({
    filter(ChickWeight, Diet %in% input[["chosen_diet"]]) %>% 
      filter(input[["chosen_time_range"]][1] < Time & Time < input[["chosen_time_range"]][2])
  })
  
  output[["chick_plot"]] <- renderPlot({
    validate(need(nrow(chicks_r()) > 0, 'Nothing to show with this set of parameters'))
    ggplot(chicks_r(), aes(x = Time, y = weight, color = Diet, group = Chick)) +
      geom_point() +
      geom_line(alpha = 0.2) +
      theme_bw()
  })
  
}

shinyApp(ui = ui, server = server)

