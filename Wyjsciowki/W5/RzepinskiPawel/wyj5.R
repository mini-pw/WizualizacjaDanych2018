library(shiny)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Wyjsciowka 5 - Pawel Rzepinski"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_diet",
                         label = "Select diet:",
                         choices = unique(ChickWeight[["Diet"]]),
                         selected = unique(ChickWeight[["Diet"]])),
      sliderInput(inputId = "chosen_time",
                  label = "Select time range:",
                  value = c(min(ChickWeight[["Time"]]), max(ChickWeight[["Time"]])),
                  min = min(ChickWeight[["Time"]]),
                  max = max(ChickWeight[["Time"]]))
    ),
    
    mainPanel(
      h2("Scatterplot"),
      plotOutput("chickens_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  chicken_r <- reactive({
    filter(ChickWeight, Diet %in% input[["chosen_diet"]]) %>% 
      filter(Time >= input[["chosen_time"]][1], Time <= input[["chosen_time"]][2])
  })
  
  output[["chickens_plot"]] <- renderPlot({
    p <- ggplot(chicken_r(), aes(x = Time, y = weight, color = Diet)) +
      geom_point() +
      theme_bw()
    
    p
  })
  
}

shinyApp(ui = ui, server = server)
