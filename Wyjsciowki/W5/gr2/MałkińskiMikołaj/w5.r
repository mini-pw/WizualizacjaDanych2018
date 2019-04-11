library(shiny)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Chick weight analysis"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_diet",
                         label = "Select diet names:",
                         choices = unique(ChickWeight[["Diet"]]),
                         selected = unique(ChickWeight[["Diet"]])),
      sliderInput(inputId = "chosen_time_range",
                  label = "Select time range:",
                  min = min(ChickWeight[["Time"]]),
                  max = max(ChickWeight[["Time"]]),
                  value = c(min(ChickWeight[["Time"]]), max(ChickWeight[["Time"]])))
      ),
    
    mainPanel(
      h2("Scatterplot"),
      plotOutput("chick_weight_plot", height = 600, brush = "diet_brush")
    )
  )
)

server <- function(input, output) {
  
  diet_colors <- c('1' = "red", '2' = "green", '3' = "orange", '4' = "blue")
  
  chick_weight_r <- reactive({
    filter(ChickWeight, Diet %in% input[["chosen_diet"]],
                        Time > min(input[["chosen_time_range"]]),
                        Time < max(input[["chosen_time_range"]]))
  })
  
  output[["chick_weight_plot"]] <- renderPlot({
    p <- ggplot(chick_weight_r(), aes(x = Time, y = weight, color = Diet)) +
      geom_point() +
      scale_color_manual(values = diet_colors[input[["chosen_diet"]]]) +
      theme_bw()
    p
  })
}

shinyApp(ui = ui, server = server)
