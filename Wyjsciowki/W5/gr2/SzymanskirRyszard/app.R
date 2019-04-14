library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("ChickWeight explorator"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_diets",
                         label = "Select chicken diet:",
                         choices = unique(ChickWeight$Diet),
                         selected = unique(ChickWeight$Diet)),
      sliderInput(inputId = "chosen_time_interval",
                  label = "Select time interval",
                  min = min(ChickWeight$Time),
                  max = max(ChickWeight$Time),
                  value = c(0, 21))
    ),
    
    mainPanel(
      h2("Chick weight plot"),
      plotOutput("chick_weight_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  diet_colors <- c("1"= "red", "2" = "green", "3" = "orange", "4" = "black")
  
  selected_chick_weights <- reactive({
    filter(ChickWeight,
           Diet %in% input$chosen_diets &
            Time >= input$chosen_time_interval[1] &
             Time <= input$chosen_time_interval[2])
  })
  
  output[["chick_weight_plot"]] <- renderPlot({
    p <- ggplot(selected_chick_weights(),
                aes(x = Time, y = weight, color = Diet)) +
      geom_point() +
      theme_bw()
    
    p
  })
  
}

shinyApp(ui = ui, server = server)