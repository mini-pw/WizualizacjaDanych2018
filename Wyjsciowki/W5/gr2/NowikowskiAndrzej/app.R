# Na podstawie danych ChickWeight stwórz aplikację shiny, \
# która wyświetla zależność wagi kurczaka (oś Y) od czasu (oś X). 
# Wykorzystaj sliderInput do wyboru przedziału czasu i 
# checkboxGroupInput do wyboru diety.


library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simple Shiny Chicken App"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_diet",
                         label = "Select diet: ",
                         choices = unique(ChickWeight[["Diet"]]),
                         selected = unique(ChickWeight[["Diet"]])),
      sliderInput(inputId = "time_range", "Time range(in days): ",
                  min = 1, max = 21,
                  value = c(1,21))
    ),
    
    mainPanel(
      h2("Scatterplot"),
      plotOutput("chicken_plot", height = 600, brush = "chicken_brush")
    )
  )
)

server <- function(input, output) {
  
  chicken_r <- reactive({
    filter(ChickWeight, 
           Diet %in% input[["chosen_diet"]] &
           Time >= min(input[["time_range"]]) & Time <= max(input[["time_range"]]))
  })
  
  output[["chicken_plot"]] <- renderPlot({
    p <- ggplot(chicken_r(), aes(x = Time, y = weight, color = Diet)) +
      geom_point() +
      theme_bw()
    
    p
  })
  
}

shinyApp(ui = ui, server = server)
